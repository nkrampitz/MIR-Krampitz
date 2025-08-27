###MIR_NCRMP_process_flow_demoAR

##Script to automate the process of getting the raw NCRMP, DRM, and MIR data and combining them into an analysis ready demo set

library(tidyverse)
setwd("/Users/juliamateski/Downloads/MIR/Project")

##########STEP 1: FK_NCRMP2022_adcorpsspp_dat1.sas##########
##First step, read in the datasets. They're located in a folder just outside the project called MIR.
NCRMP_FKEYS2022_Benthic_Data02_CoralDemographics <- read_csv("data/NCRMP_FKEYS2022_Benthic_Data02_CoralDemographics.csv")
#This has MIR data

NCRMP_FKEYS_24_CoralDemographics <- read.csv("data/FLK_MIR_2024_coral_demographics.csv")

#Combine
NCRMP_FKEYS_CoralDemographics <- full_join(NCRMP_FKEYS2022_Benthic_Data02_CoralDemographics, NCRMP_FKEYS_24_CoralDemographics)


##2022 and 2024 data. This comes from the spatial script
#NCRMP_MIR_cora_2022_psu_grps <- read_csv("data/NCRMP_MIR_cora_2022_psu_grps.csv")
NCRMP_MIR_cora_psu_grps <- read_csv("data/NCRMP_MIR_cora_2022_24_psu_grps.csv")


#Then we need to do the equivalent of FK_NCRMP2022_adcorpsspp_dat1.sas, which as I understand it is just joining the two datasets and filtering out some columns
tmp_NCRMP <- full_join(NCRMP_FKEYS_CoralDemographics, NCRMP_MIR_cora_psu_grps %>%
                         select(REGION, PRIMARY_SAMPLE_UNIT, analysis_group, survey_type, strat_cora)) %>%
  select(-c(STRAT, RUGOSITY_CD, WTD_RUG, SUB_REGION_NAME, ZONE_NAME, MPA_NAME, ADMIN, SPECIES_NAME)) ##weren't included in the dataset Dione gave


#########STEP 2a: Integrating the DRM data##################
###FK_DRM2022_adcorspp_dat1####
DRM_FLK_2014_2024_2stage_coral_demographics <- read_csv("data/DRM_FLK_2014_2024_2stage_coral_demographics.csv")
##ALSO line 41 of the original dataset given has an NA row
DRM_cora_psu_grps <- read_csv("data/DRM_cora_2022_24_psu_grps.csv")
#FK_DRM2022_PSU_STN_NEWSTN  <- read_csv("data/FK_DRM2022_PSU_STN_NEWSTN.csv")
#FK_DRM2022_PSU_NEWPSU <- read_csv("data/FK_DRM2022_PSU_NEWPSU.csv")

##Start with creating FK_DRM2022_adcorspp_dat1
tmp_DRM <- left_join(
  DRM_FLK_2014_2024_2stage_coral_demographics %>% filter (YEAR == 2022 | YEAR == 2024),
  DRM_cora_psu_grps %>% select(c(PRIMARY_SAMPLE_UNIT, strat_cora, survey_type, analysis_group))) %>%
      select(-c(STRAT, RUGOSITY_CD, WTD_RUG, SUB_REGION_NAME, ZONE_NAME, MPA_NAME, ADMIN, SPECIES_NAME)) ##weren't included in the dataset you gave me

#####STEP 2b:#####
#New Station Numbers, PSUs, and randomly selecting a station number

##Assigning the PSUs 1 & 2
#First step is creating a subset dataset with just PSU and Station for new station #s
NEW_STN <- tmp_DRM %>% select(PRIMARY_SAMPLE_UNIT, STATION_NR, YEAR) %>% distinct() %>%
  group_by(PRIMARY_SAMPLE_UNIT, YEAR) %>%
  mutate(NEW_STN = row_number())%>%
  ungroup()
##This will still work even if there's only one station or there's three etc.

##Then you want to add the new PSUs as a 9500 series? When did this occur? I think it's already in my data and I cannot undo
NEW_PSU <- NEW_STN %>% filter (NEW_STN == 1) %>%
  mutate(NEW_PSU = 9500 + row_number()) %>%
  select(-STATION_NR)

##Loop through NEW_STN and pick one randomly
##To translate my datasets to the same....
survey.domain <- full_join(NEW_STN, NEW_PSU %>% select(-NEW_STN))
allocation <- NEW_PSU
sample.allocation <- data.frame()

for(i in 1:length(allocation$NEW_PSU)) {
  temp <- subset(survey.domain, survey.domain$NEW_PSU == as.character(allocation$NEW_PSU)[i])
  sample <- temp[sample(nrow(temp), allocation$NEW_STN[i], replace = FALSE),]
  sample.allocation <- rbind(sample.allocation,sample)
}


####Helper Function that gets the Dataset ####
#this doesn't require constant updating of the datasets



#write_csv(sample.allocation, "data/fk_drm_su_select_2022_24_NK.csv")

###Moving forward, Dione had this random selection fk_drm_su_select_2022.csv

###STEP 3: Bring it All Together #####
##Filter the DRM data to only have the NEW_STN
DRM_select <- read_csv("data/fk_drm_su_select_2022_24_NK.csv")

write_csv(DRM_select, "/Users/juliamateski/Desktop/2024_DRM_ssu_for_dione.csv")

DRM_select <- DRM_select[, -1] #something weird about creating this via the loop and then saving it gives it another extraneous column
tmp_DRM <- left_join(DRM_select, tmp_DRM, by = c("PRIMARY_SAMPLE_UNIT", "STATION_NR", "YEAR"))
##Reformat the DRM data to have the same column names as NCRMP
tmp_DRM <- tmp_DRM %>%
  select(-c(PRIMARY_SAMPLE_UNIT, STATION_NR)) %>% #Removing the old names for PSU and Station for DRM
  dplyr:: rename(PRIMARY_SAMPLE_UNIT = "NEW_PSU", #putting the new PSU as regular psu
                 STATION_NR = "NEW_STN") #putting the new station as regular station
##Add in the NCRMP data
AR_demo <- bind_rows(tmp_DRM, tmp_NCRMP)

#Then remove all juveniles BUT still need to have those sites (aka can’t just filter J=1 out). So need to change the n=0 for any where juvenile = 1.
AR_demo <- AR_demo %>% mutate(
  N = case_when (
    JUV == 1 ~ 0,
    TRUE ~ N))

#Species list for DRM might be different than NCRMP. Therefore needs to pivot_wider then pivot_longer so that all the zeros for those corals exist on each DRM/NCRMP.
##WELL that doesn't work because duh there's more than one coral for each thing. Start with identifying if there are differences...

#Are there any differences in SPECIES_CD listed?

setdiff(tmp_DRM$SPECIES_CD, tmp_NCRMP$SPECIES_CD)

#Yes. These are in DRM but not NCRMP.
##"AGA SPE." "ISO SPE." "MYC FERO" "OCU SPE." "PHY AMER" "POR BRAN" "SCO SPE."
#2024 list: "AGA SPE." "MYC FERO" "MYC SPE." "POR BRAN" "ISO SPE." "PHY AMER"

##Are any in NCRMP but not DRM?
setdiff(tmp_NCRMP$SPECIES_CD, tmp_DRM$SPECIES_CD)
#Yes. These are in NCRMP but not DRM
#"ISO RIGI" "MAD FORM" "MAD SENA" "OCU DIFF" "POR SPE." "SCO LACE" "SID SPE." "ACR PALM"
#2024 list: "ISO RIGI" "MAD FORM" "SCO LACE" "ACR PALM" "MAD SPE." "ORB ANCX" "POR COLO" "PSE SPE."

#This dataset only has the corals and the Ns because if there are multiple of the same species, the flips freak
empty <- AR_demo %>%
  filter(JUV == 0) %>%
  select(c(YEAR:METERS_COMPLETED, SPECIES_CD, N, survey_type:strat_cora)) %>%
  distinct() %>%
  pivot_wider(
    id_cols=c(YEAR:METERS_COMPLETED, survey_type:strat_cora), names_from = SPECIES_CD, values_from = N, values_fill = 0) %>%
  pivot_longer(
    cols = `ACR CERV`:`PSE SPE.`, names_to = "SPECIES_CD", values_to = "N")

#This dataset does not have any juveniles, and does not have any coral health information, just the ID variables and the correct number of species per each. If we rejoin with the main dataset....
AR_ready_demo <- full_join(empty, AR_demo)
#then put 0s on JUV where there are NAs for when you added the new species
AR_ready_demo$JUV[is.na(AR_ready_demo$JUV)] <- 0

##Last step, compare with Dione's
Diones_Final <- read_csv("data/FK2022_NCRMP_DRM_MIR_corsz_AR.csv")

#I cannot compare her direct exit from this script because she did not do the transpose on her dataset. Instead I'll have to compare rows to a future mid-dataset. For right now go through the earlier steps with her selected station nrs and PSUS
write_csv(AR_ready_demo, "data/FK2024_NCRMP_DRM_MIR_corsz_AR_NK.csv")
#write_csv(AR_ready_demo, "FK2022_NCRMP_DRM_MIR_corsz_AR_NK.csv")


