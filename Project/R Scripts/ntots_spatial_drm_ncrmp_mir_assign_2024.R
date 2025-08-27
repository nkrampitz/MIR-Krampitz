###Creation of thet NTOT/Raw Data for MIR data in 2024

##Nicole Krampitz
# May 16th, 2025

##First step -- plot all the DRM, NCRMP(FLK), and MIR points overlapping the MIR shapefile
##Write down all of the points for DRM/NCRMP that fall on (or very near) that shapepoint. Manually rewrite these points to an analysis group of MIR

##Survey Type: Who took the survey (NCRMP/DRM/MIR)

##Analysis Group (NCRMP_GRP (area outside MIR), MIR (area inside MIR shapefiles), NCRMP_NA (too deep (>30 feet)))
library(tidyverse)

##From the NCRMP group
MIR_override_NCRMP <- c("1375", "1399", "1305", "1298", "1255", "1343")

##Want equivalent of this for 2024 to use as a reference
reference <- read.csv("../data/NCRMP_MIR_cora_2022_psu_grps.csv")

##NCRMP/MIR data combined
df <- read.csv("../data/FLK_MIR_2024_benthic_boatlog.csv") %>%
###I don't love the original column names, but well revert to them here early on as to not cause chaos down the line I guess
dplyr::rename(strat_cora = STRAT,
              RUGOSITY_CD = RUG_STRAT,
              DEPTH_STRAT = DEPTH_STRAT_Coral) %>%
  dplyr::mutate(
    survey_type = ifelse(PRIMARY_SAMPLE_UNIT >= 8000 , "MIR", "NCRMP"))

##Overriding ones outside and creating analysis group
tmp <- df %>%
  dplyr::mutate(
    analysis_group = dplyr::case_when(
      DEPTH_STRAT %in% c("CD3", "CD4") ~ "NCRMP_NA", ##takes out all the sites that are deeper than we're working with
      PRIMARY_SAMPLE_UNIT %in% MIR_override_NCRMP ~ "MIR_GRP", #All the NCRMP sites that fell on MIR reef go to MIR
      survey_type == "MIR" ~ "MIR_GRP",
      survey_type == "NCRMP" ~ "NCRMP_GRP",
      TRUE ~ NA_character_
    )
  )

tmp <- full_join(tmp, reference)

##Writing the .csv for NCRMP_MIR_cora_2022_psu_grps
#write_csv(tmp, "../data/NCRMP_MIR_cora_2022_24_psu_grps.csv")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
##DRM Version
##From the DRM group the following 'PSU's are in the MIR group and should be changed to MIR
MIR_override_DRM <- c("AA1311", "AA1316", "AA1312", "AA1313", "AA1314", "AA1315", "AA1462", "AA1466", "AA2452", "AA1515", "AA1516", "AA1517", "AA3107", "AA1425", "AA1426", "AA1427", "AA1428", "AA3024")


##Want equivalent of this for 2024 to use as a reference
reference <- read.csv("../data/DRM_cora_2022_psu_grps.csv")
colnames(reference)
#DRM data
df <- read.csv("../data/DRM_FLK_2014_2024_1stage_coral_demographics.csv") %>%
  ###I don't love the original column names, but well revert to them here early on as to not cause chaos down the line I guess
  select(REGION:RUGOSITY_CD, LAT_DEGREES:MAPGRID_NR, DEPTH_STRAT) %>%
  dplyr::rename(strat_cora = STRAT) %>%
  filter(YEAR == 2024) %>%
  unique()

    ##Overriding ones outside and creating analysis group
    tmp <- df %>%
      dplyr::mutate(
        survey_type = "DRM",
        analysis_group = dplyr::case_when(
          DEPTH_STRAT %in% c("CD3", "CD4") ~ "NCRMP_NA", ##takes out all the sites that are deeper than we're working with
          PRIMARY_SAMPLE_UNIT %in% MIR_override_DRM ~ "MIR_GRP", #All the NCRMP sites that fell on MIR reef go to MIR
          TRUE ~ "NCRMP_GRP"
        )
      )


 tmp <- full_join(tmp, reference)

##Writing the .csv for NCRMP_MIR_cora_2022_psu_grps
   # write_csv(tmp, "../data/DRM_cora_2022_24_psu_grps.csv")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
    #Summarization of each group and their nhs
##Want equivalent of this for 2024 to use as a reference
ncrmp <- read.csv("../data/NCRMP_MIR_cora_2022_24_psu_grps.csv") %>%
  mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))
drm <- read.csv("../data/DRM_cora_2022_24_psu_grps.csv") %>%
  mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

tmp <- full_join(ncrmp, drm) %>%
  group_by(strat_cora, analysis_group, survey_type, YEAR) %>%
  summarise(nh = n())

write_csv(tmp, "../data/NCRMP_DRM_MIR_benthic_sample_nhv2_24.csv")

