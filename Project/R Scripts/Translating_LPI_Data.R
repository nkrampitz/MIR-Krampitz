##This script is translated from a .SAS script from Dione given to Nicole K. on 1/2025.
##Original Script was called: FK_NCRMP2022_bcov_dat1.sas

##The purpose of this script is to
#formats analysis ready data set for StRS estimation
#Benthic cover
#All applicable species and categories, FL Keys 2022

##Step 1: Import the Dataset/Libraries
library(tidyverse)
##For this, we are using the LPI benthic cover data from the FLK
##Sites 9020 and 9012 from MIR data have decimal points in original FKEYS dataset, this "_points" one has their total count instead of %
#MIR_FLK_2022_benthic_cover_DUMMY <- read_csv("../data/NCRMP_FKEYS2022_Benthic_Data01_BenthicCover_points.csv") %>%
df_22 <-  read_csv("../data/NCRMP_FKEYS2022_Benthic_Data01_BenthicCover_points.csv") %>%
 ##The following columns were removed from Dione's dataset
  select(-c(ADMIN, RUGOSITY_CD, WTD_RUG, SUB_REGION_NAME, ZONE_NAME,
         MPA_NAME, COVER_CAT_NAME))
#2024 data
df_24 <- read_csv("../data/FLK_MIR_2024_benthic_cover.csv") %>%
  select(-c(ADMIN, RUGOSITY_CD, WTD_RUG, SUB_REGION_NAME, ZONE_NAME,
            MPA_NAME, COVER_CAT_NAME))


df <- full_join(df_22, df_24)
view(df)

##Step 2: Add MIR psu group types and merge
psu_grps <- read_csv("../data/NCRMP_MIR_cora_2022_24_psu_grps.csv") %>%
  select(PRIMARY_SAMPLE_UNIT, survey_type, analysis_group, strat_cora, YEAR) #These are the columns Dione selected

df <- full_join(df, psu_grps, by = c("PRIMARY_SAMPLE_UNIT", "YEAR"))

view(df)
write_csv(df, "/Users/juliamateski/Desktop/TEST_DF.csv")
##Step 3: Create a sum of points for HB, SB, Rubble, and Total.
df <- df %>%
  mutate(
    cat_p = (HARDBOTTOM_P + SOFTBOTTOM_P + RUBBLE_P))

##Step 4: Transpose and fill with 0s
df_pivot <- df %>% pivot_wider(id_cols = c(REGION:METERS_COMPLETED, survey_type:strat_cora), names_from = COVER_CAT_CD, values_from = cat_p, values_fill = 0) %>%
  pivot_longer(cols="BAR SUB.":"DIP SPE.", names_to = "COVER_CAT_CD", values_to = "cat_p")

#Step 5: Merge with Category Codes. Seems to be just incorporating A and B cat codes?
##In SAS, Dione had to rename something weird with COL NATA column, but that doesn't seem to be an issue in R
cat_codes <- read_csv("../data/bcov_cat_codes.csv") %>%
  #Changing the name of the column from CODE to COVER_CAT_CD to merge later
  dplyr::rename(COVER_CAT_CD = CODE)

df <- left_join(df_pivot, cat_codes, by = "COVER_CAT_CD")

##Step 6: Add in total LPI Points. Prepare for exporting
df_AR <- df %>%
  group_by(PRIMARY_SAMPLE_UNIT, YEAR, analysis_group) %>%
  mutate(
    LPI_total = sum(cat_p)) %>%
  ##remove extraneous strata
  filter(analysis_group != "NCRMP_NA") %>%
  filter(!(strat_cora == "CFK04" & YEAR == 2022)) %>%
  ##select final columns and order for exporting
  select(analysis_group, survey_type, REGION, YEAR, PRIMARY_SAMPLE_UNIT, STATION_NR, SUB_REGION_NR, ZONE_NR, PROT, strat_cora, COV_CAT_A, COV_CAT_B, LPI_total, cat_p, COVER_CAT_CD)

##Step 7: Export as csv.
write_csv(df_AR, "../data/FK2022_24_MIR_NCRMP_bcov_dat1_NK.csv")

##Internal Step 7: Compare with Dione's .csv

##Wait to see what hers comes up with because transpose got messed up

