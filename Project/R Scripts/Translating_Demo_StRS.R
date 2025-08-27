##This script is translated from a .SAS script from Dione given to Nicole K. on 1/2025.
# rfds1_FLK2022_MIR_NCRMP_adcorabd
library(tidyverse)
##It is responsible for creating the Florida Keys Adult coral abundance data in 2022
## with NCRMP, DRM and MIR surveys
##Other notes from Dione:
  # StRS one-stage design estimation
  # coral density-size-cond data
  # stratum-specific analysis: dens,pa,abund
  # domain-wide weighted estimates: dens, pa, abund
  # Estimates to compare inside and outside MIR areas

#Step 1: Import the data.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#The following dataset was created using the "Translating_Demo_Data.R"
df <- read_csv("../data/FK2024_NCRMP_DRM_MIR_corsz_AR_NK.csv") %>%
  filter(!(analysis_group == "NCRMP_NA" )) %>%
  filter(!(strat_cora == "CFK04" & YEAR == 2022)) ##FLAG. Don't quite understand why we're doing this but we did it in LPI data and Dione has it in her script. Will follow up

## ntot given by Dione, not sure why mtot = 250
ntot <- read_csv("../data/ncrmp_mir_2022_coral_ntot.csv") %>%
  dplyr::mutate(mtot = 250) %>%
  dplyr::rename(ntot = nht)

ntot <- full_join(
  ntot %>%
    dplyr::mutate(YEAR = 2022) %>%
    dplyr::filter(strat_cora != "CFK04"),
  ntot %>%
    dplyr::mutate(YEAR = 2024)
)

##STEP 2: Calculate Species-Specific Density and Filter for Target Species
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Data was already transposed earlier so we don't need to do it here like Dione did

#Summarize Total Number of Colonies by Species/PSU, and calculate density
tot_colonies <- df %>% group_by (analysis_group, across(YEAR:SPECIES_CD)) %>%
  summarise(n_colonies = sum(N)) %>%
  mutate(density = n_colonies/METERS_COMPLETED)

#Filter for target species
target_species <- c("ACR PALM", "ACR CERV", "COL NATA", "DEN CYLI",
                    "DIC STOK", "DIP LABY", "EUS_ AST", "MEA MEAN",
                    "PSE CLIV", "PSE STRI", "MON CAVE")

#Add column of presence for proportional estimates
filt_tot_colonies <-  tot_colonies %>% filter(SPECIES_CD %in% target_species) %>%
  mutate(present = ifelse(n_colonies > 0, 1, 0))

####Step 3: Stratum-Level Density Estimates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
dens_estimates <- filt_tot_colonies %>%
  group_by(analysis_group, strat_cora, SPECIES_CD, YEAR) %>%
  summarise(avdens = mean(density, na.rm = TRUE),
            s1 = var(density, na.rm = TRUE),
            n = n()) %>%
  ##add ntot for each strat
  left_join(., ntot) %>%
  ## summarise ngrtot avaliable ntot on each strat/analysis group
  left_join(., ntot %>% group_by(analysis_group, YEAR) %>%
              summarise(ngrtot = sum(ntot))) %>%
  ## weighted
  mutate(wh = ntot/ngrtot)


##Dione's Version....
# d1 <- filt_tot_colonies %>%
#   group_by(analysis_group, strat_cora, SPECIES_CD) %>%
#   summarise(avdens = mean(n_colonies, na.rm = TRUE),
#             s1 = var(n_colonies, na.rm = TRUE),
#             n = n())
#
# n4 <- ntot %>%
#   mutate(mrgkey = ifelse(analysis_group == "MIR_GRP", 1, 2))
#
# d2 <- d1 %>%
#   left_join(n4, by = c("analysis_group", "strat_cora")) %>%
#   mutate(mrgkey = ifelse(analysis_group == "MIR_GRP", 1, 2))
#
# n6 <- d2 %>%
#   group_by(mrgkey) %>% ##FLAG. Should it be group by mergkey AND strat_cora???
#   summarise(ngrtot = sum(ntot), .groups = "drop")
#
# d3 <- d2 %>%
#   left_join(n6, by = "mrgkey") %>%
#   mutate(wh = ntot / ngrtot)

##Resume (where d3 == dens_estimates)

##Stratum-Specific Estimates -- straight transcription of math from DS
dens_estimates <- dens_estimates %>%
  mutate(
    fn = n / ntot,
    vbar_dns = ((1 - fn) * s1 / n),
    se_dns = sqrt(vbar_dns),
    cv_dns = ifelse(avdens != 0, (se_dns / avdens) * 100, 0),
    nmtot = ntot * mtot,
    th = 10,
    abd = nmtot * avdens * th,
    vbar_abd = (ntot^2) * vbar_dns * th^2,
    se_abd = sqrt(vbar_abd),
    wavdns = wh * avdens,
    wvbar_dns = (wh^2) * vbar_dns
  )

#Proportion Presence Estimates -- straight transcription of math from DS
prop_estimates <- filt_tot_colonies %>%
  group_by(analysis_group, strat_cora, SPECIES_CD, YEAR) %>%
  summarise(
    n = n(),
    smpres = sum(present)) %>%
  mutate(
    avp = smpres / n,
    var_avp = (n / (n - 1)) * avp * (1 - avp),
    s1 = var_avp
  ) %>%
  left_join(., ntot, by = c("analysis_group", "strat_cora", "YEAR")) %>%
  mutate(
    fn = n / ntot,
    vbar_p = (1 - fn) * s1 / n,
    se_p = sqrt(vbar_p)
  ) %>%
  select(SPECIES_CD, analysis_group, strat_cora, avp, se_p, vbar_p, YEAR)

##Combine density and proportion estimates
AR_estimates <- left_join(dens_estimates, prop_estimates) %>%
  mutate(
    wavp = wh * avp,
    wvbar_p = wh^2 * vbar_p)

#Same questions as before, why no select weighted average densities? because they're stratum estimates?

write_csv(AR_estimates %>% #columns used in the end selected by DS
            select(YEAR, SPECIES_CD, analysis_group, strat_cora, ntot, n, avdens, se_dns, cv_dns, avp, se_p, abd, se_abd)%>%
            # rounding for nicer csv
            mutate(across(c(avdens, se_abd), round, 5)),
          "../data/fk2022_24_NCRMP_MIR_corabd_strat_NK.csv")

####Step 4: Domain-Level Density Estimates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#straight transcription of math from DS
AR_domain_estimates <- AR_estimates %>%
  group_by(analysis_group, SPECIES_CD, YEAR) %>%
  summarise(
    abd = sum(abd),
    vbar_abd = sum(vbar_abd),
    wavdns = sum(wavdns),
    wvbar_dns = sum(wvbar_dns),
    wavp = sum(wavp),
    wvbar_p = sum(wvbar_p),
    n = sum(n),
    nstrat = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se_abd = sqrt(vbar_abd),
    se_wdns = sqrt(wvbar_dns),
    wcv_dns = ifelse(wavdns != 0, (se_wdns / wavdns) * 100, 0),
    se_wp = sqrt(wvbar_p)
  )
write_csv(AR_domain_estimates %>% #columns used in the end selected by DS
            select(YEAR, SPECIES_CD, analysis_group, nstrat, n, wavdns, se_wdns, wcv_dns, wavp, se_wp, abd, se_abd),
  "fk2022_24_NCRMP_MIR_corabd_dom_NK.csv")


#Step 5: Compare NK's outputs to Dione's... they won't be the exact same because of random selection of DRM's transects. Does MIR have DRM data?
stratum <- read_csv("../data/fk2022_NCRMP_MIR_corabd_strat.csv")
domain <- read_csv("../data/fk2022_NCRMP_MIR_corabd_dom.csv")

