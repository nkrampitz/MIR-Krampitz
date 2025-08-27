##This script is translated from a .SAS script from Dione given to Nicole K. on 3/2025.
# rfds1_fk2022_MIR_NCRMP_bcover.sas

##It is responsible for creating the Florida Keys LPI data in 2022
## with NCRMP, DRM and MIR surveys
##Other notes from Dione:
# StRS one-stage design estimation
# coral density-size-cond data
# stratum-specific analysis: dens,pa,abund
# domain-wide weighted estimates: dens, pa, abund
# Estimates to compare inside and outside MIR areas
library(tidyverse)
setwd("/Users/juliamateski/Downloads/MIR/Project")
#Step 1: Import the data, calculate proportional cover
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- read_csv("data/FK2022_24_MIR_NCRMP_bcov_dat1_NK.csv")%>%
 mutate(prp_cov = cat_p / LPI_total)



# df <- read_csv( "/Users/juliamateski/Desktop/dat4.csv")
## ntot given by Dione, not sure why mtot = 250. Need to take out CFK04 (only for 2022) because no inside outside
ntot <- read_csv("data/ncrmp_mir_2022_coral_ntot.csv") %>%
  dplyr::mutate(mtot = 250) %>%
  dplyr::rename(ntot = nht)

ntot <- full_join(
  ntot %>%
    dplyr::mutate(YEAR = 2022) %>%
    dplyr::filter(strat_cora != "CFK04"),
  ntot %>%
    dplyr::mutate(YEAR = 2024)
)


#ntot <- read_csv("/Users/juliamateski/Desktop/test_ntot.csv")


####Step 2: Stratum-Level Density Estimates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#straight transcription of math from DS
#weighting

dens_estimates <-  df %>%
  group_by(analysis_group, strat_cora, YEAR, COVER_CAT_CD) %>%
  summarise(
    n = n(),
    smprp = sum(prp_cov, na.rm = TRUE)
  ) %>%
  mutate(
    avp = smprp / n,
    var_avp = (n / (n - 1)) * avp * (1 - avp),
    s1 = var_avp
  ) %>%
  ##add ntot for each strat
  left_join(., ntot) %>%
  ## summarise ngrtot avaliable ntot on each strat/analysis group
  left_join(., ntot %>% group_by(analysis_group) %>%
              summarise(ngrtot = sum(ntot))) %>%
  ## weighted
  mutate(wh = ntot/ngrtot)



dens_estimates <- dens_estimates %>%
  mutate(
    wh = ntot / ngrtot,
    fn = n / ntot,
    vbar_p = (1 - fn) * s1 / n,
    se_p = sqrt(vbar_p),
    wavp = wh * avp,
    wvbar_p = wh^2 * vbar_p
  )

#save as a .csv
write_csv(dens_estimates %>% #categories Dione selected
  select(COVER_CAT_CD, analysis_group, strat_cora, ntot, n, avp, se_p) %>%   # rounding for nicer csv
    mutate(across(c(avp, se_p), round, 5)),
  "../data/fk2022_24_NCRMP_MIR_bcov_catcd_strat_NK.csv")


####Step 3: Domain-Level Density Estimates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#math transcribed directly from DS

# domain_estimates <- dens_estimates %>%
#   group_by(analysis_group, YEAR, COVER_CAT_CD) %>%
#   summarise(
#     avCvr = sum(wavp),
#     Var = sum(wvbar_p),
#     n_sites = sum(n),
#     n_strat = n()
#   ) %>%
#   mutate(
#     SE = sqrt(Var)
#   ) %>%
#   mutate(across(c(avCvr, SE), round, 5))


domain_estimates <- dens_estimates %>%
  group_by(analysis_group, YEAR, COVER_CAT_CD) %>%
  summarise(
    wavp = sum(wavp),
    wvbar_p = sum(wvbar_p),
    n = sum(n),
    nstrat = n()
  ) %>%
  mutate(
    se_wp = sqrt(wvbar_p)
  ) %>%
  mutate(across(c(wavp, se_wp), round, 5))



#save as a .csv
write_csv(domain_estimates %>%
            select(COVER_CAT_CD, analysis_group, nstrat, n, wavp, se_wp, YEAR),
          "data/fk2022_24_NCRMP_MIR_bcov_catcd_dom_NK.csv")

# ###Step 4: Compare to Dione's
stratum <- read_csv("data/fk2022_NCRMP_MIR_bcov_catcd_strat.csv")
domain <- read_csv("data/fk2022_NCRMP_MIR_bcov_catcd_dom.csv")





