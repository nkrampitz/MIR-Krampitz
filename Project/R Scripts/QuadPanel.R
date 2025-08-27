###Script to output the density, maximum diameter, and mortality estimates.

quad_panel_MIR <- function (target_species) {
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

##Density is already done in translating_DEMO_StRS.R, but the species are filtered so re-doing it here


######DENSITY MEASUREMENTS##############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

##STEP 2: Calculate Species-Specific Density and Filter for Target Species
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Data was already transposed earlier so we don't need to do it here like Dione did

#Summarize Total Number of Colonies by Species/PSU, and calculate density
tot_colonies <- df %>% group_by (analysis_group, across(YEAR:SPECIES_CD)) %>%
  summarise(n_colonies = sum(N)) %>%
  mutate(density = n_colonies/METERS_COMPLETED)

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

### Analysis Ready Formatting
AR_domain_estimates <- AR_domain_estimates %>%
  select(YEAR, SPECIES_CD, analysis_group, nstrat, n, wavdns, se_wdns, wcv_dns, wavp, se_wp, abd, se_abd)


######OLD MORTALITY##############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
paste("MORALITY NOW")
  old_mortality_site <- df %>%
    # filter to corals present, remove Marquesas and any corals not sampled for mortality
    dplyr::filter(N == 1,
                  OLD_MORT != "NA",
                  OLD_MORT <= 100) %>%
    # calculate site level mortality
    dplyr::group_by(REGION, analysis_group, YEAR, PRIMARY_SAMPLE_UNIT, strat_cora, SPECIES_CD) %>% #No need to include region, will be added from ntot in wh. function
    dplyr::summarise(avsitemort = mean(OLD_MORT), .groups = "keep") %>%
    dplyr::mutate(MORT_TYPE = "Old") %>%
    dplyr::ungroup() %>%
    # update some column classes to make them compatible with pre NCRMP data
    dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

  # Calculate avmort, svar, n and std
  mortality_est <- old_mortality_site %>%
    # group by analysis level strata
    dplyr::group_by(YEAR, analysis_group, strat_cora, MORT_TYPE, SPECIES_CD) %>% # Modify this line to changes analysis substrate
    dplyr::summarise(
      # compute average mortality
      avmort = mean(avsitemort),
      # compute stratum variance
      svar = var(avsitemort),
      n = length(unique(PRIMARY_SAMPLE_UNIT)), .groups = "keep") %>%
    # convert 0 for stratum variance so that the sqrt is a small # but not a 0
    dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                          TRUE ~ svar)) %>%
    dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                  std = sqrt(svar), # std dev of density in stratum
                  SE=sqrt(Var), #SE of the mean density in stratum
                  CV_perc=(SE/avmort)*100)

  mortality_est <- mortality_est %>%
    ##add ntot for each strat
    left_join(., ntot) %>%
    ## summarise ngrtot avaliable ntot on each strat/analysis group
    left_join(., ntot %>% group_by(analysis_group, YEAR) %>%
                summarise(ngrtot = sum(ntot))) %>%
    ## weighted
    mutate(wh = ntot/ngrtot) %>%
    # stratum estimates
    dplyr::mutate(whavmort = wh * avmort,
                  whvar = wh^2 * Var,
                  n = tidyr::replace_na(n, 0))  %>%
    dplyr::ungroup()

  ## Domain Estimates
  # region/population means
  Domain_est <- mortality_est %>%
    dplyr::group_by(analysis_group, YEAR, SPECIES_CD, MORT_TYPE) %>%
    dplyr::summarise(avMort = sum(whavmort, na.rm = T), # This accounts for strata with 0 species of interest present
                     Var = sum(whvar, na.rm = T),    # This accounts for strata with N = 1
                     SE=sqrt(Var),
                     CV_perc=(SE/avMort)*100,
                     n_sites = sum(n),
                     n_strat = length(unique(strat_cora)),
                     ngrtot = sum(ntot), .groups = "keep" )  %>%
    dplyr::ungroup() %>%
    filter(SPECIES_CD %in% target_species)


  ######RECENT MORTALITY##############
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#just straight repeat??
  rec_mortality_site <- df %>%
    # filter to corals present, remove Marquesas and any corals not sampled for mortality
    dplyr::filter(N == 1,
                  RECENT_MORT != "NA",
                  RECENT_MORT <= 100) %>%
    # calculate site level mortality
    dplyr::group_by(REGION, analysis_group, YEAR, PRIMARY_SAMPLE_UNIT, strat_cora, SPECIES_CD) %>% #No need to include region, will be added from ntot in wh. function
    dplyr::summarise(avsitemort = mean(RECENT_MORT), .groups = "keep") %>%
    dplyr::mutate(MORT_TYPE = "Recent") %>%
    dplyr::ungroup() %>%
    # update some column classes to make them compatible with pre NCRMP data
    dplyr::mutate(PRIMARY_SAMPLE_UNIT = as.factor(PRIMARY_SAMPLE_UNIT))

  # Calculate avmort, svar, n and std
  rec_mortality_est <- rec_mortality_site %>%
    # group by analysis level strata
    dplyr::group_by(YEAR, analysis_group, strat_cora, MORT_TYPE, SPECIES_CD) %>% # Modify this line to changes analysis substrate
    dplyr::summarise(
      # compute average mortality
      avmort = mean(avsitemort),
      # compute stratum variance
      svar = var(avsitemort),
      n = length(unique(PRIMARY_SAMPLE_UNIT)), .groups = "keep") %>%
    # convert 0 for stratum variance so that the sqrt is a small # but not a 0
    dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                          TRUE ~ svar)) %>%
    dplyr::mutate(Var=svar/n, #variance of mean density in stratum
                  std = sqrt(svar), # std dev of density in stratum
                  SE=sqrt(Var), #SE of the mean density in stratum
                  CV_perc=(SE/avmort)*100)

  rec_mortality_est <- rec_mortality_est %>%
    ##add ntot for each strat
    left_join(., ntot) %>%
    ## summarise ngrtot avaliable ntot on each strat/analysis group
    left_join(., ntot %>% group_by(analysis_group, YEAR) %>%
                summarise(ngrtot = sum(ntot))) %>%
    ## weighted
    mutate(wh = ntot/ngrtot) %>%
    # stratum estimates
    dplyr::mutate(whavmort = wh * avmort,
                  whvar = wh^2 * Var,
                  n = tidyr::replace_na(n, 0))  %>%
    dplyr::ungroup()

  ## Domain Estimates
  # region/population means
  rec_Domain_est <- rec_mortality_est %>%
    dplyr::group_by(analysis_group, YEAR, SPECIES_CD, MORT_TYPE) %>%
    dplyr::summarise(avMort = sum(whavmort, na.rm = T), # This accounts for strata with 0 species of interest present
                     Var = sum(whvar, na.rm = T),    # This accounts for strata with N = 1
                     SE=sqrt(Var),
                     CV_perc=(SE/avMort)*100,
                     n_sites = sum(n),
                     n_strat = length(unique(strat_cora)),
                     ngrtot = sum(ntot), .groups = "keep" )  %>%
    dplyr::ungroup() %>%
    filter(SPECIES_CD %in% target_species)

  ###Analysis Ready Data
  AR_mortality <- full_join(rec_Domain_est, Domain_est)


  ###### COLONY SIZE ESTIMATES ##############
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  AR_size <- df %>%
    mutate(total_mort = OLD_MORT + RECENT_MORT) %>%
    filter(N == 1, JUV == 0, total_mort < 100) %>%
    dplyr::group_by(YEAR, analysis_group, SPECIES_CD) %>%
    summarise(
      REGION = first(REGION),
      YEAR = first(YEAR),
      SPECIES_CD = first(SPECIES_CD),
      avg_maxdiam = mean(MAX_DIAMETER, na.rm = TRUE),
      var_maxdiam = var(MAX_DIAMETER, na.rm = TRUE),
      n_colonies = n(),
      DEPTH_M = mean(MAX_DEPTH, na.rm = TRUE),
      .groups = "drop"
    )
  view(AR_size)


  # size_site <- df %>%
  #   dplyr::mutate(total_mort = OLD_MORT + RECENT_MORT) %>%
  #   dplyr::filter(N == 1, JUV == 0, total_mort < 100) %>%
  #   dplyr::group_by(REGION, YEAR, PROT,
  #                   PRIMARY_SAMPLE_UNIT, LAT_DEGREES, LON_DEGREES, strat_cora,
  #                   HABITAT_CD, METERS_COMPLETED, SPECIES_CD) %>%
  #   dplyr::summarise(REGION = REGION, YEAR = YEAR, SPECIES_CD = SPECIES_CD,
  #     avg_maxdiam = mean(MAX_DIAMETER, na.rm = TRUE),
  #                    var_maxdiam = var(MAX_DIAMETER, na.rm = TRUE),
  #                    n_colonies = n(),
  #                    DEPTH_M = mean(MAX_DEPTH, na.rm = TRUE),
  #                    .groups = "keep") %>%
  #   dplyr::ungroup()






################
# Export
################

# Create list to export
output <- list(
  "AR_mortality" = AR_mortality,
  "AR_density" = AR_domain_estimates,
 "AR_col_size" = AR_size
)

return(output)
}
