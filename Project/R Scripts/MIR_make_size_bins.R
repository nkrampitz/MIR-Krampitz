
#Inputs: Region, Project (only NCRMP at this time), the years selected,
## and inputs for the 3D surface area bin count and Length bin count

MIR_make_size_bins <- function(years, analyzed_species, size_bin_count = 10, length_bin_count = 10, species_filter = NULL) {

  #p - a constant for 3d surface area calculation
  p = 1.6


  ntot <- read_csv("../Project/data/ncrmp_mir_2022_coral_ntot.csv") %>%
    dplyr::mutate(mtot = 250) %>%
    dplyr::rename(ntot = nht) %>%
    dplyr::filter(strat_cora != "CFK04") #%>%
  #  mutate(YEAR = 2022)  %>%
   # dplyr::mutate(YEAR = as.factor(YEAR)) #%>%
   # dplyr::filter(YEAR %in% years)

  ##Weighting the NTOT
  ntot_wh <- ntot %>% group_by(analysis_group) %>%
    summarise(ngrtot = sum(ntot)) %>%
    left_join(ntot) %>%
    mutate(wh = ntot/ngrtot)

  demos<- read_csv("../Project/data/FK2024_NCRMP_DRM_MIR_corsz_AR_NK.csv") %>%
    dplyr::filter(YEAR %in% years) %>%
    dplyr::filter(SPECIES_CD %in% analyzed_species) %>%
    dplyr::rename(
      STRAT = strat_cora
    ) %>%
    filter(analysis_group != "NCRMP_NA")

  ####Clean data####
  demos <- demos %>%
    dplyr::mutate(LAT_DEGREES = sprintf("%0.4f", LAT_DEGREES),
                  LON_DEGREES = sprintf("%0.4f", LON_DEGREES),
                  PROT = as.factor(PROT))


  ####Length Calculation####
  length_demos <- demos %>%
    #Year as factor (no calc needed as length = MAX_DIAMETER)
    dplyr::mutate(YEAR = as.factor(as.character(YEAR))) %>%
    #Filter out where MAX_DIAMETER does not exist
    #and where STRAT does not exist
    dplyr::filter(!is.na(MAX_DIAMETER), !is.na(STRAT), MAX_DIAMETER >= 4) %>%
    #Calculate the Ranges and the Bin Width by...
    #...grouping by the species (all years combined),
    dplyr::group_by(SPECIES_CD) %>%
    #calculate max and min of length
    dplyr::mutate(max = max(MAX_DIAMETER),
                  min = 4) %>%
    # ROUND any diameters - there shouldn't be decimals in these, but sometimes they come up in the older data
    dplyr::mutate(MAX_DIAMETER = round(MAX_DIAMETER, digits = 0)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(bin_name = case_when(MAX_DIAMETER >=4 & MAX_DIAMETER <=10 ~ "4-10",
                                       MAX_DIAMETER >=11 & MAX_DIAMETER <=15 ~ "11-15",
                                       MAX_DIAMETER >=16 & MAX_DIAMETER <=20 ~ "16-20",
                                       MAX_DIAMETER >=21 & MAX_DIAMETER <=25 ~ "21-25",
                                       MAX_DIAMETER >=26 & MAX_DIAMETER <=30 ~ "26-30",
                                       MAX_DIAMETER >=31 & MAX_DIAMETER <=35 ~ "31-35",
                                       MAX_DIAMETER >=36 & MAX_DIAMETER <=45 ~ "36-45",
                                       MAX_DIAMETER >=46 & MAX_DIAMETER <=65 ~ "46-65",
                                       MAX_DIAMETER >=66 & MAX_DIAMETER <=85 ~ "66-85",
                                       MAX_DIAMETER >=86 & MAX_DIAMETER <=105 ~ "86-105",
                                       MAX_DIAMETER >=106 ~ "106+",
                                       TRUE ~ NA_character_),
                  bin_num = case_when(MAX_DIAMETER >=4 & MAX_DIAMETER <=10 ~ 1,
                                      MAX_DIAMETER >=11 & MAX_DIAMETER <=15 ~ 2,
                                      MAX_DIAMETER >=16 & MAX_DIAMETER <=20 ~ 3,
                                      MAX_DIAMETER >=21 & MAX_DIAMETER <=25 ~ 4,
                                      MAX_DIAMETER >=26 & MAX_DIAMETER <=30 ~ 5,
                                      MAX_DIAMETER >=31 & MAX_DIAMETER <=35 ~ 6,
                                      MAX_DIAMETER >=36 & MAX_DIAMETER <=45 ~ 7,
                                      MAX_DIAMETER >=46 & MAX_DIAMETER <=65 ~ 8,
                                      MAX_DIAMETER >=66 & MAX_DIAMETER <=85 ~ 9,
                                      MAX_DIAMETER >=86 & MAX_DIAMETER <=105 ~ 10,
                                      MAX_DIAMETER >=106 ~ 11,
                                      TRUE ~ NA_real_))


  length_demos_raw <- length_demos

  length_demos <- length_demos %>%
    #summarize findings by bin count
    dplyr::group_by(SPECIES_CD, REGION, YEAR, PRIMARY_SAMPLE_UNIT,
                    STRAT, PROT, analysis_group, bin_num, bin_name, min) %>%
    dplyr::summarise(bin_tally = dplyr::n(), .groups = "keep") %>%
    dplyr::arrange(SPECIES_CD, YEAR, PRIMARY_SAMPLE_UNIT, STRAT, PROT,
                   bin_num)


  ####CALCULATE ESTIMATES####
    #Estimates for Length
    length_estimates <- length_demos %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT,
                                             sep = " ")) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_CD, analysis_group,
                      bin_num, bin_name) %>%
      dplyr::summarise(# compute average bin_tally
        avtally = mean(bin_tally),
        # compute stratum variance
        svar = var(bin_tally),
        # calculate N
        n_sites = length(unique(PRIMARY_SAMPLE_UNIT)),
        .groups = "keep") %>%
      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svar = dplyr::case_when(svar == 0 ~ 0.00000001,
                                            TRUE ~ svar)) %>%
      dplyr::mutate(Var=svar/n_sites, #variance of mean bin_tally in stratum
                    std = sqrt(svar), # std dev of bin_tally in stratum
                    SE=sqrt(Var), #SE of the mean bin_tally stratum
                    CV_perc=(SE/avtally)*100)


    # Estimates for RELATIVE Length Frequency (ADDED OCT 2022 by BW)
    # first sum up the total number of corals of each species, by strat
    tot_corals <- length_demos %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT,
                                             sep = " ")) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_CD, analysis_group) %>%
      dplyr::summarize(tot_corals = sum(bin_tally))



    length_freq_estimates <- length_demos %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT,
                                             sep = " ")) %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, SPECIES_CD, analysis_group, PROT,
                      bin_num, bin_name) %>%
      # sum up the number of corals in each size bin in each strat
      dplyr::summarize(n_corals = sum(bin_tally)) %>%
      dplyr::ungroup() %>%
      # add total number of corals in each strat
      dplyr::left_join(., tot_corals) %>%
      # calculate relative frequency (proportion) of corals in each size bin
      dplyr::mutate(length_freq = n_corals/tot_corals)


    # mortality estimates
    avgmort_site <- length_demos_raw %>%
      dplyr::mutate(ANALYSIS_STRATUM = paste(STRAT, "/ PROT =", PROT, sep = " ")) %>%
      dplyr::group_by(SPECIES_CD, REGION, YEAR, PRIMARY_SAMPLE_UNIT, analysis_group,
                      STRAT, PROT, ANALYSIS_STRATUM, bin_num, bin_name) %>%
      dplyr::summarize(avsitemort_old = mean(OLD_MORT/100),
                       avsitemort_rec = mean(RECENT_MORT/100))




    strat_mort <- avgmort_site %>%
      dplyr::mutate(PROT = as.factor(PROT)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(REGION, YEAR, ANALYSIS_STRATUM, STRAT, PROT, SPECIES_CD, bin_num, analysis_group, bin_name) %>%
      dplyr::summarize(avmort_old = mean(avsitemort_old),
                       avmort_rec = mean(avsitemort_rec),
                       # compute stratum variance
                       svar_old = var(avsitemort_old),
                       svar_rec = var(avsitemort_rec),
                       n = length(unique(PRIMARY_SAMPLE_UNIT)), .groups = "keep") %>%
      # convert 0 for stratum variance so that the sqrt is a small # but not a 0
      dplyr::mutate(svar_old = dplyr::case_when(svar_old == 0 ~ 0.00000001,
                                                TRUE ~ svar_old),
                    svar_rec = dplyr::case_when(svar_rec == 0 ~ 0.00000001,
                                                TRUE ~ svar_rec)) %>%
      dplyr::mutate(Var_old=svar_old/n, #variance of mean density in stratum
                    std_old = sqrt(svar_old), # std dev of density in stratum
                    SE_old=sqrt(Var_old), #SE of the mean density in stratum
                    CV_perc_old=(SE_old/avmort_old)*100,
                    Var_rec=svar_rec/n, #variance of mean density in stratum
                    std_rec = sqrt(svar_rec), # std dev of density in stratum
                    SE_rec=sqrt(Var_rec), #SE of the mean density in stratum
                    CV_perc_rec=(SE_rec/avmort_rec)*100)



  length_estimates_wh <- length_estimates  %>%
    # Merge ntot with coral_est_spp
    dplyr::full_join(ntot_wh) %>%
    # stratum estimates
    dplyr::mutate(whavtally = wh * avtally,
                  whsvar = wh^2 * svar,
                  whstd = wh * std,
                  n_sites = tidyr::replace_na(n_sites, 0))  %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(SPECIES_CD))



  # For relative length frequencies, we need to re-weight ntot's because species aren't present in every strat
  # Strata are here re-weighted for each species,
  # based on the strata they are present in only
  length_freq_estimates_wh <- length_freq_estimates %>%
    # merge ntot with relative length frequency
    dplyr::full_join(ntot_wh)



  # calculate NTOTs for each species, based on only strata they're present in
  ntot_spp <- length_freq_estimates_wh %>%
    dplyr::ungroup() %>%
    dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, analysis_group, SPECIES_CD, PROT, ntot, ngrtot, wh) %>%
    dplyr::distinct() %>%
    dplyr::group_by(REGION, YEAR, SPECIES_CD, analysis_group) %>%
    dplyr::summarize(ngrtot_spp = sum(ntot))



  # add new ntots, specific to species, to length frequency estimates and re weight
  length_freq_estimates <- length_freq_estimates_wh %>%
    dplyr::full_join(ntot_spp) %>%
    # calculate new species specific weights
    dplyr::mutate(wh_new = ntot/ngrtot_spp) %>%
    # stratum estimates
    dplyr::mutate(wh_length_freq = wh_new * length_freq)


  # species and size bin specific NTOT (for mortality estimates)
  ntot_spp_bin <- strat_mort %>%
    # bring in the new ntot
    dplyr::full_join(ntot_wh)%>%
    dplyr::select(REGION, YEAR, ANALYSIS_STRATUM, analysis_group, SPECIES_CD, bin_num, bin_name, ntot, ngrtot, wh) %>%
    dplyr::distinct() %>%
    dplyr::group_by(REGION, YEAR, analysis_group, SPECIES_CD, bin_num, bin_name) %>%
    dplyr::summarize(ngrtot_spp = sum(ntot))


####Strat Mort####
  strat_mort_wh_spp <- strat_mort %>%
    # bring in the new ntot
    dplyr::full_join(ntot_wh) %>%
    dplyr::full_join(., ntot_spp_bin) %>%
    dplyr::mutate(wh_new = ntot/ngrtot_spp) %>%
    # stratum estimates
    dplyr::mutate(whavmort_old = wh_new * avmort_old,
                  whavmort_rec = wh_new * avmort_rec,
                  whvar_old = wh^2 * Var_old,
                  whvar_rec = wh^2 * Var_rec)



  ####Length Domain####
  length_domain_est <- length_estimates_wh %>%
    dplyr::group_by(REGION, YEAR, analysis_group, SPECIES_CD, bin_num, bin_name) %>%
    dplyr::summarise(avtally = sum(whavtally, na.rm = T), # This accounts for strata with 0 species of interest present
                     Var_tally = sum(whsvar, na.rm = T),
                     SE_tally=sqrt(Var_tally),
                     n_sites = sum(n_sites),
                     n_strat = length(unique(ANALYSIS_STRATUM)),
                     ngrtot = sum(ntot, na.rm = TRUE),
                     .groups = "keep")  %>%
    dplyr::ungroup() %>%
    dplyr::arrange(SPECIES_CD, bin_num, YEAR)




  ##### Relative Length Freq Domain####
  length_freq_domain_est <- length_freq_estimates %>%
    dplyr::group_by(REGION, YEAR, analysis_group, SPECIES_CD, bin_num, bin_name) %>%
    dplyr::summarize(length_freq_domain = sum(wh_length_freq, na.rm = T),
                     n_strat = length(unique(ANALYSIS_STRATUM))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(SPECIES_CD, bin_num, YEAR)



  #### Mortality by bin Domain####
  domain_mort_spp <- strat_mort_wh_spp %>%
    dplyr::group_by(REGION, YEAR, analysis_group, SPECIES_CD, bin_num, bin_name) %>%
    dplyr::summarize(oldmort_domain = sum(whavmort_old, na.rm = T),
                     recmort_domain = sum(whavmort_rec, na.rm = T),
                     n_strat = length(unique(ANALYSIS_STRATUM)))


  output <- list(
    length_demos = as.data.frame(length_demos),
    length_estimates = as.data.frame(length_estimates),
    length_domain_est = as.data.frame(length_domain_est),
    length_freq_estimates = as.data.frame(length_freq_estimates),
    length_freq_domain_est = as.data.frame(length_freq_domain_est),
    domain_mort_spp = as.data.frame(domain_mort_spp),
    strat_mort = as.data.frame(strat_mort),
    demos = demos)

  return(output)
}

