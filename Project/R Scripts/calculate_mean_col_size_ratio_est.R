coral_size_path <- "data/FK2022_NCRMP_DRM_MIR_corsz_AR.csv"
ntot_data      <- "data/ncrmp_mir_2022_coral_ntot.csv"
calculate_mean_col_size_ratio_est(data, ntot_path, year = 2022)
year = 2022
calculate_mean_col_size_ratio_est <- function(coral_size_path,ntot_data, year) {


  if(year == 2022){
    strata_to_exclude = "CFK04"
  } else{
    strata_to_exclude  = ""
  }

  coral_data <- read_csv(coral_size_path)%>%
    rename_with(toupper) %>%
    mutate(
      DISEASE = ifelse(trimws(DISEASE) == "", ".", DISEASE),
      BLEACH_CONDITION = ifelse(trimws(BLEACH_CONDITION) == "", ".", BLEACH_CONDITION)
    ) %>%
    filter(ANALYSIS_GROUP != "NCRMP_NA", STRAT_CORA != strata_to_exclude)


  coral_summary <- coral_data %>%
    group_by(ANALYSIS_GROUP, PRIMARY_SAMPLE_UNIT, SPECIES_CD, MAX_DIAMETER) %>%
    summarise(md_count = sum(N), .groups = "drop")  %>%
    left_join(coral_data %>%
                select(ANALYSIS_GROUP, PRIMARY_SAMPLE_UNIT, SPECIES_CD, MAX_DIAMETER,
                       YEAR, REGION, SUB_REGION_NR, STRAT_CORA, METERS_COMPLETED) %>% distinct(),
              by = c("ANALYSIS_GROUP", "PRIMARY_SAMPLE_UNIT", "SPECIES_CD", "MAX_DIAMETER"))



  # zero_data <- read_csv(zero_filled_data_path) %>%
  #   filter(ANALYSIS_GROUP != "AAAAA_AAA") %>%
  #   mutate(MAX_DIAMETER = replace_na(MAX_DIAMETER, 0)) %>%
  #   mutate(md_count = n) %>%
  #   select(ANALYSIS_GROUP, YEAR, REGION, SUB_REGION_NR, PRIMARY_SAMPLE_UNIT, STRAT_CORA,
  #          METERS_COMPLETED, SPECIES_CD, MAX_DIAMETER, md_count)

  coral_combined <- coral_summary %>%
    arrange(ANALYSIS_GROUP, PRIMARY_SAMPLE_UNIT, SPECIES_CD) %>%
    mutate(
      x_psu = md_count,
      y_psu = MAX_DIAMETER
    )


  coral_filtered <- coral_combined %>%
    mutate(dens_psu = x_psu / METERS_COMPLETED)

  coral_with_ratios <- coral_filtered %>%
    mutate(
      len = as.numeric(y_psu),
      num = as.numeric(dens_psu),
      xnum = num,
      ynum = num * len
    )


  psu_summary <- coral_with_ratios %>%
    group_by(ANALYSIS_GROUP, SPECIES_CD, STRAT_CORA, PRIMARY_SAMPLE_UNIT) %>%
    summarise(x = sum(xnum), y = sum(ynum), .groups = "drop")


  stratum_means <- psu_summary %>%
    group_by(ANALYSIS_GROUP, SPECIES_CD, STRAT_CORA) %>%
    summarise(nh = n(), av_x = mean(x, na.rm = TRUE), av_y = mean(y, na.rm = TRUE), .groups = "drop")


  ntot_data <- read_csv(ntot_path) %>%
    filter(strat_cora != strata_to_exclude) %>%
    mutate(
      mtot_h = 250,
      ntot_h = nht,
      nmtot_h = ntot_h * mtot_h,
      mrgkey = if_else(analysis_group == "MIR_GRP", 1, 2)
    )



  ntot_sums <- ntot_data %>%
    group_by(mrgkey) %>%
    summarise(nmtot_st = sum(nmtot_h), .groups = "drop")

  ntot_weights <- ntot_data %>%
    left_join(ntot_sums, by = "mrgkey") %>%
    mutate(wh = nmtot_h / nmtot_st)

  weighted_data <- stratum_means %>%
    left_join(ntot_weights, by = c("ANALYSIS_GROUP" = "analysis_group", "STRAT_CORA" = "strat_cora")) %>%
    mutate(
      wav_x = wh * av_x,
      wav_y = wh * av_y
    )



  domain_means <- weighted_data %>%
    group_by(ANALYSIS_GROUP, SPECIES_CD) %>%
    summarise(
      n = sum(nh, na.rm = TRUE),
      Xbar = sum(wav_x, na.rm = TRUE),
      Ybar = sum(wav_y, na.rm = TRUE),
      nstrat = n_distinct(STRAT_CORA[!is.na(STRAT_CORA)]),
      .groups = "drop"
    ) %>%
    mutate(
      nstrat = n_distinct(weighted_data$STRAT_CORA),
      Lbar = Ybar / Xbar
    )


  residuals <- psu_summary %>%
    left_join(domain_means %>% select(ANALYSIS_GROUP, SPECIES_CD, Lbar), by = c("ANALYSIS_GROUP", "SPECIES_CD")) %>%
    mutate(e = y - Lbar * x)

  stratum_variance <- residuals %>%
    group_by(ANALYSIS_GROUP, SPECIES_CD, STRAT_CORA) %>%
    summarise(
      nv = n(),
      av_x = mean(x),
      svar = ifelse(n() == 1, 0, var(e)),
      .groups = "drop"
    ) %>%
    left_join(ntot_weights, by = c("ANALYSIS_GROUP" = "analysis_group", "STRAT_CORA" = "strat_cora")) %>%
    mutate(
      f = nv / ntot_h,
      vbar_Lh = ifelse(av_x == 0, 0, (1 / (av_x^2)) * ((1 - f) * svar / nv)),
      wvbar = (wh^2) * vbar_Lh
    )


  nstrat_df <- stratum_var %>%
    group_by(ANALYSIS_GROUP) %>%
    summarise(nstrat = n_distinct(STRAT_CORA), .groups = "drop")

  # compute domain variance
  domain_var <- stratum_var %>%
    group_by(ANALYSIS_GROUP, SPECIES_CD) %>%
    summarise(vbar_L = sum(wvbar, na.rm = TRUE), .groups = "drop") %>%
    left_join(nstrat_df, by = "ANALYSIS_GROUP")


  final_estimates_new <- domain_means %>%
    select(-nstrat) %>%
   left_join(domain_var, by = c("ANALYSIS_GROUP", "SPECIES_CD")) %>%
    mutate(
      SE_L = sqrt(vbar_L),
      cv_L = SE_L / Lbar,
      df = n - nstrat,
      t_05 = ifelse(df > 0, abs(qt(0.025, df)), NA_real_),
      L_LCI = Lbar - (t_05 * SE_L),
      L_UCI = Lbar + (t_05 * SE_L)
    ) %>%
    select(ANALYSIS_GROUP, SPECIES_CD, n, Lbar, SE_L, cv_L, L_LCI, L_UCI)

  view(final_estimates_new)

  return(final_estimates)
}
