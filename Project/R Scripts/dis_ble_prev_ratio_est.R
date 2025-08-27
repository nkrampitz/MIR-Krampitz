
#fk2022_NCRMP_MIR_PVbar_Ball_dom.csv
 data <- "data/FK2022_NCRMP_DRM_MIR_corsz_ARallv2.csv"
#
# ntot_path      <- "data/ncrmp_mir_2022_coral_ntot.csv"
#
# tmp <- dis_ble_prev_ratio_est(data_2022_path, 2022)
#
year = 2022

dis_ble_prev_ratio_est <- function(data, year){


  ntot_path      <- "data/ncrmp_mir_2022_coral_ntot.csv"


  density_data <- read_csv(data) %>%
    rename_with(toupper) %>%
    filter(YEAR == year) %>%
    mutate(BLEACH_CONDITION = case_when(
      BLEACH_CONDITION == "Partly Bleached" ~ "PB",
      BLEACH_CONDITION == "Pale" ~ "P",
      BLEACH_CONDITION == "Not Bleached" ~ "N",
      BLEACH_CONDITION == "Totally Bleached" ~ "T",
      TRUE ~ BLEACH_CONDITION
    ))

  # Clean and reformat for prevalence calculations
  cleaned_data <- density_data %>%
    filter(ANALYSIS_GROUP != 'AAAAA_AAA') %>%
    mutate(
      DISEASE = if_else(trimws(DISEASE) == "", ".",
                        if_else(N == 0 & DISEASE == "A", ".", DISEASE)),
      BLEACH_CONDITION = if_else(trimws(BLEACH_CONDITION) == "", ".",
                                 if_else(N == 0 & BLEACH_CONDITION == "AA", ".", BLEACH_CONDITION))
    )

  # Add prevalence variables
  data_with_prevalence_vars <- cleaned_data %>%
    mutate(
      TOTAL_COLONY_COUNT = N,
      BLEACH_ALL = if_else(BLEACH_CONDITION %in% c('P', 'PB', 'T'), N, 0),
      BLEACH_P   = if_else(BLEACH_CONDITION == "P",  N, 0),
      BLEACH_PB  = if_else(BLEACH_CONDITION == "PB", N, 0),
      BLEACH_T   = if_else(BLEACH_CONDITION == "T",  N, 0),
      DISEASE_ALL = if_else(DISEASE %in% c('F', 'S', 'P'), N, 0),
      DISEASE_F   = if_else(DISEASE == "F", N, 0),
      DISEASE_P   = if_else(DISEASE == "P", N, 0),
      DISEASE_S   = if_else(DISEASE == "S", N, 0)
    )


  prevalence_var =   "BLEACH_ALL"
  prevalence_label= "Bleach_All"
  df = data_with_prevalence_vars


  compute_PVbar <- function(df, prevalence_var, prevalence_label) {


    psu_level <- df %>%
      group_by(ANALYSIS_GROUP, PRIMARY_SAMPLE_UNIT, SPECIES_CD, STRAT_CORA) %>%
      summarise(
        TOTAL_COLONIES = sum(TOTAL_COLONY_COUNT, na.rm = TRUE),
        CASES = sum(.data[[prevalence_var]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(
        df %>%
          select(ANALYSIS_GROUP, PRIMARY_SAMPLE_UNIT, SPECIES_CD, STRAT_CORA, YEAR, REGION, SUB_REGION_NR, METERS_COMPLETED) %>%
          distinct(),
        by = c("ANALYSIS_GROUP", "PRIMARY_SAMPLE_UNIT", "SPECIES_CD", "STRAT_CORA")
      ) %>%
      mutate(
        DENSITY = TOTAL_COLONIES / METERS_COMPLETED,
        X_NUMERATOR = DENSITY,
        Y_NUMERATOR = DENSITY * CASES
      )


    site_sum <- psu_level %>%
      group_by(ANALYSIS_GROUP, SPECIES_CD, STRAT_CORA, PRIMARY_SAMPLE_UNIT) %>%
      summarise(
        x = sum(X_NUMERATOR, na.rm = TRUE),
        y = sum(Y_NUMERATOR, na.rm = TRUE),
        YEAR = first(YEAR),
        .groups = "drop"
      )

    # strat summary
    stratum_summary <- site_sum %>%
      group_by(ANALYSIS_GROUP, SPECIES_CD, STRAT_CORA) %>%
      summarise(
        nh = n(),
        av_x = mean(x, na.rm = TRUE),
        av_y = mean(y, na.rm = TRUE),
        YEAR = first(YEAR),
        .groups = "drop"
      )

    #load ntot data and compute weights
    ntot_data <- read_csv(ntot_path) %>%
      rename_with(toupper) %>%
      mutate(
        mtot_h = 250,
        ntot_h = NHT,
        nmtot_h = ntot_h * mtot_h,
        mrgkey = if_else(ANALYSIS_GROUP == "MIR_GRP", 1, 2)
      )

    ntot_weights <- ntot_data %>%
      group_by(mrgkey) %>%
      summarise(nmtot_st = sum(nmtot_h, na.rm = TRUE), .groups = "drop") %>%
      right_join(ntot_data, by = "mrgkey") %>%
      mutate(WH = nmtot_h / nmtot_st)

    #Combine with weights
    strat_combined <- stratum_summary %>%
      left_join(ntot_weights, by = c("ANALYSIS_GROUP", "STRAT_CORA")) %>%
      mutate(
        wav_x = WH * av_x,
        wav_y = WH * av_y
      )



    # Domain estimate
    domain_est <- strat_combined %>%
      group_by(ANALYSIS_GROUP, SPECIES_CD) %>%
      summarise(
        n = sum(nh, na.rm = TRUE),
        Xbar = sum(wav_x, na.rm = TRUE),
        Ybar = sum(wav_y, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        PVbar = Ybar / Xbar
      )


    # Residuals for variance calculation
    site_sum_residuals <- site_sum %>%
      left_join(domain_est %>% select(ANALYSIS_GROUP, SPECIES_CD, PVbar),
                by = c("ANALYSIS_GROUP", "SPECIES_CD")) %>%
      mutate(e = y - PVbar * x)

    stratum_var <- site_sum_residuals %>%
      group_by(ANALYSIS_GROUP, SPECIES_CD, STRAT_CORA) %>%
      summarise(
        nv = n(),
        av_x = mean(x, na.rm = TRUE),
        svar = if_else(n() == 1, 0, var(e, na.rm = TRUE)),
        YEAR = first(YEAR),
        .groups = "drop"
      ) %>%
      left_join(ntot_weights, by = c("ANALYSIS_GROUP", "STRAT_CORA")) %>%
      mutate(
        f = nv / ntot_h,
        vbar_PVh = if_else(av_x == 0, 0, (1 / av_x^2) * ((1 - f) * svar / nv)),
        wvbar = WH^2 * vbar_PVh
      )

    nstrat_df <- stratum_var %>%
      group_by(ANALYSIS_GROUP) %>%
      summarise(nstrat = n_distinct(STRAT_CORA), .groups = "drop")

    # compute domain variance
    domain_var <- stratum_var %>%
      group_by(ANALYSIS_GROUP, SPECIES_CD) %>%
      summarise(vbar_PV = sum(wvbar, na.rm = TRUE), .groups = "drop") %>%
      left_join(nstrat_df, by = "ANALYSIS_GROUP")


    final <- domain_est %>%
      select(-nstrat) %>%
      left_join(domain_var, by = c("ANALYSIS_GROUP", "SPECIES_CD")) %>%
      mutate(
        SE_PV = sqrt(vbar_PV),
        cv_PV = SE_PV / PVbar,
        df = n - nstrat,
        t_05 = abs(qt(0.025, df)),
        PV_LCI = PVbar - (t_05 * SE_PV),
        PV_UCI = PVbar + (t_05 * SE_PV)
      ) %>%
      select(ANALYSIS_GROUP, SPECIES_CD, n, PVbar, SE_PV, cv_PV, PV_LCI, PV_UCI)


    return(final)
  }


  final_results <- bind_rows(
    compute_PVbar(data_with_prevalence_vars, "BLEACH_ALL", "Bleach_All"),
    compute_PVbar(data_with_prevalence_vars, "BLEACH_P",   "Bleach_P"),
    compute_PVbar(data_with_prevalence_vars, "BLEACH_PB",  "Bleach_PB"),
    compute_PVbar(data_with_prevalence_vars, "BLEACH_T",   "Bleach_T"),
    compute_PVbar(data_with_prevalence_vars, "DISEASE_ALL", "Disease_All"),
    compute_PVbar(data_with_prevalence_vars, "DISEASE_F",   "Disease_F"),
    compute_PVbar(data_with_prevalence_vars, "DISEASE_P",   "Disease_P"),
    compute_PVbar(data_with_prevalence_vars, "DISEASE_S",   "Disease_S")
  ) %>% filter(ANALYSIS_GROUP != "NCRMP_NA")

  return(final_results)


}
