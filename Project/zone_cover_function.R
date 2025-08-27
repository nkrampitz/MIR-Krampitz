###Percent Cover By MIR ZONE
#Nicole Krampitz
#June 23rd, 2025

zone_cover_function <- function(){

setwd("/Users/juliamateski/Downloads/MIR/Project")
###Start from the end of Translating_LPI_data

##Have this dataset, has the MIR sites and analysis grounds
tmp <- read_csv("data/FK2022_24_MIR_NCRMP_bcov_dat1_NK.csv") %>%
  filter(survey_type == "MIR")

##But in order to add the MIR groups, I need to add the MAPGRID NRs
###Pull out the MIR_zones from the data grid
MIR_zones <- read.csv("data/MIR_zones.csv") %>% select(MAPGRID_NR, MIR_zone)

##Bring in the PSU/Mapgrid Link, copied from earlier part of Translating_LPI_data
df_22 <-  read_csv("data/NCRMP_FKEYS2022_Benthic_Data01_BenthicCover_points.csv")
df_24 <- read_csv("data/FLK_MIR_2024_benthic_cover.csv")
tmp2 <- full_join(df_22, df_24) %>%
  select(REGION, YEAR, PRIMARY_SAMPLE_UNIT, MAPGRID_NR) %>% left_join(., MIR_zones) %>% unique()


###Bring it back to the original data and join it all
zones <- tmp %>%
  left_join(., tmp2 %>% select(PRIMARY_SAMPLE_UNIT, YEAR, MIR_zone)) %>%
  ##Manuall rewriting those in 2022 that fell outside the exact boundaries but were still considered MIR
  dplyr::mutate(MIR_zone = case_when(PRIMARY_SAMPLE_UNIT == 9080 ~ "Carysfort North",
                                     PRIMARY_SAMPLE_UNIT == 9079 ~ "Carysfort North",
                                     PRIMARY_SAMPLE_UNIT == 9054 ~ "Eastern Dry Rocks",
                                     TRUE ~ MIR_zone))  %>%
  filter(PRIMARY_SAMPLE_UNIT != 9151) #blank LPI, survey was pulled

##Now copying from Translating_LPI_StRS forward. This was slightly editied to be just for MIR data (no other analysis_group, and with MIR_zone)
#Step 1: Import the data, calculate proportional cover
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- zones %>%
  mutate(prp_cov = cat_p / LPI_total) %>%
   filter(analysis_group == "MIR_GRP") #Filtered again because those sites that we rewrote earlier aren't used in this analysis

## Ntot given by Dione. Need to take out CFK04 (only for 2022) because no inside outside
ntot <- read_csv("data/ncrmp_mir_2022_coral_ntot.csv") %>%
  dplyr::mutate(mtot = 250) %>%
  dplyr::rename(ntot = nht)

ntot <- full_join(
  ntot %>%
    dplyr::mutate(YEAR = 2022) %>%
    dplyr::filter(strat_cora != "CFK04"),
  ntot %>%
    dplyr::mutate(YEAR = 2024)) %>%
    filter(analysis_group == "MIR_GRP")

##MIR Zones each year that have NTOTs
##This dataset was written manually and included writing out for each zone which strats were sampled in that year.
##This step is necessary so that it is only averaging across strats that existed in that zone
zones_ntot <- read_csv("data/NTOT_Mir_Zones.csv")
ntot_filtered <- full_join(zones_ntot, ntot)

####Step 2: Stratum-Level Density Estimates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#straight transcription of math from DS
#weighting
dens_estimates <-  df %>%
  group_by(analysis_group, strat_cora, YEAR, COVER_CAT_CD, MIR_zone) %>%
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
  left_join(., ntot_filtered) %>%
  ## summarise ngrtot avaliable ntot on each strat/analysis group
  left_join(., ntot_filtered %>% group_by(MIR_zone, YEAR) %>%
              summarise(ngrtot = sum(ntot))) %>%
  ## weighted
  mutate(wh = ntot/ngrtot)

dens_estimates <- dens_estimates %>%
  mutate(
    wh = ntot / ngrtot,
    fn = n / ntot,
    vbar_p = (1 - fn) * s1 / n,
    se_p = sqrt(vbar_p),
    wavp_tmp = wh * avp,
    wvbar_p = wh^2 * vbar_p
  )

####Step 3: Domain-Level Density Estimates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~
#math transcribed directly from DS
domain_estimates <- dens_estimates %>%
  group_by(analysis_group, YEAR, COVER_CAT_CD, MIR_zone) %>%
  summarise(
    wavp = sum(wavp_tmp),
    wvbar_p = sum(wvbar_p),
    sem = sd(wavp_tmp)/sqrt(length(wavp_tmp)),
    n = sum(n),
    nstrat = n()
  ) %>%
  mutate(
    se_wp = sqrt(wvbar_p)
  ) %>%
  mutate(across(c(wavp, se_wp), round, 5))


### Cover
cat_codes <- read_csv("data/bcov_cat_codes.csv") %>%
  dplyr::rename(cover_group = CODE)
cvr <- domain_estimates %>%
  dplyr::rename(
    cover_group = COVER_CAT_CD,
    REGION = analysis_group,
    avCvr = wavp,
    SE = se_wp,
    n_sites = n
  ) %>%
  # dplyr::mutate(YEAR = 2022) %>%
  left_join(cat_codes %>% select (cover_group:COV_CAT_B))


###Formatting Data for Combined Macroalgal/Hard Coral plot
cvr_analysis <- cvr %>% group_by(REGION, COV_CAT_B, YEAR, MIR_zone) %>%
  #  filter(COV_CAT_A ==  "HCORA" | COV_CAT_A == "MACAL") %>%
  summarise(
    avCvr = sum(avCvr),
    SE = sqrt(sum(SE^2)), ##FLAG not sure on the math here for standard error
    SEM = sqrt(sum(sem^2)) ##Don't know if this one is more right or what, confusing
  )

##Graph script..
cvr_analysis %>%
  filter(COV_CAT_B %in% c("HCORS", "MACAL")) %>% ##Can choose any categories
  ggplot(aes(x = as.integer(YEAR),
             y = (avCvr),
             color = COV_CAT_B)) +
  # Add points
  geom_point(size = 2,  position = position_dodge(width = 0.06)) +
  # Add lines with specific colors for coral and macroalgae
  geom_line(size = 1, position = position_dodge(width = 0.06)) +
  geom_errorbar(aes(ymin = avCvr - SEM,
                    ymax = avCvr + SEM),
                width = .1, position = position_dodge(width = 0.06)) +
  # Customize colors for coral and macroalgae
  scale_color_manual(values = c("HCORS" = "#FF7F50", "MACAL" = "#26BA41"),
                     labels=c('Coral', 'Macroalgae'), name = "") +
  # Customize labels and title
  labs(x = "Year",
       y = "Benthic Cover",
   #    title = "Percent Cover by MIR Zone"
   ) +
  theme_Publication(base_size = 20) +
  scale_x_continuous(breaks = c(2022, 2024)) +
  # Customize theme
  theme(
    strip.text = element_text(face = "bold.italic", size = 18),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.size = unit(1.2, "cm"),
    legend.margin = margin(0, 0, 0, 0, "cm"),
    legend.text = element_text(size = 18),
    legend.background = element_blank(),
    legend.title = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.title= element_text(size =18)) +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  scale_y_continuous(limits = c(0, .5), expand = c(0, 0), labels = scales::percent_format(accuracy = 2)) +
  facet_grid(~MIR_zone) +
  ##Rewrite all the titles on the facets so that they're not cut off
  facet_grid(~MIR_zone, labeller = labeller(MIR_zone = c(
    "Carysfort North" = "Carysfort\nNorth",
    "Carysfort South" = "Carysfort\nSouth",
    "Cheeca Rocks" = "Cheeca\nRocks",
    "Eastern Dry Rocks" = "Eastern\nDry Rocks",
    "Horseshoe" = "Horsehoe\nReef",
    "Looe" = "Looe\nReef",
    "Sombrero" = "Sombrero\nReef",
    "Newfound Harbor" = "Newfound\nHarbor"
  )))


}


t <- zone_cover_function()
print(t)


