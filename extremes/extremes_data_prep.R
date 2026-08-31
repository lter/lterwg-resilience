
library(lubridate)
library(tidyverse)
library(googledrive)
library(cowplot)

dir.create(file.path("data", "harmonized_data"), showWarnings = F)
dir.create(file.path("data", "pre_processed_data"), showWarnings = F)

theme_set(theme_bw(12))

#read in ppt water year data
wyr_ppt<-'01_wyr_ppt_all_yrs.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == wyr_ppt) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))
wyr_ppt_data <- read.csv(file = file.path("data", "harmonized_data", wyr_ppt))

#read in annp, precip, and trt info
file2<-'anpp_wyr_trt_merged.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))
anpp_data <- read.csv(file = file.path("data", "harmonized_data", file2))

#sites with normally distributed ppt 
site_ppt_test <- anpp_data %>%
  group_by(site) %>%
  filter(n() >= 3) %>%
  summarise(shapiro_p = shapiro.test(wyr_ppt)$p.value) %>%
  filter(shapiro_p >= 0.05) #this gives us only 34 out of 79 sites to work with

#calculate +-1SD of long-term avg MSWEP ppt
wyr_ppt_summary <- wyr_ppt_data %>%
  group_by(site_id, network) %>%
  summarize(mean_ppt_40yr = mean(wyr_ppt),
            sd_ppt_40yr = sd(wyr_ppt)) %>%
  rename(site = site_id)

#calculate site mean of ppt in biomass data
biomass_ppt_mean <- anpp_data %>%
  group_by(site) %>%
  summarise(mean_ppt_site = mean(wyr_ppt))

#merge ANPP and ppt summary tables
merge_anpp_wyr_ppt <- anpp_data %>%
  left_join(., biomass_ppt_mean, by = c("site")) %>%
  left_join(., wyr_ppt_summary, by = c("site", "network")) %>%
  mutate(
    z_score = (mean_ppt_site - mean_ppt_40yr)/sd_ppt_40yr,
    climate_cat = case_when(
      z_score < -0.5 ~ "dry", #site-avg ppt is drier than -0.5 z-score of 40-yr mean ppt
      z_score > 0.5 ~ "wet", #site-avg ppt is wetter than 0.5 z-score of 40-yr mean ppt
      TRUE ~ "average"
    )
  )


#identify sites less than 5 years of anpp data or sites with no average year 
omit_select_sites <- merge_anpp_wyr_ppt %>%
  filter(climate_cat != "average") %>%
  distinct(site) %>%
  pull(site)
print(omit_select_sites)

#borrow Dave's data processing script in stability_analysis_DLH.R:
dat <- merge_anpp_wyr_ppt %>%
  filter(!site %in% c(omit_select_sites)) %>% #drop all omit sites identified above
  filter(site!='look.us'& site!='bnch.us' & site!= 'DAP') %>% #drop two odd NutNet sites
  filter(treatment!="PRHPA_NEMERREM_CCN4N")%>%#removing second treatment for PRHPA
  #filter(crop!='Garbanzo'&crop!='Canola'&crop!='Oats') %>% 
  #filter(!is.na(anpp_g_m2))%>% #this removes sites with grain yield but not anpp
  mutate(crop=tolower(crop)) %>% 
  mutate(crop2=case_when(
    crop %in% c('orchardgrass/white clover', 'orchard/fescue/clover/alfalfa/chicory', 'sorghum-sudangrass') ~ 'mixed_grass',
    TRUE~crop)) %>% 
  mutate(fertilized=ifelse(is.na(fertilized), 0, fertilized)) %>% #this is wrong b/c it is making CSCAP and ISI... 0 when should prob be 1.
  mutate(keep=ifelse(network=='NutNet'&treatment=='NPK'|network=='NutNet'&treatment=='Control', 1, 0)) %>% #dropping all nutnet treatments except control and NPK
  filter(keep==1|network !='NutNet') %>% 
  mutate(type=case_when(
    site == 'KNZ' & treatment == 'KNZ_Cropland' & crop2 == 'corn' ~ 'Corn',
    site == 'KNZ' & treatment == 'KNZ_Cropland' & crop2 == 'soybean' ~ 'Soybean',
    site == 'KNZ' & treatment == 'KNZ_Cropland' & crop2 == 'wheat' ~ 'Wheat',
    network=='LTER'~ 'Grassland',
    network=='NutNet'&fertilized==0 ~ 'Grassland', 
    network=='NutNet'&fertilized==1 ~ 'Fert. Grassland', 
    !network %in% c('LTER', 'NutNet') & crop2=="" ~ 'Grassland',
    !network %in% c('LTER', 'NutNet') & crop2 %in% c('mixed_grass', 'switchgrass', 'alfalfa') ~ 'Pasture', 
    !network %in% c('LTER', 'NutNet') & crop2=='corn' ~ 'Corn', 
    !network %in% c('LTER', 'NutNet') & crop2=='soybean' ~ 'Soybean',
    !network %in% c('NutNet') & crop2 %in% c('winter_wheat', 'spring_wheat', 'wheat') ~ 'Wheat',
    TRUE~'999'
  ))%>%
  mutate(duration_years = ifelse(site == 'LCB', 9, duration_years))%>%
  filter(!treatment %in% c('004b', '020b'))

#classify systems to just four land management types
dat_4cat<-dat %>% 
  mutate(type2=ifelse(type %in% c('Grassland', 'Fert. Grassland', 'Pasture'), type, 'Cropland')) %>%
  filter(type2 != "Pasture" )


# ── Download helpers ──────────────────────────────────────────────────────────
dl <- function(folder_id, filename) {
  googledrive::drive_ls(googledrive::as_id(folder_id)) %>%
    dplyr::filter(name == filename) %>%
    googledrive::drive_download(file = .$id, overwrite = TRUE,
                                path = file.path("data", "tidy", .$name))
}

anpp_folder <- "13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ"
spei_folder <- "1JtFMD4IAizjNGd0wbLLZIBdgqnk4YR97"


dl(spei_folder, "spei12.csv")

dl(anpp_folder, "01_wyr_ppt_all_yrs.csv")
dl(anpp_folder, "heat_indices_site.csv")

# ── Load & process ────────────────────────────────────────────────────────────

ppt_data  <- read.csv(file.path("data", "tidy", "01_wyr_ppt_all_yrs.csv"))
temp_data <- read.csv(file.path("data", "tidy", "heat_indices_site.csv"))
spei_raw  <- read.csv(file.path("data", "tidy", "spei12.csv"))


# October SPEI-12 (water-year endpoint), 1982–2024
spei_clean <- spei_raw %>%
  mutate(month = month(as.Date(date)), w_yr = year(as.Date(date))) %>%
  filter(month == 10, w_yr > 1981, w_yr < 2025) %>%
  pivot_longer(cols = 2:56, names_to = "site", values_to = "SPEI") %>%
  mutate(spei.cat = ifelse(SPEI > 0.99, "wet", ifelse(SPEI < -0.99, "dry", "normal"))) %>%
  dplyr::select(w_yr, site, SPEI, spei.cat)

# Site-level scaled PPT
ppt_scaled <- ppt_data %>%
  group_by(site_id, network) %>%
  mutate(mean_ppt     = mean(wyr_ppt),
         per_dev_ppt  = (wyr_ppt - mean_ppt) / mean_ppt,
         scaled_ppt   = scale(wyr_ppt)[, 1],
         ppt_cat      = ifelse(scaled_ppt < -1, "D", ifelse(scaled_ppt < 1, "A", "W"))) %>%
  rename(site = site_id)%>%
  dplyr::select(-mean_ppt)



# Site-level scaled Tmax
temp_scaled <- temp_data %>%
  rename(site = site_id, w_yr = year) %>%
  group_by(site, network) %>%
  mutate(mean_tmax    = mean(Tmaxc),
         per_dev_tmax = (Tmaxc - mean_tmax) / mean_tmax,
         scaled_tmax  = scale(Tmaxc)[, 1]) %>%
  dplyr::select(w_yr, site, network, Tmaxc, mean_tmax, scaled_tmax,
                num_days_95th, warm_day_90th, meanTmax_95th, Tmax_95th)

########### Merge and classify type #################

anpp_scaled <- dat_4cat%>%
  ungroup()%>%
  group_by(site, type, type2)%>%
  mutate(mean_anpp    = mean(anpp_g_m2),
         per_dev_anpp = (anpp_g_m2 - mean_anpp) / mean_anpp,
         scaled_anpp  = scale(anpp_g_m2)[, 1])

grain_scaled <- dat_4cat%>%
  group_by(site, type, type2)%>%
  mutate(mean_grain = mean(grain_g_m2),
         per_dev_anpp = (grain_g_m2 - mean_grain)/ mean_grain,
         scaled_grain = scale(grain_g_m2)[,1]) %>%
  ungroup() %>%
  dplyr::select(scaled_grain, w_yr, network, site, type, type2, treatment)

ext_data_clean <- ppt_scaled %>%
  merge(anpp_scaled, by = c("w_yr", "site", "network", "wyr_ppt")) %>%
  left_join(spei_clean,  by = c("w_yr", "site")) %>%
  left_join(temp_scaled, by = c("w_yr", "network", "site"))%>%
  left_join(grain_scaled, by = c("w_yr", "network", "site", "type", "type2", "treatment")) %>%
  mutate(n.obs   = n()) %>% #in most cases this is years, but a few sites have multiple harvest in a year
  filter(n.obs > 4) %>%
  ungroup()

# summary table of data
summary_dat_5yr <- ext_data_clean %>%
  group_by(type) %>%
  summarize(n_plot = length(unique(site)))

summary_dat_7yr <- ext_data_clean %>%
  filter(duration_years >= 7) %>%
  group_by(type) %>%
  summarize(n_plot = length(unique(site)))

summary_dat_10yr <- ext_data_clean %>%
  filter(duration_years >= 10) %>%
  group_by(type) %>%
  summarize(n_plot = length(unique(site)))



# # require > 4 site-years per group; classify sites as wet/dry relative to grand mean
#   mutate(Trt=ifelse(!network %in% c("LTER", "NutNet"), type, treatment)) %>% 
#   group_by(category, Trt, site) %>%
#   mutate(n.obs   = n()) %>% #in most cases this is years, but a few sites have multiple harvest in a year
#   filter(n.obs > 4) %>%
#   ungroup()
# 
