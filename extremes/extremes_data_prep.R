library(googledrive)
library(tidyverse)
library(lubridate)

# ── Download helpers ──────────────────────────────────────────────────────────
dl <- function(folder_id, filename) {
  googledrive::drive_ls(googledrive::as_id(folder_id)) %>%
    dplyr::filter(name == filename) %>%
    googledrive::drive_download(file = .$id, overwrite = TRUE,
                                path = file.path("data", "tidy", .$name))
}

anpp_folder <- "13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ"
spei_folder <- "1JtFMD4IAizjNGd0wbLLZIBdgqnk4YR97"

dl(anpp_folder, "anpp_wyr_trt_merged.csv")
dl(spei_folder, "spei12.csv")
dl(anpp_folder, "01_wyr_ppt_all_yrs.csv")
dl(anpp_folder, "heat_indices_site.csv")

# ── Load & process ────────────────────────────────────────────────────────────
anpp_data <- read.csv(file.path("data", "tidy", "anpp_wyr_trt_merged.csv"))
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
  rename(site = site_id)

# Site-level scaled ANPP (treatment groups preserved)
anpp_scaled <- anpp_data %>%
  group_by(site, network, crop, fertilized, N, P, K, grazed, burned, burn_freq, seeded, till) %>%
  mutate(mean_anpp    = mean(anpp_g_m2, na.rm = TRUE),
         per_dev_anpp = (anpp_g_m2 - mean_anpp) / mean_anpp,
         scaled_anpp  = (anpp_g_m2 - mean_anpp) / sd(anpp_g_m2, na.rm = TRUE),
         n.obs        = n())

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
ext_data_clean <- ppt_scaled %>%
  merge(anpp_scaled, by = c("w_yr", "site", "network", "wyr_ppt")) %>%
  left_join(spei_clean,  by = c("w_yr", "site")) %>%
  left_join(temp_scaled, by = c("w_yr", "network", "site")) %>%
  filter(!site %in% c("look.us", "bnch.us")) %>%   # drop two anomalous NutNet sites
  mutate(
    crop = tolower(crop),
    crop2 = case_when(
      crop %in% c("orchardgrass/white clover",
                  "orchard/fescue/clover/alfalfa/chicory",
                  "sorghum-sudangrass") ~ "mixed_grass",
      TRUE ~ crop),
    fertilized = ifelse(is.na(fertilized), 0, fertilized),
    keep       = ifelse(network == "NutNet" & treatment %in% c("NPK", "Control"), 1, 0)
  ) %>%
  filter(keep == 1 | network != "NutNet") %>%       # keep only Control/NPK for NutNet
  mutate(type = case_when(
    network == "LTER"                                                              ~ "Grassland",
    network == "NutNet" & fertilized == 0                                          ~ "Grassland",
    network == "NutNet" & fertilized == 1                                          ~ "Fert. Grassland",
    !network %in% c("LTER", "NutNet") & crop2 == ""                               ~ "Grassland",
    !network %in% c("LTER", "NutNet") & crop2 %in% c("mixed_grass", "switchgrass", "alfalfa") ~ "Pasture",
    !network %in% c("LTER", "NutNet") & crop2 == "corn"                           ~ "Corn",
    !network %in% c("LTER", "NutNet") & crop2 == "soybean"                        ~ "Soybean",
    !network %in% c("LTER", "NutNet") & crop2 %in% c("winter_wheat", "spring_wheat") ~ "Wheat",
    TRUE ~ "999")) %>%
  filter(type != "999") %>%
  mutate(
    category = ifelse(type %in% c("Soybean", "Corn", "Wheat"), "Crop", type),
    category = as.factor(category)
  ) %>%
  # require > 4 site-years per group; classify sites as wet/dry relative to grand mean
  mutate(Trt=ifelse(!network %in% c("LTER", "NutNet"), type, treatment)) %>% 
  group_by(category, Trt, site) %>%
  mutate(n.obs   = n()) %>% #in most cases this is years, but a few sites have multiple harvest in a year
  filter(n.obs > 4) %>%
  ungroup()
