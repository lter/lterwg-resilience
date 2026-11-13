## ------------------------------------------------------ ##
# Pre-Processing - Grace's DAP Data
## ------------------------------------------------------ ##
# Author(s): Olivia Hajek, Nick J Lyon, ...

# Purpose
## Do needed pre-processing for (some) LTAR files
## See sub-sections below for information on needed alterations for each file

## -------------------------------------------- ## 
# Housekeeping ----
## -------------------------------------------- ## 

# Load needed libraries
librarian::shelf(tidyverse, googledrive, supportR, lubridate)
library(readxl)

# Make needed folder(s)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "raw"), showWarnings = F)
dir.create(file.path("data", "pre_processed_data"), showWarnings = F)

# Clear environment
rm(list = ls()); gc()

## -------------------------------------------- ## 
# Download Raw Data ----
## -------------------------------------------- ## 

# Identify raw files in Drive
drive_raw <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1zI1KYBlROyBZSgjSEYmVjsIfCmRPpUPq"))

# Check that worked
drive_raw

# Download them
purrr::walk2(.x = drive_raw$id, .y = drive_raw$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "raw", .y)))

## -------------------------------------------- ## 
# Pre-Process "DAP" ----
## -------------------------------------------- ## 

# Needed pre-processing:

# Read in file
dap_raw <- read.csv(file.path("data", "raw", "Sterling_wheat_1986_2009.csv"))

# Check structure
dplyr::glimpse(dap_raw)
unique(dap_raw$crop)
summary(dap_raw$total_anpp)

# Do needed repairs
dap_pp <- dap_raw %>% 
  # Replace 0 or 9999 with NA in the grain and stover columns
  mutate(across(c(st_wt_kg_ha_dm, g_wt_kg_ha_dm), ~ na_if(.x, 0))) %>%
  mutate(across(c(st_wt_kg_ha_dm, g_wt_kg_ha_dm), ~ na_if(.x, 99999.000))) %>%
  # Add desired columns, network, site_ID, ANPP summing
  dplyr::mutate(network = "DAP", site_ID = "dap",
                .before = dplyr::everything()) %>% 
  dplyr::mutate(anpp = total_anpp, 
                date = as.Date(day - 1, origin = paste0(year, "-01-01")), 
                site_ID = "Sterling") %>%
  # drop the old crop now and the stover
  dplyr::select(-c(crop_code, st_wt_kg_ha_dm)) %>%
  # remove super high ANPP values iwth the stover
  dplyr::filter(total_anpp < 12500 | is.na(total_anpp))%>%
  # Select just the NP treatment - THERE IS ONLY EVER ONE "SIDE" per treatment/year/strip/topo position (confirmed)
  dplyr::filter(n_np == "NP") %>%
  # Summarize the values across topographic position, strip will come next
  dplyr::group_by(network, site_ID, strip, trt, year, crop) %>%
  dplyr::summarize(mean_grain = mean(g_wt_kg_ha_dm, na.rm = T), mean_anpp = mean(anpp, na.rm = TRUE), 
                   min_date = min(date), .groups = "drop") %>%
  # Summarize values across strip
  dplyr::group_by(network, site_ID, trt, min_date, year, crop)%>%
  dplyr::summarize(grain_kg_ha = mean(mean_grain, na.rm = T),  anpp_kg_ha= mean(mean_anpp, na.rm = T), .groups = "drop") %>%
  # rename the date
  dplyr::rename(Date = min_date) %>%
  # Drop crops that are 0, 3, 10
  dplyr::filter(!is.na(crop)) 



# Is the harvest date the day/year as included here
# How to handle when crop == 0, 3, 10

glimpse(dap_pp)

# What columns are lost/gained
supportR::diff_check(old = names(dap_raw), new = names(dap_pp))

# Re-check structure
dplyr::glimpse(dap_pp)

# Export locally
write.csv(x = dap_pp, na = '', row.names = F,
          file = file.path("data", "pre_processed_data", "DAP_dap_pre-process.csv"))

# Clear environment / collect garbage
rm(list = ls()); gc()

## Summary of dap_pp
# need to get the wheat fallow and the wheat-corn-fallow treatments separated
glimpse(dap_pp)
dap.sum <- dap_pp %>%
  mutate(trt = ifelse(TRT %in% c(1,2), "WF", "WCF")) %>%
  group_by(trt, Crop) %>%
  summarize(n= n(), manpp = mean(anpp_kg_ha, na.rm = T), sd = sd(anpp_kg_ha, na.rm = T)) %>%
  mutate(var = (sd)^2)
ggplot(dap_pp, aes(anpp_kg_ha))+
  geom_histogram()+
  facet_wrap(~Crop)
