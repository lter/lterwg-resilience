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
dap_raw <- read_excel(file.path("data", "raw", "DAP_Dryland_Sterling_production_1986_2009.xlsx"), sheet = "Sheet1")

# Check structure
dplyr::glimpse(dap_raw)

# Do needed repairs
dap_pp <- dap_raw %>% 
  # Select the correct treatments
  dplyr::filter(TRT %in% c(1,2,3,4,5)) %>%
  # Replace 0 or 9999 with NA in the grain and stover columns
  mutate(across(c(ST_WT_kg_ha_DM, G_WT_kg_ha_DM), ~ na_if(.x, 0))) %>%
  mutate(across(c(ST_WT_kg_ha_DM, G_WT_kg_ha_DM), ~ na_if(.x, 99999.000))) %>%
  # Add desired columns, network, site_ID, ANPP summing
  dplyr::mutate(network = "DAP", site_ID = "dap",
                .before = dplyr::everything()) %>% 
  dplyr::mutate(anpp = ST_WT_kg_ha_DM + G_WT_kg_ha_DM, 
                date = as.Date(DAY - 1, origin = paste0(YEAR, "-01-01"))) %>%
  # Drop the unwanted columns - for now include STN_PPM, STP_PPM, GN_PPM, GP_PPM
  dplyr::select(-c(STN_PPM, STP_PPM, GN_PPM, GP_PPM)) %>%
  # update the crop name where 1 = "Wheat" and 2 = "Corn"
  dplyr::mutate(Crop = case_when(
    CROP == 1 ~ "Wheat", 
    CROP == 2 ~ "Corn",
    TRUE ~ NA
  )) %>%
  # drop the old crop now and the stover
  dplyr::select(-c(CROP, ST_WT_kg_ha_DM)) %>%
  # Select just the NP treatment - THERE IS ONLY EVER ONE "SIDE" per treatment/year/strip/topo position (confirmed)
  dplyr::filter(N_NP == "NP") %>%
  # Summarize the values across topographic position, strip will come next
  dplyr::group_by(network, site_ID, STRIP, TRT, YEAR, Crop) %>%
  dplyr::summarize(mean_grain = mean(G_WT_kg_ha_DM, na.rm = T), mean_anpp = mean(anpp, na.rm = TRUE), 
                   min_date = min(date), .groups = "drop") %>%
  # Summarize values across strip
  dplyr::group_by(network, site_ID, TRT, min_date, YEAR, Crop)%>%
  dplyr::summarize(grain_kg_ha = mean(mean_grain, na.rm = T),  anpp_kg_ha= mean(mean_anpp, na.rm = T), .groups = "drop") %>%
  # rename the date
  dplyr::rename(Date = min_date) %>%
  # Drop crops that are 0, 3, 10
  dplyr::filter(!is.na(Crop))


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
