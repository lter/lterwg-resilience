## ----------------------------------------------------------------- ##
# Resilience Management - Site Aggregation Workflow
## ----------------------------------------------------------------- ##
# Authors: Nick J Lyon, ...

# Purpose
## 

## --------------------------------------- ##
# Housekeeping ----
## --------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive)

# Make needed folder(s)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "tidy"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Identify desired file
focal_file <- "03_resilience_filtered.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "tidy", .$name))

# Read in harmonized data
site_v1 <- read.csv(file = file.path("data", "tidy", focal_file))

# Check structure
dplyr::glimpse(site_v1)

## ------------------------------------------- ##
# Column Re-Ordering ----
## ------------------------------------------- ##

# Reorder remaining columns more intuitively
site_v2 <- site_v1 %>% 
  # NPP/biomass after everything
  dplyr::relocate(dplyr::contains(c("biomass", "npp")), .after = dplyr::everything()) %>% 
  # Coords near site info
  dplyr::relocate(lat, long, .after = site) %>% 
  dplyr::relocate(elevation_m, habitat, .before = site) %>% 
  # Treatment specifics after main 'treatment' column
  dplyr::relocate(dplyr::contains("_y.n"), .after = treatment) 

# Check structure
dplyr::glimpse(site_v2)

## ------------------------------------------- ##
# Summarize Within Sites ----
## ------------------------------------------- ##

# Identify all columns at/above "site" column
(grp_cols <- setdiff(x = names(site_v2), y = c("block", "plot", "quadrat", "date",
                                               "annual_precip_mean", "annual_precip_var",
                                               "biomass_kg.ha", "biomass_g", "biomass_units",
                                               "anpp_units", "npp_units")))

# Summarize within "sites"
site_v3 <- site_v2 %>% 
  dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) %>% 
  dplyr::summarize(biomass_kg.ha = mean(biomass_kg.ha, na.rm = T),
                   biomass_g = mean(biomass_g, na.rm = T),
                   biomass_units = mean(biomass_units, na.rm = T),
                   anpp_units = mean(anpp_units, na.rm = T),
                   npp_units = mean(npp_units, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# Check structure
dplyr::glimpse(site_v3)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Final pre-export tweaks
site_v99 <- site_v3

# Check structure
dplyr::glimpse(site_v99)

# Make nice output name
focal_output <- "04_resilience_site-means.csv"

# Export locally
write.csv(x = site_v99, row.names = F, na = '', file = file.path("data", "tidy", focal_output))

# Upload to Drive
googledrive::drive_upload(media = file.path("data", "tidy", focal_output), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ"))

# End ----
