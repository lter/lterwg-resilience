## ----------------------------------------------------------------- ##
# Resilience Management - Wrangling Workflow
## ----------------------------------------------------------------- ##
# Authors: Nick J Lyon, ...

# Purpose
## Do needed quality control (QC) and miscellaneous data wrangling tasks.
## Essentially anything after "harmonization" _sensu stricto"

## --------------------------------------- ##
# Housekeeping ----
## --------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive)

# Make needed folder(s)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "tidy"), showWarnings = F)

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == "01_resilience_harmonized.csv") %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "tidy", .$name))

# Clear environment + collect garbage
rm(list = ls()); gc()

# Read in harmonized data
tidy_v1 <- read.csv(file = file.path("data", "tidy", "01_resilience_harmonized.csv"))

# Check structure
dplyr::glimpse(tidy_v1)

## --------------------------------------- ##
# Consolidate Multi-Measurements ----
## --------------------------------------- ##

# Occasionally, some measurements were done multiple times in separate columns, need to resolve this
tidy_v2 <- tidy_v1 %>% 
  # Aggregate these multi-column observations
  ## Biomass
  dplyr::mutate(biomass_units = dplyr::case_when(
    !is.na(biomass_units) ~ biomass_units,
    any(!is.na(biomass_units.1), !is.na(biomass_units.2), !is.na(biomass_units.3), !is.na(biomass_units.4), !is.na(biomass_units.5)) ~ biomass_units.1 + biomass_units.2 + biomass_units.3 + biomass_units.4 + biomass_units.5,
    any(!is.na(biomass_kg.ha_non.grain), !is.na(biomass_kg.ha_grain)) ~ biomass_kg.ha_non.grain + biomass_kg.ha_grain,
    T ~ NA)) %>% 
  ## Species richness
  dplyr::mutate(spp_richness = dplyr::case_when(
    !is.na(spp_richness) ~ spp_richness,
    any(!is.na(spp_richness.1), !is.na(spp_richness.2), !is.na(spp_richness.3), !is.na(spp_richness.4), !is.na(spp_richness.5)) ~ spp_richness.1 + spp_richness.2 + spp_richness.3 + spp_richness.4 + spp_richness.5,
    T ~ NA)) %>% 
  # Drop now-superseded columns
  dplyr::select(-dplyr::starts_with(c("spp_richness.", "biomass_units.")),
                -biomass_kg.ha_non.grain, -biomass_kg.ha_grain)

# Make sure we only lose expected columns
supportR::diff_check(old = names(tidy_v1), new = names(tidy_v2))

# Check number of NAs before/after doing this
## Biomass
supportR::count(is.na(tidy_v1$biomass_units))
supportR::count(is.na(tidy_v2$biomass_units))
## Species richness
supportR::count(is.na(tidy_v1$spp_richness))
supportR::count(is.na(tidy_v2$spp_richness))

# Check structure
dplyr::glimpse(tidy_v2)

## ------------------------------------------- ##
# Aggregate Across Multiple Veg. Categories ----
## ------------------------------------------- ##

# Identify all columns other than measurements / veg. categories
(grp_cols <- setdiff(x = names(tidy_v2), y = c("plant_species", "plant_fraction", "vegetation_category",
                                               "biomass_g", "biomass_units", "spp_richness", "anpp_units",
                                               "npp_units")) )

# Want to filter and/or summarize across categories of veg
tidy_v3 <- tidy_v2 %>% 
  # Filter out categories that are not wanted
  ## !!! TBD; pending group decision !!!
  # Any conditional algebra (e.g., 75% plant biomass for some LTAR data)
  ## !!! TBD; pending group decision !!!
  # Summarize across remaining categories
  dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) %>% 
  dplyr::summarize(biomass_g = mean(biomass_g, na.rm = T),
                   biomass_units = mean(biomass_units, na.rm = T),
                   spp_richness = mean(spp_richness, na.rm = T),
                   anpp_units = mean(anpp_units, na.rm = T),
                   npp_units = mean(npp_units, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()

# Make sure we didn't lose/gain unexpected columns
supportR::diff_check(old = names(tidy_v2), new = names(tidy_v3))

# How many rows are lost as a result of this?
message(nrow(tidy_v2) - nrow(tidy_v3), " rows lost from this step.")

# Check structure
dplyr::glimpse(tidy_v3)

## ------------------------------------------- ##
# Handle Date Data ----
## ------------------------------------------- ##

# Date data are horrible but possibly useful so we need a single 'date' column
tidy_v4 <- tidy_v3 %>% 
  # Relocate date columns to end
  dplyr::relocate(dplyr::starts_with("date_"), .after = dplyr::everything()) %>% 
  # Fill missing dates with NA instead of just 0-length characters
  dplyr::mutate(dplyr::across(.cols = dplyr::starts_with("date_"),
                              .fns = ~ ifelse(nchar(.) == 0, yes = NA, no = .))) %>% 
  # Make a temp column containing some likely useful bits of date info
  dplyr::mutate(date_temp = dplyr::case_when(
    !is.na(date_m.d.yyyy.time) ~ stringr::str_extract(string = date_m.d.yyyy.time,
                                                      pattern = "[:digit:]{1,2}\\/[:digit:]{1,2}\\/[:digit:]{4}"),
    !is.na(date_m.d.yyyy) ~ date_m.d.yyyy,
    !is.na(date_m.d.yy) ~ date_m.d.yy,
    !is.na(date_m) ~ paste0(date_m, "/01/", year),
    T ~ NA)) %>% 
  # Separate into component bits
  tidyr::separate_wider_delim(cols = date_temp, names = c("tmp_month", "tmp_day", "tmp_year"), delim = "/") %>% 
  # Make them numbers
  dplyr::mutate(dplyr::across(.cols = tmp_month:tmp_year, .fns = ~ as.numeric(.))) %>% 
  # Do needed formatting for to assemble standardized dates
  dplyr::mutate(tmp_month = ifelse(nchar(tmp_month) == 2,
                                   yes = as.character(tmp_month), no = paste0("0", tmp_month)),
                tmp_day = ifelse(nchar(tmp_day) == 2,
                                 yes = as.character(tmp_day), no = paste0("0", tmp_day)),
                tmp_year = dplyr::case_when(nchar(tmp_year) == 4 ~ as.character(tmp_year),
                                            # NOTE GUESS HERE (vvv)
                                            nchar(tmp_year) == 2 & tmp_year > 25 ~ paste0("19", tmp_year),
                                            nchar(tmp_year) == 2 & tmp_year <= 25 ~ paste0("20", tmp_year))) %>% 
  # Assemble into real date column!
  dplyr::mutate(date = as.Date(paste(tmp_month, tmp_day, tmp_year, sep = "/"), format = "%m/%d/%Y"),
                .after = long) %>% 
  # Drop superseded separate 'date' columns & temporary columns
  dplyr::select(-dplyr::starts_with(c("date_", "tmp_")))

# Check that no unexpected columns are lost/gained
supportR::diff_check(old = names(tidy_v3), new = names(tidy_v4))

# Check structure
dplyr::glimpse(tidy_v4)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Final pre-export tweaks
tidy_v99 <- tidy_v4

# Check structure
dplyr::glimpse(tidy_v99)

# Export locally
write.csv(x = tidy_v99, row.names = F, na = '',
          file = file.path("data", "tidy", "02_resilience_wrangled.csv"))

# Upload to Drive
googledrive::drive_upload(media = file.path("data", "tidy", "02_resilience_wrangled.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ"))

# End ----
