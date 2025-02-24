## ----------------------------------------------------------------- ##
# Resilience Management - Quality Control Workflow
## ----------------------------------------------------------------- ##
# Authors: Nick J Lyon, ...

# Purpose
## Do needed quality control (QC) and miscellaneous data wrangling tasks.
## Essentially anything after "harmonization" _sensu stricto"

## --------------------------------------- ##
# Housekeeping ----
## --------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive, supportR)

# Make needed folder(s)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "tidy"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Identify relevant tidy file
focal_file <- "01_resilience_harmonized.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "tidy", .$name))

# Read in harmonized data
tidy_v1 <- read.csv(file = file.path("data", "tidy", focal_file))

# Check structure
dplyr::glimpse(tidy_v1)

## --------------------------------------- ##
# Identify Network ----
## --------------------------------------- ##

# Identify network from which each dataset was sourced
sort(unique(tidy_v1$source))

# Begin identifying networks
tidy_v2 <- tidy_v1 %>% 
  dplyr::mutate(network = dplyr::case_when(
    source %in% c("cdr_anpp_nceas.csv", "KBS-biomass-compilation-herb-systems.csv",
                  "knz_anpp_nceas.csv", "nwt_anpp_nceas.csv", "sev_anpp_nceas.csv") ~ "LTER",
    source %in% c("ABS_UF_BIR_NIFA_ANPP_Betsey.csv", "CAF_DET_20231218_MeasHarvestFraction.csv",
                  "CPER_LTNPP_MeasGrazingPlants.csv",  "ECB_MeasHarvestFractionv2.csv",
                  "GB_MeasGrazingPlants_04242024.csv", "NP_InOut_MeasGrazingPlants.csv", 
                  "NPMA_MeasHarvestFraction.csv", "PRHPA_NEMELTCRS_MeasResidueMgnt.csv",
                  "PRHPA_NEMERREM_MeasHarvestFraction.csv", "SP_RotGraz_MeasGrazingPlants_OLH.csv",
                  "TG_GSWRL_LTBE_MeasHarvestFractionv2.csv", "UCB_Pahaw_MeasGrazingPlants.csv",
                  "UCB_Pahaw_MeasResidueMgnt.csv", "UMRB_AMES_IAKFT_MeasHarvestFraction.csv",
                  "UMRB_MeasHarvestFrac_v2.csv") ~ "LTAR",
    stringr::str_detect(string = source, pattern = "NutNet") ~ "NutNet",
    ## Combo ones
    source %in% c("jrn_anpp_nceas.csv") ~ "LTER & LTAR",
    T ~ "unknown"), .before = dplyr::everything())

# Did those all get identified?
tidy_v2 %>% 
  dplyr::filter(network == "unknown") %>% 
  dplyr::select(source, network) %>% 
  dplyr::distinct()

## --------------------------------------- ##
# Consolidate Multi-Measurements ----
## --------------------------------------- ##

# Occasionally, some measurements were done multiple times in separate columns, need to resolve this
tidy_v3 <- tidy_v2 %>% 
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
supportR::diff_check(old = names(tidy_v2), new = names(tidy_v3))

# Check number of NAs before/after doing this
## Biomass
supportR::count(is.na(tidy_v2$biomass_units))
supportR::count(is.na(tidy_v3$biomass_units))
## Species richness
supportR::count(is.na(tidy_v2$spp_richness))
supportR::count(is.na(tidy_v3$spp_richness))

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

# Identify nice name for exported object
focal_output <- "02_resilience_wrangled.csv"

# Export locally
write.csv(x = tidy_v99, row.names = F, na = '',
          file = file.path("data", "tidy", focal_output))

# Upload to Drive
googledrive::drive_upload(media = file.path("data", "tidy", focal_output), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ"))

# End ----
