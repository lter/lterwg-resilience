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
dir.create(file.path("data", "environment"), showWarnings = F)

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
# Identify Network and Fix Capitalization----
## --------------------------------------- ##




## --------------------------------------- ##
# Fix Dates and Year----
## --------------------------------------- ##

# Date data are horrible but possibly useful so we need a single 'date' column
tidy_v4 <- tidy_v2 %>% 
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
    #!is.na(date_m.d.yy) ~ date_m.d.yy, #meghan notes - this is not longer a column in the new dataset
    !is.na(date_m) ~ paste0(date_m, "/01/", year),
    T ~ NA)) %>% 
  # Separate into component bits
  tidyr::separate_wider_delim(cols = date_temp, names = c("tmp_month", "tmp_day", "tmp_year"), delim = "/", too_few = 'debug') %>% 
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
                .after = year) %>% 
    dplyr::mutate(year2=ifelse(is.na(tmp_year), year, tmp_year), .after=year) %>% 
 #Drop superseded separate 'date' columns & temporary columns
  dplyr::select(-dplyr::starts_with(c("date_", "tmp_")))

  
# Check that no unexpected columns are lost/gained
supportR::diff_check(old = names(tidy_v3), new = names(tidy_v4))

# Check structure
dplyr::glimpse(tidy_v4)

## ------------------------------------------- ##
# Calculate Duration ----
## ------------------------------------------- ##

# Calculate duration for each "site"
#MEGHAN NOTES: year is not filled out for each dataset so this isn't working, i created a new year2 column above, it works now.

tidy_v5 <- tidy_v4 %>% 
  dplyr::group_by(source) %>% 
  dplyr::mutate(duration_years = length(unique(year2)),
                .after = source) %>% 
  dplyr::ungroup() %>% 
  filter(!is.na(year2)) %>% 
  select(-year)

# Check structure
dplyr::glimpse(tidy_v5)

## ------------------------------------------- ##
# Getting a site_ID ----
## ------------------------------------------- ##
tidy_v6<-tidy_v5 %>% 
  mutate(site_ID=ifelse(network=='NutNet', site, ifelse(network=='LTER', lter, 'tbd')), .after=network)

## ------------------------------------------- ##
# ANPP Unit Conversions ----
## ------------------------------------------- ##

# Need to convert ANPP variants into a single column
tidy_v6 <- tidy_v5 %>% 
  # NPP/biomass after everything
  dplyr::relocate(dplyr::contains(c("biomass", "npp")), 
                  .after = dplyr::everything()) %>% 
  # Do unit conversion(s)
  dplyr::mutate(anpp_actual = dplyr::case_when(
    !is.na(biomass_kg.ha) ~ biomass_kg.ha,
    !is.na(biomass_g) ~ biomass_g,
    !is.na(biomass_units) ~ biomass_units,
    !is.na(anpp_units) ~ anpp_units,
    !is.na(npp_units) ~ npp_units,
    # If no provided biomass value, put NA in the 'actual ANPP' column
    T ~ NA)) %>% 
  # Drop superseded columns
  dplyr::select(-dplyr::starts_with("biomass_"), -anpp_units, -npp_units)

# Check to make sure only unwanted columns are lost
supportR::diff_check(old = names(tidy_v5), new = names(tidy_v6))

# Check structure
dplyr::glimpse(tidy_v6)

## ------------------------------------------- ##
# Download Precip Data ----
## ------------------------------------------- ##

# NOTE: "enviro-covariates/precipitation.R" generates the file(s) downloaded here
## Re-run that script if you want to update the precip data

# Identify the relevant file(s) & download it/them
drive_ppt <- googledrive::drive_ls(path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/16KZhR5CGu7YDze72Y2-LaNEdGNC39Kcf")) %>% 
  dplyr::filter(name %in% c("precip_annual-summary.csv"))

# Check identified files
drive_ppt

# Download them
purrr::walk2(.x = drive_ppt$id, .y = drive_ppt$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "environment", .y)))

## ------------------------------------------- ##
# Wrangle / QC Precip Data ----
## ------------------------------------------- ##

# Read in precip data
precip_v1 <- read.csv(file = file.path("data", "environment", "precip_annual-summary.csv")) %>% 
  mutate(site_ID=site)

# Check structure
dplyr::glimpse(precip_v1)

# Do needed wrangling (if any)
precip_v2 <- precip_v1 %>%
  dplyr::mutate(treatment = ifelse(network == "LTAR", yes = site, no = NA)) %>% 
  dplyr::select(-network, -mean_daily_precip_mm) %>% 
  dplyr::distinct()

# Re-check structure
dplyr::glimpse(precip_v2)

# Split by network
ltar_ppt <- precip_v2 %>% 
  dplyr::filter(!is.na(treatment)) %>% 
  dplyr::rename(total_annual_precip_mm.ltar = total_annual_precip_mm) %>% 
  dplyr::select(-site) %>% 
  dplyr::distinct()
nutnet_ppt <- precip_v2 %>% 
  dplyr::filter(is.na(treatment)) %>% 
  dplyr::rename(total_annual_precip_mm.nutnet = total_annual_precip_mm) %>% 
  dplyr::select(-treatment) %>% 
  dplyr::distinct()

## ------------------------------------------- ##
# Attach Precip Data ----
## ------------------------------------------- ##

# Attach to 'actual' data
tidy_v7 <- tidy_v5 %>% 
  rename(year=year2) %>% 
  mutate(year=as.integer(year)) %>% 
  dplyr::left_join(x = ., y = ltar_ppt, by = c("treatment", "year")) %>% 
  dplyr::left_join(x = ., y = nutnet_ppt, by = c("site", "year")) %>% 
  # Coalesce values
  dplyr::mutate(total_annual_precip_mm = dplyr::coalesce(total_annual_precip_mm.ltar, 
                                                         total_annual_precip_mm.nutnet),
                .after = date) %>% 
  # Drop superseded columns
  dplyr::select(-dplyr::starts_with("total_annual_precip_mm."))
  
# Check structure
dplyr::glimpse(tidy_v7)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Final pre-export tweaks
tidy_v99 <- tidy_v7

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
