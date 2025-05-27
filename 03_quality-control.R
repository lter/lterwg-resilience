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
dir.create(file.path("data", "harmonized_data"), showWarnings = F)
dir.create(file.path("data", "environment"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Identify relevant tidy file
focal_file <- "02_anpp_filter1.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

# Read in harmonized data
tidy_v1 <- read.csv(file = file.path("data", "harmonized_data", focal_file))


# Check structure
dplyr::glimpse(tidy_v1)

## --------------------------------------- ##
# Identify Network and Fix Capitalization----
## --------------------------------------- ##
tidy_v2 <- tidy_v1 %>%
  # make LTER sites capitalized
  dplyr::mutate(site = ifelse(network=="LTER", toupper(site), site)) %>%
  # make lowercase lter network capitalized
  dplyr::mutate(network = ifelse(network=="lter", "LTER", network))

# make sure that every site has a network
network_nas <- tidy_v2 %>%
  filter(is.na(network))

# Fix site name for PRHPA
tidy_v2 <- tidy_v2 %>%
  dplyr::mutate(site = ifelse(site=="PRHPA_NEMELTCRS", "PRHPA", site))%>%
  dplyr::mutate(site = ifelse(site=="PRHPA_NEMERREM", "PRHPA", site)) %>%
  dplyr::mutate(site = ifelse(site=="jrn", "JRN", site)) %>%
  dplyr::mutate(site = ifelse(site=="sev", "SEV", site))
## --------------------------------------- ##
# Fix Dates and Year----
## --------------------------------------- ##

tidy_v3 <- tidy_v2 %>%
  # fix dates and extract year
  dplyr::mutate(date = lubridate::as_date(date, format="%m/%d/%Y"), 
                date_m.d.yyy = lubridate::as_date(date_m.d.yyyy, format= "%m-%d-%Y"), 
                date_m.d.yyy2 = lubridate::as_date(date_m.d.yyyy, format= "%Y-%m-%d")) %>%
  #make a single date column 
  dplyr::mutate(dates =coalesce(date, date_m.d.yyy, date_m.d.yyy2))%>%
  # add year
  dplyr::mutate(year = ifelse(is.na(year), lubridate::year(dates), year))%>%
  #Get rid of extra date columns
  dplyr::select(-c("date", "date_m.d.yyy", "date_m.d.yyy2"))
  
# Check that no unexpected columns are lost/gained
supportR::diff_check(old = names(tidy_v3), new = names(tidy_v4))

# Check structure
dplyr::glimpse(tidy_v3)

## ------------------------------------------- ##
# Calculate Duration ----
## ------------------------------------------- ##

# Calculate duration for each "site"
#MEGHAN NOTES: year is not filled out for each dataset so this isn't working, i created a new year2 column above, it works now.

tidy_v4 <- tidy_v3 %>% 
  dplyr::group_by(site, treatment) %>% 
  dplyr::mutate(duration_years = length(unique(year))) %>% 
  dplyr::ungroup()

# Check structure
dplyr::glimpse(tidy_v4)

## ------------------------------------------- ##
# ANPP Unit Conversions ----
## ------------------------------------------- ##
tidy_v5 <- tidy_v4 %>%
  # ANPP - convert KG/ha to g/m2
  dplyr::mutate(anpp_g_m2 = ifelse(is.na(anpp_g_m2), anpp_kg_ha/10, anpp_g_m2))

# Check to make sure only unwanted columns are lost
supportR::diff_check(old = names(tidy_v4), new = names(tidy_v5))

# Check structure
dplyr::glimpse(tidy_v5)

## ------------------------------------------- ##
# Crop Name Consistency----
## ------------------------------------------- ##
unique(tidy_v5$crop)

tidy_v6 <- tidy_v5 %>%
  #change the crop names to all be consistent
  mutate(crop = fct_recode(as.factor(crop),  Mixed_grass = 'Mixed Grass',Spring_Wheat = 'Triticum aestivum (Spring Wheat)', 
                       Winter_Wheat = 'Triticum aestivum (Winter Wheat)', Soybean = 'Glycine max (Soybean)', 
                       Corn = 'Zea mays (Corn)',Garbanzo = 'Cicer arietinum (Garbonzo Beans)',
                       Canola = 'Brassica napus (Canola)',Corn = 'Zea mays L. (*)', Soybean = 'Glycine max L. (*)',
                       Oats = 'Avena sativa (Oats)', Rye = 'Secale cereale (Rye)',  
                       Safflower='Carthamus tinctorius (Safflower)' , Millet = 'Setaria italica (Foxtail Millet)',
                       Winter_Wheat = 'Triticum aestivum L. (*)', Alfalfa = 'Medicago sativa (Alfalfa)',
                       Spring_Wheat = 'Triticum aestivum (Spring Spring wheat)', 
                       Sorghum = 'Sorghum bicolor (Sorghum)', Sorghum =  'Sorghum bicolor (sorghum)',    
                       Switchgrass = 'Panicum virgatum (Switchgrass)', Spring_Wheat = 'Triticum aestivum (Spring wheat)',
                       Soybean = 'Soybean', Corn = 'Corn'))

## ------------------------------------------- ##
# ANPP Checks ----
## ------------------------------------------- ##

# Look at rows without ANPP or grain yield data
anpp_na <- tidy_v6 %>%
  filter(is.na(anpp_g_m2)) %>%
  filter(is.na(grain_kg_ha))

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

## ------------------------------------------- ##
# Attach Precip Data ----
## ------------------------------------------- ##
tidy_v6 <- left_join(tidy_v5, precip_v1, by=c("network", "site","year"))

# Check missing precip data
ppt_check <- tidy_v6 %>%
  filter(is.na(total_annual_precip_mm))%>%
  group_by(site, treatment) %>%
  count()

# Check structure
dplyr::glimpse(tidy_v6)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Final pre-export tweaks
tidy_v99 <- tidy_v6

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
