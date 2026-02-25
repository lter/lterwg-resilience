## Neon Process
## Konza data

## -------------------------------------------- ## 
# Housekeeping ----
## -------------------------------------------- ## 

# Load needed libraries
librarian::shelf(tidyverse, googledrive, supportR, lubridate)

# Make needed folder(s)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "raw_data"), showWarnings = F)
dir.create(file.path("data", "pre_processed_data"), showWarnings = F)

# Clear environment
rm(list = ls()); gc()

# Identify desired file
focal_file <- "konza_crop_neon.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1zI1KYBlROyBZSgjSEYmVjsIfCmRPpUPq")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "raw_data", .$name))

# Read in harmonized data
knz <- read.csv(file = file.path("data", "raw_data", focal_file))

# Check structure
dplyr::glimpse(knz)


## Format and structure data for cleaning
knz.2 <- knz %>%
  # mutate
  mutate(network = "LTER", 
         site_id = "KNZ", 
         Treatment = "KNZ_Cropland") %>%
  rename(Crop = "herbGroup", 
         Date = "setDate", 
         anpp_g_m2 = "dryMass") %>%
  select(c(network, site_id, Treatment, Crop, plotID, Date, anpp_g_m2))

# Export locally
write.csv(x = knz.2, na = '', row.names = F,
          file = file.path("data", "pre_processed_data", "knz_crop.csv")) 

  

