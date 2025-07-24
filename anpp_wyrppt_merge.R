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
dir.create(file.path("data", "harmonized_data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()


######################
##Read in ANPP DATA
######################

# Identify desired file
focal_file <- "04_anpp_aggregated-site-crop.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

# Read in harmonized data
anpp_v1 <- read.csv(file = file.path("data", "harmonized_data", focal_file))

# Check structure
dplyr::glimpse(anpp_v1)

######################
##Read in WATER YEAR PRECIP DATA
######################

# Identify desired file
ppt_file <- "01_wyr_ppt_all_yrs.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == ppt_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

# Read in harmonized data
ppt_v1 <- read.csv(file = file.path("data", "harmonized_data", ppt_file))

# Check structure
dplyr::glimpse(ppt_v1)

######################
##JOIN PPT AND ANPP Data
######################
ppt_v2 <- ppt_v1 %>%
  dplyr::rename(site=site_id)%>%
  dplyr::mutate(year = w_yr) %>%
  dplyr::select(-network)
dplyr::glimpse(ppt_v2)

anpp.ppt <- left_join(anpp_v1, ppt_v2, by=c("site","year"))
str(anpp.ppt)

anpp.ppt.na <- anpp.ppt %>%
  filter(is.na(wyr_ppt)==TRUE)
anpp.na <- anpp_v1 %>%
  filter(is.na(year)==TRUE)
ppt.na <- ppt_v2 %>%
  filter(is.na(w_yr)==TRUE)

# Identify nice name for exported object
focal_name <- "anpp_wyr_merged.csv"

# Export locally
write.csv(x = anpp.ppt , row.names = F, na = '',
          file = file.path("data","harmonized_data", focal_name))

# Upload to Drive
googledrive::drive_upload(media = file.path("data", "harmonized_data", focal_name), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ"))

