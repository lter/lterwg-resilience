# Relationships between ANPP and climate indices 
#7/24/2025

# Load libraries
library(lubridate)
library(tidyverse)
library(googledrive)
library(ggplot2)
drive_auth()

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

# Calculate mean ANPP by site and treatment
mean_anpp <- anpp_v1 %>%
  group_by(site, treatment) %>%
  summarise(mean_anpp = mean(anpp_g_m2, na.rm = TRUE)) %>%
  filter(treatment != "ECB_B1bau") # no ANPP data at this site

#######################
#Read in CLIMATE DATA 
#######################

# Identify desired file
climate_data <- "site_climate_mswep.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == climate_data) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

# Read in harmonized data
climate_file <- read.csv(file = file.path("data", "harmonized_data", climate_data)) %>%
  rename(site = site_id)

# Check structure
dplyr::glimpse(climate_file)

###################################
# Join Mean ANPP AND CLIMATE DATA #
###################################
merge_ANPP_climate <- left_join(mean_anpp, climate_file, by = c("site"))
  
######################################
#Plot ANPP and Climate relationships!#
######################################
ggplot(merge_ANPP_climate, aes(x = ppt_max_event, y = mean_anpp)) +
  geom_point()

