## ----------------------------------------------------------------- ##
# Resilience Management - Merging ANPP-Precipitation Dat with Management Table
## ----------------------------------------------------------------- ##
# Authors: Olivia Hajek,...

# Purpose
## This script joins the merged ANPP-WaterYear script with the management table

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


########################################
##Read in ANPP-Water Year DATA
####################################

# Identify desired file
focal_file <- "anpp_wyr_merged.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

# Read in harmonized data
anpp_ppt_v1 <- read.csv(file = file.path("data", "harmonized_data", focal_file))

# Check structure
dplyr::glimpse(anpp_ppt_v1)

########################################
##Read in the management table 
########################################

# Identify desired file
focal_file2 <- "treatment_table"

# Grab the data key
mngmt_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1Ty7QX7vyvD797eKJzMWbr8AwIo-GyBFO")) %>%
  dplyr::filter(name == "treatment_table")

# Did that work?
mngmt_drive

# Download the data key
googledrive::drive_download(file = mngmt_drive$id, overwrite = T, type = "csv",
                            path = file.path("data", mngmt_drive$name))

# Read in data key
mngmt_trt <- read.csv(file = file.path("data", "treatment_table.csv"))


# Check structure
dplyr::glimpse(mngmt_trt)

########################################
##Merge anpp_ppt and management
########################################
str(mngmt_trt)
##Join by site, treatment, and network

anpp_ppt_trt_v1 <- left_join(anpp_ppt_v1, mngmt_trt, by=c("site","treatment", "network"))
str(anpp_ppt_trt_v1)


##Check for rows without management data
anpp.na <- anpp_ppt_trt_v1  %>%
  filter(is.na(grazed)==TRUE)


########################################
##Export file
########################################
# Identify nice name for exported object
focal_name <- "anpp_wyr_trt_merged.csv"

# Export locally
write.csv(x = anpp_ppt_trt_v1, row.names = F, na = '',
          file = file.path("data","harmonized_data", focal_name))

# Upload to Drive
googledrive::drive_upload(media = file.path("data", "harmonized_data", focal_name), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ"))



