# Ingrid Slette
# started February 19, 2025

# retrieving MSWEP (http://www.gloh2o.org/mswep/) precipitation data for lter and ltar sites
# for LTER working group Resiliance and Managment

# first have to request access to MSWEP data, which is shared via Google Drive
# then install and set up rclone to acess MSWEP Google Drive

# I copied daily MSWEP precip data from all years into a file on my Desktop (so as to not fill up the shared wg google drive) 
# by running the following code in the R terminal: 
# rclone  copy --drive-shared-with-me gdrive:MSWEP_V280/Past/Daily /Users/ingridslette/Desktop/mswep_daily
# rclone  copy --drive-shared-with-me gdrive:MSWEP_V280/NRT/Daily /Users/ingridslette/Desktop/mswep_daily

library(terra)
library(lubridate)
library(tidyverse)

# read in list of sites and coordinates 
sites <- read.csv("/Users/ingridslette/Library/CloudStorage/GoogleDrive-slett152@umn.edu/Shared drives/LTER-WG_Resilience-Management/data/raw_data/ltar_lter_site_coordinates.csv")


unique(sites$site_id)

# make that a SpatVector
site <- sites %>% vect(geom = c("longitude", "latitude"), crs = "EPSG:4326")

# list all of the monthly mswep precip data files
r_paths <- list.files("/Users/ingridslette/Desktop/mswep_daily",
                      full.names = TRUE) %>% 
  sort()

# make that a SpatRaster
r <- rast(r_paths)

# extract daily precip data for each site
ppt_daily <- terra::extract(r, site, bind = TRUE)

df <- as.data.frame(ppt_daily)

names(df) <- c("site_id", "project_id", paste0("precip_", time(r)))

out <- pivot_longer(df, -c("site_id", "project_id"), names_to = "date",
                    values_to = "precip") %>% 
  mutate(date = str_replace(date, "^precip_", ""))

# create a new column for the year
out$year <- substr(out$date, 1, 4)

# create a new column for the month
out$month <- substr(out$date, 6, 7)

# create a new column for the day
out$day <- substr(out$date, 9, 10)

write.csv(out, file = "/Users/ingridslette/Library/CloudStorage/GoogleDrive-slett152@umn.edu/Shared drives/LTER-WG_Resilience-Management/data/raw_data/mswep-daily-ppt-lter-ltar-sites.csv")

