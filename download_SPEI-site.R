## ----------------------------------------------------------------- ##
# Resilience Management - Harmonization Workflow
## ----------------------------------------------------------------- ##
# Authors: Makki Khorchani

# Purpose
## Download SPEI data for all sites and save a data frame on the drive.
## The SPEI NC files are downloaded from this website https://spei.csic.es/spei_database/#map_name=spei01#map_position=1475


# Load libraries
library(googledrive)
library(leaflet)
library(ncdf4)
library(readxl)
library(raster)
library(tidyverse)

# Clear environment + collect garbage
rm(list = ls()); gc()

## ------------------------------------------- ##
# Download Data ----
## ------------------------------------------- ##

# NOTE
## This script assumes (1) access to the "LTER-WG_Resilience-Management" Shared Drive (2) authentication with R
## For more information on authentication, see the following tutorial:
### https://lter.github.io/scicomp/tutorial_googledrive-pkg.html


drive_auth() 

# Grab the data key for the site summary info file from Google drive. 
# link below is the path for the location of the file on the drive
key_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name %in% "site_coordinates_combined.csv")

# Did that work?
key_drive

# Download the file
googledrive::drive_download(file = key_drive$id, overwrite = T, type = "csv",
                            path = file.path("data", key_drive$name))


# importing site data (coordinates and siteID)
sites <- read.csv(file.path("data","site_coordinates_combined.csv")) %>%
  rename(site_id=site)


# Creating a leaflet map of the site locations
leaflet(data = sites) %>%
  addTiles() %>%  # Add default OpenStreetMap layer
  addCircleMarkers(
    lng = ~longitude,      # use the longitude column
    lat = ~latitude,       # use the latitude column
    radius = 5,            # point size
    color = "red",         # outline color
    fillColor = "red",     # fill color
    fillOpacity = 0.8,
    
    # Option 1: Show site ID when hovering (label)
    label = ~site_id,
    labelOptions = labelOptions(noHide = FALSE, textsize = "12px"),
    
    # Option 2: Show site ID when clicked (popup)
    popup = ~paste("Site ID:", site_id)
  )

#spei_data path="https://drive.google.com/drive/u/0/folders/1JtFMD4IAizjNGd0wbLLZIBdgqnk4YR97"
#location to upload spei.csv outputs
folder_id <- as_id("https://drive.google.com/drive/u/0/folders/1JtFMD4IAizjNGd0wbLLZIBdgqnk4YR97")

#list gloabl ncdf files on the spei_data folder
ncs<-list.files(file.path("data","spei_data"),pattern = ".nc")

extract=TRUE
upload=TRUE
#extract site spei times series for all scales (01-24) for all sites and upload output files to spei_data
for (nc in ncs) {
  nc_filename<-file.path("data","spei_data",nc)  

# Creates an output name for the final csv file
output_name<-gsub(".nc$", ".csv", nc_filename)

if (extract==TRUE) {
# Open the ncdf file containing the global SPEI data
ncfile <- nc_open(nc_filename)

# Extract the main variables
spei_var <- ncvar_get(ncfile, "spei")  # shape typically [lon, lat, time]
lon <- ncvar_get(ncfile, "lon")   # vector of longitude
lat <- ncvar_get(ncfile, "lat")   # vector of latitude
time <- ncvar_get(ncfile, "time")  # numeric time axis

# Get the time units (e.g., "days since 1900-01-01")
time_units <- ncatt_get(ncfile, "time", "units")$value

nc_close(ncfile)  # always a good idea to close the file after reading

# Converting to date format
time_as_date <- as.Date(time, origin = "1900-01-01")

# This function returns the index of xVec that’s closest to x. 
# This function will be used to identify the closest corresponding coordinate for the NC file.
nearestIndex <- function(x, xVec) {
  which.min(abs(xVec - x))
}

# Extract just the site coordinates
coords <- sites[, c("longitude", "latitude")]

# coords is an n x 2 data frame. We'll find nearest indices in lon, lat
idx <- apply(coords, 1, function(pt) {
  lon_idx <- nearestIndex(pt["longitude"], lon)
  lat_idx <- nearestIndex(pt["latitude"],  lat)
  c(lon_idx, lat_idx)
})


# Initialize the final data frame:
final_df <- data.frame(
  date = time_as_date  # or 'time' if you prefer numeric
)

# Number of sites
n_sites <- nrow(sites)

# Loop over each site and extract its time series
for(i in seq_len(n_sites)) {
  # We'll use the actual site ID (character/string) as the column name
  site_colname <- as.character(sites$site_id[i])
  
  # Extract the SPEI time series for this site (third dimension is time)
  lon_idx <- idx[1, i]
  lat_idx <- idx[2, i]
  
  ts_values <- spei_var[lon_idx, lat_idx, ]
  
  # Add the time series as a new column in final_df
  final_df[[site_colname]] <- ts_values
}

write.csv(final_df,output_name,row.names = F)
}
if(upload==TRUE){
drive_upload(output_name,path=folder_id)}
}



