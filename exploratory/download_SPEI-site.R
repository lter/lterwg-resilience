## ----------------------------------------------------------------- ##
# Download SPEI time series
## ----------------------------------------------------------------- ##
# Authors: Makki Khorchani

# Purpose
## Download SPEI 6 months data for all sites and save a data frame on the drive.
## The SPEI NC files are downloaded from this website https://spei.csic.es/spei_database/#map_name=spei01#map_position=1475

# Clear environment + collect garbage
rm(list = ls()); gc()

# Load libraries
library(googledrive)
library(leaflet)
library(ncdf4)
library(readxl)
library(purrr)



# NOTE
## This script assumes (1) access to the "LTER-WG_Resilience-Management" Shared Drive (2) authentication with R
## For more information on authentication, see the following tutorial:
### https://lter.github.io/scicomp/tutorial_googledrive-pkg.html

# Identify wanted files
files_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Sw-CdVIsCNvnS3laPn1a90WHoZsEoMif")) %>% 
  dplyr::filter(stringr::str_detect(string = .$name, pattern = "\\.csv"))

# Did that work?
files_drive

# Identify local files
files_local <- dir(path = file.path("data", "pre_processed_data"))
files_local

# Overwrite local data files?
update <- FALSE

# Identify desired files
if(update == T) {
  files_wanted <- files_drive 
} else {
  files_wanted <- files_drive %>%
    dplyr::filter(!name %in% files_local)
}

# Download them!
purrr::walk2(.x = files_wanted$id, .y = files_wanted$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "pre_processed_data", .y)))

# Grab the data key
site_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Ty7QX7vyvD797eKJzMWbr8AwIo-GyBFO")) %>% 
  dplyr::filter(name == "site_summary_info")

# Did that work?
site_drive

# Download the site data
googledrive::drive_download(file = site_drive$id, overwrite = T, type = "csv",
                            path = file.path("data", site_drive$name))



# Choose an output name for the final csv file  and the SPEI dataset you want to work with (the outputname and nc_filename should include the path to the right folder)
output_name<-"SPEI12.csv"
nc_filename<-"spei12.nc"


# importing site data (coordinates and siteID)
sites <- read.csv(file = file.path("data", "site_summary_info.csv"), header = TRUE)

# Creating a leaflet map of the site locations
# leaflet(data = sites) %>%
#   addTiles() %>%  # Add default OpenStreetMap layer
#   addCircleMarkers(
#     lng = ~longitude,      # use the longitude column
#     lat = ~latitude,       # use the latitude column
#     radius = 5,            # point size
#     color = "red",         # outline color
#     fillColor = "red",     # fill color
#     fillOpacity = 0.8,
#     
#     # Option 1: Show site ID when hovering (label)
#     label = ~site_id,
#     labelOptions = labelOptions(noHide = FALSE, textsize = "12px"),
#     
#     # Option 2: Show site ID when clicked (popup)
#     popup = ~paste("Site ID:", site_id)
#   )

# Open the ncdf file containing the global SPEI data
ncfile <- nc_open(file.path("data","pre_processed_data", nc_filename))

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


# Export final dataset
write.csv(final_df,file=file.path("data","pre_processed_data", output_name),row.names = F)


# Upload them to the drive
purrr::walk(.x = output_name,
            .f = ~ googledrive::drive_upload(media = file.path("data", "pre_processed_data", .x), overwrite = T, path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Sw-CdVIsCNvnS3laPn1a90WHoZsEoMif")))


