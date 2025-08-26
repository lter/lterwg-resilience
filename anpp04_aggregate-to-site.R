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

# Identify desired file
focal_file <- "03_anpp_wrangled.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

# Read in harmonized data
site_v1 <- read.csv(file = file.path("data", "harmonized_data", focal_file))

# Check structure
dplyr::glimpse(site_v1)

## ------------------------------------------- ##
# Column Re-Ordering ----
## ------------------------------------------- ##

# Reorder remaining columns more intuitively
#site_v2 <- site_v1 %>% 
  # Coords near site info
#  dplyr::relocate(lat, long, .after = site) %>% 
#  dplyr::relocate(elevation_m, habitat, .before = site) %>% 
  # Treatment specifics after main 'treatment' column
#  dplyr::relocate(dplyr::contains("_y.n"), .after = treatment) 

# Check structure
#dplyr::glimpse(site_v2)

## ------------------------------------------- ##
# Summarize Within Sites ----
## ------------------------------------------- ##

# Identify all columns at/above "site" column
#(grp_cols <- setdiff(x = names(site_v1), y = c("block", "plot", "quadrat", "date",
#                                               "anpp_actual")))

# Summarize across subsamples within a plot/level
site_v2 <- site_v1 %>% 
  dplyr::group_by(site,location, year, network,treatment,month,crop,country,duration_years, plot) %>% 
  dplyr::summarize(anpp_g_m2 = mean(anpp_g_m2, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()


# Summarize within "sites" and "locations"
site_v3 <- site_v2 %>% 
  dplyr::group_by(site,location, year, network,treatment,month,crop,country,duration_years) %>% 
  dplyr::summarize(anpp_g_m2 = mean(anpp_g_m2, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()

site_v4 <- site_v3 %>% 
  subset(treatment != "004b" & treatment != "020b")%>% #remove additional treatments from KNZ
  dplyr::group_by(site, year, network,treatment,month,crop,country,duration_years) %>% 
  dplyr::summarize(anpp_g_m2 = mean(anpp_g_m2, na.rm = T),
                   .groups = "keep") %>% 
  dplyr::ungroup()


# Check structure
dplyr::glimpse(site_v3)

## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##


# Make nice output name
focal_output <- "04_anpp_aggregated-site-crop.csv"

# Export locally
write.csv(x = site_v4, row.names = F, na = '', file = file.path("data", "harmonized_data", focal_output))

# Upload to Drive
googledrive::drive_upload(media = file.path("data", "harmonized_data", focal_output), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ"))

# Export the treatment - year combination to generate a table for management
trt <- site_v4 %>%
  select (c(network, site, treatment, year, crop)) %>%
  unique()

# Identify nice name for exported object
focal_name <- "treatment_table_year.csv"

# Export locally
write.csv(x = trt , row.names = F, na = '',
          file = file.path("data", focal_name))

# Upload to Drive
googledrive::drive_upload(media = file.path("data",  focal_name), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Ty7QX7vyvD797eKJzMWbr8AwIo-GyBFO"))


# End ----
