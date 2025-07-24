#generate Water Year based metrics
#7/24/25



#library

library(lubridate)
library(tidyverse)
library(googledrive)
drive_auth()

# Identify wanted files
files_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1Ty7QX7vyvD797eKJzMWbr8AwIo-GyBFO")) %>% 
  dplyr::filter(stringr::str_detect(string = .$name, pattern = "\\.csv"))

# Did that work?
files_drive

# Identify local files
files_local <- dir(path = file.path("data"))
files_local

# Overwrite local data files?
update <- TRUE

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
                                                path = file.path("data", .y)))

# #make the key - we need to just do this once.
# key<-begin_key(raw_folder = file.path("data", "pre-processed_data"))
# 
# 
# Grab the data key
key_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1Ty7QX7vyvD797eKJzMWbr8AwIo-GyBFO")) %>%
  dplyr::filter(name == "resilience_data_key")

# Did that work?
key_drive

# Download the data key
googledrive::drive_download(file = key_drive$id, overwrite = T, type = "csv",
                            path = file.path("data", key_drive$name))

#load mswep data retrieved in script "retrieve-mswep-ppt-data.r"

# Identify desired file
focal_file <- "mswep-daily-ppt-lter-ltar-sites.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1zI1KYBlROyBZSgjSEYmVjsIfCmRPpUPq")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "raw", .$name))

# Read in harmonized data
mswep_lter.ltar <- read.csv(file = file.path("data", "raw", focal_file))%>%
  dplyr::mutate(date = ymd(date))%>%
  filter(!(site_id == "KBS" & project_id == "LTER"))


# Identify desired file
focal_file <- "mswep-daily-ppt-nutnet-sites.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1zI1KYBlROyBZSgjSEYmVjsIfCmRPpUPq")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "raw", .$name))

# Read in harmonized data
mswep_nutnet <- read.csv(file = file.path("data", "raw", focal_file))%>%
  dplyr::mutate(project_id = "NutNet")%>%
  dplyr::mutate(date = ymd(date))



mswep <- rbind(mswep_lter.ltar, mswep_nutnet)

#calculate water year
wyr_ppt_allyrs <- mswep%>%
  mutate(w_yr = ifelse(month > 9, year + 1, year) )%>%
  group_by(w_yr, site_id, project_id)%>%
  summarise(wyr_ppt  = sum(precip, na.rm = T) )%>%
  rename(network = project_id)


#calculate site level
wyr_site_map <- wyr_ppt_allyrs%>%
  group_by(site_id, network)%>%
  summarize(map = mean(wyr_ppt, na.rm = T),
            cv_ppt = sd(wyr_ppt, na.rm = T)/ map * 100)

#get treatment data 

site_treatment_yr <- read.csv(file = file.path("data", 'treatment_table_year.csv'))%>%
  rename(site_id = site,
         w_yr = year)


#merge treatment yrs and water yr ppt
unique(site_treatment_yr$site_id)
unique(wyr_ppt_allyrs$site_id)

#get precip variables over only site x treatment x year combos with ANPP data
wyr_site_anppyrs <- merge(wyr_ppt_allyrs,site_treatment_yr, by = c('site_id', 'network', 'w_yr'))

#summarize
wyr_focal_map <- wyr_site_anppyrs%>%
  group_by(site_id, network)%>%
  summarize(map = mean(wyr_ppt, na.rm = T),
            cv_ppt = sd(wyr_ppt, na.rm = T)/ map * 100,
            )
