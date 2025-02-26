## Script started by Katherine Muller, 2025-02-26
## purpose is to display sites with summarized weather variables. 

## download the site info figure:
source("ancillary/google_drive_urls.R")

## downloaded data
## site info with lat-lon
site_drive <-googledrive::drive_ls(googledrive::as_id(dir.data)) %>% 
  dplyr::filter(name == "site_summary_info")
googledrive::drive_download(file = site_drive$id, overwrite = T, type = "csv",
                            path = file.path("data", site_drive$name))
site_info <- read.csv("data/site_summary_info.csv",fileEncoding = "UTF-8-BOM")

tidy_drive <- googledrive::drive_ls(googledrive::as_id(dir.tidy_data))
weather_drive <- tidy_drive %>% dplyr::filter(grepl("daymet", name))

purrr::walk2(.x = weather_drive$name,
             .y = weather_drive$id,
              ~ googledrive::drive_download(file = .y, overwrite = T, type = "csv",
                                         path = file.path("data","tidy_data", .x)))
            
maw <- read.csv("data/tidy_data/daymet_meanannual_weather.csv")

# make plot------

ggplot(maw,aes(x = mean_tmax_degC, 
               y = mean_precip_mmyear, 
               color = network,
               label = site_id)) + geom_point()+geom_label()
  
