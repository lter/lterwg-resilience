# Purpose of this script is to calculate daily VPD and 
# visualize distribution of network annual VPD
# Beatriz A. Aguirre
# Febuary 26, 2025

library(dplyr)

#create necessary folders:
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "raw"), showWarnings = F)
dir.create(file.path("data", "pre_processed_data"), showWarnings = F)

#Run Katherine's code to access daymet data in drive: 
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

# Read in daymet daily weather data
daymet.daily <- read.csv(file = file.path("data", "tidy_data", "daymet_daily_weather.csv"))


# Define Acronyms:
# VPD: Vapor Pressure Deficit
# SVP: Saturated Vapor Pressure
# WVP: Water Vapor Pressure (vp_Pa column in the daymet data)

#Calculate VPD 
#(VPD = SVP - WVP )
daymet.daily %>% 
  mutate(SVP_Pa = 610.78 * exp(tmean_degC / (tmean_degC +237.3) * 17.2694), # units: Pascals
         VPD_kPa = ((SVP_Pa - vp_Pa)/1000)) %>% # VPD units: kPa
  group_by(site_id, network, year) %>% 
  summarise(mean.annual.vpd = mean(VPD_kPa)) -> daymet.annual.weather.vpd

#VPD histogram
library(ggplot2)
library(ggpubr)
vpd.p1 <- ggplot(daymet.annual.weather.vpd) + geom_histogram(aes(x = mean.annual.vpd,
                                       color = network), fill = NA, lwd=2.3) 
vpd.p1


vpd.p2 <- ggplot(daymet.annual.weather.vpd) + geom_dotplot(aes(x = mean.annual.vpd,y = network,
                                     fill = network), binwidth=.02, stackdir = "center")
vpd.p2

vpd.plots <- ggarrange(vpd.p1, vpd.p2)
vpd.plots

#create necessary folder:
dir.create(file.path("data", "exploratory_graphs"), showWarnings = F)

#save figure locally
ggsave(filename = file.path("data", "exploratory_graphs", "vpd_network_histogram_dotplot_BAA.png"),
                            plot = vpd.plots, width = 12, height = 6, units = "in", dpi = 300)

#upload figure to google drive
googledrive::drive_upload(media = file.path("data", "exploratory_graphs", "vpd_network_histogram_dotplot_BAA.png"), overwrite = T,
                          path = googledrive::as_id(dir.exploratory_graphs))





