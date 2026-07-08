library(daymetr)
library(purrr)
library(dplyr)
library(tibble)
library(lubridate)
library(dplyr)
library(googledrive)
source("ancillary/google_drive_urls.R")

## downloaded data
site_drive <-googledrive::drive_ls(googledrive::as_id(dir.data)) %>% 
  dplyr::filter(name == "site_summary_info.csv")
googledrive::drive_download(file = site_drive$id, overwrite = T, type = "csv",
                            path = file.path("data", site_drive$name))
site_info <- read.csv("data/site_summary_info.csv",fileEncoding = "UTF-8-BOM")
str(site_info)

#--- download daymet data ---#
output <- c()
for(i in 1:nrow(site_info)){
  temp_daymet <- download_daymet(
    lat = site_info$latitude[i],
    lon = site_info$longitude[i],
    start = 1980,
    end = 2023
  ) 
  temp_daymet_data <- temp_daymet$data %>% add_column(site_id = site_info$site_id[i],
                                                      network = site_info$network[i],
                                                      .before = 1)
  output <- rbind(output, temp_daymet_data)
  rm(temp_daymet_data)
  cat(paste(site_info$site_id[i],"check\n"))
}

## fix names. 
namefix <- c("daylight_secday"= "dayl..s."  ,
             "precip_mmday" =  "prcp..mm.day.",
             "shortwave_rad_Wm2" = "srad..W.m.2."  ,
             "snow_water_eq_kgm2" = "swe..kg.m.2."  ,
             "tmax_degC" = "tmax..deg.c." ,
             "tmin_degC" = "tmin..deg.c." ,
             "vp_Pa" = "vp..Pa."  )


output2 <- output %>% rename(all_of(namefix))
# add mean temp. 
output2$tmean_degC <- (output2$tmax_degC + output2$tmin_degC)/2

## add date and month.
startDate <- as.Date("1980-01-01")
endDate <- as.Date("2023-12-31")
endDate - startDate
datedf <- data.frame(date = startDate + 0:16070)
datedf$year <- format(datedf$date, format = "%Y") %>% as.integer()
datedf$month <- format(datedf$date, format = "%m") %>% as.integer()
datedf$yday <- lubridate::yday(datedf$date)
weather <- left_join(output2, datedf, by = c("year", "yday" ))

## saved in google drive temp_raw folder:
#write.csv(weather, file = "data/harmonized_data/daymet_daily_weather.csv", row.names = FALSE)
# upload to google drive
googledrive::drive_upload(media = file.path("data", "harmonized_data","daymet_daily_weather.csv"), overwrite = T,
                          path = googledrive::as_id(dir.harmonized_data))

# Summarize to monthly, annual, and mean annual--------
monthly <- weather %>% group_by(site_id, network, year, month) %>%
              summarize(mean_tmax_degC = mean(tmax_degC),
                        mean_tmin_degC = mean(tmin_degC),
                        mean_tmean_degC = mean(tmean_degC),
                        precip_mmmonth = sum(precip_mmday))
annual <- monthly %>% group_by(site_id, network, year) %>%
              summarize(mean_tmax_degC = mean(mean_tmax_degC),
                        mean_tmin_degC = mean(mean_tmin_degC),
                        mean_tmean_degC = mean(mean_tmean_degC),
                        precip_mmyear = sum(precip_mmmonth)
                        )
meanannual <- annual %>% group_by(site_id, network) %>%
                summarize(mean_tmax_degC = mean(mean_tmax_degC),
                          mean_tmin_degC = mean(mean_tmin_degC),
                          mean_tmean_degC = mean(mean_tmean_degC),
                          mean_precip_mmyear = mean(precip_mmyear)
                          )
write.csv(monthly, file = "data/harmonized_data/daymet_monthly_weather.csv" , row.names = FALSE)
write.csv(annual, file = "data/harmonized_data/daymet_annual_weather.csv" , row.names = FALSE)
write.csv(meanannual, file = "data/harmonized_data/daymet_meanannual_weather.csv" , row.names = FALSE)

googledrive::drive_upload(media = file.path("data", "harmonized_data","daymet_monthly_weather.csv"), overwrite = T,
                          path = googledrive::as_id(dir.harmonized_data))
googledrive::drive_upload(media = file.path("data", "harmonized_data","daymet_annual_weather.csv"), overwrite = T,
                          path = googledrive::as_id(dir.harmonized_data))
googledrive::drive_upload(media = file.path("data", "harmonized_data","daymet_meanannual_weather.csv"), overwrite = T,
                          path = googledrive::as_id(dir.harmonized_data))
