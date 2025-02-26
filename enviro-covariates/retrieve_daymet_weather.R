library(daymetr)
library(purrr)
library(dplyr)
library(tibble)
library(lubridate)
library(dplyr)
## downloaded data
site_info <- read.csv("data/site-coordinates.csv",fileEncoding = "UTF-8-BOM")
str(site_info)

## remove non-us nutnet sites. 
#unique(site_info$site_id)
#tapply(site_info$site_id, site_info$project_id, function(x){sort(unique(x))})
## filtering only applies to sites with project_id nutnet

sitesToExclude <- site_info$site_id[which(site_info$project_id=="NutNet" &
                          !grepl("(.us$)|(.ca$)",site_info$site_id))]
setdiff(site_info$site_id, sitesToExclude)# OK.

site_info <- site_info[which(!site_info$site_id %in% sitesToExclude),]

#--- download daymet data ---#
output <- c()
for(i in 1:nrow(site_info)){
  temp_daymet <- download_daymet(
    lat = site_info$latitude[i],
    lon = site_info$longitude[i],
    start = 1980,
    end = 2023
  ) 
  temp_daymet_data <- temp_daymet$data %>% add_column(site = site_info$site_id[i],
                                                      lat_dd = site_info$latitude[i],
                                                      lon_dd = site_info$longitude[i],
                                                      .before = 1)
  output <- rbind(output, temp_daymet_data)
  rm(temp_daymet_data)
  cat(paste(site_info$site_id[i],"check"))
  }

#--- structure ---#
str(temp_daymet)

## add date and month.
startDate <- as.Date("1980-01-01")
endDate <- as.Date("2023-12-31")
endDate - startDate
datedf <- data.frame(date = startDate + 0:16070)
datedf$year <- format(datedf$date, format = "%Y") %>% as.integer()
datedf$month <- format(datedf$date, format = "%m") %>% as.integer()
datedf$yday <- lubridate::yday(datedf$date)
weather <- left_join(output, datedf, by = c("year", "yday" ))


## saved in google drive temp_raw folder:
#write.csv(weather, file = "data/daymet_20250225.csv", row.names = FALSE)

