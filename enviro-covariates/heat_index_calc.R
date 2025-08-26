#Calculating Heat Indices for Sites using Daymet Data
#Beatriz A. Aguirre & Lina Aoyama

library(dplyr)
library(ggplot2)
library(tidyverse)
library(googledrive)

###Read in Daymet daily weather data###
#setwd("~/Desktop")
#import data
#daymet.data <- read.csv("daymet_daily_weather.csv")

# Make needed folder(s)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "pre_processed_data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Identify desired file
focal_file <- "daymet_daily_weather.csv"

# Download data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Sw-CdVIsCNvnS3laPn1a90WHoZsEoMif")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "pre_processed_data", .$name))

# Read in data
daymet_daily_raw <- read.csv(file = file.path("data", "pre_processed_data", focal_file))

# Check structure
dplyr::glimpse(daymet_daily_raw)

##################################################
#Maximum temperature during the growing season - Vogel 2019 
max.temp.growing.season <- daymet_daily_raw%>% 
  filter(month %in% c('3', '4', '5', '6', '7', '8')) %>% 
  group_by(network, site_id, year) %>% 
  summarise(Tmaxc = max(tmax_degC))

#Warm day frequency = percentage of days during the growing season with daily max temp above 90th percentile - Vogel 2019
#Find 90th percentile for each site: 
#DISCUSS AS A GROUP IF THIS QUANTILE IS WHAT WE WANT TO GO WITH
quant.90th.maxtemp <- daymet_daily_raw %>%
  filter(month %in% c('3', '4', '5', '6', '7', '8')) %>%
  group_by(network, site_id) %>%
  summarise(per_90_quant = quantile(tmax_degC, probs=0.90))
#Count number of days above the 90th percentile threshold
daymet_data_90th <- merge(daymet_daily_raw, quant.90th.maxtemp, by = c("network", "site_id")) %>%
  filter(month %in% c('3', '4', '5', '6', '7', '8')) %>% 
  mutate(extreme_day = ifelse(tmax_degC>per_90_quant, 1, 0)) %>%
  group_by(network, site_id, year) %>%
  summarise(warmday_freq = sum(extreme_day)/180)



#Plot something
mean.daily.max.temp %>% 
  group_by(network, site_id) %>% 
  ggplot(aes(x=year, y=mean.daily.Tmaxc, group = network)) +
  geom_point(aes(color=site_id)) + 
  facet_grid(~site_id)