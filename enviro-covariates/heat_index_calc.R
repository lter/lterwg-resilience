#Calculating Heat Indices for Sites using Daymet Data
#Beatriz A. Aguirre & Lina Aoyama

library(dplyr)
library(ggplot2)
library(tidyverse)
library(googledrive)

################################################
#Read in Daymet daily weather data

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
#Maximum temperature (absolute) during the growing season - Vogel 2019 
#length of the growing season: March-August
max.temp.growing.season <- daymet_daily_raw%>% 
  filter(month %in% c('3', '4', '5', '6', '7', '8')) %>% 
  group_by(network, site_id, year) %>% 
  summarise(Tmaxc = max(tmax_degC))

#Find 90th-99th percentiles for each site: 
quantile.maxtemp <- daymet_daily_raw %>%
  filter(month %in% c('3', '4', '5', '6', '7', '8')) %>%
  group_by(network, site_id) %>%
  summarise(per_90_quant = quantile(tmax_degC, probs=0.90),
            per_95_quant = quantile(tmax_degC, probs=0.95),
            per_98_quant = quantile(tmax_degC, probs=0.98),
            per_99_quant = quantile(tmax_degC, probs=0.99))

#plot max temp and 90th percentile quantile
ggplot(max.temp.growing.season, aes(y=Tmaxc, x=year)) + 
  geom_point() + 
  geom_line()+
  geom_hline(data = quantile.maxtemp, aes(yintercept= per_90_quant), color = "red", linetype = "solid")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~site_id)


##################################################
#Number of days during the growing season with daily maximum temp above the 90th-99th percentiles
#Count number of days above the thresholds
extreme_temp_days <- merge(daymet_daily_raw, quantile.maxtemp, by = c("network", "site_id")) %>%
  filter(month %in% c('3', '4', '5', '6', '7', '8')) %>% 
  mutate(over_90th = ifelse(tmax_degC>per_90_quant, 1, 0),
         over_95th = ifelse(tmax_degC>per_95_quant, 1, 0),
         over_98th = ifelse(tmax_degC>per_98_quant, 1, 0),
         over_99th = ifelse(tmax_degC>per_99_quant, 1, 0)) %>%
  group_by(network, site_id, year) %>%
  summarise(num_days_90th = sum(over_90th),
            num_days_95th = sum(over_95th),
            num_days_98th = sum(over_98th),
            num_days_99th = sum(over_99th))

#DISCUSS AS A GROUP IF THIS QUANTILE IS WHAT WE WANT TO GO WITH
#plot total number of days where daily max temp was greater than or equal to the thresholds over time 
ggplot(extreme_temp_days, aes(y=site_id, x=year, color=num_days_90th)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_color_gradient2(low = "white", mid = "pink", high = "red", midpoint = 40)
ggplot(extreme_temp_days, aes(y=site_id, x=year, color=num_days_95th)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_color_gradient2(low = "white", mid = "pink", high = "red", midpoint = 30)
ggplot(extreme_temp_days, aes(y=site_id, x=year, color=num_days_98th)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_color_gradient2(low = "white", mid = "pink", high = "red", midpoint = 20)
ggplot(extreme_temp_days, aes(y=site_id, x=year, color=num_days_99th)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_color_gradient2(low = "white", mid = "pink", high = "red", midpoint = 15)

################################################################
#Warm day frequency = percentage of days during the growing season with daily max temp above 90th percentile - Vogel 2019
warm_day_freq <- extreme_temp_days %>%
  mutate(warm_day_90th = num_days_90th/180)

#plot warm day freq by site
ggplot(warm_day_freq, aes(y=warm_day_90th, x=year, color = site_id)) + 
  geom_point() + 
  geom_line()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~network)

#################################################################
#2 + consecutive days of hot days

#################################################################
#5 + consecutive days of hot days



#regional analysis of when the heat waves are happening
