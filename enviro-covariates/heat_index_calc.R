#Calculating Heat Indices for Sites using Daymet Data
#Beatriz A. Aguirre & Lina Aoyama

library(dplyr)
library(ggplot2)
library(tidyverse)
library(googledrive)
library(slider)

################################################
#Read in Daymet daily weather data

# Make needed folder(s)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "pre_processed_data"), showWarnings = F)

# Clear environment + collect garbage
#rm(list = ls()); gc()

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
calc_hot_days <- merge(daymet_daily_raw, quantile.maxtemp, by = c("network", "site_id")) %>%
  filter(month %in% c('3', '4', '5', '6', '7', '8')) %>% 
  mutate(over_90th = ifelse(tmax_degC>per_90_quant, 1, 0),
         over_95th = ifelse(tmax_degC>per_95_quant, 1, 0),
         over_98th = ifelse(tmax_degC>per_98_quant, 1, 0),
         over_99th = ifelse(tmax_degC>per_99_quant, 1, 0)) 
extreme_temp_days<- calc_hot_days%>%
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
#Lengths of heat waves (max consecutive days over 95th percentile) during the growing season in days - Smith et al 2013
calc_heat_wave_duration <- function(x) {
  rle_result <- rle(x)
  heat_waves <- rle_result$lengths[rle_result$values == 1]
  ifelse(length(heat_waves) > 0, max(heat_waves), 0)
}
  
rle_result <- rle((calc_hot_days%>%filter(site_id == "ABS_RCREC" & year == "1981"))$over_95th) 
print(rle$lengths)
print(rle$values)
heat_waves <- rle_result$lengths[rle_result$values == 1]
max(rle_result$lengths[rle_result$values == 1])

heat_wave_duration <- calc_hot_days %>%
  group_by(network, site_id, year) %>%
  summarise(consecutive_days_heat_wave = calc_heat_wave_duration(over_95th))

#plot heat_wave_duration (max number of consecutive days where daily max temp was over 95th percentile during the growing season) 
ggplot(heat_wave_duration, aes(y=site_id, x=year, color=consecutive_days_heat_wave)) + 
  geom_point() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_color_gradient2(low = "white", mid = "pink", high = "red", midpoint = 12)

#################################################################
#Mean and max daily Tmax of heat waves (2+ consecutive days over 95th percentile)
heat_wave_meanT <- left_join(calc_hot_days, heat_wave_duration, by = c("network", "site_id", "year")) %>%
  filter(consecutive_days_heat_wave >= 2) %>%
  filter(over_95th == 1) %>%
  group_by(network, site_id, year) %>%
  summarise(meanTmax_95th = mean(tmax_degC), 
            Tmax_95th = max(tmax_degC))

#################################################################
#Rolling window 3 day average of daily max temperature 
rolling_3day_tmax <- daymet_daily_raw %>%
  arrange(site_id, yday) %>%
  group_by(site_id, year) %>%
  mutate(temp_roll3 = slide_dbl(
    .x = tmax_degC,
    .f = mean, 
    .before = 2, #look back 2 rows + current row = 3 days
    .complete = TRUE #returns NA for the first 2 days of each year
  )) %>%
  ungroup()
#max 3-day ave tmax during the growing season
max_3day_temp <- rolling_3day_tmax %>%
  filter(month %in% c('3', '4', '5', '6', '7', '8')) %>% 
  group_by(network, site_id, year) %>% 
  summarise(Tmaxroll3 = max(temp_roll3))

#################################################################
#Make a temp extreme csv
heat_indices <- merge(max.temp.growing.season, quantile.maxtemp, by = c("network", "site_id")) %>%
  merge(., extreme_temp_days, by = c("network", "site_id", "year")) %>%
  left_join(., warm_day_freq, by = c("network", "site_id", "year", "num_days_90th", "num_days_95th", "num_days_98th", "num_days_99th")) %>%
  left_join(., heat_wave_duration, by = c("network", "site_id", "year")) %>%
  left_join(., heat_wave_meanT, by = c("network", "site_id", "year")) %>%
  left_join(., max_3day_temp, by = c("network", "site_id", "year"))
write.csv(heat_indices, "data/harmonized_data/heat_indices_site.csv")
#update the temp extreme csv
drive_upload(
  path = as_id("13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ"), #harmonized data folder
  media = "data/harmonized_data/heat_indices_site.csv", 
  name = "heat_indices_site.csv",
  overwrite = TRUE
  )
#Correlation matrix of temp metrics
library(corrplot)
heat_matrix <- cor(heat_indices[,c(4,9, 10, 11, 12, 13, 14 )])
corrplot(heat_matrix, type = "upper", order = "hclust", tlcol = "black", tlsrt = 45)

##################################################################
##Biomass vs temp extreme

#Read in ANPP DATA
# Identify desired file
biomass_file <- "04_anpp_aggregated-site-crop.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == biomass_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

# Read in harmonized data
anpp_v1 <- read.csv(file = file.path("data", "harmonized_data", biomass_file))

# Check structure
dplyr::glimpse(anpp_v1)

#Join biomass and temp metrics
anpp_v2 <- anpp_v1 %>% dplyr::rename(site_id = site)
anpp_heat <- left_join(anpp_v2, heat_indices, by = c("network", "site_id", "year"))

#plot Tmax and biomass
ggplot(anpp_heat %>% drop_na(Tmaxc, anpp_g_m2), aes(x = Tmaxc, y = anpp_g_m2))+
  geom_point()+
  theme_bw()
ggplot(anpp_heat%>% drop_na(Tmaxc, anpp_g_m2), aes(x = Tmaxc, y = anpp_g_m2))+
  geom_point()+
  theme_bw()+
  facet_wrap(~site_id)

#plot number of days over the 95th percentile during the growing season and biomass
ggplot(anpp_heat%>%drop_na(num_days_95th, anpp_g_m2), aes(x = num_days_95th, y = anpp_g_m2, na.rm = TRUE))+
  geom_point()+
  theme_bw()
ggplot(anpp_heat%>%drop_na(num_days_95th, anpp_g_m2), aes(x = num_days_95th, y = anpp_g_m2, na.rm = TRUE))+
  geom_point()+
  theme_bw()+
  facet_wrap(~site_id)

#plot warm day frequency (percentage of days during the growing season with daily max temp above 90th percentile) and biomass
ggplot(anpp_heat%>%drop_na(warm_day_90th, anpp_g_m2), aes(x = warm_day_90th, y = anpp_g_m2, na.rm = TRUE))+
  geom_point()+
  theme_bw()
ggplot(anpp_heat%>%drop_na(warm_day_90th, anpp_g_m2), aes(x = warm_day_90th, y = anpp_g_m2, na.rm = TRUE))+
  geom_point()+
  theme_bw()+
  facet_wrap(~site_id)

#plot consecutive days heat wave length and biomass
ggplot(anpp_heat%>%drop_na(consecutive_days_heat_wave, anpp_g_m2), aes(x = consecutive_days_heat_wave, y = anpp_g_m2, na.rm = TRUE))+
  geom_point()+
  theme_bw()
ggplot(anpp_heat%>%drop_na(consecutive_days_heat_wave, anpp_g_m2), aes(x = consecutive_days_heat_wave, y = anpp_g_m2, na.rm = TRUE))+
  geom_point()+
  theme_bw()+
  facet_wrap(~site_id)

#plot mean Tmax 2+ consecutive days, 95th percentile and biomass
ggplot(anpp_heat%>%drop_na(meanTmax_95th, anpp_g_m2), aes(x = meanTmax_95th, y = anpp_g_m2, na.rm = TRUE))+
  geom_point()+
  theme_bw()
ggplot(anpp_heat%>%drop_na(meanTmax_95th, anpp_g_m2), aes(x = meanTmax_95th, y = anpp_g_m2, na.rm = TRUE))+
  geom_point()+
  theme_bw()+
  facet_wrap(~site_id)

#plot max Tmax 2+ consecutive days, 95th percentile and biomass
ggplot(anpp_heat%>%drop_na(Tmax_95th, anpp_g_m2), aes(x = Tmax_95th, y = anpp_g_m2, na.rm = TRUE))+
  geom_point()+
  theme_bw()
ggplot(anpp_heat%>%drop_na(Tmax_95th, anpp_g_m2), aes(x = Tmax_95th, y = anpp_g_m2, na.rm = TRUE))+
  geom_point()+
  theme_bw()+
  facet_wrap(~site_id)
###################################################################
#Next steps: 
#regional analysis of when the heat waves are happening



