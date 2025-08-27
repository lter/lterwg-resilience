#This script uses Daymet data to calculate different percentiles of max temperature 
#Beatriz A. Aguirre
#August 26, 2025

rm(list = ls())
library(dplyr)
library(ggplot2)
library(googledrive)

#create necessary folders:
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "raw"), showWarnings = F)
dir.create(file.path("data", "pre_processed_data"), showWarnings = F)

# Download data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Sw-CdVIsCNvnS3laPn1a90WHoZsEoMif")) %>% 
  dplyr::filter(name == "daymet_daily_weather.csv") %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "pre_processed_data", .$name)

# Read in Daymet data
daymet.data <- read.csv(file = file.path("data", "tidy_data", "daymet_daily_weather.csv"))

####################################

#Create df with only with desired months:
daymet.data %>%
  filter(month %in% c('3', '4', '5', '6', '7', '8')) -> daymet.data

#Find 90th, 95th, 98th and 99th percentile of Temp. max for each site: 
######################################################################
#DISCUSS AS A GROUP WHAT PERCENTILE WE WANT TO GO WITH
######################################################################
daymet.data %>%
  group_by(network, site_id) %>%
  summarise(per_90_quant = quantile(tmax_degC, probs=0.90),
            per_95_quant = quantile(tmax_degC, probs=0.95),
            per_98_quant = quantile(tmax_degC, probs=0.98),
            per_99_quant = quantile(tmax_degC, probs=0.99))-> maxtemp.quantiles

#add percentile columns to Daymet data
daymet.data <- left_join(daymet.data, maxtemp.quantiles)

#Add column that identifies if the daily max temp was greater than or equal to 
#the percentile (0- indicates no, 1- indicates yes)
daymet.data %>% 
  mutate(maxtemp.above90per = case_when(
    tmax_degC >= per_90_quant ~ 1,
    TRUE ~ 0),
    maxtemp.above95per = case_when(
      tmax_degC >= per_95_quant ~ 1,
      TRUE ~ 0),
    maxtemp.above98per = case_when(
      tmax_degC >= per_98_quant ~ 1,
      TRUE ~ 0),
    maxtemp.above99per = case_when(
      tmax_degC >= per_99_quant ~ 1,
      TRUE ~ 0,
    )) -> daymet.data

#calculate the total days over the percentile for each network, site, year: 
daymet.data %>% 
  group_by(network, site_id, year) %>% 
  summarise(n_days_over90thperc = sum(maxtemp.above90per),
            n_days_over95thperc = sum(maxtemp.above95per),
            n_days_over98thperc = sum(maxtemp.above98per),
            n_days_over99thperc = sum(maxtemp.above99per)) ->days.over.perc

##############################################################################
#Create plots for each percentile
##############################################################################

# 90TH PERCENTILE

#plot total number of days where daily max temp was greater than or equal 
#to the 90th percentile over time by network
days.over.perc %>% 
  ggplot(aes(x=year, y=n_days_over90thperc, group = network)) + 
  geom_point(aes(color=site_id)) + 
  labs(x = "Year", y = "Total days where daily Tmax >= 90th percentile") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(~network)

#plot total number of sites with days where max temp was greater than or 
#equal to 90th percentile
days.over.perc %>% 
  filter(n_days_over90thperc > 0) %>%
  group_by(network, year) %>% 
  summarise(n_sites_over90 = n_distinct(site_id),
            .groups = "drop") %>% 
  ggplot(aes(x=year, y=n_sites_over90, group = network)) + 
  geom_point() + 
  labs(x = "Year", y = "Total sites with daily Tmax >= 90th percentile") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(~network)

###################
# 95TH PERCENTILE

#plot total number of days where daily max temp was greater than or equal 
#to the 95th percentile over time by network
days.over.perc %>% 
  ggplot(aes(x=year, y=n_days_over95thperc, group = network)) + 
  geom_point(aes(color=site_id)) + 
  labs(x = "Year", y = "Total days where daily Tmax >= 95th percentile") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(~network)

#plot total number of sites with days where max temp was greater than or 
#equal to 95th percentile
days.over.perc %>% 
  filter(n_days_over95thperc > 0) %>%
  group_by(network, year) %>% 
  summarise(n_sites_over95 = n_distinct(site_id),
            .groups = "drop") %>% 
  ggplot(aes(x=year, y=n_sites_over95, group = network)) + 
  geom_point() + 
  labs(x = "Year", y = "Total sites with daily Tmax >= 95th percentile") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(~network)

###################
# 98TH PERCENTILE

#plot total number of days where daily max temp was greater than or equal 
#to the 98th percentile over time by network
days.over.perc %>% 
  ggplot(aes(x=year, y=n_days_over98thperc, group = network)) + 
  geom_point(aes(color=site_id)) + 
  labs(x = "Year", y = "Total days where daily Tmax >= 98th percentile") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(~network)

#plot total number of sites with days where max temp was greater than or 
#equal to 98th percentile
days.over.perc %>% 
  filter(n_days_over98thperc > 0) %>%
  group_by(network, year) %>% 
  summarise(n_sites_over98 = n_distinct(site_id),
            .groups = "drop") %>% 
  ggplot(aes(x=year, y=n_sites_over98, group = network)) + 
  geom_point() + 
  labs(x = "Year", y = "Total sites with daily Tmax >= 98th percentile") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(~network)

###################
# 99TH PERCENTILE

#plot total number of days where daily max temp was greater than or equal 
#to the 98th percentile over time by network
days.over.perc %>% 
  ggplot(aes(x=year, y=n_days_over99thperc, group = network)) + 
  geom_point(aes(color=site_id)) + 
  labs(x = "Year", y = "Total days where daily Tmax >= 99th percentile") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(~network)

#plot total number of sites with days where max temp was greater than or 
#equal to 98th percentile
days.over.perc %>% 
  filter(n_days_over99thperc > 0) %>%
  group_by(network, year) %>% 
  summarise(n_sites_over99 = n_distinct(site_id),
            .groups = "drop") %>% 
  ggplot(aes(x=year, y=n_sites_over99, group = network)) + 
  geom_point() + 
  labs(x = "Year", y = "Total sites with daily Tmax >= 99th percentile") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(~network)
