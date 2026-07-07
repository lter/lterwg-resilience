# Precip Exploration - Looking at MSWEP and LTAR

# Load necessary libraries
library(tidyverse)
library(googledrive)
library(broom)
library(corrplot)
library(multcompView)
library(purrr)
library(emmeans)

# Read in the different precipitation data sources

# Create the folders necessary for data download locally
dir.create(file.path("exploratory_graphs"), showWarnings = F)
dir.create(file.path("exploratory_graphs", 'anpp_year'), showWarnings = F)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "harmonized_data"), showWarnings = F)


# READ IN THE ANPP AND PRECIPITATION DATA
file2<-'stability_anpp.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

##STEP 1: Read in the ANPP data
dat_4cat <- read.csv(file = file.path("data", "harmonized_data", file2)) 

##STEP 2: Read in MAP data
file3<-'site_climate_mswep.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file3) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

climatedat<- read.csv(file = file.path("data", "harmonized_data", file3)) %>% rename(site=site_id)


# calc MAP from water year record length and join water year vs typical jan-dec MAP

climatedata.sub <- climatedat %>%
  select(c(site, MAP))


str(dat_4cat)
wyr_map <- dat_4cat %>%
  select(site, year, network, treatment, type2, wyr_ppt) %>%
  distinct() %>%
  group_by(site, network, treatment, type2) %>%
  summarize(n=n(), MAP_wyr = mean(wyr_ppt), sd = sd(wyr_ppt)) %>%
  left_join(climatedata.sub) %>%
  mutate(map_diff = MAP - MAP_wyr)
  
# quick graph to see
ggplot(wyr_map, aes(MAP, map_diff))+
  geom_point()


# Compare to the site-level
ltar <- read.csv('/Users/olhajek/Downloads/LTAR_LegProd_ANPP-PPT (1).csv') %>%
  rename(site = site.code) %>%
  select(site, MAP) %>%
  distinct() %>%
  group_by(site) %>%
  summarize(ltar_MAP = mean(MAP)) %>%
  mutate(site = ifelse(site == "ABS-UF", "ABS", site))

# join the ltar data
wyr_map <- left_join(wyr_map, ltar)

# read in daymet to compare too
##STEP 3: Read in Temp data
file4<-'daymet_meanannual_weather.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file4) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

ann.temp <- read.csv(file = file.path("data", "harmonized_data", file4)) %>%
  rename(site = site_id) %>%
  select(c(site, mean_precip_mmyear))

wyr_map <- left_join(wyr_map, ann.temp)

# Look at the precip
str(wyr_map)
wyr_map$diff_ltar <- wyr_map$MAP_wyr - wyr_map$ltar_MAP
wyr_map$diff_ltar.daymet <- wyr_map$mean_precip_mmyear - wyr_map$ltar_MAP

ggplot(wyr_map, aes(MAP_wyr, diff_ltar)) +
  geom_point()


ggplot(wyr_map, aes(MAP_wyr, diff_ltar.daymet)) +
  geom_point()

#daymet difference
wyr_map$diff_mswep.daymet <- wyr_map$MAP_wyr - wyr_map$mean_precip_mmyear

ggplot(wyr_map, aes(MAP_wyr, diff_mswep.daymet)) +
  geom_point()

# Filter to just the LTAR site
wyr_ltar <- wyr_map %>%
  filter(network == "LTAR")

mean(wyr_ltar$diff_ltar.daymet, na.rm = TRUE)
