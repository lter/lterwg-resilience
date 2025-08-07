## ----------------------------------------------------------------- ##
# Resilience Management - ANPP Exploration Graphs
## ----------------------------------------------------------------- ##
# Authors: Olivia Hajek

# Purpose
## Understand the mean and variability, etc of ANPP data

## --------------------------------------- ##
# Housekeeping ----
## --------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive, supportR, cowplot, plyr)

# Make needed folder(s)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "harmonized_data"), showWarnings = F)
dir.create(file.path("data", "environment"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Identify relevant tidy file
focal_file <- "anpp_wyr_merged.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

# Read in harmonized data
anpp_v1 <- read.csv(file = file.path("data", "harmonized_data", focal_file))

# Check structure
dplyr::glimpse(anpp_v1)

##################################################################
##Check means and variability by site - trt - crop
##################################################################

# Update blank crops to native_veg
anpp_v1$crop <- ifelse(anpp_v1$crop =="", "Native_Veg", anpp_v1$crop)
unique(anpp_v1$crop)

# Summarize data by mean and variability
str(anpp_v1)
anpp_sum <- anpp_v1 %>%
  group_by(site, network, treatment, crop) %>%
  summarize(mean = mean(anpp_g_m2, na.rm=TRUE),
            sd = sd(anpp_g_m2, na.rm=TRUE),
            cv = (sd/mean) * 100,
            n = length(anpp_g_m2)) %>%
  # Filter out sites with NA means (grain yield only)
  filter(!is.na(mean)) %>%
  # Filter out observations with fewer than 5 years of data?
  filter(n > 4) %>%
  mutate(network =ifelse(network=="LTAR and LTER", "LTER", network))

# Make some visualizations
hists.mean <- ggplot(anpp_sum, aes(mean, group=network))+
  geom_histogram(position="identity")+
  facet_wrap(~network)+
  ggtitle("Mean")
hists.mean

hists.cv <- ggplot(anpp_sum, aes(cv, group=network))+
  geom_histogram(position="identity")+
  facet_wrap(~network)+
  ggtitle("CV")
hists.cv

plot_grid(hists.mean, hists.cv, nrow=2)

# Across all networks look at boxplots of production in precipitaiton brackets
anpp_sum2 <- anpp_v1 %>%
  dplyr::group_by(site, network, treatment, crop) %>%
  dplyr::summarize(mean = mean(anpp_g_m2, na.rm=TRUE),
            sd = sd(anpp_g_m2, na.rm=TRUE),
            cv = (sd/mean) * 100,
            n = length(anpp_g_m2), 
            MAP = mean(wyr_ppt), 
            cv_ppt = (sd(wyr_ppt)/MAP) * 100) %>%
  # Filter out sites with NA means (grain yield only)
  dplyr::filter(!is.na(mean)) %>%
  # Filter out observations with fewer than 5 years of data?
  dplyr::filter(n > 2) %>%
  dplyr::mutate(network =ifelse(network=="LTAR and LTER", "LTER", network))%>%
  # Round precipitation to the nearest 100
  dplyr::mutate(MAP_hund = round_any(MAP, 100, f= ceiling))

boxes <- ggplot(anpp_sum2, aes(MAP_hund, mean, group=MAP_hund))+
  geom_boxplot()+
  geom_jitter(aes(color=network), alpha=0.5)+
  theme_bw()+
  xlab("MAP (by nearest 100 mm) mm")+
  ylab("Mean ANPP")+
  ggtitle("Mean ANPP (by site, treatment, network) vs Mean PPT Rounded to Nearest 100mm")
boxes

