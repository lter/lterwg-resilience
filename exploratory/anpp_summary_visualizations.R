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
librarian::shelf(plyr, tidyverse, googledrive, supportR, cowplot)

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
  dplyr::group_by(site, network, treatment, crop) %>%
  dplyr::summarize(mean = mean(anpp_g_m2, na.rm=TRUE),
            sd = sd(anpp_g_m2, na.rm=TRUE),
            cv = (sd/mean) * 100,
            n = length(anpp_g_m2)) %>%
  # Filter out sites with NA means (grain yield only)
  dplyr::filter(!is.na(mean)) %>%
  # Filter out observations with fewer than 5 years of data?
  dplyr::filter(n > 4) %>%
  dplyr::mutate(network =ifelse(network=="LTAR and LTER", "LTER", network))

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
            map = mean(wyr_ppt), 
            cv_ppt = (sd(wyr_ppt)/map) * 100) %>%
  # Filter out sites with NA means (grain yield only)
  dplyr::filter(!is.na(mean)) %>%
  # Filter out observations with fewer than 5 years of data?
  dplyr::filter(n > 2) %>%
  dplyr::mutate(network =ifelse(network=="LTAR and LTER", "LTER", network))%>%
  # Round precipitation to the nearest 100
  dplyr::mutate(MAP_hund = round_any(map, 100, f= ceiling)) %>%
  dplyr::mutate(CV_ten = round_any(cv_ppt, 10, f= ceiling))

boxes <- ggplot(anpp_sum2, aes(MAP_hund, mean, group=MAP_hund))+
  geom_boxplot()+
  geom_jitter(aes(color=network), alpha=0.5)+
  theme_bw()+
  xlab("MAP (by nearest 100 mm) mm")+
  ylab("Mean ANPP")+
  ggtitle("Mean ANPP (by site, treatment, network) vs Mean PPT Rounded to Nearest 100mm")
boxes

boxes.2 <- ggplot(anpp_sum2, aes(CV_ten, cv, group=CV_ten))+
  geom_boxplot()+
  geom_jitter(aes(color=network), alpha=0.5)+
  theme_bw()+
  xlab("CV (nearest 10th%)")+
  ylab("CV of ANPP")+
  ggtitle("ANPP CV (by site, treatment, network) vs Mean CV Rounded to Nearest 100mm")
boxes.2

##Look at which sites have the most variable precipitation in this window
var <- anpp_sum2 %>%
  filter(cv_ppt>40)


### LOOK AT PRECIP Variation for the ANPP record length versus for 30 years
##ANPP record length

anpp_sum3 <- anpp_v1 %>%
  dplyr::group_by(site, network) %>%
  dplyr::summarize(
                   n = length(anpp_g_m2), 
                   map = mean(wyr_ppt), 
                   cv_ppt = (sd(wyr_ppt)/map) * 100) %>%
  # Filter out observations with fewer than 5 years of data?
  dplyr::filter(n > 2) %>%
  dplyr::mutate(network =ifelse(network=="LTAR and LTER", "LTER", network))

anpp.rec.length <- ggplot(anpp_sum3, aes(map, cv_ppt, color=network))+
  geom_point()+
  theme_bw()+
  xlim(200, 2000)+
  ylim(5, 50)+
  ylab("PPT CV (%) - ANPP Record Length")+
  xlab("MAP (mm) - ANPP Record Length")
anpp.rec.length

#######################
#Read in CLIMATE DATA 
#######################

# Identify desired file
climate_data <- "site_climate_mswep.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == climate_data) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

# Read in harmonized data
climate_file <- read.csv(file = file.path("data", "harmonized_data", climate_data)) %>%
  dplyr::rename(site = site_id)

# Check structure
dplyr::glimpse(climate_file)

###################################
# Join Mean ANPP AND CLIMATE DATA #
###################################
merge_ANPP_climate <- left_join(anpp_sum3, climate_file, by = c("site"))
rec.length.30 <- ggplot(merge_ANPP_climate, aes(MAP, cv_ppt_inter, color=network))+
  geom_point()+
  theme_bw()+
  xlim(200, 2000)+
  ylim(5, 50)+
  ylab("PPT CV (%) - 30 year record")+
  xlab("MAP (mm) - 30 year record")
rec.length.30

plot_grid(anpp.rec.length, rec.length.30)

##Look at teh one to one lines
map_graph <- ggplot(merge_ANPP_climate, aes(MAP, map, color=network))+
  geom_point()+
  theme_bw()+
  geom_abline(slope=1)+
  ylab("MAP (mm) - ANPP record")+
  xlab("MAP (mm) - 30 year record")+
  ggtitle("MAP")
map_graph

cv_graph <- ggplot(merge_ANPP_climate, aes(cv_ppt_inter, cv_ppt, color=network))+
  geom_point()+
  theme_bw()+
  geom_abline(slope=1)+
  ylab("CV - ANPP record")+
  xlab("CV - 30 year record")+
  ggtitle("CV")
cv_graph

plot_grid(map_graph, cv_graph)

