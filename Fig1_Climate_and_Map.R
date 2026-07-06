# Stability MS - Fig 1
# OLH
# July 5

# This script makes the map and climate figures

# Load Libraries
library(tidyverse)
#devtools::install_github("valentinitnelav/plotbiomes")
library(plotbiomes)

theme_set(theme_bw(12))

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


##STEP 3: Read in Temp data
file4<-'daymet_meanannual_weather.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file4) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

ann.temp <- read.csv(file = file.path("data", "harmonized_data", file4)) %>%
  rename(site = site_id)

## STEP 3: Site Locations
file5<-'site_summary_info.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Ty7QX7vyvD797eKJzMWbr8AwIo-GyBFO")) %>% 
  dplyr::filter(name == file5) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", .$name))

site.locs <- read.csv(file = file.path("data", file5)) %>%
  rename(site = site_id) %>%
  select(c(site, latitude, longitude))

## Join the data
sites <- dat_4cat %>%
  select(site, type2)%>%
  distinct() %>%
  left_join(climatedat) %>%
  left_join(ann.temp) %>%
  left_join(site.locs)

## Make the figures

# A) Map

library(tidyverse)
library(sf)
library(rnaturalearth)
library(scatterpie)

# assumes df has: site, lon, lat, forest (0/1), ag (0/1), urban (0/1)
sites_wide <- sites %>%
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = c(site, longitude, latitude),   # whatever uniquely identifies a site + its coordinates
    names_from = type2,           # the column holding "forest", "ag", "urban" etc.
    values_from = present,
    values_fill = 0                 # sites missing a given land use get 0
  ) %>%
  filter(!is.na(longitude))

us <- ne_states(country = "united states of america", returnclass = "sf") %>%
  filter(!name %in% c("Alaska", "Hawaii"))

str(sites_wide)
map <- ggplot() +
  geom_sf(data = us, fill = "beige", color = "black", linewidth = 0.2) +
  geom_scatterpie(data = sites_wide, 
                  aes(x = longitude, y = latitude), 
                  cols = c("Grassland", "Cropland", "Fert. Grassland"),
                  pie_scale = 0.6, 
                  color = "black") +
  scale_fill_manual(values = c(
    "Grassland" =  "#7570b3",
    "Cropland" =  "#1b9e77",
    "Fert. Grassland" = "#d95f02"
  )) +
  coord_sf() +
  theme_void() +
  labs(fill = "Land use")
map

# B) Whittaker
library(plotbiomes)

sites$MAPcm <- sites$MAP/10
whitplot <- plotbiomes::whittaker_base_plot() + 
  geom_point(data = sites, mapping = aes(x = mean_tmean_degC,
                                       y = MAPcm, color = type2))+
  scale_color_manual(values = c(
    "Grassland" =  "#7570b3",
    "Cropland" =  "#1b9e77",
    "Fert. Grassland" = "#d95f02"
  ))
  
whitplot

# C) MAP and MAT
mat <- ggplot(sites, aes(mean_tmean_degC, type2))+
  geom_point()
mat

mat.dot <- ggplot(sites) + 
  geom_dotplot(aes(x = mean_tmean_degC,y = type2, fill = type2), 
               binwidth=0.6, stackdir = "center")+
  scale_fill_manual(values = c(
    "Grassland" =  "#7570b3",
    "Cropland" =  "#1b9e77",
    "Fert. Grassland" = "#d95f02"
  )) + 
  xlab("MAT")+
  ylab("")
mat.dot


map.dot <- ggplot(sites) + 
  geom_dotplot(aes(x = MAP,y = type2, fill = type2), 
               binwidth=25, stackdir = "center")+
  scale_fill_manual(values = c(
    "Grassland" =  "#7570b3",
    "Cropland" =  "#1b9e77",
    "Fert. Grassland" = "#d95f02"
  )) + 
  xlab("MAP")+
  ylab("")
map.dot

# Save the figures
library(cowplot)

map <- map + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )

mat.map <- plot_grid(map, whitplot, mat.dot+ theme(legend.position = "none"), map.dot+ theme(legend.position = "none"))
mat.map

#save figure locally
ggsave(filename = "/Users/olhajek/Desktop/nceas/lterwg-resilience/exploratory_graphs/mat_map.png",
       plot = mat.map, width = 14, height =8.5, units = "in", dpi = 600, bg = "white")

