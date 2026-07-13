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
file2<-'stability_anpp2.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

##STEP 1: Read in the ANPP data
dat_4cat <- read.csv(file = file.path("data", "harmonized_data", file2)) 


# Make a table of the ANPP data
data.summary <- dat_4cat%>% #using raw data, not detrended
  filter(stab.analysis == 1)%>% #sites must have 5 or more years of data, might need to up to 15 based on Doring 2018 paper?
  group_by(network, site, type, type2)%>% #need to average over type to make independent calcs of croplands with different types (e.g. corn, soy, wheat)
  # dropping anpp pulse summarize(nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2), anpp_pulse = (max(anpp_g_m2)-mean(anpp_g_m2))/mean(anpp_g_m2))
  summarize(nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2), map = mean(wyr_ppt))

data.summary2 <- data.summary %>%
  ungroup()%>%
  group_by(type) %>%
  summarize(min_anpp = min(manpp), max_anpp = max(manpp), min_n = min(nobs), max_n = max(nobs), 
            mean_n= mean(nobs), n = n(), min_map = min(map), max_map = max(map))

## LIKELY WILL JUST 
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
file5<-'site_coordinates_combined.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file5) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

site.locs <- read.csv(file = file.path("data", "harmonized_data", file5)) %>%
  select(c(site, latitude, longitude))

site.locs.oh <- read.csv("/Users/olhajek/Desktop/nceas/lterwg-resilience/data/harmonized_data/sitelist.csv")

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


# subset join data
site.loc2 <- left_join(data.summary, site.locs.oh)
glimpse(site.loc2)

siteloc2 <- site.loc2 %>%
  filter(country != "ca") %>%
  select(-c(longitude1, latitude1, country)) 

# assumes df has: site, lon, lat, forest (0/1), ag (0/1), urban (0/1)
glimpse(siteloc2)

siteloc3 <- siteloc2 %>% 
  dplyr::ungroup() %>%
  dplyr::select(c(site2, type2, longitude, latitude))%>%
  distinct()


sites_wide <- siteloc3 %>%
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = c(site2, longitude, latitude),   # whatever uniquely identifies a site + its coordinates
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

library(terra)
library(tidyterra)  # for geom_spatraster with ggplot
library(prism)

prism_set_dl_dir("~/prismtmp")  # folder where the raster will be downloaded
get_prism_normals(type = "ppt", resolution = "4km", annual = TRUE, keepZip = FALSE)

# find the file it downloaded
prism_archive_ls()
list.files("~/prismtmp/prism_ppt_us_25m_2020_avg_30y", full.names = TRUE)

prism_file <- pd_to_file(prism_archive_ls()[1])  # grabs the path to the file just downloaded
prism_map <- rast("/Users/olhajek/prismtmp/prism_ppt_us_25m_2020_avg_30y/prism_ppt_us_25m_2020_avg_30y.tif")

values(prism_map) %>% quantile(probs = c(0.5, 0.9, 0.95, 0.99, 1), na.rm = TRUE)


map <- ggplot() +
  geom_spatraster(data = prism_map) +
  scale_fill_distiller(
    palette = "BrBG", direction = 1, name = "MAP (mm)",
    limits = c(0, 1500),   # 95th percentile as the ceiling
    oob = scales::squish
  )+
  ggnewscale::new_scale_fill() +  # needed since you have two fill scales (raster + pie)
  geom_sf(data = us, fill = NA, color = "black", linewidth = 0.2) +
  geom_scatterpie(data = sites_wide, 
                  aes(x = longitude, y = latitude), 
                  cols = c("Grassland", "Cropland", "Fert. Grassland"),
                  pie_scale = 0.6, 
                  color = "white") +
  scale_fill_manual(values = c(
    "Grassland" =  "#7570b3",
    "Cropland" =  "#1b9e77",
    "Fert. Grassland" = "#d95f02"
  )) +
  coord_sf() +
  theme_void() +
  labs(fill = "Land use")
map


# count how many land-use columns are non-zero for each site
sites_wide <- sites_wide %>%
  mutate(n_landuse = rowSums(across(c("Grassland", "Cropland", "Fert. Grassland")) > 0))

sites_single <- sites_wide %>% filter(n_landuse == 1)
sites_multi  <- sites_wide %>% filter(n_landuse > 1)

# for single-landuse sites, figure out which land use it is, so geom_point can use the right color
sites_single <- sites_single %>%
  mutate(landuse = case_when(
    Grassland > 0 ~ "Grassland",
    Cropland > 0 ~ "Cropland",
    `Fert. Grassland` > 0 ~ "Fert. Grassland"
  ))

library(colorspace)

my_palette <- diverge_hcl(12, h = c(40, 246), c = 96)

map <- ggplot() +
  geom_spatraster(data = prism_map) +
  scale_fill_gradientn(
    colors = my_palette,
    name = "MAP (mm)",
    limits = c(0, 1500),
    oob = scales::squish,na.value = "white" 
  )+
  ggnewscale::new_scale_fill() +
  geom_sf(data = us, fill = NA, color = "black", linewidth = 0.2) +
  
  # single-landuse sites: plain points
  geom_point(data = sites_single,
             aes(x = longitude, y = latitude, fill = landuse),
             shape = 21, size = 4, color = "black") +
  
  # multi-landuse sites: pies
  geom_scatterpie(data = sites_multi,
                  aes(x = longitude, y = latitude),
                  cols = c("Grassland", "Cropland", "Fert. Grassland"),
                  pie_scale = 0.6,
                  color = "black") +
  
  scale_fill_manual(values = c(
    "Grassland" =  '#117733',
    "Cropland" =  '#D55E00',
    "Fert. Grassland" = '#56B4E9'
  )) +
  coord_sf() +
  theme_bw() +
  theme_void()

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

