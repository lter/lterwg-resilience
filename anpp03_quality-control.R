## ----------------------------------------------------------------- ##
# Resilience Management - Quality Control Workflow
## ----------------------------------------------------------------- ##
# Authors: Nick J Lyon, ...

# Purpose
## Do needed quality control (QC) and miscellaneous data wrangling tasks.
## Essentially anything after "harmonization" _sensu stricto"

## --------------------------------------- ##
# Housekeeping ----
## --------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive, supportR)

# Make needed folder(s)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "harmonized_data"), showWarnings = F)
dir.create(file.path("data", "environment"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()

# Identify relevant tidy file
focal_file <- "02_anpp_filter1.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

# Read in harmonized data
tidy_v1 <- read.csv(file = file.path("data", "harmonized_data", focal_file))


# Check structure
dplyr::glimpse(tidy_v1)

## --------------------------------------- ##
# Identify Network and Fix Capitalization----
## --------------------------------------- ##
tidy_v2 <- tidy_v1 %>%
  # make LTER sites capitalized
  dplyr::mutate(site = ifelse(network=="LTER", toupper(site), site)) %>%
  # make lowercase lter network capitalized
  dplyr::mutate(network = ifelse(network=="lter", "LTER", network), 
                network = ifelse(network=="", "DRIVES", network), 
                site = ifelse(network=="DAP", toupper(site), site))%>%
  # make jornada in the "lter" network
  dplyr::mutate(network = ifelse(site=="jrn", "LTER", network))

# make sure that every site has a network
network_nas <- tidy_v2 %>%
  filter(is.na(network))

# Fix site name for PRHPA
tidy_v2 <- tidy_v2 %>%
  dplyr::mutate(site = ifelse(site=="PRHPA_NEMELTCRS", "PRHPA", site))%>%
  dplyr::mutate(site = ifelse(site=="PRHPA_NEMERREM", "PRHPA", site)) %>%
  dplyr::mutate(site = ifelse(site=="jrn", "JRN", site)) %>%
  dplyr::mutate(site = ifelse(site=="sev", "SEV", site))
# ## --------------------------------------- ##
# # Fix Dates and Year----
# ## --------------------------------------- ##
# 
tidy_v3 <- tidy_v2 %>%
  # fix dates and extract year
  dplyr::mutate(date2 = lubridate::as_date(date, format = "%m/%d/%Y"),
                date3 = lubridate::as_date(lubridate::mdy_hm(date)),
                date4 = lubridate::as_date(lubridate::mdy_hms(date)),
                date5 = lubridate::as_date(lubridate::ymd_hms(date)),
                date6 = lubridate::as_date(lubridate::ymd(date)),
                date_m.d.yyy = lubridate::as_date(date_m.d.yyyy, format = "%m-%d-%Y"),
                date_m.d.yyy2 = lubridate::as_date(date_m.d.yyyy, format = "%Y-%m-%d")) %>%
  # make a single date column
  dplyr::mutate(dates = coalesce(date2, date3, date4, date5, date_m.d.yyy, date_m.d.yyy2, date6)) %>%
  # add year
  dplyr::mutate(year = ifelse(is.na(year), lubridate::year(dates), year)) %>%
  # add month from dates to existing month column
  dplyr::mutate(month = coalesce(month, lubridate::month(dates))) %>%
  # get rid of extra date columns
  dplyr::select(-c("date", "date_m.d.yyy", "date2", "date3", "date4", "date5", "date6", "date_m.d.yyy2"))
#   
# Check that no unexpected columns are lost/gained
#supportR::diff_check(old = names(tidy_v3), new = names(tidy_v4))

# Check structure
#dplyr::glimpse(tidy_v3)

## ------------------------------------------- ##
# Calculate Duration ----
## ------------------------------------------- ##

# Calculate duration for each "site"
#MEGHAN NOTES: year is not filled out for each dataset so this isn't working, i created a new year2 column above, it works now.

tidy_v4 <- tidy_v3 %>% 
  dplyr::group_by(site, treatment) %>% 
  dplyr::mutate(duration_years = length(unique(year))) %>% 
  dplyr::ungroup()

# Check structure
dplyr::glimpse(tidy_v4)

## ------------------------------------------- ##
# ANPP Unit Conversions ----
## ------------------------------------------- ##
tidy_v5 <- tidy_v4 %>%
  # ANPP - convert KG/ha to g/m2
  dplyr::mutate(anpp_g_m2 = ifelse(is.na(anpp_g_m2), yes = anpp_kg_ha/10, no = anpp_g_m2)) %>%
  # Drop kg/ha column 
  dplyr::select(-anpp_kg_ha)

# Check to make sure only unwanted columns are lost
supportR::diff_check(old = names(tidy_v4), new = names(tidy_v5))

# Check structure
dplyr::glimpse(tidy_v5)

## ------------------------------------------- ##
# Crop Name Consistency----
## ------------------------------------------- ##
unique(tidy_v5$crop)

# tidy_v6 <- tidy_v5 %>%
#   #change the crop names to all be consistent
#   mutate(crop = fct_recode(as.factor(crop),  Mixed_grass = 'Mixed Grass',Spring_Wheat = 'Triticum aestivum (Spring Wheat)', 
#                        Winter_Wheat = 'Triticum aestivum (Winter Wheat)', Soybean = 'Glycine max (Soybean)', 
#                        Corn = 'Zea mays (Corn)',Garbanzo = 'Cicer arietinum (Garbonzo Beans)',
#                        Canola = 'Brassica napus (Canola)',Corn = 'Zea mays L. (*)', Soybean = 'Glycine max L. (*)',
#                        Oats = 'Avena sativa (Oats)', Rye = 'Secale cereale (Rye)',  
#                        Safflower='Carthamus tinctorius (Safflower)' , Millet = 'Setaria italica (Foxtail Millet)',
#                        Winter_Wheat = 'Triticum aestivum L. (*)', Alfalfa = 'Medicago sativa (Alfalfa)',
#                        Spring_Wheat = 'Triticum aestivum (Spring Spring wheat)', Winter_Wheat = 'Wheat',
#                        Sorghum = 'Sorghum bicolor (Sorghum)', Sorghum =  'Sorghum bicolor (sorghum)',    
#                        Switchgrass = 'Panicum virgatum (Switchgrass)', Spring_Wheat = 'Triticum aestivum (Spring wheat)',
#                        Soybean = 'Soybean', Corn = 'Corn'))


tidy_v6 <- tidy_v5 %>%
  mutate(crop_clean = str_to_lower(str_trim(crop)),
         crop = case_when(
           crop_clean == "" ~ NA_character_,
           
           str_detect(crop_clean, "spring wheat") ~ "Spring_Wheat",
           str_detect(crop_clean, "winter wheat") ~ "Winter_Wheat",
           crop_clean == "wheat" ~ "Winter_Wheat",  # ambiguous bare "Wheat" - defaulting per your original mapping
           str_detect(crop_clean, "triticum aestivum") ~ "Winter_Wheat",  # unlabeled sci-name-only entries default to winter
           
           str_detect(crop_clean, "garbonzo|garbanzo|cicer arietinum") ~ "Garbanzo",
           str_detect(crop_clean, "canola|brassica napus") ~ "Canola",
           str_detect(crop_clean, "zea mays|corn") & !str_detect(crop_clean, "sorghum") ~ "Corn",
           str_detect(crop_clean, "glycine max|soybean") ~ "Soybean",
           str_detect(crop_clean, "avena sativa|oats") ~ "Oats",
           str_detect(crop_clean, "switchgrass|panicum virgatum") ~ "Switchgrass",
           str_detect(crop_clean, "mixed grass") ~ "Mixed_grass",
           str_detect(crop_clean, "medicago sativa|^alfalfa$") ~ "Alfalfa",
           str_detect(crop_clean, "alfalfa mix") ~ "Alfalfa_mix",
           str_detect(crop_clean, "sorghum.*sudangrass|sudangrass.*sorghum") ~ "Sorghum_sudangrass",
           str_detect(crop_clean, "sudangrass") ~ "Sudangrass",
           str_detect(crop_clean, "sorghum") ~ "Sorghum",
           str_detect(crop_clean, "triticale") ~ "Triticale",
           str_detect(crop_clean, "cereal rye|^rye$") ~ "Rye",
           str_detect(crop_clean, "red clover") ~ "Red_clover",
           str_detect(crop_clean, "crimson clover") ~ "Crimson_clover",
           str_detect(crop_clean, "annual ryegrass") ~ "Annual_ryegrass",
           str_detect(crop_clean, "safflower|carthamus tinctorius") ~ "Safflower",
           str_detect(crop_clean, "millet|setaria italica") ~ "Millet",
           str_detect(crop_clean, "barley") ~ "Barley",
           str_detect(crop_clean, "^bean$") ~ "Bean",
           str_detect(crop_clean, "tomato") ~ "Tomato",
           str_detect(crop_clean, "pennycress") ~ "Pennycress",
           str_detect(crop_clean, "winter small grain") ~ "Winter_small_grain",
           str_detect(crop_clean, "annual grass-legume mix") ~ "Annual_grass_legume_mix",
           str_detect(crop_clean, "annual legume-only mix") ~ "Annual_legume_mix",
           str_detect(crop_clean, "perennial mix") ~ "Perennial_mix",
           str_detect(crop_clean, "orchardgrass/white clover") ~ "Orchardgrass_clover",
           str_detect(crop_clean, "orchard/fescue/clover/alfalfa/chicory") ~ "Perennial_mix",
           
           TRUE ~ str_to_title(crop_clean)  # catch-all: flag anything unmatched instead of silently dropping
         )) %>%
  select(-crop_clean)


## ------------------------------------------- ##
# Update treatment for CPER and LTER sites without treatment----
## ------------------------------------------- ##
tidy_v7 <- tidy_v6 %>%
  # Fix CPER
  dplyr::mutate(treatment = ifelse(site == "CPER", yes="CPER_CTRL", no= treatment)) %>%
  # Fix LTER that are blank
  dplyr::mutate(treatment = ifelse(network == "LTER" & treatment=="", yes = paste(site, "CTRL", sep="_"), no = treatment))%>%
  # Fix JRN to be control
  dplyr::mutate(treatment = ifelse(site == "JRN", yes="JRN_CTRL", no= treatment)) 


## ------------------------------------------- ##
# Update site IDs----
## ------------------------------------------- ##
str(tidy_v7)
unique(tidy_v7$site)
##UCB-Pastures to UCB
tidy_v7$site <- ifelse(tidy_v7$site == "UCB-Pastures", yes="UCB", no= tidy_v7$site)

#ECB_B1bau to ECB_C1
tidy_v7$site <- ifelse(tidy_v7$site == "ECB_B1bau", yes="ECB_C1", no= tidy_v7$site)

#ECB_D2bau to ECB_D2
tidy_v7$site <- ifelse(tidy_v7$site == "ECB_D2bau", yes="ECB_D2", no= tidy_v7$site)



## ------------------------------------------- ##
# Column Checks on Data ----
## ------------------------------------------- ##
glimpse(tidy_v7)


# Check overlap of replicate information 
#tidy_v8 <- tidy_v7 %>%
#  group_by(network, site, treatment, location, plot, transect, subsample, web, quad) %>%
#  summarize(crops = paste(unique(crop),collapse="&"), years = paste(unique(year), collapse="&"))
  

# tidy column order

# Columns wanted: site_id, network, treatment, year, anpp,location, crop + any replicate information
  
  

## ------------------------------------------- ##
# ANPP Checks ----
## ------------------------------------------- ##

# Look at rows without ANPP or grain yield data
anpp_na <- tidy_v7 %>%
  filter(is.na(anpp_g_m2)) %>%
  filter(is.na(grain_kg_ha))

str(tidy_v7)
# ANPP by network
# Some quick visualization
ggplot(tidy_v7, aes(anpp_g_m2, fill=network))+
  geom_histogram()+
  facet_wrap(~network, scales="free")

ggplot(data=subset(tidy_v7, tidy_v6$network=="LTAR"), aes(anpp_g_m2, fill=crop))+
  geom_histogram()+
  facet_wrap(~crop)

ggplot(tidy_v7, aes(network, anpp_g_m2, color=site))+
         geom_boxplot()

## ------------------------------------------- ##
# QA/QC - LTAR ----
## ------------------------------------------- ##
# Select sites in LTAR network
ltar <- tidy_v7 %>%
  dplyr::filter(network=="LTAR") %>%
  dplyr::mutate(site=as.factor(site))
str(ltar)
# Visualize ANPP over time for each site, colored by treatment, shaped by crop
sites <- unique(ltar$site)

for (Site in sites) {
  # Subset data for the current species
  data_subset <- subset(ltar, ltar$site == Site)
  
  # Create the plot
  p <- ggplot(data_subset, aes(x = year, y = anpp_g_m2, color=treatment, shape=crop)) +
    geom_point(size = 1.2) +
    labs(title = paste("Scatter Plot for", Site),
         x = "Year",
         y = "ANPP") +
    theme_minimal()
  
  # Print the plot
  print(p)
}


## ------------------------------------------- ##
# CALCULATE AVG. HARVEST DATE ----
## ------------------------------------------- ##

# Generate column for average harvest month by site-crop
tidy_v7 %>% 
  dplyr::group_by(network, site, crop) %>% 
  dplyr::mutate(avg_harvest_month = mean(month),
                avg_harvest_month_rounded = round(avg_harvest_month)) %>% #round avg. harvest month
  dplyr::ungroup() -> tidy_v8

# Create df of sites with no harvest month
tidy_v8 %>% 
  dplyr::filter(is.na(month)) %>% 
  dplyr::distinct(network, site, crop, month) -> no_harvest_month_df

# CREATE INITIAL NO HARVEST MONTH FILE TO BE FILLED OUT MANUALLY
# Identify name for exported object
no_harvest_month_df_output <- "no_harvest_month_table_updated_cropnames.csv"

# Export locally
write.csv(x = no_harvest_month_df, row.names = F, na = '',
          file = file.path("data", "harmonized_data", no_harvest_month_df_output))

# Upload to Drive
# googledrive::drive_upload(media = file.path("data", "harmonized_data", no_harvest_month_df_output), overwrite = T,
#                           path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ"))


#import csv file with manually filled out harvest month (Ingrid, Tim & Beatriz)

# Identify relevant tidy file
focal_file2 <- "filled_harvest_month_table_updated_cropnames.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

# Read in harmonized data
filled_out_df <- read.csv(file = file.path("data", "harmonized_data", focal_file2))

#check how many networks there are
unique(filled_out_df$network)

no_crop_filled <- filled_out_df %>% 
  filter(network %in% c("LTER", "NutNet"))

#merge manually filled out month with avg. harvest month in tidy_v8
tidy_v9 <- tidy_v8 %>%
  left_join(filled_out_df %>% 
      select(network, site, crop, month),by = c("network", "site", "crop")) %>% 
  rename(manual_month = month.y) %>% 
  mutate(mean_harvest_month_rounded = coalesce(avg_harvest_month_rounded, manual_month)) %>% 
  select(-c(avg_harvest_month_rounded, manual_month)) %>% 
  rename(avg_harvest_month_rounded = mean_harvest_month_rounded,
         month = month.x)

#merge manually filled out month with avg. harvest month in filled out data for LTER & Nutnet (sites w/ no crop)
tidy_v10 <- tidy_v9 %>%
  left_join(no_crop_filled %>% 
              select(network, site, month),by = c("network", "site")) %>% 
  rename(manual_month = month.y) %>% 
  mutate(mean_harvest_month_rounded = coalesce(avg_harvest_month_rounded, manual_month)) %>% 
  select(-c(avg_harvest_month_rounded, manual_month)) %>% 
  rename(avg_harvest_month_rounded = mean_harvest_month_rounded,
         month = month.x)

# Get final list with newly added sites (Katherine's drive date) without harvest dates:
tidy_v10 %>% 
  dplyr::filter(is.na(avg_harvest_month_rounded)) %>% 
  dplyr::distinct(network, site, crop, avg_harvest_month_rounded) -> no_harvest_month_df_07_09_26

# Identify name for exported object
no_harvest_month_output2 <- "no_harvest_month_df_07_09_26.csv"

# Export locally
write.csv(x = no_harvest_month_df_07_09_26, row.names = F, na = '',
           file = file.path("data", "harmonized_data", no_harvest_month_output2))

# Upload to Drive
googledrive::drive_upload(media = file.path("data", "harmonized_data", no_harvest_month_output2), overwrite = T,
                           path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ"))


## ------------------------------------------- ##
# Export ----
## ------------------------------------------- ##

# Final pre-export tweaks
tidy_v99 <- tidy_v10

# Check structure
dplyr::glimpse(tidy_v99)

# Identify nice name for exported object
focal_output <- "03_anpp_wrangled.csv"

# Export locally
write.csv(x = tidy_v99, row.names = F, na = '',
          file = file.path("data", "harmonized_data", focal_output))

# Upload to Drive
googledrive::drive_upload(media = file.path("data", "harmonized_data", focal_output), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ"))

# Export the treatments only to generate a table for management
# trt <- tidy_v99 %>%
#   select (c(network, site, treatment)) %>%
#   unique()
# 
# # Identify nice name for exported object
# focal_name <- "treatment_table.csv"
# 
# # Export locally
# write.csv(x = trt , row.names = F, na = '',
#           file = file.path("data", focal_name))
# 
# # Upload to Drive
# googledrive::drive_upload(media = file.path("data",  focal_name), overwrite = T,
#                           path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Ty7QX7vyvD797eKJzMWbr8AwIo-GyBFO"))
# 

# End ----
