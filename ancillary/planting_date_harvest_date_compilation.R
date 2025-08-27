##Extact Planting Date

##load libraries:
library(tidyverse)

# Make needed folder(s)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "pre_processed_data"), showWarnings = F)
dir.create(file.path("data", "harmonized_data"), showWarnings = F)
dir.create(file.path("data", "diagnostic", "anpp"), showWarnings = F)

#################################
# 1) Read in planting date and 
#################################

# Grab planting date file
pd_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1zeTZMf5kv0ZG3bJFi-qPimwgbz314m2V")) %>%
  dplyr::filter(name == "planting_dates.csv")

# Did that work?
pd_drive

# Download the excluded treatmetn file
googledrive::drive_download(file = pd_drive$id, overwrite = T, type = "csv",
                            path = file.path("data", "diagnostic", "anpp", pd_drive$name))

# Read in excluded treatment file
pd <- read.csv(file = file.path("data", "diagnostic", "anpp", "planting_dates.csv"))

## update the treatment column to be treatmetn
pd <- pd %>%
  dplyr::rename(treatment = Treatment.ID)

#################################
# 2) Remove unwanted LTAR treatments (irrigated, mid-season harvest)
#################################

# Grab excluded treatmetn file
excl_trt_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1Ty7QX7vyvD797eKJzMWbr8AwIo-GyBFO")) %>%
  dplyr::filter(name == "excluded_treatments")

# Did that work?
excl_trt_drive

# Download the excluded treatmetn file
googledrive::drive_download(file = excl_trt_drive$id, overwrite = T, type = "csv",
                            path = file.path("data", excl_trt_drive$name))

# Read in excluded treatment file
excl_trt <- read.csv(file = file.path("data", "excluded_treatments.csv"))

##
pd.2 <- pd%>%
  filter(!treatment %in% excl_trt$treatment)

#################################
# 3) Remove unpublished LTAR datasets
#################################

# Grab excluded treatmetn file
pub_trt_ltar <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1Ty7QX7vyvD797eKJzMWbr8AwIo-GyBFO")) %>%
  dplyr::filter(name == "LTAR_published_trts.csv")

# Did that work?
pub_trt_ltar 

# Download the excluded treatmetn file
googledrive::drive_download(file = pub_trt_ltar$id, overwrite = T, type = "csv",
                            path = file.path("data", pub_trt_ltar$name))

# Read in excluded treatment file
pub_trt2 <- read.csv(file = file.path("data", "LTAR_published_trts.csv"))

# Remove treatments that aren't published
pubtrt <- pub_trt2$treatments
pd.3 <- pd.2 %>%
  dplyr::filter(treatment %in% pub_trt2$treatments)

##Clean the crop type names 
crop_name <- read.csv("/Users/olhajek/Desktop/nceas/crop_name_dictionary.csv")

pd.4 <- left_join(pd.3, crop_name)
pd.5 <- pd.4 %>%
  dplyr::select(-Crop) %>%
  dplyr::rename(crop = "Crop_tidy")

##Guess the harvest year
cd <- pd.5 %>%
  group_by(treatment, crop) %>%
  summarize(max = max(plant.month), min=min(plant.month))

str(pd.5)
pd.6 <- pd.5 %>%
  dplyr::mutate(year = ifelse (plant.month > 7, plant.year+1, plant.year))

##Try to join with the ANPP data
anpp <- read.csv('/Users/olhajek/Desktop/nceas/LTAR DET CSVs/data/harmonized_data/03_anpp_wrangled.csv')
str(anpp)
str(pd.6)


test <- left_join(anpp, pd.6, by=c("treatment","year","crop"))
test.2 <- test %>%
  filter(crop != "")

##Clean this up to identify gaps
str(test.2)

##summarize by year
test.3 <- test.2 %>%
  group_by(location, network, site, year, treatment, month, crop, dates, Start.Date, End.Date, date, plant.month, plant.year) %>%
  summarize(mean_anpp=mean(anpp_g_m2))


write.csv(test.3, '/Users/olhajek/Desktop/nceas/LTAR DET CSVs/data/harmonized_data/pd_hd_combo.csv')


