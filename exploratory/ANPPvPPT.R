## ----------------------------------------------------------------- ##
# Resilience Management - Site Aggregation Workflow
## ----------------------------------------------------------------- ##
# Authors: Nick J Lyon, ...

# Purpose
## 

## --------------------------------------- ##
# Housekeeping ----
## --------------------------------------- ##

# Load libraries
librarian::shelf(tidyverse, googledrive)

# Make needed folder(s)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "harmonized_data"), showWarnings = F)

# Clear environment + collect garbage
rm(list = ls()); gc()


######################
##Read in ANPP DATA
######################

# Identify desired file
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


#change jrn from "LTAR and LTER" to LTER
anpp_v1$network <-ifelse(anpp_v1$network == "LTAR and LTER", "LTER", anpp_v1$network)

#calculate mean by site and crop
  anpp_v1$crop <-ifelse(anpp_v1$crop == "", "native", anpp_v1$crop) #change any non crop to native
  anpp_v1$sitecrop<-paste(anpp_v1$site, anpp_v1$crop, sep = "_")
  
  #select controls in nutnet
  anpp_v2<-filter(anpp_v1, !network == "NutNet") 
  nutnet_cont<-filter(anpp_v1, network == "NutNet" & treatment == "Control")
  anpp_v2<-bind_rows(anpp_v2, nutnet_cont)
  
  #calc means
  anpp_sitecrop_mean<-anpp_v2%>%
    group_by(sitecrop, network, crop)%>%
    summarize(n = n(),
              anpp.mean = mean(anpp_g_m2, na.rm = T),
              wyrppt.mean = mean (wyr_ppt, na.rm = T))%>%
    filter(n > 5)

#Plots  ppt vs anpp

### 1. All trts
  ## 1.1 by site
  ggplot(anpp_v1, aes(x = wyr_ppt, y = anpp_g_m2, color = site)) +
    geom_point(size = 1)
  
  
  ## 1.2 by network
  ggplot(anpp_v1, aes(x = wyr_ppt, y = anpp_g_m2, color = network)) +
    geom_point(size = 1)
  
  
### 2. Just controls means 
  ## 3.1 by site
  ggplot(anpp_sitecrop_mean, aes(x = wyrppt.mean, y = anpp.mean, color = sitecrop)) +
    geom_point(size = 2)
  
  ## 3.2 by network
  ggplot(anpp_sitecrop_mean, aes(x = wyrppt.mean, y = anpp.mean, color = network)) +
    geom_point(size = 2)
  
  ## 3.3 by crop
  ggplot(anpp_sitecrop_mean, aes(x = wyrppt.mean, y = anpp.mean, color = crop)) +
    geom_point(size = 2)
  
  

