
library(Kendall)
library(tidyverse)



# Identify relevant tidy file
focal_file <- "anpp_wyr_merged.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "tidy", .$name))

# Read in 03 data 
anpp_data  <- read.csv(file = file.path("data", "tidy", focal_file))

# Identify relevant tidy file
focal_file <- "03_anpp_wrangled.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "tidy", .$name))

# Read in harmonized data
anpp_data_03  <- read.csv(file = file.path("data", "tidy", focal_file))


#mann-kendall test ppt 
ppt_trend <- anpp_data %>%
  group_by(site) %>%
  summarise(
    tau = Kendall(wyr_ppt, w_yr)$tau,
    p_value = Kendall(wyr_ppt, w_yr)$sl,
    length = length(unique(year))
  )

#mann-kendall test ppt 
anpp_trend <- anpp_data %>%
  group_by(site, treatment, crop, w_yr) %>%
  summarise(mean_anpp = mean(anpp_g_m2, na.rm = T))%>%
  ungroup()%>%
  group_by(site, treatment, crop) %>%
  mutate(n.obs = n())%>%
  filter(n.obs > 3 )%>%
  filter(!is.na(mean_anpp))%>%
  summarise(
    tau = Kendall(mean_anpp, w_yr)$tau,
    p_value = Kendall(mean_anpp, w_yr)$sl,
    lm_sig = summary(lm(mean_anpp~w_yr))$coefficients[2,4],
    lm_trend = summary(lm(mean_anpp~w_yr))$coefficients[2,1],
    length = length(unique(w_yr))
  )%>%
  mutate(sig = ifelse(p_value  < 0.05, 1, 0))

anpp_data %>%
  group_by(site, treatment, crop, w_yr) %>%
  summarise(mean_anpp = mean(anpp_g_m2, na.rm = T))%>%
  ungroup()%>%
  group_by(site, treatment, crop) %>%
  mutate(n.obs = n())%>%
  filter(n.obs > 3 )%>%
  filter(treatment == "ECB_B1bau" & crop == "Corn")

###plot some kendall tests
library(ggpubr)

anpp_data %>%
   group_by(site, treatment, crop, year) %>%
   summarise(mean_anpp = mean(anpp_g_m2, na.rm = T))%>%
  # ungroup()%>%
  # group_by(site, treatment, crop, location) %>%
  # mutate(n.obs = n())%>%
  # filter(n.obs > 3 )%>%
  filter(treatment == "A" & site == 'CDR' ) %>%
  ggscatter( x = "year", y = "mean_anpp", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          title = "CDR Kendall Correlation Plot")


anpp_data_03 %>%
   group_by(site, treatment, crop, year, location) %>%
  summarise(mean_anpp = mean(anpp_g_m2, na.rm = T))%>%
  # ungroup()%>%
  # group_by(site, treatment, crop, location) %>%
  # mutate(n.obs = n())%>%
  # filter(n.obs > 3 )%>%
  filter(treatment == "001d" & site == 'KNZ' ) %>%
  ggscatter( x = "year", y = "mean_anpp", color = 'location',
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "kendall",
             xlab = "Year Values", ylab = "ANPP",
             title = "Kendall Correlation Plot")


anpp_data_03 %>%
  filter(treatment == "001d" & site == 'KNZ' ) %>%
  group_by(site, treatment, year, location, plot) %>%
  summarize(m.anpp.plt = mean(anpp_g_m2, na.rm =T))%>%
   group_by(site, treatment, year, location) %>%
   summarise(mean_anpp = mean(m.anpp.plt, na.rm = T))%>%
  ggplot(aes(x = year, y = mean_anpp, color = location))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)

#konza data 
anpp_data_03 %>%
filter( site == "KNZ")

#cedar creek data 
cdr.a <- anpp_data %>%
  #group_by(site, treatment, crop, w_yr) %>%
  #summarise(mean_anpp = mean(anpp_g_m2, na.rm = T))%>%
 # ungroup()%>%
  #group_by(site, treatment, crop) %>%
  #mutate(n.obs = n())%>%
  #filter(n.obs > 3 )%>%
  filter( site == "CDR")

anpp_data %>%
  group_by(site, treatment, crop, year) %>%
  summarise(mean_anpp = mean(anpp_g_m2, na.rm = T))%>%
  # ungroup()%>%
  # group_by(site, treatment, crop, location) %>%
  # mutate(n.obs = n())%>%
  # filter(n.obs > 3 )%>%
  filter(treatment == "A" & site == 'CDR' ) %>%
  ggscatter( x = "year", y = "mean_anpp", 
             add = "reg.line", conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "kendall",
             title = "CDR Kendall Correlation Plot")

###################
m <- summary(lm(data = anpp_data, anpp_g_m2~w_yr))$coefficients[2,1]
m$coefficients



m.anpp.trend <- lm(data = anpp_data_clean, anpp ~ w_yr*site)

site.trends <- emtrends(m.wyrppt.trend, var = 'w_yr', ~site)


#### trend analysis for climate

# Identify relevant tidy file precip
focal_file <- "01_wyr_ppt_all_yrs.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "tidy", .$name))

#site summary 
# Identify relevant tidy file
focal_file <- "site_summary_info.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "tidy", .$name))


# Read in harmonized data
site_summary <- read.csv(file = file.path("data", "tidy", focal_file))



ppt_trend <- ppt_data %>%
  group_by(site) %>%
  summarise(
    tau = Kendall(wyr_ppt, w_yr)$tau,
    mk_p = Kendall(wyr_ppt, w_yr)$sl,
    lm_p = summary(lm(wyr_ppt~w_yr))$coefficients[2,4],
    lm_trend = summary(lm(wyr_ppt~w_yr))$coefficients[2,1],
    length = length(unique(w_yr))
  )%>%
  mutate(sig.mk = ifelse(mk_p  < 0.05, 1, 0),
         sig.lm = ifelse(lm_p  < 0.05, 1, 0),
         sum = sig.mk + sig.lm)


ppt_trend%>%
  group_by(sum, sig.mk, sig.lm)%>%
  summarise(n.obs = n())


ppt_trend%>%
  filter(sum > 1)

#plot kendall test for cper and sgs 
ppt_data %>%
  filter( site %in% c('CPER', 'sgs.us') ) %>%
  ggscatter( x = "w_yr", y = "wyr_ppt", 
             add = "reg.line", color = 'site', conf.int = TRUE, 
             cor.coef = TRUE, cor.method = "kendall",
             xlab = "Year Values", ylab = "PPT Values",
             title = "Kendall Correlation Plot")


ppt_data %>%
  filter( site %in% c('CPER', 'sgs.us') )%>%
  group_by(site)%>%
  summarize(mean_ppt = mean(wyr_ppt, na.rm = T))
#location data 
library(sf)

points <- site_summary%>%
  filter( site_id %in% c('CPER', 'sgs.us') ) %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs = 'WGS84' )
  st_transform(crs = st_crs(carm.shp))


carm.shp <- st_read('C:/Users/tkm29/OneDrive/Documents/INTERN/CARM_data/CARM_data.shp')

#


site_summary%>%
  filter( site_id %in% c('CPER', 'sgs.us') ) %>%
  ggplot(aes(x = longitude, y = latitude, color = map_mm))+
  #geom_sf(data = carm.shp, inherit.aes = F, aes(color = Past_Name_))+
  geom_point()

  
  
  
ggplot()+
  #geom_sf()+
  geom_sf(data = carm.shp, inherit.aes = F, aes(color = Past_Name_))+
  geom_sf(data = points)
