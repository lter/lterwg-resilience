#site durations 
#started 022625 TKM

library(readxl)
library(tidyverse)

`%notin%` <- Negate(`%in%`)

colz <- c("wet"= "blue", "dry" = "red", "neither" = "grey90")

#set up site and duration datasets
ltar_sites <- read_excel("G:/Shared drives/LTER-WG_Resilience-Management/data/Treatment_overview_LTAR.xlsx")%>%
  dplyr::rename( site_ID = site)%>%
  dplyr::select(site_ID, treatment_ID, start_yr, end_yr)%>%
  mutate(network = "LTAR")


nutnet_sites <- read.csv("G:/Shared drives/LTER-WG_Resilience-Management/data/pre_processed_data/nutnet_fay_2025_pre-process.csv")%>%
  filter(country %in% c('us', 'ca'))%>%
  dplyr::group_by(network, site_code)%>%
  dplyr::summarize(start_yr = min(year), 
                   end_yr = max(year))%>%
  dplyr::rename(site_ID = site_code)


lter_sites <- read.csv("G:/Shared drives/LTER-WG_Resilience-Management/data/lter_site_duration.csv")%>%
  mutate(network = 'LTER')

## merge duration datasets

all_site_duration <- bind_rows(ltar_sites, nutnet_sites)%>%
  bind_rows(lter_sites)%>%
  unite(site_trt, site_ID, treatment_ID, remove = F)%>%
  dplyr::mutate(site_ID = ifelse(site_trt == 'ECB_ECB_E1bau', 'ecb_e1', ifelse( site_trt == 'ECB_ECB_C1asp', 'ecb_c1', ifelse( site_trt == 'ECB_ECB_D2bau', 'ecb_d2', site_ID ))))%>%
  dplyr::select(site_ID, start_yr, end_yr, network)%>%
  unique()%>%
  mutate(length = end_yr - start_yr,
         site_ID = tolower(site_ID))

##find mean duration 
mean(all_site_duration$length, na.rm = T)

##plots
#plot site duration
ggplot(all_site_duration)+
  geom_segment( aes(y = site_ID, x = start_yr, xend = end_yr))

#plot length histogram 
ggplot(all_site_duration)+
  geom_histogram(aes(x = length), binwidth = 5)+
  facet_wrap(~network)

####bring in precip data ####
# merge precip with  site duration

#run code from the file examinExtremeWeather.r to generate precipdf

precip_dur <- precipdf%>%
  mutate(site_ID = ifelse(site_id == 'SGS', 'cper', ifelse(site_id == 'cedr.us', 'cdcr.us', tolower(site_id))))%>%
  merge(all_site_duration)%>%
  group_by(site_ID, start_yr, end_yr)%>%
  mutate(site_mean_ppt_mmyr = mean(precip_mmyear, na.rm = T))%>%
  filter(year >= start_yr & year <= end_yr)

##Plot extremes with sites filted to data period

sampled_extremes <- ggplot() +
  geom_point(precip_dur,mapping = aes(x = year, y = site_ID, color = precip_extreme ))+
  scale_color_manual(values = colz)  +
  geom_hline(yintercept = c(35.5, 21.5,7.5)) +
  theme_bw()

ggsave(sampled_extremes, filename = "G:/Shared drives/LTER-WG_Resilience-Management/exploratory_graphs/sampled_precip_extremes_022725.jpeg")



##checks for missing data 
filter(all_site_duration, site_ID %notin% tolower(precipdf$site_id ))

filter( precipdf, tolower(site_id) %notin% all_site_duration$site_ID)%>%
  select(site_id)%>%
  unique()

length(unique(precip_dur$site_ID) )

length(unique(precipdf$site_id) )

length(unique(all_site_duration$site_ID) )


#built a table with recored mean_hist, mean_ppt_sampled, sd, number of extremes, number of double extremes

precip_summary_df <- precip_dur%>%
  group_by(site_ID, network, start_yr, end_yr, site_mean_ppt_mmyr)%>%
  arrange(site_ID, year)%>%
  mutate(prev_extreme = lag(precip_extreme))%>%
  summarise(tmean_sampled_degC = mean(mean_tmean_degC),
            precip_mean_mmyr = mean(precip_mmyear),
            precip_sd_mmyr = sd(precip_mmyear),
            n_extreme_dry = sum(precip_extreme == 'dry'),
            n_extreme_wet = sum(precip_extreme == 'wet'),
            n_double_dry = sum(precip_extreme == 'dry' & prev_extreme == 'dry', na.rm = T),
            n_double_wet = sum(precip_extreme == 'wet' & prev_extreme == 'wet', na.rm = T),
            n_drywet  = sum(precip_extreme == 'wet' & prev_extreme == 'dry', na.rm = T),
            n_wetdry = sum(precip_extreme == 'dry' & prev_extreme == 'wet', na.rm = T))

#write.csv(precip_summary_df, 'G:/Shared drives/LTER-WG_Resilience-Management/data/precip_extremes_site_summary.csv')


sum(precip_summary_df$n_extreme_dry)  
sum(precip_summary_df$n_extreme_wet) 
sum(precip_summary_df$n_double_dry)  
sum(precip_summary_df$n_double_wet)
sum(precip_summary_df$n_drywet)  
sum(precip_summary_df$n_wetdry)
