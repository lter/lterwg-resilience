## Script started by Katherine Muller, 2025-02-26
## purpose is to display sites with summarized weather variables. 

## download the site info figure:
source("ancillary/google_drive_urls.R")

## downloaded data
## site info with lat-lon
site_drive <-googledrive::drive_ls(googledrive::as_id(dir.data)) %>% 
  dplyr::filter(name == "site_summary_info")
googledrive::drive_download(file = site_drive$id, overwrite = T, type = "csv",
                            path = file.path("data", site_drive$name))
site_info <- read.csv("data/site_summary_info.csv",fileEncoding = "UTF-8-BOM")

tidy_drive <- googledrive::drive_ls(googledrive::as_id(dir.tidy_data))
weather_drive <- tidy_drive %>% dplyr::filter(grepl("daymet", name))

purrr::walk2(.x = weather_drive$name,
             .y = weather_drive$id,
              ~ googledrive::drive_download(file = .y, overwrite = T, type = "csv",
                                         path = file.path("data","tidy_data", .x)))
            
maw <- read.csv("data/tidy_data/daymet_meanannual_weather.csv")
daily <- read.csv("data/tidy_data/")

# make plot------
## overlay on whitaker
#devtools::install_github("valentinitnelav/plotbiomes")
library(plotbiomes)
maw$mean_precip_cmyear <- maw$mean_precip_mmyear/10
whitplot <- plotbiomes::whittaker_base_plot() + 
  geom_point(data = maw, mapping = aes(x = mean_tmean_degC,
                                       y = mean_precip_cmyear, color = network)) 

ggsave(filename = "data/exploratory_graphs/whittakerplot.png",
       plot = whitplot,width = 8, height = 6,units = "in",dpi = 300)

googledrive::drive_upload(media = file.path("data", "exploratory_graphs","whittakerplot.png"), overwrite = T,
                          path = googledrive::as_id(dir.exploratory_graphs))

## histograms.
library(ggpubr)
p1 <- ggplot(maw) + geom_histogram(aes(x = mean_precip_mmyear,
                               color = network), fill = NA, lwd=2.3) 
p2 <- ggplot(maw) + geom_histogram(aes(x = mean_tmean_degC,
                               color = network), fill=NA, lwd=2.3) 


  ggarrange(p1,p2)

p1 <- ggplot(maw) + geom_dotplot(aes(x = mean_precip_mmyear,y = network,
                                      fill = network) ,binwidth=50, stackdir = "center")
p2 <- ggplot(maw) + geom_dotplot(aes(x = mean_tmean_degC,y = network,
                                   fill = network) ,binwidth=1, stackdir = "center")

dplot <- ggarrange(p1,p2)

ggsave(filename = "data/exploratory_graphs/mat_map_dotplot.png",
       plot = dplot,width = 12, height = 6,units = "in",dpi = 300)

googledrive::drive_upload(media = file.path("data", "exploratory_graphs","mat_map_dotplot.png"), overwrite = T,
                          path = googledrive::as_id(dir.exploratory_graphs))
