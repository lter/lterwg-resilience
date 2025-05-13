###Playing around with roughly harmonized adn processed data

# Identify nice name for exported object
focal_output <- "02_resilience_wrangled.csv"

# Export locally
anpp <- read.csv(file = file.path("data", "tidy", focal_output))

str(combo_ppt)

ggplot(subset(anpp, anpp$network != "LTAR and LTER"), aes(total_annual_precip_mm, anpp_g_m2, color=network))+
  geom_point()+
  ylim(0, 3000)+
  theme_bw()+
  facet_wrap(~network)

##summarize
str(anpp)

agg <- anpp %>%
  group_by(site, location, treatment, year, total_annual_precip_mm, network) %>%
  summarize(mean.anpp = mean(anpp_g_m2))

g1 <- ggplot(
  subset(agg, agg$network != "LTAR and LTER"), 
  aes(total_annual_precip_mm, mean.anpp, color=network))+
  geom_point()+
  ylim(0, 3000)+
  theme_bw()+
  facet_wrap(~network)

#create necessary folder:
dir.create(file.path("data", "exploratory_graphs"), showWarnings = F)
#save figure locally
ggsave(filename = file.path("data", "exploratory_graphs", "annual_ppt_anpp_graph_OLH.png"),
       plot = g1, width = 12, height = 6, units = "in", dpi = 300)

#upload figure to google drive
googledrive::drive_upload(media = file.path("data", "exploratory_graphs", "annual_ppt_anpp_graph_OLH.png"), overwrite = T,
                          path = googledrive::as_id(dir.exploratory_graphs))

