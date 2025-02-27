##Katherine
## This script compares precip estimates from daymet and mswep
library(dplyr); library(ggplot2); library(ggrepel)
# Import data------------
site_info <- read.csv("data/site_summary_info.csv")
daymet <- read.csv("data/tidy_data/daymet_daily_weather.csv")
daymet_m <- read.csv("data/tidy_data/daymet_monthly_weather.csv")
daymet_a <- read.csv("data/tidy_data/daymet_annual_weather.csv")
daymet_ma <- read.csv("data/tidy_data/daymet_meanannual_weather.csv")

mswep <- read.csv("data/tidy_data/mswep_daily.csv")
mswep_m <- read.csv("data/tidy_data/mswep_monthly.csv")
mswep_a <- read.csv("data/tidy_data/mswep_annual.csv")
# Get MAP for MSWEP (same years)
mswep_ma <- mswep_a[which(mswep_a$year %in% c(1980:2023)),] %>% group_by(site_id, network) %>%
              summarize(MAP = mean(precip_mmyear))


## Compare MAP------
map <- left_join(daymet_ma, mswep_ma[,c('site_id',"MAP")])
# summarize difference. 
mapdiff <- map$mean_precip_mmyear - map$MAP
map$absdiff <- abs(map$mean_precip_mmyear - map$MAP)

hist(map$absdiff)

mapplot <- ggplot()+ 
  geom_abline(slope=1, intercept=0, lty=2) +
  geom_point(data=map,mapping = aes(x = mean_precip_mmyear, y = MAP, color = network)) +
  labs(x = "daymet MAP 1980-2023", y = "mswep MAP 1980-2023")+ 
  geom_text_repel(data = map[which(map$absdiff > 200),], 
                  mapping = aes(x = mean_precip_mmyear, y = MAP, label = site_id),min.segment.length = 0.1,box.padding = 0.5)

ggsave(plot = mapplot, filename = "data/exploratory_graphs/daymetVsMswepMAP.png",width = 6, height =  4, units = "in", dpi = 300)


## within year differences: compare monthly
mswep_m <- mswep_m[which(mswep_m$year %in% c(1980:2023)),]
daymet_m <- daymet_m %>% rename("daymet_precip" = "precip_mmmonth")
mswep_m <- mswep_m %>% rename("mswep_precip" = "precip_mmmonth")
# compare monthly precip.
monthly <- left_join(daymet_m, subset(mswep_m, select = -c(network)), by = c("site_id","year","month"))
monthly$diff <- monthly$daymet_precip - monthly$mswep_precip
monthly$absdiff <- abs(monthly$diff)

ggplot(monthly) + geom_point(aes(x = daymet_precip, y = mswep_precip, color = network)) + 
  geom_abline(slope=1, intercept = 0, lty=2)+
  facet_wrap(vars(month))

ggplot(monthly) + geom_density(aes(x = diff, color = network)) + 
  facet_wrap(vars(month),scales = "free_y")

ggplot(monthly) + geom_histogram(aes(x = diff, color = network),binwidth = 50) + 
  facet_wrap(vars(month),scales = "free_y")

## do monthly differences follow a trend over time?
timetrend <- ggplot(monthly, aes(x = year, y = diff)) + geom_point( aes(color = network)) + 
  stat_smooth(method = "lm")+
  ylab("(daymet - mswep) mm/month")+
  facet_wrap(vars(month))
ggsave(plot = timetrend,filename="data/exploratory_graphs/monthlyprecip_mswep_daymet_time.pdf",
       width = 8, height = 6)

ggsave(plot = timetrend,filename="data/exploratory_graphs/monthlyprecip_mswep_daymet_time.png",
       width = 8, height = 6,dpi=300)

## is there a trend across sites?
site_info <- site_info[order(site_info$network, site_info$latitude),]
monthly$site_id <- factor(monthly$site_id, levels = site_info$site_id)
monthlyplot <- ggplot(monthly, aes(x = site_id, y = diff)) + geom_violin( aes(color = network)) + 
  ylim(-300,200)+
  ylab("(daymet - mswep) mm/month")+
  xlab("site ordered by latitude") +
  facet_wrap(vars(month))
ggsave(plot = monthlyplot,filename="data/exploratory_graphs/monthlyprecip_mswep_daymet.pdf",
       width = 10, height = 9)

ggsave(plot = monthlyplot,filename="data/exploratory_graphs/monthlyprecip_mswep_daymet_site.png",
       width = 10, height = 9, dpi = 300)
