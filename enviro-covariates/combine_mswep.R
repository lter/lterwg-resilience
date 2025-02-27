#
library(googledrive); library(dplyr)
source("ancillary/google_drive_urls.R")
# download raw mswep from google drive

dir.create(file.path("data", "raw_data"), showWarnings = F)

mswep_drive <- googledrive::drive_ls(googledrive::as_id(dir.raw_data)) %>% 
  dplyr::filter(grepl("mswep", name))

purrr::walk2(.x = mswep_drive$name,
             .y = mswep_drive$id,
             ~ googledrive::drive_download(file = .y, overwrite = T, type = "csv",
                                           path = file.path("data","raw_data", .x)))
mswep1 <- read.csv("data/raw_data/mswep-daily-ppt-lter-ltar-sites.csv")
mswep2 <- read.csv("data/raw_data/mswep-daily-ppt-nutnet-sites.csv")

str(mswep1)
str(mswep2)
mswep2$project_id <- "NutNet"

mswep <- dplyr::bind_rows(mswep1, mswep2)
mswep <- mswep %>% subset(select = -c(X)) %>%
            rename("network" = "project_id")
## remove sites excluded. 
site_info <- read.csv("data/site_summary_info.csv")
setdiff(site_info$site_id, mswep$site_id)
setdiff(mswep$site_id, site_info$site_id)
write.csv(mswep, file = "data/tidy_data/mswep_daily.csv", row.names = FALSE)

googledrive::drive_upload(media = file.path("data", "tidy_data","mswep_daily.csv"), overwrite = T,
                          path = googledrive::as_id(dir.tidy_data))

monthly <- mswep %>% group_by(site_id, network, year, month) %>% 
              summarize(precip_mmmonth = sum(precip))
annual <- monthly %>% group_by(site_id, network, year) %>%
              summarize(precip_mmyear = sum(precip_mmmonth))
mfile = "mswep_monthly.csv"
afile = "mswep_annual.csv"
write.csv(monthly, file = glue::glue("data/tidy_data/{mfile}"), row.names = FALSE)
write.csv(annual, file = glue::glue("data/tidy_data/{afile}"), row.names = FALSE)

googledrive::drive_upload(media = file.path("data", "tidy_data",mfile), overwrite = T,
                          path = googledrive::as_id(dir.tidy_data))
googledrive::drive_upload(media = file.path("data", "tidy_data",afile), overwrite = T,
                          path = googledrive::as_id(dir.tidy_data))
