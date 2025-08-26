# Tim Ohlert
# started June 5, 2025

# Purpose:
# Some potentially useful functions for calculating intra-annual (and maybe other)
# precipitation metrics


#Make some functions
# Author: Martin Holdrege

# helper function used below
check_ppt_input <- function(x) {
  stopifnot(is.numeric(x),
            sum(is.na(x)) < 10)
  if(!length(x) %in% c(365, 366)) {
    stop("Input must have length of 365(6), a years worth of daily ppt")
  }
}

#' Number of days on which half of annual precip falls
#'
#' @param x vector of daily precip
#' @param cutoff value below which ppt is considered 0
#'
#' @return
#' a number between 0 and 365 (or 366)
#' @export
#'
#' @examples
#' # generate fake data
#' n <- 365
#' rained <- runif(n) > 0.8 
#' x <- rep(0, 365)
#' x[rained] <- abs(rnorm(sum(rained), sd = 5))
#' hist(x)
#' days_half_ppt(x)
days_half_ppt <- function(x, cutoff = 1) {
  
  check_ppt_input(x)
  
  # trivial amounts of ppt are considered zero
  x[x < cutoff] <- 0
  
  half <- sum(x, na.rm = TRUE)/2 # half annual precip
  x_ord <- sort(x, decreasing = TRUE) # ordered precip
  
  # cumulative precip
  x_c <- cumsum(x_ord)
  less <- x_c < half #  has half of annual ppt been reached?
  
  # number of days it takes to reach half of annual precip
  days <- sum(less) + 1 # adding 1 because takes partial day to reach half
  days
}

#' Number of wet days in a year
#'
#' @param x vector of daily precip (typically in units of mm)
#' @param cutoff value above which day is considered wet (a cutoff
#' greater than zero is recommended so trivial amounts of precip is 
#' discared)
#'
#' @return
#' a number between 0 and 365 (or 366)
#' 
#' @examples
#' x <- rep(0, 365)
#' x[6:10] <- 10
#' n_wet_days(x)
n_wet_days <- function(x, cutoff = 1) {
  
  check_ppt_input(x)
  
  sum(x > cutoff)
}

# next: create functions to calculate alpha and SI as per Jeff Dukes' research


#' average length of dry spells
#' 
#' @description
#' this is the average length of consecutive days without precipitaiton
#' 
#'
#' @param x daily precipitation (for a year)
#' @param cutoff 
#'
#' @return a number between 0 and 365
#' @export
#'
#' @examples
avg_dryspell_length <- function(x, cutoff = 1) {
  
  check_ppt_input(x)
  
  is_dry <- x < cutoff
  rle <- rle(is_dry)
  dry_lengths <- rle$lengths[rle$values] # selecting only dry lengths
  mean(dry_lengths) # mean length of dry periods
}


#' calculate size of a given percentile (for days with precip)
#'
#' @param x vector of ppt
#' @param prob percentile want to calculate the size of
#' @param cutoff below which ppt is considered 0
#'
#' @return numeric value (event size of the percentile)
#' 
#' @examples
#' x <- rep(0, 365)
#' x[1:20] <- 1:20
#' ppt_percentile_size(x)
ppt_percentile_size <- function(x, prob = 0.95, cutoff = 1) {
  check_ppt_input(x)
  x2 <- x[x > cutoff]
  as.numeric(quantile(x2, probs = prob))
}



#' Calculate D of daily precipitation
#' 
#' @description
#' Similar to cv, calculates variability between values
#'
#' @param var daily ppt
#' @param cutoff 
#' @param k 
#'
#' @examples
#' #' # generate fake data
#' n <- 365
#' rained <- runif(n) > 0.8 
#' x <- rep(0, 365)
#' x[rained] <- abs(rnorm(sum(rained), sd = 10))
#' hist(x)
#' d_variability(x)
d_variability <- function (var, cutoff = 1, k=0){
  var <- var[var > cutoff]
  var<-var[!is.na(var)]
  if(length(var)<3){return(NA)}
  if(sd(var)==0){return(0)}
  if (min(var)<0){var <- var + abs(min(var)) + 0.01*(max(var)-min(var))}
  if (length(var)<2){return(NA)}else{
    var <- var + k
    aux <- numeric(length(var)-1)
    for (i in 1:(length(var)-1)){
      aux [i] <- abs(log(var[i+1]/var[i]))
    }
  }
  return(mean(aux, na.rm=T))
}


#' calculate MAP and other precipitation endices
#'
#' @param df dataframe with site_code, date and ppt columns
#' @param min_date minimum date (string) to use for filtering
#' @param max_date maximum date (string) to use for filtering
#'
#' @return dataframe with:
#' MAP--mean annual ppt
#' cv_ppt_intra (intra annual ppt cv--based on monthly ppt)
#' cv_ppt_inter interannual cv of precipitation
#' seasonality_index--an measure of intra-annual ppt variation (also
#' based on monthly ppt)
ppt_mean_annual <- function(df, min_date, max_date) {
  
  if("precip" %in% names(df)) {
    df <- rename(df, ppt = precip)
  }
  
  stopifnot(
    c('site_id', "ppt", "date") %in% names(df)
  )
  
  out <- df %>% 
    filter(.data$date >= min_date,
           .data$date <= max_date) %>% 
    mutate(month = lubridate::month(date),
           year = lubridate::year(date)) %>% 
    # order of grouping matters (b/ drop_last below)
    group_by(site_id, year, month) %>% 
    # monthly ppt
    summarise(ppt = sum(ppt), .groups = "drop_last",
              n = n()) %>% 
    # calculatings for the given year (across months), supposedly better
    # then averaged across yrs (next step)
    summarise(seasonality_index = seasonality_index(ppt),
              # intra-annual ppt calculated for the given year
              # (then averaged across years in the next step)
              cv_ppt_intra = sd(ppt)/mean(ppt)*100,
              ppt = sum(ppt),# total ppt for the given year
              n = sum(n),
              .groups = "drop_last")
  
  if(any(out$n > 366 | out$n < 365)) {
    stop("not every year as 365[6] dates")
  }
  
  out <- out %>% 
    summarize(
      MAP = mean(ppt),
      cv_ppt_intra = mean(cv_ppt_intra),
      # inter annual cv of precipitation
      cv_ppt_inter = sd(ppt)/mean(ppt)*100,
      yearly_ppt_d = d_variability(ppt, cutoff = 0),
      seasonality_index = mean(seasonality_index)
    ) %>% 
    mutate(data_period = paste(lubridate::year(min_date),
                               lubridate::year(max_date),
                               sep = "-"))
  out
}


# seasonality_index -------------------------------------------------------

seasonality_index <- function(x){
  # args:
  #   x--monthly precipitation
  # returns:
  #   precipitation seasonality index, as defined here:
  #  https://esdac.jrc.ec.europa.eu/public_path/shared_folder/projects/DIS4ME/indicator_descriptions/rainfall_seasonality.htm
  #  derived by Walsh and Lawler (1981)
  #   note: this is best when calculated for each year, and then an average
  #   it is not as good to calculate it with average monthly precipitation
  stopifnot(length(x) == 12)
  Ri <- sum(x) # annual precipitation
  SI <- sum(abs(x - Ri/12))/Ri
  SI
}

# example
if (FALSE) {
  # less variable
  seasonality_index(rnorm(12, mean = 10, sd = 2))
  # more variable
  seasonality_index(rnorm(12, mean = 10, sd = 4))
}















#using this script to summarize climate variables for each site

library(lubridate)
library(tidyverse)
library(googledrive)
drive_auth()


#load mswep data retrieved in script "retrieve-mswep-ppt-data.r"

# Identify desired file
focal_file <- "mswep-daily-ppt-lter-ltar-sites.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1zI1KYBlROyBZSgjSEYmVjsIfCmRPpUPq")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "raw", .$name))

# Read in harmonized data
mswep_lter.ltar <- read.csv(file = file.path("data", "raw", focal_file))%>%
  dplyr::mutate(date = ymd(date))%>%
  filter(!(site_id == "KBS" & project_id == "LTER"))


# Identify desired file
focal_file <- "mswep-daily-ppt-nutnet-sites.csv"

# Download harmonized data file
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1zI1KYBlROyBZSgjSEYmVjsIfCmRPpUPq")) %>% 
  dplyr::filter(name == focal_file) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "raw", .$name))

# Read in harmonized data
mswep_nutnet <- read.csv(file = file.path("data", "raw", focal_file))%>%
                dplyr::mutate(project_id = "NutNet")%>%
                dplyr::mutate(date = ymd(date))



mswep <- rbind(mswep_lter.ltar, mswep_nutnet)


# time period used to calculate MAP and other ppt metrics from CHIRPS
# and mswep gridded data products
min_date = "1981-01-01"
max_date = "2020-12-31"


cutoff <- 1 # excluding 1mm or less events


mswep2 <- mswep %>% 
  filter( date >= min_date,
          date <= max_date) 

ann0 <- mswep2 %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(site_id, year) %>% 
  dplyr::summarize(ppt_max_event = max(precip),
            ppt_mean_event = mean(precip[precip > cutoff]),
            # number of biggest event days in which half of precip fell
            days_half_ppt = days_half_ppt(precip),
            # measure of variability in event sizes (similar to CV)
            daily_ppt_d = d_variability(precip, cutoff = 1),
            # number of wet days per year
            n_wet_days = n_wet_days(precip),
            # average length of precip free periods
            avg_dryspell_length = avg_dryspell_length(precip),
            # size of the 95th percentile event size (on days with precip)
            ppt_95th_percentile_size = ppt_percentile_size(precip, prob = 0.95)
  ) %>% 
  dplyr::group_by(site_id) %>% 
  dplyr::select(-year) %>% 
  dplyr::summarize(across(where(is.numeric), .fns = mean))

ann1 <- ppt_mean_annual(mswep, min_date = min_date,
                        max_date = max_date) %>% 
   #not including dataperiod here b/ also including worldclim data
  # below which isn't the same period
  select(-data_period)

ann2 <- left_join(ann0, ann1, by = "site_id")


# Export locally
write.csv(x = ann2, row.names = F, na = '',
          file = file.path("G:", "Shared drives", "LTER-WG_Resilience-Management", "data", "harmonized_data", "site_climate_mswep.csv"))

# Upload to Drive
googledrive::drive_upload(media = file.path("G:", "Shared drives", "LTER-WG_Resilience-Management", "data", "harmonized_data", "site_climate_mswep.csv"), overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ"))

