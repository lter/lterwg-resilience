# ============================================================
# DATA PREP: ANPP + SPEI + Climate (Precip)
#   - Imports
#   - Harvest-aligned lags (0:-11)
#   - Scales (1:S_spei, 1:S_ppt)
#   - Detrending (residuals + mean)
# Output objects (main):
#   anpp_few
#   spei_all, spei_anpp_lag, spei_anpp_lag_dt
#   clim_monthly, clim_anpp_lag, clim_anpp_scaled, clim_anpp_scaled_dt
# ============================================================

# -----------------------------
# 0) Libraries
# -----------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(slider)
library(readr)
library(dendroTools)

# -----------------------------
# 1) Settings 
# -----------------------------
sites_keep  <- c("PRHPA","CPER")
treat_keep  <- c("CPER_CTRL","PRHPA_NEMELTCRS_ROT42","PRHPA_NEMERREM_CRN3L")

plot_lags <- 0:-11     # lags relative to harvest; 0 = harvest month
S_spei <- 24           # SPEI scales to load (1..S_spei)
S_ppt  <- 24           # precip “scales” (rolling k-month sums) (1..S_ppt)

# -----------------------------
# 2) Detrending (residuals + mean)
#    (used to remove long-term trends in both ANPP and climate series)
# -----------------------------
detrend_resid_plus_mean <- function(df, y_col, t_col) {
  y <- df[[y_col]]
  t <- df[[t_col]]
  ok <- is.finite(y) & is.finite(t)
  
  if (sum(ok) < 3) {
    df[[paste0(y_col, "_dt")]] <- NA_real_
    return(df)
  }
  
  fit <- lm(y[ok] ~ t[ok])
  yhat <- rep(NA_real_, length(y))
  yhat[ok] <- predict(fit)
  
  df[[paste0(y_col, "_dt")]] <- (y - yhat) + mean(y[ok], na.rm = TRUE)
  df
}

# ============================================================
# 3) ANPP: Import + filter + harvest month
# ============================================================
anpp_dat <- read.csv("data/harmonized_data/anpp_wyr_trt_merged.csv",
                     stringsAsFactors = FALSE)

anpp_few <- anpp_dat %>%
  filter(site %in% sites_keep,
         treatment %in% treat_keep) %>%
  mutate(
    year = as.integer(year),
    crop = ifelse(crop == "" | is.na(crop), "Grassland", crop),
    hrvst_mth = as.integer(month)
  ) %>%
  group_by(site, year, crop) %>%
  summarize(
    hrvst_mth = first(hrvst_mth),
    anpp_g_m2 = mean(anpp_g_m2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  drop_na(anpp_g_m2)

# ============================================================
# 4) SPEI: Import all scales -> stack long -> harvest-align lags
# ============================================================

## download the SPEI data if not already downloaded

dir.create(file.path("data", "spei_data"), showWarnings = F)
# Identify wanted files
files_drive <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1JtFMD4IAizjNGd0wbLLZIBdgqnk4YR97")) %>% 
  dplyr::filter(stringr::str_detect(string = .$name, pattern = "\\.csv"))

# Identify local files
files_local <- dir(path = file.path("data", "spei_data"))
files_local

# Overwrite local data files?
update <- TRUE

# Identify desired files
if(update == T) {
  files_wanted <- files_drive 
} else {
  files_wanted <- files_drive %>%
    dplyr::filter(!name %in% files_local)
}

# Download them!
purrr::walk2(.x = files_wanted$id, .y = files_wanted$name,
             .f = ~ googledrive::drive_download(file = .x, overwrite = T,
                                                path = file.path("data", "spei_data", .y)))


# 4A) Reader: one SPEI file (scale-specific)
read_spei_long <- function(path, scale_chr) {
  x <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  
  # Parse dates
  x$date <- as.Date(substr(as.character(x$date), 1, 10),
                    tryFormats = c("%Y-%m-%d", "%m/%d/%y"))
  
  # Year/month
  x$year    <- as.integer(format(x$date, "%Y"))
  x$speimth <- as.integer(format(x$date, "%m"))
  
  # Fix 2-digit year parsing artifacts if present
  bad <- which(x$year >= 2024)
  if (length(bad) > 0) x$year[bad] <- x$year[bad] - 100
  
  site_cols <- setdiff(names(x), c("date","year","speimth"))
  
  x %>%
    pivot_longer(cols = all_of(site_cols),
                 names_to = "site",
                 values_to = "spei") %>%
    mutate(
      spei  = suppressWarnings(as.numeric(spei)),
      scale = as.integer(scale_chr)
    ) %>%
    select(site, year, speimth, scale, spei)
}

# 4B) Load all SPEI01..SPEI24 (or subset via S_spei)
spei_dir <- "data/spei_data"

# NOTE: adjust pattern if your files are "SPEI01.csv" (uppercase)
spei_paths <- list.files(spei_dir, pattern = "^spei\\d{2}\\.csv$", full.names = TRUE)

spei_scales <- spei_paths %>%
  basename() %>%
  str_extract("\\d{2}") %>%
  as.integer()

keep <- which(!is.na(spei_scales) & spei_scales >= 1 & spei_scales <= S_spei)
spei_paths  <- spei_paths[keep]
spei_scales <- spei_scales[keep]

spei_all <- map2_dfr(spei_paths, spei_scales, read_spei_long) %>%
  filter(
    year >= 1980,
    speimth >= 1, speimth <= 12,
    scale %in% 1:S_spei
  ) %>%
  arrange(site, year, speimth, scale)

# 4C) Harvest-align SPEI to ANPP year via lag months (0:-11)
#     lag_mo = 0 means SPEI in harvest month of that ANPP year
#     negative lag_mo goes back in time from harvest
spei_anpp_lag <- anpp_few %>%
  distinct(site, year, crop, hrvst_mth, anpp_g_m2) %>%
  expand_grid(lag_mo = plot_lags) %>%
  mutate(
    speimth = ((hrvst_mth + lag_mo - 1) %% 12) + 1,
    year_offset = ifelse(speimth <= hrvst_mth, 0L, -1L),
    spei_year = year + year_offset
  ) %>%
  left_join(
    spei_all %>% transmute(site, spei_year = year, speimth, scale, spei),
    by = c("site","spei_year","speimth"),
    relationship = "many-to-many"
  )

# 4D) Detrend SPEI and ANPP within each lag×scale cell
spei_anpp_lag_dt <- spei_anpp_lag %>%
  group_by(site, crop, scale, lag_mo) %>%
  group_modify(~{
    df <- .x
    df <- detrend_resid_plus_mean(df, "spei", "spei_year")
    df <- detrend_resid_plus_mean(df, "anpp_g_m2", "spei_year")
    df
  }) %>%
  ungroup()

# ============================================================
# 5) Climate (Precip): harvest-align monthly precip -> compute scales -> detrend
# ============================================================
library(readr)
daymet_monthly_weather <- read_csv("data/pre_processed_data/daymet_monthly_weather.csv")
# 5A) Monthly precip table
#     Required cols: site_id, year, month, precip_mmmonth
clim_monthly <- daymet_monthly_weather %>%
  rename(site = site_id, clim_mth = month) %>%
  mutate(
    year = as.integer(year),
    clim_mth = as.integer(clim_mth)
  ) %>%
  select(site, year, clim_mth, precip_mmmonth)

# 5B) Build a long aligned monthly sequence long enough to compute rolling scales
build_climate_aligned <- function(anpp_df, clim_df, window_months) {
  anpp_df %>%
    distinct(site, year, crop, hrvst_mth, anpp_g_m2) %>%
    expand_grid(lag_mo = seq(-(window_months - 1), 0, by = 1)) %>%
    mutate(
      target_month = ((hrvst_mth + lag_mo - 1) %% 12) + 1,
      year_offset  = ifelse(target_month <= hrvst_mth, 0L, -1L),
      target_year  = year + year_offset
    ) %>%
    left_join(
      clim_df %>% transmute(site, target_year = year, target_month = clim_mth, precip_mmmonth),
      by = c("site","target_year","target_month"),
      relationship = "many-to-many"
    ) %>%
    arrange(site, crop, year, lag_mo)
}

# Need enough months so that for lag=-11 and scale=24 we can look back 23 months:
window_months_ppt <- length(plot_lags) + (S_ppt - 1)

clim_anpp_lag <- build_climate_aligned(anpp_few, clim_monthly, window_months_ppt)

# 5C) Compute rolling k-month precip sums along the aligned lag series
#     (scale = k months, ending at each lag_mo)
scales_ppt <- 1:S_ppt

clim_anpp_scaled <- clim_anpp_lag %>%
  group_by(site, crop, year) %>%
  arrange(lag_mo) %>%
  group_modify(~{
    df <- .x
    map_dfr(scales_ppt, function(k) {
      df %>%
        mutate(
          scale = k,
          precip_sc = slide_dbl(precip_mmmonth, sum, .before = k - 1, .complete = TRUE)
        )
    })
  }) %>%
  ungroup() %>%
  # keep only the plotted lags (0:-11); scales already computed using prior months
  filter(lag_mo %in% plot_lags)

# 5D) Detrend precip scales and ANPP within each lag×scale cell
clim_anpp_scaled_dt <- clim_anpp_scaled %>%
  group_by(site, crop, scale, lag_mo) %>%
  group_modify(~{
    df <- .x
    df <- detrend_resid_plus_mean(df, "precip_sc", "year")
    df <- detrend_resid_plus_mean(df, "anpp_g_m2", "year")
    df
  }) %>%
  ungroup()

## 6) Work on trying the dendrotools and creating visuals for the selected sites and crops
clim_anpp_dendro <- clim_anpp_scaled_dt %>%
  filter(scale == 1) %>%
  select(c(site, crop, scale, year, target_month, target_year, anpp_g_m2, precip_mmmonth)) %>%
  mutate(site_crop = paste(site, crop, sep = "_"))

site_list <- unique(clim_anpp_dendro$site_crop)

# 2. Create a function to handle the processing for one site
process_site_climate <- function(current_site) {
  
  message(paste("Processing site:", current_site))
  
  # Filter Precipitation data for the specific site
  ppt_subset <- clim_anpp_dendro %>%
    dplyr::filter(site_crop == current_site) %>%
    #dplyr::filter(site_crop == "CPER_Grassland") %>%
    select(year, target_month, precip_mmmonth) %>%
    rename(Year = "year", Month = "target_month", Precipitation = "precip_mmmonth") %>%
    filter(Year > 1980)
  
  # Filter and summarize ANPP data for the specific site
  # Note: Assuming your 'cper' dataframe has a site column; replace 'site1' if different
  anpp_subset <- clim_anpp_dendro %>%
    dplyr::filter(site_crop == current_site) %>% 
    #dplyr::filter(site_crop == "CPER_Grassland") %>%
    rename(Year = "year") %>%
    group_by(Year) %>%
    summarize(mean_anpp = mean(anpp_g_m2, na.rm = TRUE)) %>%
    column_to_rownames(var = "Year")
  
  # Run the monthly response analysis
  # We use tryCatch so that if one site fails (e.g., too few years), the loop continues
  result <- tryCatch({
    monthly_response(
      response = anpp_subset,
      env_data = ppt_subset,
      lower_limit = 1, upper = 24,
      fixed_width = 0,
      method = "lm", metric = "r.squared",
      row_names_subset = TRUE,
      remove_insignificant = TRUE, 
      previous_year = TRUE,
      reference_window = "end",
      alpha = 0.05, aggregate_function = 'sum', 
      boot = TRUE, boot_n = 100,
      tidy_env_data = TRUE, 
      month_interval = c(-5, 10)
    )
  }, error = function(e) {
    message(paste("Error at site", current_site, ":", e$message))
    return(NULL)
  })
  
  # If successful, plot the results
  if (!is.null(result)) {
    # Customize title for each plot
    p <- plot(result, type = 2,title = paste("Climate Response:", current_site))
    return(p)
  }
}

# 3. Run the loop for all sites
all_plots <- map(site_list, process_site_climate)

# 4. View a specific plot (e.g., the first one)
all_plots[[1]]

