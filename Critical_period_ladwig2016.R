# ============================================================
# Timing & Critical Period (ANPP) — MATCHED-SCALE WINDOW PIPELINE
#   - SPEI matched-scale window search:
#       window length determines SPEI scale
#       example: start=-4, end=-2 => scale=3, uses SPEI-3 ending at -2
#   - Precip matched-scale window search:
#       window length determines rolling precipitation sum
#       example: start=-4, end=-2 => 3-month precip sum ending at -2
#   - Tmax matched-scale window search:
#       window length determines rolling Tmax mean
#   - Tmin matched-scale window search:
#       window length determines rolling Tmin mean
#   - Ladwig-style condensed critical climate period plots
#
# Notes:
#   * Uses stability_anpp.csv
#   * Loads daymet_monthly_weather.csv from data/pre_processed_data/
#   * Alignment uses absolute month index
#   * Row labels count actual ANPP years only
#   * Panels are retained only if they have >= min_years_fit ANPP years
# ============================================================

# -----------------------------
# 0) Libraries
# -----------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(slider)
library(ggplot2)
library(readr)

# -----------------------------
# 1) Settings
# -----------------------------
sites_keep  <- c("CDR", "CPER", "KBS", "KNZ", "PRHPA")   # NULL => keep all sites
treat_keep  <- NULL                                       # NULL => keep all treatments

plot_lags  <- 0:-23
scales_use <- 1:24

## check the matched windows being tested
wins_matched <- expand_grid(
  start_lag = plot_lags,
  end_lag   = plot_lags
) %>%
  filter(start_lag <= end_lag) %>%
  mutate(
    scale = end_lag - start_lag + 1L,
    window_label = paste0(start_lag, " to ", end_lag)
  ) %>%
  filter(scale %in% scales_use) %>%
  arrange(scale, start_lag, end_lag)

wins_matched

# Ladwig-style CCP plot settings
ccp_p_cut <- 0.01
ccp_min_abs_r_to_show <- 0

# Crop row order in the figure
crop_order <- c("Grassland", "corn", "soybean", "winter_wheat")  

# Optional gray shading in the CCP plot.
# Example: ccp_shade_lags <- c(-8, -3)
ccp_shade_lags <- NULL

# Minimum number of ANPP years required for each site × crop panel
min_years_fit <- 9

# Climate facet order
climate_order <- c(
  "SPEI\nmatched scale",
  "Precip\nmatched scale",
  "Tmax\nmatched mean",
  "Tmin\nmatched mean"
)

set.seed(123)

# ============================================================
# 2) Helper functions
# ============================================================

maybe_filter_in <- function(df, col, keep_vec) {
  if (is.null(keep_vec)) return(df)
  dplyr::filter(df, .data[[col]] %in% keep_vec)
}

first_non_na <- function(x, default = NA_integer_) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(default)
  x[1]
}

AICc <- function(fit) {
  k <- length(coef(fit))
  n <- nobs(fit)
  aic <- AIC(fit)
  aic + (2 * k * (k + 1)) / (n - k - 1)
}

cor_rp <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3) return(tibble(n = sum(ok), r = NA_real_, p = NA_real_))
  ct <- suppressWarnings(cor.test(x[ok], y[ok], method = "pearson"))
  tibble(n = sum(ok), r = as.numeric(ct$estimate), p = ct$p.value)
}

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

format_n_label <- function(n) {
  ifelse(
    is.na(n) | !is.finite(n),
    "n=NA",
    paste0("n=", n)
  )
}

add_target_year_month <- function(df, year_col = "year", mth_col = "hrvst_mth", lag_col = "lag_mo") {
  df %>%
    mutate(
      abs_m = .data[[year_col]] * 12L + (.data[[mth_col]] - 1L) + .data[[lag_col]],
      target_year  = abs_m %/% 12L,
      target_month = abs_m %% 12L + 1L
    ) %>%
    dplyr::select(-abs_m)
}

all_windows_matched <- function(lags, scales_allowed) {
  expand_grid(end_lag = lags, start_lag = lags) %>%
    filter(start_lag <= end_lag) %>%
    mutate(scale = end_lag - start_lag + 1L) %>%
    filter(scale %in% scales_allowed) %>%
    arrange(end_lag, start_lag)
}

sum_if_any <- function(x) {
  if (all(!is.finite(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

mean_if_any <- function(x) {
  if (all(!is.finite(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

# ============================================================
# 3) Import ANPP and count years correctly
# ============================================================
anpp_dat <- read.csv(
  "data/harmonized_data/stability_anpp.csv",
  stringsAsFactors = FALSE
)

# Important:
# This keeps your current temporary filter to remove KNZ cropland.
anpp_raw_filtered <- anpp_dat %>%
  { if (!is.null(sites_keep)) dplyr::filter(., site %in% sites_keep) else . } %>%
  { if (!is.null(treat_keep)) dplyr::filter(., treatment %in% treat_keep) else . } %>%
  filter(duration_years > 10) %>% ### temporary filter to remove KNZ cropland
  mutate(
    year = as.integer(year),
    crop = ifelse(crop == "" | is.na(crop), "Grassland", crop),
    hrvst_mth = as.integer(month),
    anpp_g_m2 = suppressWarnings(as.numeric(anpp_g_m2))
  ) %>%
  filter(
    is.finite(year),
    is.finite(anpp_g_m2)
  )

anpp_all_panels <- anpp_raw_filtered %>%
  group_by(site, year, crop) %>%
  summarize(
    hrvst_mth = first_non_na(hrvst_mth, default = 8L),
    anpp_g_m2 = mean(anpp_g_m2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    hrvst_mth = if_else(is.na(hrvst_mth), 8L, hrvst_mth)
  ) %>%
  filter(
    is.finite(year),
    is.finite(anpp_g_m2)
  )

anpp_panel_n <- anpp_all_panels %>%
  group_by(site, crop) %>%
  summarize(
    anpp_n_years = n_distinct(year),
    anpp_year_min = min(year, na.rm = TRUE),
    anpp_year_max = max(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(site, crop)

valid_anpp_panels <- anpp_panel_n %>%
  filter(anpp_n_years >= min_years_fit)

anpp_few <- anpp_all_panels %>%
  semi_join(valid_anpp_panels, by = c("site", "crop"))

anpp_panel_n <- anpp_panel_n %>%
  semi_join(valid_anpp_panels, by = c("site", "crop"))

print(anpp_panel_n)

# ============================================================
# 4) Read SPEI data
# ============================================================
read_spei_long <- function(path, scale_chr) {
  x <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  
  x$date <- as.Date(
    substr(as.character(x$date), 1, 10),
    tryFormats = c("%Y-%m-%d", "%m/%d/%y")
  )
  
  x$year    <- as.integer(format(x$date, "%Y"))
  x$speimth <- as.integer(format(x$date, "%m"))
  
  bad <- which(x$year >= 2024)
  if (length(bad) > 0) x$year[bad] <- x$year[bad] - 100
  
  site_cols <- setdiff(names(x), c("date", "year", "speimth"))
  
  x %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(site_cols),
      names_to = "site",
      values_to = "spei"
    ) %>%
    dplyr::mutate(
      spei = suppressWarnings(as.numeric(spei)),
      scale = as.integer(scale_chr)
    ) %>%
    dplyr::select(site, year, speimth, scale, spei)
}

spei_paths  <- list.files("data/spei_data", pattern = "^spei\\d{2}\\.csv$", full.names = TRUE)
spei_scales <- spei_paths %>% basename() %>% str_extract("\\d{2}") %>% as.integer()

keep <- which(!is.na(spei_scales) & spei_scales %in% scales_use)
spei_paths  <- spei_paths[keep]
spei_scales <- spei_scales[keep]

spei_all <- map2_dfr(spei_paths, spei_scales, read_spei_long) %>%
  filter(
    year >= 1980,
    speimth >= 1,
    speimth <= 12,
    scale %in% scales_use
  ) %>%
  { if (!is.null(sites_keep)) dplyr::filter(., site %in% sites_keep) else . } %>%
  group_by(site, year, speimth, scale) %>%
  summarize(
    spei = mean(spei, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(site, year, speimth, scale)

# ============================================================
# 5) Read Daymet climate data: precip, Tmax, Tmin
# ============================================================
daymet_monthly_weather <- read_csv("data/pre_processed_data/daymet_monthly_weather.csv")

clim_monthly <- daymet_monthly_weather %>%
  rename(site = site_id, clim_mth = month) %>%
  mutate(
    year = as.integer(year),
    clim_mth = as.integer(clim_mth),
    precip_mmmonth = suppressWarnings(as.numeric(precip_mmmonth)),
    mean_tmax_degC = suppressWarnings(as.numeric(mean_tmax_degC)),
    mean_tmin_degC = suppressWarnings(as.numeric(mean_tmin_degC))
  ) %>%
  dplyr::select(site, year, clim_mth, precip_mmmonth, mean_tmax_degC, mean_tmin_degC) %>%
  { if (!is.null(sites_keep)) dplyr::filter(., site %in% sites_keep) else . } %>%
  group_by(site, year, clim_mth) %>%
  summarize(
    precip_mmmonth = sum_if_any(precip_mmmonth),
    mean_tmax_degC = mean_if_any(mean_tmax_degC),
    mean_tmin_degC = mean_if_any(mean_tmin_degC),
    .groups = "drop"
  )

# ============================================================
# 6) Build matched windows
# ============================================================
wins_matched <- all_windows_matched(plot_lags, scales_use)

# ============================================================
# 7) SPEI matched-scale window data
# ============================================================
# For each start/end window:
#   scale = end_lag - start_lag + 1
#   target month = harvest month + end_lag
#   SPEI value = SPEI-scale at that target month

spei_window_dat <- anpp_few %>%
  distinct(site, year, crop, hrvst_mth, anpp_g_m2) %>%
  crossing(wins_matched) %>%
  mutate(lag_mo = end_lag) %>%
  add_target_year_month(year_col = "year", mth_col = "hrvst_mth", lag_col = "lag_mo") %>%
  rename(spei_year = target_year, speimth = target_month) %>%
  left_join(
    spei_all %>%
      transmute(site, spei_year = year, speimth, scale, spei),
    by = c("site", "spei_year", "speimth", "scale")
  )

spei_window_dt <- spei_window_dat %>%
  group_by(site, crop, start_lag, end_lag, scale) %>%
  group_modify(~{
    df <- .x
    df <- detrend_resid_plus_mean(df, "spei", "spei_year")
    df <- detrend_resid_plus_mean(df, "anpp_g_m2", "spei_year")
    df
  }) %>%
  ungroup()

# ============================================================
# 8) Daymet matched-scale window data: Precip, Tmax, Tmin
# ============================================================
max_scale_clim <- max(scales_use)

# Need enough months back to compute the largest climate window
window_months_clim <- length(plot_lags) + (max_scale_clim - 1)

build_climate_aligned <- function(anpp_df, clim_df, window_months) {
  anpp_df %>%
    distinct(site, year, crop, hrvst_mth, anpp_g_m2) %>%
    expand_grid(lag_mo = seq(-(window_months - 1), 0, by = 1)) %>%
    add_target_year_month(year_col = "year", mth_col = "hrvst_mth", lag_col = "lag_mo") %>%
    left_join(
      clim_df %>%
        transmute(
          site,
          target_year = year,
          target_month = clim_mth,
          precip_mmmonth,
          mean_tmax_degC,
          mean_tmin_degC
        ),
      by = c("site", "target_year", "target_month")
    ) %>%
    arrange(site, crop, year, lag_mo)
}

clim_anpp_lag <- build_climate_aligned(anpp_few, clim_monthly, window_months_clim)

make_climate_scaled <- function(clim_lag_df,
                                value_col,
                                out_col,
                                summary_type = c("sum", "mean"),
                                scales_use,
                                plot_lags) {
  
  summary_type <- match.arg(summary_type)
  
  clim_lag_df %>%
    group_by(site, crop, year) %>%
    arrange(lag_mo) %>%
    group_modify(~{
      df <- .x
      
      map_dfr(scales_use, function(k) {
        vals <- df[[value_col]]
        
        rolled <- if (summary_type == "sum") {
          slide_dbl(vals, sum_if_any, .before = k - 1, .complete = TRUE)
        } else {
          slide_dbl(vals, mean_if_any, .before = k - 1, .complete = TRUE)
        }
        
        df %>%
          mutate(
            scale = k,
            !!out_col := rolled
          )
      })
    }) %>%
    ungroup() %>%
    filter(lag_mo %in% plot_lags)
}

precip_anpp_scaled <- make_climate_scaled(
  clim_lag_df = clim_anpp_lag,
  value_col = "precip_mmmonth",
  out_col = "precip_sc",
  summary_type = "sum",
  scales_use = scales_use,
  plot_lags = plot_lags
)

tmax_anpp_scaled <- make_climate_scaled(
  clim_lag_df = clim_anpp_lag,
  value_col = "mean_tmax_degC",
  out_col = "tmax_sc",
  summary_type = "mean",
  scales_use = scales_use,
  plot_lags = plot_lags
)

tmin_anpp_scaled <- make_climate_scaled(
  clim_lag_df = clim_anpp_lag,
  value_col = "mean_tmin_degC",
  out_col = "tmin_sc",
  summary_type = "mean",
  scales_use = scales_use,
  plot_lags = plot_lags
)

make_matched_window_dat <- function(scaled_df) {
  scaled_df %>%
    rename(end_lag = lag_mo) %>%
    left_join(
      wins_matched,
      by = c("end_lag", "scale")
    ) %>%
    filter(!is.na(start_lag))
}

precip_window_dat <- make_matched_window_dat(precip_anpp_scaled)
tmax_window_dat   <- make_matched_window_dat(tmax_anpp_scaled)
tmin_window_dat   <- make_matched_window_dat(tmin_anpp_scaled)

precip_window_dt <- precip_window_dat %>%
  group_by(site, crop, start_lag, end_lag, scale) %>%
  group_modify(~{
    df <- .x
    df <- detrend_resid_plus_mean(df, "precip_sc", "year")
    df <- detrend_resid_plus_mean(df, "anpp_g_m2", "year")
    df
  }) %>%
  ungroup()

tmax_window_dt <- tmax_window_dat %>%
  group_by(site, crop, start_lag, end_lag, scale) %>%
  group_modify(~{
    df <- .x
    df <- detrend_resid_plus_mean(df, "tmax_sc", "year")
    df <- detrend_resid_plus_mean(df, "anpp_g_m2", "year")
    df
  }) %>%
  ungroup()

tmin_window_dt <- tmin_window_dat %>%
  group_by(site, crop, start_lag, end_lag, scale) %>%
  group_modify(~{
    df <- .x
    df <- detrend_resid_plus_mean(df, "tmin_sc", "year")
    df <- detrend_resid_plus_mean(df, "anpp_g_m2", "year")
    df
  }) %>%
  ungroup()

# ============================================================
# 9) Window correlation/model function
# ============================================================
window_cor_model <- function(df, x_col_dt, y_col_dt = "anpp_g_m2_dt",
                             year_col = "year",
                             min_years_fit = 10,
                             group_cols = c("site", "crop", "start_lag", "end_lag", "scale")) {
  
  panels <- df %>% distinct(across(all_of(group_cols)))
  
  map_dfr(seq_len(nrow(panels)), function(i) {
    g <- panels[i, , drop = FALSE]
    
    d_cell <- df %>%
      semi_join(g, by = group_cols) %>%
      transmute(
        year = .data[[year_col]],
        x_dt = .data[[x_col_dt]],
        y_dt = .data[[y_col_dt]]
      ) %>%
      filter(is.finite(year), is.finite(x_dt), is.finite(y_dt)) %>%
      group_by(year) %>%
      summarize(
        x_dt = mean(x_dt, na.rm = TRUE),
        y_dt = mean(y_dt, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(is.finite(year), is.finite(x_dt), is.finite(y_dt))
    
    n_years <- n_distinct(d_cell$year)
    
    if (n_years < min_years_fit) {
      return(
        g %>%
          mutate(
            n_years = n_years,
            r = NA_real_,
            p = NA_real_,
            r2 = NA_real_,
            beta = NA_real_,
            p_beta = NA_real_,
            AICc = NA_real_
          )
      )
    }
    
    fit <- lm(y_dt ~ x_dt, data = d_cell)
    ct  <- cor_rp(d_cell$x_dt, d_cell$y_dt)
    
    g %>%
      mutate(
        n_years = n_years,
        r = ct$r,
        p = ct$p,
        r2 = ct$r^2,
        beta = coef(fit)[["x_dt"]],
        p_beta = summary(fit)$coefficients["x_dt", "Pr(>|t|)"],
        AICc = AICc(fit)
      )
  })
}

# ============================================================
# 10) Run matched-scale window correlations
# ============================================================
spei_win_cor <- window_cor_model(
  df = spei_window_dt,
  x_col_dt = "spei_dt",
  y_col_dt = "anpp_g_m2_dt",
  year_col = "spei_year",
  min_years_fit = min_years_fit,
  group_cols = c("site", "crop", "start_lag", "end_lag", "scale")
) %>%
  left_join(anpp_panel_n, by = c("site", "crop"))

ppt_win_cor <- window_cor_model(
  df = precip_window_dt,
  x_col_dt = "precip_sc_dt",
  y_col_dt = "anpp_g_m2_dt",
  year_col = "year",
  min_years_fit = min_years_fit,
  group_cols = c("site", "crop", "start_lag", "end_lag", "scale")
) %>%
  left_join(anpp_panel_n, by = c("site", "crop"))

tmax_win_cor <- window_cor_model(
  df = tmax_window_dt,
  x_col_dt = "tmax_sc_dt",
  y_col_dt = "anpp_g_m2_dt",
  year_col = "year",
  min_years_fit = min_years_fit,
  group_cols = c("site", "crop", "start_lag", "end_lag", "scale")
) %>%
  left_join(anpp_panel_n, by = c("site", "crop"))

tmin_win_cor <- window_cor_model(
  df = tmin_window_dt,
  x_col_dt = "tmin_sc_dt",
  y_col_dt = "anpp_g_m2_dt",
  year_col = "year",
  min_years_fit = min_years_fit,
  group_cols = c("site", "crop", "start_lag", "end_lag", "scale")
) %>%
  left_join(anpp_panel_n, by = c("site", "crop"))

best_window_cor <- function(win_df) {
  win_df %>%
    filter(is.finite(r)) %>%
    group_by(site, crop) %>%
    slice_max(abs(r), n = 1, with_ties = FALSE) %>%
    ungroup()
}

print(best_window_cor(spei_win_cor))
print(best_window_cor(ppt_win_cor))
print(best_window_cor(tmax_win_cor))
print(best_window_cor(tmin_win_cor))

# ============================================================
# 11) Ladwig-style CCP plot helpers
# ============================================================
make_ccp_table <- function(win_df, climate_name,
                           p_show = ccp_p_cut,
                           min_abs_r_show = ccp_min_abs_r_to_show) {
  win_df %>%
    mutate(
      climate = climate_name,
      abs_r = abs(r),
      direction = case_when(
        is.na(r) ~ NA_character_,
        r < 0 ~ "Negative",
        r > 0 ~ "Positive",
        TRUE ~ "Zero"
      ),
      is_significant = is.finite(r) & is.finite(p) & p < p_show & abs_r >= min_abs_r_show
    )
}

get_ccp_best <- function(ccp_df, significant_only = TRUE) {
  df <- ccp_df %>%
    filter(is.finite(r), is.finite(p))
  
  if (significant_only) {
    df <- df %>% filter(is_significant)
  }
  
  df %>%
    group_by(climate, site, crop) %>%
    slice_max(r2, n = 1, with_ties = FALSE) %>%
    ungroup()
}



plot_ccp_ladwig_style <- function(ccp_df,
                                  title_txt,
                                  plot_lags,
                                  shade_lags = ccp_shade_lags,
                                  significant_only = TRUE,
                                  r_axis_labels_each_row = TRUE) {
  
  # Local vertical scale within each site/crop row:
  # row center = r 0
  # row center + 0.45 = r +1
  # row center - 0.45 = r -1
  r_axis_half_height <- 0.45
  
  ccp_df <- ccp_df %>%
    mutate(
      climate = factor(as.character(climate), levels = climate_order)
    )
  
  panel_order <- ccp_df %>%
    distinct(site, crop, anpp_n_years) %>%
    mutate(
      crop_order_id = match(tolower(crop), tolower(crop_order)),
      crop_order_id = ifelse(is.na(crop_order_id), 999L, crop_order_id)
    ) %>%
    arrange(crop_order_id, crop, site) %>%
    mutate(
      panel_label = paste0(
        site, " | ", crop, " | ",
        format_n_label(anpp_n_years)
      ),
      panel_id = rev(seq_len(n()))
    ) %>%
    dplyr::select(-crop_order_id)
  
  df <- ccp_df %>%
    left_join(
      panel_order,
      by = c("site", "crop", "anpp_n_years")
    ) %>%
    mutate(
      r_plot = pmax(pmin(r, 1), -1),
      xmin = start_lag - 0.5,
      xmax = end_lag + 0.5,
      xmid = (xmin + xmax) / 2,
      ymid = panel_id,
      y_r = ymid + r_axis_half_height * r_plot,
      ymin = pmin(ymid, y_r),
      ymax = pmax(ymid, y_r)
    )
  
  if (significant_only) {
    df_show <- df %>% filter(is_significant)
  } else {
    df_show <- df %>% filter(is.finite(r), is.finite(p))
  }
  
  best_df <- get_ccp_best(ccp_df, significant_only = significant_only) %>%
    mutate(
      climate = factor(as.character(climate), levels = climate_order)
    ) %>%
    left_join(
      panel_order,
      by = c("site", "crop", "anpp_n_years")
    ) %>%
    mutate(
      r_plot = pmax(pmin(r, 1), -1),
      xmin = start_lag - 0.5,
      xmax = end_lag + 0.5,
      xmid = (xmin + xmax) / 2,
      ymid = panel_id,
      y_r = ymid + r_axis_half_height * r_plot,
      ymin = pmin(ymid, y_r),
      ymax = pmax(ymid, y_r),
      r2_label = paste0("R\u00B2=", sprintf("%.2f", r2)),
      r2_y = if_else(
        abs(r_plot) >= 0.20,
        (ymid + y_r) / 2,
        ymid + if_else(r_plot >= 0, 0.12, -0.12)
      )
    )
  
  if (r_axis_labels_each_row) {
    r_axis_breaks <- as.vector(
      t(sapply(panel_order$panel_id, function(z) {
        z + r_axis_half_height * c(-1, 0, 1)
      }))
    )
    r_axis_labels <- rep(c("-1", "0", "+1"), nrow(panel_order))
  } else {
    r_axis_row <- max(panel_order$panel_id)
    r_axis_breaks <- r_axis_row + r_axis_half_height * c(-1, 0, 1)
    r_axis_labels <- c("-1", "0", "+1")
  }
  
  p <- ggplot()
  
  if (!is.null(shade_lags)) {
    shade_df <- tibble(
      xmin = min(shade_lags) - 0.5,
      xmax = max(shade_lags) + 0.5,
      ymin = 0.5,
      ymax = nrow(panel_order) + 0.5
    )
    
    p <- p +
      geom_rect(
        data = shade_df,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE,
        fill = "grey85",
        color = NA,
        alpha = 0.7
      )
  }
  
  p +
    geom_hline(
      yintercept = panel_order$panel_id,
      color = "grey60",
      linewidth = 0.35
    ) +
    geom_rect(
      data = df_show,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = direction),
      color = NA,
      alpha = 0.25
    ) +
    geom_rect(
      data = best_df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = NA,
      color = "black",
      linewidth = 0.75
    ) +
    geom_text(
      data = best_df,
      aes(x = xmid, y = r2_y, label = r2_label),
      size = 2.4,
      fontface = "bold",
      color = "black"
    ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.4,
      color = "grey30"
    ) +
    facet_grid(. ~ climate, drop = FALSE) +
    scale_x_reverse(
      limits = c(max(plot_lags) + 0.5, min(plot_lags) - 0.5),
      breaks = seq(max(plot_lags), min(plot_lags), by = -1),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = panel_order$panel_id,
      labels = panel_order$panel_label,
      limits = c(0.5, nrow(panel_order) + 0.5),
      expand = c(0, 0),
      sec.axis = dup_axis(
        breaks = r_axis_breaks,
        labels = r_axis_labels,
        name = "Pearson r"
      )
    ) +
    scale_fill_manual(
      values = c(
        "Negative" = "#D73027",
        "Positive" = "#4575B4",
        "Zero" = "grey70"
      ),
      name = "Correlation"
    ) +
    labs(
      x = "Months relative to harvest (0 = harvest month)",
      y = NULL,
      title = title_txt,
      subtitle = paste0(
        "SPEI uses matched scale; Precip uses matched-window sums; Tmax/Tmin use matched-window means; ",
        "colored bars = p < ", ccp_p_cut,
        ifelse(
          ccp_min_abs_r_to_show > 0,
          paste0(" and |r| >= ", ccp_min_abs_r_to_show),
          ""
        ),
        "; bar height and direction = Pearson r; black outline = strongest R² window"
      )
    ) +
    theme_classic() +
    theme(
      panel.grid = element_blank(),
      strip.background = element_rect(fill = "white", color = "black"),
      strip.text = element_text(face = "bold"),
      axis.text.y = element_text(size = 8),
      axis.text.y.right = element_text(size = 7),
      axis.title.y.right = element_text(angle = 90),
      legend.position = "top",
      plot.title = element_text(face = "bold")
    )
}

summarize_ccp_best <- function(ccp_df, significant_only = TRUE) {
  get_ccp_best(ccp_df, significant_only = significant_only) %>%
    arrange(climate, site, crop) %>%
    dplyr::select(
      climate, site, crop,
      anpp_n_years, anpp_year_min, anpp_year_max,
      start_lag, end_lag, scale,
      n_years, r, r2, p, beta, p_beta, AICc
    )
}

# ============================================================
# 12) Build and plot CCP tables
# ============================================================
ccp_matched <- bind_rows(
  make_ccp_table(
    spei_win_cor,
    climate_name = "SPEI\nmatched scale"
  ),
  make_ccp_table(
    ppt_win_cor,
    climate_name = "Precip\nmatched scale"
  ),
  make_ccp_table(
    tmax_win_cor,
    climate_name = "Tmax\nmatched mean"
  ),
  make_ccp_table(
    tmin_win_cor,
    climate_name = "Tmin\nmatched mean"
  )
) %>%
  mutate(
    climate = factor(as.character(climate), levels = climate_order),
    
  ) %>% 
  filter(start_lag<(-12))

ccp_matched_best <- summarize_ccp_best(
  ccp_matched,
  significant_only = TRUE
)

print(ccp_matched_best)

p_ccp_matched <- plot_ccp_ladwig_style(
  ccp_matched,
  title_txt = "Critical climate periods for ANPP — matched-scale climate windows",
  plot_lags = plot_lags,
  shade_lags = ccp_shade_lags,
  significant_only = TRUE,
  r_axis_labels_each_row = TRUE
)

print(p_ccp_matched)

# ============================================================
# 13) Save outputs
# ============================================================

ggsave(
  filename = "../../Figures/ccp_matched_ladwig_style_all_climate.png",
  plot = p_ccp_matched,
  width = 18,
  height = 8,
  dpi = 300
)


# Done.