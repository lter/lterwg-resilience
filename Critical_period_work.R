# ============================================================
# Timing & Critical Period (ANPP) — CLEAN PIPELINE
#   - SPEI & Precip correlation surfaces (lag × scale)
#   - Matched-scale window search (ΔAICc) for SPEI and Precip
#
# Notes:
#   * Uses stability_anpp.csv
#   * Loads daymet_monthly_weather.csv from data/pre_processed_data/
#   * Alignment uses an absolute month index (robust for long lags like 0:-48)
#   * If sites_keep or treat_keep are NULL => keep all data
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
library(viridisLite)
library(readr)

# -----------------------------
# 1) Settings (EDIT HERE)
# -----------------------------
sites_keep  <- c('CPER','KBS','KNZ','PRHPA')   # NULL => keep all sites
treat_keep  <- NULL   # NULL => keep all treatments

plot_lags  <- 0:-23
scales_use <- 1:24

# thresholds only used for correlation-surface contour overlays
p_cut <- 0.05
r_cut <- 0.50

# bootstrap (kept commented below)
score_mode <- "combo"
B <- 300

# matched-scale window regression requirements
min_years_fit <- 9

set.seed(123)

# ============================================================
# 2) Helper functions
# ============================================================

# ---------- conditional filters ----------
maybe_filter_in <- function(df, col, keep_vec) {
  if (is.null(keep_vec)) return(df)
  dplyr::filter(df, .data[[col]] %in% keep_vec)
}

# ---------- Basic stats helpers ----------
AICc <- function(fit) {
  k <- length(coef(fit))
  n <- nobs(fit)
  aic <- AIC(fit)
  aic + (2*k*(k+1)) / (n - k - 1)
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

# ---------- Surface scoring helpers (used by bootstrap; bootstrap remains commented) ----------
neglog10p <- function(p) -log10(pmax(p, 1e-300))

score_cell <- function(r, p, mode = "combo") {
  if (mode == "neglogp") return(neglog10p(p))
  if (mode == "absr")    return(abs(r))
  abs(r) * neglog10p(p)
}

sig_rule <- function(r, p, r_cut = 0.30, p_cut = 0.05) {
  as.integer(is.finite(r) & is.finite(p) & (p < p_cut) & (abs(r) >= r_cut))
}

format_n_years_label <- function(n) {
  n <- n[is.finite(n)]
  if (length(n) == 0) return("n = NA ANPP years")
  paste0("n = ", max(n, na.rm = TRUE), " ANPP years")
}

make_panel <- function(df) {
  if ("anpp_n_years" %in% names(df)) {
    df %>%
      group_by(site, crop) %>%
      mutate(panel = paste0(site, " | ", crop, " | ", format_n_years_label(anpp_n_years))) %>%
      ungroup()
  } else {
    df %>% mutate(panel = paste(site, crop, sep = " | "))
  }
}

# ---------- Robust lag->(year,month) mapping ----------
add_target_year_month <- function(df, year_col = "year", mth_col = "hrvst_mth", lag_col = "lag_mo") {
  df %>%
    mutate(
      abs_m = .data[[year_col]] * 12L + (.data[[mth_col]] - 1L) + .data[[lag_col]],
      target_year  = abs_m %/% 12L,
      target_month = abs_m %% 12L + 1L
    ) %>%
    select(-abs_m)
}

# ---------- Plot helpers: discrete lag×scale surfaces ----------
plot_heatmap_r_discrete <- function(df, title_txt, scales_show, plot_lags) {
  df <- df %>% mutate(sig = sig_rule(r, p, r_cut, p_cut))
  
  ggplot(df, aes(x = lag_mo, y = factor(scale), group = panel)) +
    geom_tile(aes(fill = r), na.rm = TRUE) +
    geom_contour(aes(z = sig, group = panel),
                 breaks = 0.5, linetype = "dotted",
                 linewidth = 0.6, color = "black", na.rm = TRUE) +
    facet_wrap(~ panel, ncol = 2) +
    scale_x_reverse(
      limits = c(max(plot_lags) + 0.5, min(plot_lags) - 0.5),
      breaks = seq(max(plot_lags), min(plot_lags), by = -1),
      expand = c(0, 0)
    ) +
    scale_y_discrete(limits = as.character(sort(scales_show))) +
    scale_fill_gradient2(
      low  = "#1B2A6B",
      mid  = "#FDE725",
      high = "#B40426",
      midpoint = 0,
      limits = c(-1, 1),
      name = "Pearson r"
    ) +
    labs(
      x = "Months relative to harvest (0 = harvest month)",
      y = "Scale (months)",
      title = title_txt
    ) +
    theme_classic() +
    theme(panel.grid = element_blank())
}

plot_heatmap_neglogp_discrete <- function(df, title_txt, scales_show, plot_lags) {
  df <- df %>% mutate(sig = as.integer(is.finite(p) & p < p_cut),
                      p_fill = neglog10p(p))
  
  ggplot(df, aes(x = lag_mo, y = factor(scale), group = panel)) +
    geom_tile(aes(fill = p_fill), na.rm = TRUE) +
    geom_contour(aes(z = sig, group = panel),
                 breaks = 0.5, linetype = "dotted",
                 linewidth = 0.6, color = "black", na.rm = TRUE) +
    facet_wrap(~ panel, ncol = 2) +
    scale_x_reverse(
      limits = c(max(plot_lags) + 0.5, min(plot_lags) - 0.5),
      breaks = seq(max(plot_lags), min(plot_lags), by = -1),
      expand = c(0, 0)
    ) +
    scale_y_discrete(limits = as.character(sort(scales_show))) +
    scale_fill_viridis_c(name = expression(-log[10](p)), option = "C") +
    labs(
      x = "Months relative to harvest (0 = harvest month)",
      y = "Scale (months)",
      title = title_txt
    ) +
    theme_classic() +
    theme(panel.grid = element_blank())
}

# ---------- Window search helpers ----------
all_windows <- function(lags = 0:-11) {
  expand_grid(end_lag = lags, start_lag = lags) %>%
    filter(start_lag <= end_lag) %>%
    arrange(end_lag, start_lag)
}

all_windows_matched <- function(lags, scales_allowed) {
  expand_grid(end_lag = lags, start_lag = lags) %>%
    filter(start_lag <= end_lag) %>%
    mutate(scale = (end_lag - start_lag + 1L)) %>%
    filter(scale %in% scales_allowed) %>%
    arrange(end_lag, start_lag)
}

window_search_aicc <- function(df_monthly, wins, value_col, sum_or_mean = c("mean", "sum"),
                               year_col = "year", group_cols = c("site", "crop"),
                               min_years = 10) {
  sum_or_mean <- match.arg(sum_or_mean)
  groups <- df_monthly %>% distinct(across(all_of(group_cols)))
  
  map_dfr(seq_len(nrow(groups)), function(i) {
    g <- groups[i, , drop = FALSE]
    d0 <- df_monthly %>% semi_join(g, by = group_cols)
    
    res <- map_dfr(seq_len(nrow(wins)), function(j) {
      w <- wins[j, ]
      d_w <- d0 %>% filter(lag_mo >= w$start_lag, lag_mo <= w$end_lag)
      
      clim_y <- d_w %>%
        group_by(.data[[year_col]]) %>%
        summarize(
          clim_win = if (sum_or_mean == "mean") mean(.data[[value_col]], na.rm = TRUE)
          else sum(.data[[value_col]], na.rm = TRUE),
          anpp = mean(anpp_g_m2_dt, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(is.finite(clim_win), is.finite(anpp))
      
      if (nrow(clim_y) < min_years) return(tibble())
      
      fit <- lm(anpp ~ clim_win, data = clim_y)
      
      tibble(
        start_lag = w$start_lag,
        end_lag   = w$end_lag,
        n_years   = nrow(clim_y),
        AICc      = AICc(fit),
        beta      = coef(fit)[["clim_win"]],
        p_beta    = summary(fit)$coefficients["clim_win", "Pr(>|t|)"]
      )
    })
    
    if (nrow(res) == 0) return(tibble())
    res %>% mutate(site = g$site, crop = g$crop)
  })
}

window_search_aicc_matched <- function(df_grid, wins, x_col_dt, year_col,
                                       min_years_fit = 10,
                                       group_cols = c("site", "crop")) {
  
  panels <- df_grid %>% distinct(across(all_of(group_cols)))
  
  map_dfr(seq_len(nrow(panels)), function(i) {
    g <- panels[i, , drop = FALSE]
    d0 <- df_grid %>% semi_join(g, by = group_cols)
    
    map_dfr(seq_len(nrow(wins)), function(j) {
      w <- wins[j, ]
      
      d_cell <- d0 %>%
        filter(lag_mo == w$end_lag, scale == w$scale) %>%
        transmute(
          year = .data[[year_col]],
          x_dt = .data[[x_col_dt]],
          y_dt = anpp_g_m2_dt
        ) %>%
        filter(is.finite(year), is.finite(x_dt), is.finite(y_dt))
      
      n_years <- nrow(d_cell)
      
      if (n_years < min_years_fit) {
        return(tibble(
          start_lag = w$start_lag,
          end_lag   = w$end_lag,
          scale     = w$scale,
          n_years   = n_years,
          AICc      = NA_real_,
          beta      = NA_real_,
          p_beta    = NA_real_
        ) %>% bind_cols(g))
      }
      
      fit <- lm(y_dt ~ x_dt, data = d_cell)
      
      tibble(
        start_lag = w$start_lag,
        end_lag   = w$end_lag,
        scale     = w$scale,
        n_years   = n_years,
        AICc      = AICc(fit),
        beta      = coef(fit)[["x_dt"]],
        p_beta    = summary(fit)$coefficients["x_dt", "Pr(>|t|)"]
      ) %>% bind_cols(g)
    })
  })
}

plot_window_surface_deltaAICc <- function(win_df, title_txt, plot_lags) {
  win_df %>%
    group_by(site, crop) %>%
    mutate(deltaAICc = AICc - min(AICc, na.rm = TRUE)) %>%
    ungroup() %>%
    make_panel() %>%
    ggplot(aes(x = end_lag, y = start_lag, fill = deltaAICc)) +
    geom_tile() +
    facet_wrap(~ panel, ncol = 2) +
    scale_x_reverse(breaks = seq(max(plot_lags), min(plot_lags), by = -1)) +
    scale_y_reverse(breaks = seq(max(plot_lags), min(plot_lags), by = -1)) +
    scale_fill_viridis_c(name = expression(Delta*AICc), option = "C") +
    labs(x = "Window end (lag)", y = "Window start (lag)", title = title_txt) +
    theme_classic()
}

best_window <- function(win_df) {
  win_df %>% group_by(site, crop) %>% slice_min(AICc, n = 1, with_ties = FALSE) %>% ungroup()
}

# ============================================================
# 3) Import ANPP (stability dataset)
# ============================================================
anpp_dat <- read.csv("data/harmonized_data/stability_anpp.csv",
                     stringsAsFactors = FALSE)

anpp_few <- anpp_dat %>%
  { if (!is.null(sites_keep)) dplyr::filter(., site %in% sites_keep) else . } %>%
  { if (!is.null(treat_keep)) dplyr::filter(., treatment %in% treat_keep) else . } %>%
  { if (!is.null(min_years_fit)) dplyr::filter(., duration_years >= min_years_fit) else . } %>%
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
  mutate(hrvst_mth = if_else(is.na(hrvst_mth), 8L, hrvst_mth)) %>%
  drop_na(anpp_g_m2)

anpp_panel_n <- anpp_few %>%
  group_by(site, crop) %>%
  summarize(
    anpp_n_years = n_distinct(year[is.finite(year) & is.finite(anpp_g_m2)]),
    .groups = "drop"
  )

# ============================================================
# 4) SPEI: Import -> Align -> Detrend -> lag×scale correlation surfaces
# ============================================================
read_spei_long <- function(path, scale_chr) {
  x <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  
  x$date <- as.Date(substr(as.character(x$date), 1, 10),
                    tryFormats = c("%Y-%m-%d", "%m/%d/%y"))
  x$year    <- as.integer(format(x$date, "%Y"))
  x$speimth <- as.integer(format(x$date, "%m"))
  
  bad <- which(x$year >= 2024)
  if (length(bad) > 0) x$year[bad] <- x$year[bad] - 100
  
  site_cols <- setdiff(names(x), c("date", "year", "speimth"))
  
  x %>%
    pivot_longer(cols = all_of(site_cols), names_to = "site", values_to = "spei") %>%
    mutate(spei = suppressWarnings(as.numeric(spei)),
           scale = as.integer(scale_chr)) %>%
    select(site, year, speimth, scale, spei)
}

spei_paths  <- list.files("data/spei_data", pattern = "^spei\\d{2}\\.csv$", full.names = TRUE)
spei_scales <- spei_paths %>% basename() %>% str_extract("\\d{2}") %>% as.integer()

keep <- which(!is.na(spei_scales) & spei_scales %in% scales_use)
spei_paths  <- spei_paths[keep]
spei_scales <- spei_scales[keep]

spei_all <- map2_dfr(spei_paths, spei_scales, read_spei_long) %>%
  filter(year >= 1980,
         speimth >= 1, speimth <= 12,
         scale %in% scales_use) %>%
  { if (!is.null(sites_keep)) dplyr::filter(., site %in% sites_keep) else . } %>%
  arrange(site, year, speimth, scale)

spei_anpp_lag <- anpp_few %>%
  distinct(site, year, crop, hrvst_mth, anpp_g_m2) %>%
  expand_grid(lag_mo = plot_lags) %>%
  add_target_year_month(year_col = "year", mth_col = "hrvst_mth", lag_col = "lag_mo") %>%
  rename(spei_year = target_year, speimth = target_month) %>%
  left_join(
    spei_all %>% transmute(site, spei_year = year, speimth, scale, spei),
    by = c("site", "spei_year", "speimth"),
    relationship = "many-to-many"
  )

spei_anpp_lag_dt <- spei_anpp_lag %>%
  group_by(site, crop, scale, lag_mo) %>%
  group_modify(~{
    df <- .x
    df <- detrend_resid_plus_mean(df, "spei", "spei_year")
    df <- detrend_resid_plus_mean(df, "anpp_g_m2", "spei_year")
    df
  }) %>%
  ungroup()

sens_spei <- spei_anpp_lag_dt %>%
  group_by(site, crop, scale, lag_mo) %>%
  summarize(cor_rp(spei_dt, anpp_g_m2_dt), .groups = "drop") %>%
  left_join(anpp_panel_n, by = c("site", "crop")) %>%
  make_panel()

s1<-plot_heatmap_r_discrete(sens_spei, "SPEI → ANPP (r) — retained scales", scales_use, plot_lags)
print(s1)
print(plot_heatmap_neglogp_discrete(sens_spei, "SPEI → ANPP (−log10 p) — retained scales", scales_use, plot_lags))

ggsave("../../Figures/SPEI-ANPP correlations.png",
       plot = s1,
       width = 12, height = 14, units = "in")
# ============================================================
# 5) PRECIP: Load -> Align -> rolling sums -> Detrend -> lag×scale correlation surfaces
# ============================================================
daymet_monthly_weather <- read_csv("data/pre_processed_data/daymet_monthly_weather.csv")

clim_monthly <- daymet_monthly_weather %>%
  rename(site = site_id, clim_mth = month) %>%
  mutate(year = as.integer(year),
         clim_mth = as.integer(clim_mth)) %>%
  select(site, year, clim_mth, precip_mmmonth) %>%
  { if (!is.null(sites_keep)) dplyr::filter(., site %in% sites_keep) else . }

max_scale_ppt <- max(scales_use)

# Need enough months back to compute the largest scale at the earliest plotted lag
window_months_ppt <- length(plot_lags) + (max_scale_ppt - 1)

build_climate_aligned <- function(anpp_df, clim_df, window_months) {
  anpp_df %>%
    distinct(site, year, crop, hrvst_mth, anpp_g_m2) %>%
    expand_grid(lag_mo = seq(-(window_months - 1), 0, by = 1)) %>%
    add_target_year_month(year_col = "year", mth_col = "hrvst_mth", lag_col = "lag_mo") %>%
    left_join(
      clim_df %>% transmute(site, target_year = year, target_month = clim_mth, precip_mmmonth),
      by = c("site", "target_year", "target_month"),
      relationship = "many-to-many"
    ) %>%
    arrange(site, crop, year, lag_mo)
}

clim_anpp_lag <- build_climate_aligned(anpp_few, clim_monthly, window_months_ppt)

clim_anpp_scaled <- clim_anpp_lag %>%
  group_by(site, crop, year) %>%
  arrange(lag_mo) %>%
  group_modify(~{
    df <- .x
    map_dfr(scales_use, function(k) {
      df %>% mutate(
        scale = k,
        precip_sc = slide_dbl(precip_mmmonth, sum, .before = k - 1, .complete = TRUE)
      )
    })
  }) %>%
  ungroup() %>%
  filter(lag_mo %in% plot_lags)

clim_anpp_scaled_dt <- clim_anpp_scaled %>%
  group_by(site, crop, scale, lag_mo) %>%
  group_modify(~{
    df <- .x
    df <- detrend_resid_plus_mean(df, "precip_sc", "year")
    df <- detrend_resid_plus_mean(df, "anpp_g_m2", "year")
    df
  }) %>%
  ungroup()

sens_precip <- clim_anpp_scaled_dt %>%
  group_by(site, crop, scale, lag_mo) %>%
  summarize(cor_rp(precip_sc_dt, anpp_g_m2_dt), .groups = "drop") %>%
  left_join(anpp_panel_n, by = c("site", "crop")) %>%
  make_panel()

p1<-plot_heatmap_r_discrete(sens_precip, "Precip → ANPP (r) — retained scales", scales_use, plot_lags)
print(p1)

p2<-plot_heatmap_neglogp_discrete(sens_precip, "Precip → ANPP (−log10 p) — retained scales", scales_use, plot_lags)
print(p2)

ggsave("../../Figures/Precip-ANPP correlations.png",
       plot = p1,
       width = 12, height = 14, units = "in")
# ============================================================
# Monthly window search (ΔAICc) | APPROACH 1 — average or sum of monthly clims
# ============================================================
# wins_monthly <- all_windows(plot_lags)
# 
# spei_scale_for_windows <- if (1 %in% scales_use) 1 else min(scales_use)
# 
# spei_monthly_dt <- spei_anpp_lag_dt %>%
#   filter(scale == spei_scale_for_windows) %>%
#   transmute(site, crop, lag_mo,
#             year = spei_year,
#             spei_dt_forwin = spei_dt,
#             anpp_g_m2_dt)
# 
# #the below didn't make much sense, so we are removing
# # spei_win_aicc <- window_search_aicc(
# #   df_monthly = spei_monthly_dt,
# #   wins = wins_monthly,
# #   value_col = "spei_dt_forwin",
# #   sum_or_mean = "mean",
# #   year_col = "year",
# #   group_cols = c("site","crop"),
# #   min_years = 10
# # ) %>%
# #   left_join(anpp_panel_n, by = c("site", "crop"))
# 
# precip_monthly_dt <- clim_anpp_lag %>%
#   filter(lag_mo %in% plot_lags) %>%
#   group_by(site, crop, lag_mo) %>%
#   group_modify(~{
#     df <- .x
#     df <- detrend_resid_plus_mean(df, "precip_mmmonth", "year")
#     df <- detrend_resid_plus_mean(df, "anpp_g_m2", "year")
#     df
#   }) %>%
#   ungroup() %>%
#   transmute(site, crop, lag_mo, year,
#             ppt1_dt = precip_mmmonth_dt,
#             anpp_g_m2_dt)
# 
# ppt_win_aicc <- window_search_aicc(
#   df_monthly = precip_monthly_dt,
#   wins = wins_monthly,
#   value_col = "ppt1_dt",
#   sum_or_mean = "sum",
#   year_col = "year",
#   group_cols = c("site","crop"),
#   min_years = 10
# ) %>%
#   left_join(anpp_panel_n, by = c("site", "crop"))
# 
# # print(best_window(spei_win_aicc))
# print(best_window(ppt_win_aicc))
# 
# # print(plot_window_surface_deltaAICc(spei_win_aicc,
# #                                     paste0("SPEI (scale=", spei_scale_for_windows, "): Monthly window search (ΔAICc)"),
# #                                     plot_lags))
# print(plot_window_surface_deltaAICc(ppt_win_aicc,
#                                     "Precip (monthly): Monthly window search (ΔAICc)",
#                                     plot_lags))

# ============================================================
# APPROACH 2 — Matched-scale window search (ΔAICc)
# ============================================================
wins_matched <- all_windows_matched(plot_lags, scales_use)

spei_win_aicc_matched <- window_search_aicc_matched(
  df_grid = spei_anpp_lag_dt,
  wins = wins_matched,
  x_col_dt = "spei_dt",
  year_col = "spei_year",
  min_years_fit = min_years_fit,
  group_cols = c("site","crop")
) %>%
  left_join(anpp_panel_n, by = c("site", "crop"))

ppt_win_aicc_matched <- window_search_aicc_matched(
  df_grid = clim_anpp_scaled_dt,
  wins = wins_matched,
  x_col_dt = "precip_sc_dt",
  year_col = "year",
  min_years_fit = min_years_fit,
  group_cols = c("site","crop")
) %>%
  left_join(anpp_panel_n, by = c("site", "crop"))

plot_deltaAICc_surface_matched <- function(win_df, title_txt, plot_lags, panel_scale = FALSE) {
  df <- win_df %>%
    group_by(site, crop) %>%
    mutate(deltaAICc = ifelse(is.finite(AICc), AICc - min(AICc, na.rm = TRUE), NA_real_)) %>%
    ungroup()
  
  if (panel_scale) {
    df <- df %>%
      group_by(site, crop) %>%
      mutate(deltaAICc_scaled = ifelse(is.finite(deltaAICc),
                                       (deltaAICc - min(deltaAICc, na.rm=TRUE)) /
                                         (max(deltaAICc, na.rm=TRUE) - min(deltaAICc, na.rm=TRUE) + 1e-12),
                                       NA_real_)) %>%
      ungroup()
    fill_var <- "deltaAICc_scaled"
    fill_lab <- expression(Delta*AICc~"(scaled within panel)")
  } else {
    fill_var <- "deltaAICc"
    fill_lab <- expression(Delta*AICc)
  }
  
  df %>%
    make_panel() %>%
    ggplot(aes(x = end_lag, y = start_lag, fill = .data[[fill_var]])) +
    geom_tile() +
    facet_wrap(~ panel, ncol = 2) +
    scale_x_reverse(breaks = seq(max(plot_lags), min(plot_lags), by = -1)) +
    scale_y_reverse(breaks = seq(max(plot_lags), min(plot_lags), by = -1)) +
    scale_fill_viridis_c(name = fill_lab, option = "C", na.value = "white") +
    labs(x = "Window end (lag)", y = "Window start (lag)", title = title_txt) +
    theme_classic()
}

a1<-plot_deltaAICc_surface_matched(spei_win_aicc_matched, "SPEI (matched-scale): Window search (ΔAICc, raw)", plot_lags, panel_scale = FALSE)
a2<-plot_deltaAICc_surface_matched(spei_win_aicc_matched, "SPEI (matched-scale): Window search (ΔAICc, panel-scaled)", plot_lags, panel_scale = TRUE)
a3<-plot_deltaAICc_surface_matched(ppt_win_aicc_matched,  "Precip (matched-scale): Window search (ΔAICc, raw)", plot_lags, panel_scale = FALSE)
a4<-plot_deltaAICc_surface_matched(ppt_win_aicc_matched,  "Precip (matched-scale): Window search (ΔAICc, panel-scaled)", plot_lags, panel_scale = TRUE)

ggsave("../../Figures/SPEI-ANPP AICc.png",
       plot = a1,
       width = 12, height = 14, units = "in")
ggsave("../../Figures/Precip-ANPP AICc.png",
       plot = a3,
       width = 12, height = 14, units = "in")
print(best_window(spei_win_aicc_matched))
print(best_window(ppt_win_aicc_matched))

