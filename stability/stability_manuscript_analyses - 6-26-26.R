# This is a working file for the stability analysis
# Data is prepped in teh Stability_ANPP_DataPrep.R script
# This includes comparing mean, stability, PCA analysis, and looking at relationship between precipitation 
# and characteristics of the precipitation regime and ANPP

# Load necessary libraries
library(tidyverse)
library(googledrive)
library(broom)
library(corrplot)
library(multcompView)
library(purrr)
library(emmeans)
library(car)
library(lme4)
library(lmerTest)
library(performance)
library(broom.mixed)
library(knitr) 
library(stringr)
library(kableExtra)
library(glmmTMB) 
library(forcats)


theme_set(theme_bw(12))


# Create the folders necessary for data download locally
dir.create(file.path("exploratory_graphs"), showWarnings = F)
dir.create(file.path("exploratory_graphs", 'anpp_year'), showWarnings = F)
dir.create(file.path("data"), showWarnings = F)
dir.create(file.path("data", "harmonized_data"), showWarnings = F)


# READ IN THE ANPP AND PRECIPITATION DATA
file2<-'stability_anpp.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file2) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

##STEP 1: Read in the ANPP data
dat_4cat <- read.csv(file = file.path("data", "harmonized_data", file2)) 
  
##STEP 2: Read in MAP data
file3<-'site_climate_mswep.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file3) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

climatedat<- read.csv(file = file.path("data", "harmonized_data", file3)) %>% rename(site=site_id)



##STEP 2: Read in MAP data
file4<-'01_wyr_ppt_all_yrs.csv'
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/13Ymkrr-kRLDmpaj1jwwVOnOSmEYnF-dJ")) %>% 
  dplyr::filter(name == file4) %>% 
  googledrive::drive_download(file = .$id, overwrite = T,
                              path = file.path("data", "harmonized_data", .$name))

ann.climatedat<- read.csv(file = file.path("data", "harmonized_data", file4)) %>% rename(site=site_id, year = w_yr)


##########################################################################
# Site x year anpp ppt model (like Hajek et al 2025)
##########################################################################

dat1.1<-dat_4cat |> ##this used to have join with ann.climate, but it lookes like the data is already in there.
  mutate(type2 = factor(type2))%>%
  filter(stab.analysis == 1)#sites must have 5 or more years of data,

plot(dat1.1$wyr_ppt.x, dat1.1$wyr_ppt.y)

#normality tests
transforms <- list(
  raw = dat1.1$anpp_g_m2,
  log = log(dat1.1$anpp_g_m2),
  sqrt = sqrt(dat1.1$anpp_g_m2),
  cube = (dat1.1$anpp_g_m2)^(1/3)
)

lapply(transforms, function(x) {
  m <- aov(x ~ type2, data = dat1.1)
  shapiro.test(residuals(m))
})

#selected sqrt transformation

#Fit LMM with log-transformed ANPP (after comparing shapiro test, diagnostic plots)
m_sqrt2 <- lmer(sqrt(anpp_g_m2) ~ wyr_ppt.y * type2 + (1 | site),
           data = dat1.1)
m_log2 <- lmer(log(anpp_g_m2) ~ wyr_ppt.y * type2 + (1 | site),
               data = dat1.1)
m_raw <- lmer(anpp_g_m2 ~ wyr_ppt.y * type2 + (1 | site),
                data = dat1.1)

# Basic diagnostic plots comparing log and sqrt transformation 
par(mfrow = c(2, 2))  # 2x2 plotting layout
plot(m_sqrt2)
plot(m_log2)

par(mfrow = c(2, 2))  # 2x2 plotting layout


# Q-Q plots of residuals
qqnorm(resid(m_sqrt2)); 
qqline(resid(m_sqrt2))
qqnorm(resid(m_log2));  
qqline(resid(m_log2))

#transformation decision - log is better - fitted vs residuals - sqrt has clear funnel suggesting variance increases with the mean, errors are 
#heteroscedastic - mean-variance coupling; log model residuals a constant band. QQ plots also favor log transformation - sqrt has an s-shape with heavy tails, 
#log closer fit to line with some deviation in the lower tail

m_log <- lmer(log(anpp_g_m2) ~ wyr_ppt.y * type2 + (1 | site),
               data = dat1.1)
summary(m_log)
car::Anova(m_log, type = 3)

emtrends(m_log, ~ type2, var = "wyr_ppt.y")
pairs(emtrends(m_log, ~ type2, var = "wyr_ppt.y"))  # test differences in slopes


# Table 1: Fixed effects (tidy)
tab1 <- broom.mixed::tidy(m_log, conf.int = FALSE) %>%
  filter(effect == "fixed") %>%
  transmute(
    Term = term,
    Estimate = estimate,
    SE = std.error,
    df = df,
    `t value` = statistic,
    `p-value` = p.value
  )

write.csv(tab1, "C:/Users/david.hoover/OneDrive - USDA/HomeDrive/Manuscripts/NCEAS - Stability/output/anpp vs ppt/fixed_effects_tab_6-18-25.csv", row.names = F)
# Table 2: Type III ANOVA
tab2 <- car::Anova(m_log, type = 3) %>% as.data.frame() %>%
  tibble::rownames_to_column("Effect") %>%
  rename(Chi2 = Chisq, df = Df, `p-value` = `Pr(>Chisq)`)
write.csv(tab2, "C:/Users/david.hoover/OneDrive - USDA/HomeDrive/Manuscripts/NCEAS - Stability/output/anpp vs ppt/anova_tab_6-18-25.csv", row.names = F)


# Table 3:pairwise comparison

# Convert to a clean data frame

# Make sure emmeans uses your preferred df method
emm_options(lmer.df = "satterthwaite")

# Get trends (slopes) by type
emtr_log <- emtrends(m_log, ~ type2, var = "wyr_ppt.y")

# Pairwise differences in slopes (Tukey-adjusted)
pair_tr <- pairs(emtr_log, adjust = "tukey")

pair_df <- as.data.frame(pair_tr)

write.csv(pair_df, "C:/Users/david.hoover/OneDrive - USDA/HomeDrive/Manuscripts/NCEAS - Stability/output/anpp vs ppt/pairwise_tab_6-18-25.csv", row.names = F)

#could try a Gamma GLMM with a log link like this:
#glmmTMB(anpp_g_m2 ~ precip * type2 + (1 | site),
#        family = Gamma(link = "log"))


#plot

ggplot(dat1.1, aes(x = wyr_ppt.y, y = anpp_g_m2, color = type2)) +
  geom_point(alpha = 0.65, size = 1.8) +
  #geom_smooth(aes(group=site, color='black'), method = "lm", se=F, linewidth=0.01)+
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 1) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = "Annual precipitation (mm)",
    y = expression(paste("ANPP ", "(g m"^{-2}, ")")),
    color = "type2"
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )+
  scale_y_log10()

ggplot(subset(dat1.1, site=='sava.us'), aes(x = wyr_ppt.y, y = anpp_g_m2, color = type2)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(aes(group=site), method = "lm", se=F, linewidth=0.01)+
  #geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 1) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = "Annual precipitation (mm)",
    y = expression(paste("ANPP ", "(g m"^{-2}, ")")),
    color = "type2"
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )+
  scale_y_log10()+
  facet_wrap(~type2)


####pull out slopes
slopes<-dat1.1 |> 
  group_by(site, type, type2) |> 
  summarise(b=lm(anpp_g_m2~wyr_ppt.x))

##########################################################################
# Main effects of type  on manpp, sd, stability
##########################################################################
dat4.1<-dat_4cat%>% #using raw data, not detrended
  filter(stab.analysis == 1)%>% #sites must have 5 or more years of data, might need to up to 15 based on Doring 2018 paper?
  group_by(network, site, type, type2)%>% #need to average over type to make independent calcs of croplands with different types (e.g. corn, soy, wheat)
  # dropping anpp pulse summarize(nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2), anpp_pulse = (max(anpp_g_m2)-mean(anpp_g_m2))/mean(anpp_g_m2))
  summarize(nobs=n(), manpp=mean(anpp_g_m2), sd=sd(anpp_g_m2))


###test analysis with Grace's data
dummydata<-data.frame(
  network=c('dummy', 'dummy'),
  site=c('dryland', 'dryland'),
  type=c('Corn', 'Wheat'),
  type2=c('Cropland', 'Cropland'),
  nobs=c(24, 24),
  manpp=c(571.08, 587.829),
  sd=c(249.58, 139.8712))

dat4.1<-dat4.1 |> 
  bind_rows(dummydata)

#calc some stability metrics - taking mean across site (not Ingrids approach, which calcs across all sites within a type)
dat4.2<-dat4.1%>%
  mutate(cv = sd/manpp)%>%
  mutate(stab = 1/cv)

means.1<-dat4.2%>%
  select(-nobs)%>%
  pivot_longer(cols = c(manpp:stab), names_to = "var", values_to = "val")%>%
  group_by(type2, var)%>%
  summarise(nobs=n(), 
            mean=mean(val), 
            sd=sd(val),
            se = sd/sqrt(nobs))%>%
  #filter(var %in% c("manpp", "sd", "stab", "anpp_pulse" ))
  filter(var %in% c("manpp", "sd", "stab"))


#################
# Anova's on raw data (not detrended)

test_normality <- function(x) {
  list(
    raw = shapiro.test(x),
    log = shapiro.test(log(x)),
    sqrt = shapiro.test(sqrt(x)),
    cube = shapiro.test(x)^(1/3)
  )
}

#anpp###########
transforms <- list(
  raw = dat4.2$manpp,
  log = log(dat4.2$manpp),
  sqrt = sqrt(dat4.2$manpp),
  cube = (dat4.2$manpp)^(1/3)
)

lapply(transforms, function(x) {
  m <- aov(x ~ type2, data = dat4.2) #might want to update with the final model
  shapiro.test(residuals(m))
})

#Normality tests - $sqrt and $cube had w >0.95 and pvalue >0.05
#using visual assessment to determine which model is best

#log <-- the plots were pretty close but liked the q-q residuals better on this so using this model
#added site as a random intercept (58 sites) to account for baseline differences across sites
#weighted by nobs (years per site x type2 ) to weight sites with longer data sets

m_mean_log <- lmer(log(manpp) ~ type2 + (1 | site),
                   data = dat4.2, weights = nobs) 
summary(m_mean_log)
emm_mean_log <- emmeans(m_mean_log, ~ type2)

pairs_mean_log <- pairs(emm_mean_log, adjust = "tukey")    
pairs_mean_log_df <- as.data.frame(pairs_mean_log)



#sd###########
transforms <- list(
  raw = dat4.2$sd,
  log = log(dat4.2$sd),
  sqrt = sqrt(dat4.2$sd)
)

lapply(transforms, function(x) {
  m <- aov(x ~ type2, data = dat4.2)
  shapiro.test(residuals(m))
})

#Normality tests - $log and $cube had w >0.95 and pvalue >0.05
#using visual assessment to determine which model is best

#log <-- the plots were pretty close but liked the q-q residuals better on this so using this model
m_sd_log <- lmer(log(sd) ~ type2 + (1 | site),
                   data = dat4.2, weights = nobs) 
summary(m_sd_log)
emm_sd_log <- emmeans(m_sd_log, ~ type2)

pairs_sd_log <- pairs(emm_sd_log, adjust = "tukey")    
pairs_sd_log_df <- as.data.frame(pairs_sd_log)



#stab###########
transforms <- list(
  raw = dat4.2$stab,
  log = log(dat4.2$stab),
  sqrt = sqrt(dat4.2$stab)
)

lapply(transforms, function(x) {
  m <- aov(x ~ type2, data = dat4.2)
  shapiro.test(residuals(m))
})

#Normality tests - $log and $cube had w >0.95 and pvalue >0.05
#using visual assessment to determine which model is best

#log <-- the plots were pretty close but liked the q-q residuals better on this so using this model

m_stab_log <- lmer(log(stab) ~ type2 + (1 | site),
                 data = dat4.2, weights = nobs) 
summary(m_stab_log)
emm_stab_log <- emmeans(m_stab_log, ~ type2)

pairs_stab_log <- pairs(emm_stab_log, adjust = "tukey")    
pairs_stab_log_df <- as.data.frame(pairs_stab_log)

####bar graph of manpp, sd, stab by type ###########################<-check code, some of the output functions below can be dropped


# Output directory
out_dir <- "C:/Users/david.hoover/OneDrive - USDA/HomeDrive/Manuscripts/NCEAS - Stability/output/anpp sd stab by type"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Ensure correct type order; keep var as-is ("manpp", "sd", "stab")
means_plot <- means.1 %>%
  mutate(
    type2 = fct_relevel(type2, "Cropland", "Fert. Grassland", "Grassland"),
    var   = factor(var, levels = c("manpp", "sd", "stab"))  # ensures facet order
  )

# ------------------------------
# Helper function: mean ± SE bar plot
# ------------------------------
plot_bar_single <- function(df, ylabel, title, outfile) {
  
  p <- ggplot(df, aes(x = type2, y = mean, fill = type2)) +
    geom_col(width = 0.7, color = "grey20") +
    geom_errorbar(aes(ymin = pmax(mean - se, 0), ymax = mean + se),
                  width = 0.15, size = 0.9) +
    scale_fill_brewer(palette = "Dark2", guide = "none") +
    labs(x = NULL, y = ylabel, title = title) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 11),
      plot.title = element_text(face = "bold")
    )
  
  ggsave(outfile, p, width = 6.5, height = 4.5, dpi = 300)
  p
}

# ------------------------------
# Individual bar plots
# ------------------------------

# 1. manpp
p_manpp <- plot_bar_single(
  means_plot %>% filter(var == "manpp"),
  ylabel = "Mean ANPP (arithmetic mean ± SE, g m⁻²)",
  title  = "Mean ANPP by Vegetation Type",
  outfile = file.path(out_dir, "bar_mean_ANPP_manpp.png")
)

# 2. sd
p_sd <- plot_bar_single(
  means_plot %>% filter(var == "sd"),
  ylabel = "Interannual SD of ANPP (arithmetic mean ± SE, g m⁻²)",
  title  = "Interannual SD by Vegetation Type",
  outfile = file.path(out_dir, "bar_SD_ANPP_sd.png")
)

# 3. stab
p_stab <- plot_bar_single(
  means_plot %>% filter(var == "stab"),
  ylabel = "Stability (mean/SD; arithmetic mean ± SE)",
  title  = "Stability by Vegetation Type",
  outfile = file.path(out_dir, "bar_stability_stab.png")
)

# ------------------------------
# Faceted figure (order: manpp -> sd -> stab)
# ------------------------------

p_faceted <- ggplot(means_plot, aes(x = type2, y = mean, fill = type2)) +
  geom_col(width = 0.7, color = "grey20") +
  geom_errorbar(aes(ymin = pmax(mean - se, 0), ymax = mean + se),
                width = 0.15, size = 0.9) +
  scale_fill_brewer(palette = "Dark2", guide = "none") +
  facet_wrap(~ var, scales = "free_y", nrow = 1) +  # uses var instead of metric
  labs(x = NULL, y = "Mean ± SE (original scale)") +
  theme_bw(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = file.path(out_dir, "bar_faceted_manpp_sd_stab.png"),
  plot = p_faceted, width = 11, height = 4.2, dpi = 300
)

# Optional: save the tidy table
write_csv(means_plot, file.path(out_dir, "means1_for_bargraphs_original_scale.csv"))

# Show in RStudio viewer
p_manpp; p_sd; p_stab; p_faceted

# Output pairwise comparisons 
write_csv(pairs_stab_log_df, file.path(out_dir, "pairwise_stab_log.csv"))



# Helper: produce letters from a model's emmeans pairwise comparisons
letters_from_model <- function(model, metric_label) {
  # EMMs by type
  emm <- emmeans(model, ~ type2)
  
  # Tukey-adjusted pairwise comparisons
  prs <- pairs(emm, adjust = "tukey")
  df  <- as.data.frame(prs)
  
  # Build a named p-value vector of the form "A-B" = pval
  # Ensure consistent ordering of type names in labels
  p_named <- df$p.value
  names(p_named) <- paste(df$contrast)  # contrast already "Cropland - Grassland", etc.
  
  # Convert to letters (alpha = 0.05). multcompLetters expects lower = better (significant)
  # It parses names like "A-B"; we also trim spaces to be safe
  names(p_named) <- str_replace_all(names(p_named), " ", "")
  cld <- multcompView::multcompLetters(p_named, threshold = 0.05)
  
  # Return a tibble: metric, type2, letters
  tibble(
    metric  = metric_label,
    type2   = names(cld$Letters),
    letters = cld$Letters
  ) %>%
    # order type2 consistently
    mutate(type2 = forcats::fct_relevel(type2, "Cropland", "Fert.Grassland", "Grassland")) %>%
    arrange(type2)
}

# NOTE: Because we removed spaces in contrast names above (e.g., "Fert. Grassland" -> "Fert.Grassland"),
# we relevel with the same no-space label in the helper. To keep original labels in the final table,
# map back to the spaced version at the end.

# Build tables for each metric
letters_mean <- letters_from_model(m_mean_log,  "manpp")      # Mean ANPP
letters_sd   <- letters_from_model(m_sd_log,    "sd")         # Interannual SD
letters_stab <- letters_from_model(m_stab_log,  "stab")       # Stability

# Combine and restore original spacing in type names for readability
letters_all <- bind_rows(letters_mean, letters_sd, letters_stab) %>%
  mutate(type2 = forcats::fct_recode(
    type2,
    "Cropland" = "Cropland",
    "Fert. Grassland" = "Fert.Grassland",
    "Grassland" = "Grassland"
  )) %>%
  arrange(match(metric, c("manpp","sd","stab")), type2) %>%
  mutate(type2 = as.character(type2))  # plain strings in output

# View in console
print(letters_all)

# Save to CSV
readr::write_csv(letters_all, file.path(out_dir, "significance_letters_type2_manpp_sd_stab.csv"))

# ----------------------------------------------------------
# Combine fixed effects & Type III ANOVA across:
#  - m_mean_log  (log(manpp)  ~ type2 + (1 | site))
#  - m_sd_log    (log(sd)     ~ type2 + (1 | site))
#  - m_stab_log  (log(stab)   ~ type2 + (1 | site))
# Writes combined CSVs to OneDrive folder
# ----------------------------------------------------------

# Helper: fixed effects (log-scale) + response-scale multipliers
extract_fixed <- function(model, metric_label) {
  broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE) %>%
    mutate(
      metric       = metric_label,
      exp_estimate = exp(estimate),
      exp_conf.low = exp(conf.low),
      exp_conf.high= exp(conf.high)
    ) %>%
    select(metric, term, estimate, std.error, df, statistic, p.value,
           conf.low, conf.high, exp_estimate, exp_conf.low, exp_conf.high)
}

# Helper: Type III ANOVA (Wald χ²) for a model
extract_anova <- function(model, metric_label) {
  car::Anova(model, type = 3) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Effect") %>%
    rename(Chisq = Chisq, Df = Df, p.value = `Pr(>Chisq)`) %>%
    mutate(metric = metric_label) %>%
    select(metric, Effect, Chisq, Df, p.value)
}

# (Optional) Helper: random effects variance components
extract_random <- function(model, metric_label) {
  broom.mixed::tidy(model, effects = "ran_pars") %>%
    mutate(metric = metric_label) %>%
    select(metric, group, term, estimate)
}

# Build combined tables
fixed_all <- bind_rows(
  extract_fixed(m_mean_log,  "manpp"),
  extract_fixed(m_sd_log,    "sd"),
  extract_fixed(m_stab_log,  "stab")
)

anova_all <- bind_rows(
  extract_anova(m_mean_log,  "manpp"),
  extract_anova(m_sd_log,    "sd"),
  extract_anova(m_stab_log,  "stab")
)

random_all <- bind_rows(  # optional
  extract_random(m_mean_log,  "manpp"),
  extract_random(m_sd_log,    "sd"),
  extract_random(m_stab_log,  "stab")
)

# Write combined CSVs
readr::write_csv(fixed_all,  file.path(out_dir, "combined_site_type_fixed_effects.csv"))
readr::write_csv(anova_all,  file.path(out_dir, "combined_site_type_typeIII_anova.csv"))
readr::write_csv(random_all, file.path(out_dir, "combined_site_type_random_effects.csv"))




##########################################################################
# Effects of ppt on manpp, sd, stability
##########################################################################
#
#limiting range of MAP to fit croplands - didn't make a huge difference but worth considering

#rather than using site MAP across all years, using just years for ANPP
ppt_by_group <- dat_4cat %>%
  filter(stab.analysis == 1) %>%
  group_by(network, site, type, type2) %>%
  summarise(
    n_years   = n(),
    ppt_mean  = mean(wyr_ppt, na.rm = TRUE),
    ppt_sd    = sd(wyr_ppt,   na.rm = TRUE),
    ppt_min   = min(wyr_ppt,  na.rm = TRUE),
    ppt_max   = max(wyr_ppt,  na.rm = TRUE),
    .groups   = "drop"
  )

dat4.2b<-left_join(dat4.2, ppt_by_group, by = c("network", "site", "type", "type2"))%>%
  mutate(type2 = factor(type2))


### run models between precip and production ###

#using same transformations as anova

m_mean_ppt <- lmer(log(manpp) ~ ppt_mean * type2 + (1 | site),
                   data = dat4.2b, weights = nobs)
summary(m_mean_ppt)
car::Anova(m_mean_ppt, type = 3)

m_mean_ppt2 <- lm(log(manpp) ~ ppt_mean*type2,
                   data = dat4.2b, weights = nobs)
summary(m_mean_ppt2)
car::Anova(m_mean_ppt2, type = 3)

m_mean_ppt3 <- lmer(log(manpp) ~ ppt_mean * type2 + (1 | site),
                   data = subset(dat4.2b, type2!='Cropland'), weights = nobs)

summary(m_mean_ppt3)
car::Anova(m_mean_ppt3, type = 3)

m_sd_ppt <- lmer(log(sd) ~ ppt_mean * type2 + (1 | site),
                   data = dat4.2b, weights = nobs)
summary(m_sd_ppt)
car::Anova(m_sd_ppt, type = 3)


m_stab_ppt <- lmer(log(stab) ~ ppt_mean * type2 + (1 | site),
                   data = dat4.2b, weights = nobs)
summary(m_stab_ppt)
car::Anova(m_stab_ppt, type = 3)

m_stab_ppt2 <- lm(log(stab) ~ ppt_mean * type2,
                   data = subset(dat4.2b, type2!='Cropland'), weights = nobs)
summary(m_stab_ppt2)
car::Anova(m_stab_ppt2, type = 3)


#make dat long below
ggplot(data=dat_long, aes(x=ppt_mean, y=value))+
  geom_point()+
  geom_smooth(method = 'lm')+
  facet_grid(metric~type2, scales='free')

dat_long_stats<-dat_long %>% 
  group_by(metric, type2) %>% 
  summarise(p=summary(lm(log(value)~ppt_mean, weights = nobs))$coefficients['ppt_mean', "Pr(>|t|)"]) %>% 
  mutate(pajd=p.adjust(p, method = 'bonferroni', n=3))



# ============================
# 3-panel figure: manpp, sd, stab vs ppt_mean across type2
# ============================

# ===========================================
# 3-panel figure: manpp, sd, stab vs ppt_mean
# Linear (original) y-axis; predictions back-transformed
# ===========================================

# Output directory
out_dir <- "C:/Users/david.hoover/OneDrive - USDA/HomeDrive/Manuscripts/NCEAS - Stability/output/stab metrics vs ppt"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Keep df method consistent with your workflow
emm_options(lmer.df = "satterthwaite")

# 1) Long-format points (original scale)
dat_long <- dat4.2b %>%
  ungroup() %>%  # avoid grouped-column warnings
  mutate(type2 = fct_relevel(factor(type2),
                             "Cropland", "Fert. Grassland", "Grassland")) %>%
  select(site, type2, ppt_mean, nobs, manpp, sd, stab) %>%
  pivot_longer(cols = c(manpp, sd, stab),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(metric, levels = c("manpp", "sd", "stab")))

# 2) Helper: predictions (back-transform exp to original scale)
pred_grid_for_model <- function(model, metric_label, x_vals, type_levels) {
  pred <- emmeans(model, specs = ~ ppt_mean | type2,
                  at = list(ppt_mean = x_vals))
  as.data.frame(pred) %>%
    mutate(
      metric = metric_label,
      y_hat  = exp(emmean),    # back-transform from log
      y_lo   = exp(lower.CL),
      y_hi   = exp(upper.CL),
      type2  = fct_relevel(type2, !!!type_levels)
    )
}

# 3) Common x-grid over observed precipitation
x_seq <- seq(min(dat4.2b$ppt_mean, na.rm = TRUE),
             max(dat4.2b$ppt_mean, na.rm = TRUE),
             length.out = 200)
type_levels <- c("Cropland", "Fert. Grassland", "Grassland")

# 4) Build predictions for all three metrics
pred_mean <- pred_grid_for_model(m_mean_ppt, "manpp", x_seq, type_levels)
pred_sd   <- pred_grid_for_model(m_sd_ppt,   "sd",    x_seq, type_levels)
pred_stab <- pred_grid_for_model(m_stab_ppt, "stab",  x_seq, type_levels)

pred_all <- bind_rows(pred_mean, pred_sd, pred_stab) %>%
  mutate(metric = factor(metric, levels = c("manpp","sd","stab")))

# 5) Plot: points (original scale) + ribbons + lines; linear y-axis
p_linear <- ggplot() +
  geom_point(data = dat_long,
             aes(x = ppt_mean, y = value, color = type2),
             alpha = 0.6, size = 2) +
  geom_ribbon(data = pred_all,
              aes(x = ppt_mean, ymin = y_lo, ymax = y_hi, fill = type2),
              alpha = 0.15, color = NA) +
  geom_line(data = pred_all,
            aes(x = ppt_mean, y = y_hat, color = type2),
            linewidth = 1.2) +
  scale_color_brewer(palette = "Dark2", name = "Type") +
  scale_fill_brewer(palette = "Dark2", name = "Type") +
  facet_wrap(~ metric, scales = "free_y", nrow = 1) +
  labs(x = "Mean water-year precipitation (mm)",
       y = "Metric value (original scale)",
       title = "Precipitation vs ANPP, SD, and Stability by vegetation type") +
  theme_bw(base_size = 12) +
  theme(strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank())

# Save figure (linear y-axis)
ggsave(filename = file.path(out_dir, "ppt_vs_manpp_sd_stab_by_type_faceted_LINEAR.png"),
       plot = p_linear, width = 11, height = 4.2, dpi = 300)

# (Optional) Also save a companion figure with a log y-axis (straight lines visually)
p_logaxis <- p_linear + scale_y_log10() + labs(y = "Metric value (log scale)")
ggsave(file.path(out_dir, "ppt_vs_manpp_sd_stab_by_type_faceted_LOGAXIS.png"),
       p_logaxis, width = 11, height = 4.2, dpi = 300)


# ============================
# Combined tables: fixed effects + Type III ANOVA + random effects
# (across m_mean_ppt, m_sd_ppt, m_stab_ppt)
# ============================

# Output directory
out_dir <- "C:/Users/david.hoover/OneDrive - USDA/HomeDrive/Manuscripts/NCEAS - Stability/output/stab metrics vs ppt"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Helpers
extract_fixed <- function(model, metric_label) {
  broom.mixed::tidy(model, effects = "fixed", conf.int = TRUE) %>%
    mutate(
      metric        = metric_label,
      exp_estimate  = exp(estimate),
      exp_conf.low  = exp(conf.low),
      exp_conf.high = exp(conf.high)
    ) %>%
    select(metric, term, estimate, std.error, df, statistic, p.value,
           conf.low, conf.high, exp_estimate, exp_conf.low, exp_conf.high)
}

extract_anova <- function(model, metric_label) {
  car::Anova(model, type = 3) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Effect") %>%
    rename(Chisq = Chisq, Df = Df, p.value = `Pr(>Chisq)`) %>%
    mutate(metric = metric_label) %>%
    select(metric, Effect, Chisq, Df, p.value)
}

extract_random <- function(model, metric_label) {
  broom.mixed::tidy(model, effects = "ran_pars") %>%
    mutate(metric = metric_label) %>%
    select(metric, group, term, estimate)
}

# Build combined tables from your three ppt models
fixed_all <- bind_rows(
  extract_fixed(m_mean_ppt,  "manpp"),
  extract_fixed(m_sd_ppt,    "sd"),
  extract_fixed(m_stab_ppt,  "stab")
)

anova_all <- bind_rows(
  extract_anova(m_mean_ppt,  "manpp"),
  extract_anova(m_sd_ppt,    "sd"),
  extract_anova(m_stab_ppt,  "stab")
)

random_all <- bind_rows(
  extract_random(m_mean_ppt,  "manpp"),
  extract_random(m_sd_ppt,    "sd"),
  extract_random(m_stab_ppt,  "stab")
)

# Write combined CSVs
readr::write_csv(fixed_all,  file.path(out_dir, "combined_fixed_effects_ppt_models.csv"))
readr::write_csv(anova_all,  file.path(out_dir, "combined_typeIII_anova_ppt_models.csv"))
readr::write_csv(random_all, file.path(out_dir, "combined_random_effects_ppt_models.csv"))

##########################################################################
# taylor law testing
##########################################################################

# Prepare log-mean and log-variance
# dat4.2b <- dat4.2b %>%
#   mutate(
#     log_mean = log(manpp),
#     log_var  = log(sd^2),
#     w_var    = 2 * (nobs - 1) / (sd^2)   #precision weight for variance, adds higher weight to sites with more years/lower variance (2 is a scaling constant)
#   )
# 
# # Mixed model for Taylor’s law with type-specific slopes
# m_taylor <- lmer(log_var ~ log_mean * type2 + (1 | site),
#                  data = dat4.2b,
#                  weights = w_var)  # or weights = nobs
# 
# summary(m_taylor)
# car::Anova(m_taylor, type = 3)  # tests for slope differences (interaction)

##take 2 trying mean var scaling following Doering et al. 2018 CVa

#things we still have to do.
##step one add is there a relationship
#run model if sig interaction between type2 and mean anpp and stability. diff linear models.

dat5<-dat4.2%>%
  mutate(mi = log10(manpp))%>%
  mutate(var = sd^2)%>% 
  mutate(vi = log10(var))
##Combining all types to calc aCV#####


#getting overall slopes and intercepts not for different type2
ggplot(dat5, aes(mi, vi))+
  geom_point(aes(color=type2))+
  geom_smooth(method='lm')+
  geom_abline(slope=2)+
  xlab('Log(Mean ANPP)')+
  ylab('Log(sd^2 ANPP)')

mod.1<-lm(vi ~ mi, data = dat5)
summary(mod.1)
coef(mod.1)
b<-coef(mod.1)["mi"]
a<-coef(mod.1)["(Intercept)"]
a2<-a + (b-2)*mean(dat5$mi, na.rm=T)

#calculate adjusted coefficent of variaion based on Doring and Recking 2018
dat6<-dat5%>%
  mutate(ui = vi - (a +b*mi), #calculate residuals from regression line (Power Law Residuals, POLAR) 
 vi2 = a2 + 2*mi + ui, #adjusting variance based on slope of 2
 aCV = (sqrt(10^vi2)/manpp), #creating adjusted CV
 astab=(1/aCV)) #calculating adjusted stability
 

ggplot(dat6, aes(cv, aCV))+
  geom_point(aes(color=type2))+
  geom_smooth(method='lm')

data6.1<-dat6%>%
  pivot_longer(cols = c(astab, stab), names_to = "vartype", values_to = "var2")%>%
  group_by(type2, vartype)%>%
  summarise(stab = mean(var2),
            sestab = sd(var2)/sqrt(n()))

data6.1$vartype<-factor(data6.1$vartype, levels = c("stab", "astab"))

ggplot(data6.1, aes(x = type2, y = stab, fill = vartype)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = stab - sestab, ymax = stab + sestab),
                width = 0.2, position = position_dodge(width = 0.8)) +
  theme_minimal()

#correction does change pairwise comparisions
TukeyHSD(aov(stab~type2, data=dat6))
TukeyHSD(aov(astab~type2, data=dat6))

####redoing above but doing seperate correction for each type2
types<-as.data.frame(unique(dat5$type2))
colnames(types)[1]<-"type2"
l=length(types$type2)
acv_bytype.1<-data.frame()

for (i in 1:l){
  #i=1
  t1<-types[i,]
  d1<-filter(dat5, type2 == t1)
  mod.2<-lm(vi ~ mi, data = d1)
  summary(mod.2)
  coef(mod.2)
  bx<-coef(mod.2)["mi"]
  ax<-coef(mod.2)["(Intercept)"]
  
  #calculate adjusted coefficent of variaion based on Doring and Recking 2018
  d1<-d1%>%
    mutate(ui = vi - (ax +bx*mi)) #calculate residuals from regression line (Power Law Residuals, POLAR) 
  
  a2x<-ax + (bx-2)*mean(d1$mi, na.rm=T)
  
  d1<-d1%>%
    mutate(vi2 = a2x + 2*mi + ui)%>%
    mutate(aCV = (sqrt(10^vi2)/manpp))%>%
    mutate(astab=1/aCV)
  acv_bytype.1<-rbind(acv_bytype.1, d1)
  
}

ggplot(acv_bytype.1, aes(cv, aCV, color = type2))+
  geom_point()+
  geom_smooth(method='lm')


acv_bytype.2<-acv_bytype.1%>%
  pivot_longer(cols = c(astab, stab), names_to = "vartype", values_to = "var2")%>%
  group_by(type2, vartype)%>%
  summarise(stab = mean(var2),
            sestab = sd(var2)/sqrt(n()))

acv_bytype.2$vartype<-factor(acv_bytype.2$vartype, levels = c("stab", "astab"))

ggplot(acv_bytype.2, aes(x = type2, y = stab, fill = vartype)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = stab - sestab, ymax = stab + sestab),
                width = 0.2, position = position_dodge(width = 0.8)) +
  theme_minimal()

#correction does not change pairwise comparisons
TukeyHSD(aov(astab~type2, data=acv_bytype.1))
TukeyHSD(aov(stab~type2, data=acv_bytype.1))

m<-(lm(vi~mi*type2, data=dat6))
m2<-emtrends(m, spec='type2',var="mi")
pairs(m2)


#graph#####################################################


# Ungroup and normalize labels; ensure type2 is a factor with clean levels
dat4.2b <- dat4.2b %>%
  ungroup() %>%
  mutate(
    type2 = fct_drop(fct_inorder(trimws(as.character(type2))))
  )

# 1) Build an evenly spaced sequence of log_mean over the observed range
x_seq <- seq(min(dat4.2b$log_mean, na.rm = TRUE),
             max(dat4.2b$log_mean, na.rm = TRUE),
             length.out = 200)

# 2) Get model-based predictions from emmeans (on the linear predictor = log variance scale)
#    This avoids emmip() internals that caused the factor-level duplication
pred <- emmeans(m_taylor, specs = ~ log_mean | type2, at = list(log_mean = x_seq))
pred_df <- as.data.frame(pred)  # columns: type2, log_mean, emmean, SE, lower.CL, upper.CL

# 3) Plot observed points + fitted lines + CI ribbons
ggplot(dat4.2b, aes(x = log_mean, y = log_var, color = type2)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_ribbon(data = pred_df,
              aes(x = log_mean, ymin = lower.CL, ymax = upper.CL, fill = type2),
              alpha = 0.15, color = NA, inherit.aes = FALSE) +
  geom_line(data = pred_df,
            aes(x = log_mean, y = emmean, color = type2),
            size = 1.2, inherit.aes = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "log(Mean ANPP)", y = "log(Variance of ANPP)",
       color = "Type", fill = "Type",
       title = "Taylor’s law: log(Variance) vs log(Mean), by type") +
  theme_bw() +
  theme(legend.position = "right")

#show taylor exponents

# Use Satterthwaite df (more stable than KR for sparse designs)
emm_options(lmer.df = "satterthwaite")

emtr_taylor <- emtrends(m_taylor, ~ type2, var = "log_mean")  # slopes (β) by type
emtr_df <- as.data.frame(summary(emtr_taylor)) %>%
  rename(beta_hat = log_mean.trend)

ggplot(emtr_df, aes(x = type2, y = beta_hat, color = type2)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15, size = 1) +
  geom_hline(yintercept = 2, linetype = "dashed", color = "grey40") +
  annotate("text", x = 0.6, y = 2.03, label = "β = 2 (constant CV)", hjust = 0, size = 3) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = NULL, y = expression(hat(beta)),
       title = "Taylor exponents (β) by type with 95% CI") +
  theme_bw() +
  theme(legend.position = "none")



# Simple slopes (β by type2) and pairwise tests
emtr_taylor <- emtrends(m_taylor, ~ type2, var = "log_mean")
emtr_taylor
pairs(emtr_taylor, adjust = "tukey")


emm_options(lmer.df = "satterthwaite")
emtr_taylor <- emtrends(m_taylor, ~ type2, var = "log_mean")
pairs(emtr_taylor, adjust = "tukey")


# Test the null: β = 2 within each type
test(emtr_taylor, null = 2)

# Type-specific Taylor slopes (β) with CIs
emtr_taylor <- emtrends(m_taylor, ~ type2, var = "log_mean")
summary(emtr_taylor)       # shows β̂, SE, df, and 95% CIs

#Use the per‑type slopes from emtr_taylor (Satterthwaite df)
# emtr_taylor: from emtrends(m_taylor, ~ type2, var = "log_mean") with Satterthwaite df
beta_by_type <- as.data.frame(summary(emtr_taylor)) %>%
  select(type2, log_mean.trend) %>%
  rename(beta_hat_type = log_mean.trend)

dat4.2b <- dat4.2b %>%
  left_join(beta_by_type, by = "type2") %>%
  mutate(
    # Taylor-corrected sd: removes Mean^(β/2) scaling
    sd_corr_type   = sd / (manpp^(beta_hat_type / 2)),
    # Taylor-corrected stability = 1 / CV_β = Mean^(β/2)/sd = 1/sd_corr_type
    stab_corr_type = 1 / sd_corr_type
  )

# Sanity checks
stopifnot(all(is.finite(dat4.2b$sd_corr_type), dat4.2b$sd_corr_type > 0))


# Variability - dropping becaue after correction it's the inverse of stability
#m_sd_corr <- lmer(log(sd_corr_type) ~ type2 + (1 | site),
#                  data = dat4.2b, weights = nobs)
#summary(m_sd_corr)
#car::Anova(m_sd_corr, type = 3)

# Stability
m_stab_corr <- lmer(log(stab_corr_type) ~ type2 + (1 | site),
                    data = dat4.2b, weights = nobs)
summary(m_stab_corr)
car::Anova(m_stab_corr, type = 3)

# Post-hoc (Tukey-adjusted) on the log scale
#pairs(emmeans(m_sd_corr,   ~ type2), adjust = "tukey")
pairs(emmeans(m_stab_corr, ~ type2), adjust = "tukey")


# 1) Get EMMs on model scale
emm_stab <- emmeans(m_stab_corr, ~ type2)

# 2) Back-transform to response scale
emm_stab_resp <- summary(emm_stab, type = "response")

# 3) Create cleaned data frame AND set the desired order manually
stab_df <- as.data.frame(emm_stab_resp) %>%
  mutate(
    type2 = factor(type2,
                   levels = c("Cropland", "Fert. Grassland", "Grassland"))
  )

# 4) Plot bar graph with 95% CIs in the specified order
ggplot(stab_df, aes(x = type2, y = response, fill = type2)) +
  geom_col(width = 0.7, color = "grey15") +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.15, size = 0.8) +
  scale_fill_brewer(palette = "Dark2", guide = "none") +
  labs(
    x = NULL,
    y = "Taylor-corrected stability (geometric mean, 1/CVβ)",
    title = "Corrected stability by vegetation type (EMMs ± 95% CI)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11)
  )

# final check to see if Taylor corrections worked (variance independent of the mean) - should be not significant


summary(lm(log(sd_corr_type) ~ log(manpp), data = filter(dat4.2b, type2 == "Cropland")))
summary(lm(log(sd_corr_type) ~ log(manpp),data = filter(dat4.2b, type2 == "Grassland")))
summary(lm(log(sd_corr_type) ~ log(manpp), data = filter(dat4.2b, type2 == "Fert. Grassland"))) #model significant and has negative relationship (corrected vs uncorrected) 
                                                                                                #indicates that it was over corrected - residual mean variance coupling small but still present


################ see if Taylor correction impacts ppt stability relationship
# not running mean or SD because of the mean variance correction - the taylor correction removes the mean dependence on SD


####compare stability vs ppt across type (raw vs taylor corrected)###

# --- RAW STABILITY ---
m_stab_ppt_raw <- lmer(log(stab) ~ ppt_mean * type2 + (1 | site),
                       data = dat4.2b, weights = nobs)


# --- TAYLOR-CORRECTED STABILITY ---
m_stab_ppt_corr <- lmer(log(stab_corr_type) ~ ppt_mean * type2 + (1 | site),
                        data = dat4.2b, weights = nobs)


car::Anova(m_stab_ppt_raw, type = 3)
car::Anova(m_stab_ppt_corr, type = 3)


emm_options(lmer.df = "satterthwaite")  # stable df for mixed models

# Slopes on the log scale for corrected stability
emtr_stab_corr <- emtrends(m_stab_ppt_corr, ~ type2, var = "ppt_mean")
emtr_stab_corr


#graph



# Build a prediction grid across the observed ppt range
x_seq <- seq(min(dat4.2b$ppt_mean, na.rm = TRUE),
             max(dat4.2b$ppt_mean, na.rm = TRUE),
             length.out = 200)

pred <- emmeans(m_stab_ppt_corr, specs = ~ ppt_mean | type2,
                at = list(ppt_mean = x_seq))
pred_df <- as.data.frame(pred) %>%
  mutate(  # back-transform from log to corrected stability (response scale)
    stab_hat = exp(emmean),
    stab_lo  = exp(lower.CL),
    stab_hi  = exp(upper.CL)
  )

# Plot observed points + model lines + CI ribbons
ggplot() +
  geom_point(data = dat4.2b,
             aes(x = ppt_mean, y = stab_corr_type, color = type2),
             alpha = 0.5, size = 2) +
  geom_ribbon(data = pred_df,
              aes(x = ppt_mean, ymin = stab_lo, ymax = stab_hi, fill = type2),
              alpha = 0.15, color = NA) +
  geom_line(data = pred_df,
            aes(x = ppt_mean, y = stab_hat, color = type2),
            size = 1.2) +
  scale_color_brewer(palette = "Dark2", name = "Type") +
  scale_fill_brewer(palette = "Dark2", name = "Type") +
  labs(x = "Water-year precipitation",
       y = expression(paste("Taylor-corrected stability  ", 1 / sd[beta])),
       title = "Stability (Taylor-corrected) vs precipitation by type") +
  theme_bw()


#### No interaction - compare stability vs ppt by type (raw vs taylor corrected)###
# Ensure your preferred type order
dat4.2b <- dat4.2b %>%
  mutate(type2 = factor(trimws(as.character(type2)),
                        levels = c("Cropland","Fert. Grassland","Grassland")))

# Build long table: Raw vs Taylor-corrected stability side-by-side
stab_long <- dat4.2b %>%
  select(site, type2, nobs, ppt_mean, stab, stab_corr_type) %>%
  pivot_longer(
    cols = c(stab, stab_corr_type),
    names_to = "metric_raw_corr",
    values_to = "stab_val"
  ) %>%
  mutate(
    correction = ifelse(metric_raw_corr == "stab", "Raw", "Taylor-corrected"),
    correction = factor(correction, levels = c("Raw", "Taylor-corrected"))
  )

# Helper to fit per-type weighted OLS and test slope interaction
fit_by_type <- function(df_type) {
  # Weighted OLS; no random effects within a type (sites mostly single rows)
  m <- lm(log(stab_val) ~ ppt_mean * correction, data = df_type, weights = nobs)
  list(
    model = m,
    anova = anova(m),                         # overall terms (Type I); see note below
    tidy  = broom::tidy(m)                    # coefficients incl. ppt_mean:correctionTaylor-corrected
  )
}

# Split and fit
res_crop  <- fit_by_type(filter(stab_long, type2 == "Cropland"))
res_fert  <- fit_by_type(filter(stab_long, type2 == "Fert. Grassland"))
res_grass <- fit_by_type(filter(stab_long, type2 == "Grassland"))

# Quick look at interaction term for each type
res_crop$tidy  %>% filter(term == "ppt_mean:correctionTaylor-corrected")
res_fert$tidy  %>% filter(term == "ppt_mean:correctionTaylor-corrected")
res_grass$tidy %>% filter(term == "ppt_mean:correctionTaylor-corrected")




# next step - check slopes for stability - while there is not interaction, a main effect would be useful to know

# Subset
dat_crop <- filter(dat4.2b, type2 == "Cropland")

# Weighted OLS (no random effects)
m_stab_raw_crop  <- lm(log(stab) ~ ppt_mean,        data = dat_crop, weights = nobs)
m_stab_corr_crop <- lm(log(stab_corr_type) ~ ppt_mean, data = dat_crop, weights = nobs)

# Slope p-values and 95% CI on log scale
tidy(m_stab_raw_crop)  %>% filter(term == "ppt_mean")
confint(m_stab_raw_crop)["ppt_mean", ]

tidy(m_stab_corr_crop) %>% filter(term == "ppt_mean")
confint(m_stab_corr_crop)["ppt_mean", ]

# Multiplicative factor per +100 ppt units (response-scale effect size)
crop_raw_factor_100  <- exp(coef(m_stab_raw_crop)["ppt_mean"]  * 100)
crop_corr_factor_100 <- exp(coef(m_stab_corr_crop)["ppt_mean"] * 100)

dat_fert <- filter(dat4.2b, type2 == "Fert. Grassland")

m_stab_raw_fert  <- lm(log(stab) ~ ppt_mean,        data = dat_fert, weights = nobs)
m_stab_corr_fert <- lm(log(stab_corr_type) ~ ppt_mean, data = dat_fert, weights = nobs)

tidy(m_stab_raw_fert)  %>% filter(term == "ppt_mean")
confint(m_stab_raw_fert)["ppt_mean", ]

tidy(m_stab_corr_fert) %>% filter(term == "ppt_mean")
confint(m_stab_corr_fert)["ppt_mean", ]

fert_raw_factor_100  <- exp(coef(m_stab_raw_fert)["ppt_mean"]  * 100)
fert_corr_factor_100 <- exp(coef(m_stab_corr_fert)["ppt_mean"] * 100)
fert_raw_factor_100; fert_corr_factor_100

dat_grass <- filter(dat4.2b, type2 == "Grassland")

m_stab_raw_grass  <- lm(log(stab) ~ ppt_mean,        data = dat_grass, weights = nobs)
m_stab_corr_grass <- lm(log(stab_corr_type) ~ ppt_mean, data = dat_grass, weights = nobs)

tidy(m_stab_raw_grass)  %>% filter(term == "ppt_mean")
confint(m_stab_raw_grass)["ppt_mean", ]

tidy(m_stab_corr_grass) %>% filter(term == "ppt_mean")
confint(m_stab_corr_grass)["ppt_mean", ]

grass_raw_factor_100  <- exp(coef(m_stab_raw_grass)["ppt_mean"]  * 100)
grass_corr_factor_100 <- exp(coef(m_stab_corr_grass)["ppt_mean"] * 100)
grass_raw_factor_100; grass_corr_factor_100

# Helper to collect slope row + CI + factor per +100 ppt
collect <- function(model, type_label, metric_label){
  s  <- tidy(model) %>% filter(term == "ppt_mean")
  ci <- confint(model)["ppt_mean", ]
  tibble(
    type2              = type_label,
    metric             = metric_label,
    slope_log          = s$estimate,
    SE                 = s$std.error,
    t                  = s$statistic,
    p_value            = s$p.value,
    CI_low_log         = ci[1],
    CI_high_log        = ci[2],
    factor_per_100     = exp(s$estimate * 100),
    factor_low_100     = exp(ci[1] * 100),
    factor_high_100    = exp(ci[2] * 100)
  )
}

stab_ppt_by_type <- bind_rows(
  collect(m_stab_raw_crop,   "Cropland",        "Raw"),
  collect(m_stab_corr_crop,  "Cropland",        "Taylor-corrected"),
  collect(m_stab_raw_fert,   "Fert. Grassland", "Raw"),
  collect(m_stab_corr_fert,  "Fert. Grassland", "Taylor-corrected"),
  collect(m_stab_raw_grass,  "Grassland",       "Raw"),
  collect(m_stab_corr_grass, "Grassland",       "Taylor-corrected")
)

stab_ppt_by_type
# Optional export:
# readr::write_csv(stab_ppt_by_type, "stability_ppt_by_type_raw_vs_corrected.csv")