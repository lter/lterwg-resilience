**Assessing the Resilience of Productivity to Climate Variability Across Management and Climate Gradients**
An NCEAS-supported LTER working group repository for compiling, harmonizing, and analyzing long-term aboveground net primary productivity (ANPP) data alongside climate covariates (precipitation, SPEI, temperature extremes) across croplands, grasslands, and other managed/unmanaged systems, in order to understand how productivity stability and climate sensitivity vary across management and climate gradients.

Principal Investigators
-	David Hoover
-	Olivia Hajek

Workflow Overview
The core ANPP workflow proceeds in three stages: pre-processing (network-specific cleanup), harmonization (combining across networks into one tidy table), and downstream analysis (stability, thresholds, timing/critical-period, and extremes work).
1.	Raw ANPP data collected/stored in a shared Google Drive
2.	pre-process/ scripts handle network-specific idiosyncrasies so each source dataset conforms to a common structure
3.	anpp01_harmonize.R + a data "key" harmonize pre-processed data across networks into a single table
4.	anpp02_initial_filter.R performs initial filtering (e.g., removing unwanted treatments/sites, non-US NutNet sites)
5.	anpp03_quality-control.R runs QA/QC and remaining data-wrangling tasks
6.	anpp04_aggregate-to-site.R aggregates to site/year/treatment replicates
7.	Environmental covariates (precipitation, SPEI, soils, etc.; see enviro-covariates/) and management/site-attribute tables (see make_site_attribute_table.R) are joined to the tidy ANPP data (anpp_wyrppt_merge.R, anpp_wyrppt_management_join.R) to produce the analysis-ready dataset
8.	Downstream analyses (stability, extremes, thresholds, timing/critical-period) are run on this joined dataset

Run 00_create_data_folder.R first — it creates the local folder structure the rest of the scripts expect.
Repository Structure

Core pipeline (top level)
-	00_create_data_folder.R — creates local data folder structure; run this first
-	anpp01_harmonize.R — ingests all raw/pre-processed data files and harmonizes them into a single data table (standardizes column names, combines comparable columns)
-	anpp02_initial_filter.R — removes unwanted treatments and sites (e.g., LTAR treatments not of interest, non-USA NutNet sites)
-	anpp03_quality-control.R — QC and miscellaneous data-wrangling after harmonization
-	anpp04_aggregate-to-site.R — aggregates data to site/year/treatment level
-	anpp_wyrppt_merge.R / anpp_wyrppt_management_join.R — merge harmonized ANPP + water-year precipitation data with the management info table
-	make_site_attribute_table.R — builds the site attribute table (climate normals, soils, etc.); assumes local sync with the shared Google Drive
-	retrieve-mswep-ppt-data.R / calculate_mswep_metrics.R — retrieve and process daily MSWEP precipitation data for each site
-	download_SPEI-site.R — downloads site-level SPEI (6-month) data
-	Stability_ANPP_DataPrep.R — builds the cleaned ANPP data frame used throughout the stability analyses (updates crop names, requires ≥5 years of data per site)
-	Fig1_Climate_and_Map.R — generates the stability manuscript's site map and climate (Whittaker biome) figures
-	Crop_Division_Length_Exploration.R — exploratory work on crop rotation/division length
-	Critical_period_work.R / data_prep_Timing_Critical.R / timing_critical_dendrotools.R — timing and critical-period analysis pipeline relating ANPP to lagged SPEI/precipitation windows
-	trends_eda.R — exploratory trend analysis on the harmonized/tidy data

Folders
-	pre-process/ — network-specific scripts (DAP, DRIVES, LTAR, LTER, CSCAP, ISU-drainage, Konza, NutNet, etc.) that resolve idiosyncrasies in each raw dataset before it enters the harmonization workflow
-	enviro-covariates/ — scripts for processing environmental covariates: precipitation, aridity index, heat index, SPEI extremes/whiplash windows, SSURGO soils data, Daymet weather retrieval
-	stability/ — mean-variance scaling (Taylor's Power Law) and ANPP stability manuscript analyses; includes Old Code/ for prior analysis versions
-	extremes/ — data prep and threshold analysis for climate extremes
-	thresholds/ — analysis of ANPP-precipitation and ANPP-temperature relationships and their non-linearities across systems and sites
-	tools/ — custom helper functions (e.g., difference-windows, whiplash identification) used across workflow scripts; see individual scripts for function documentation
-	ancillary/ — supporting scripts not part of the core iterative workflow (e.g., expand-key.R for generating data-key rows via ltertools::expand_key, Google Drive URL helpers, planting/harvest date compilation)
-	exploratory/ — exploratory data analysis and in-progress scripts (ANPP-PPT relationships, SPEI/whiplash EDA, weather source comparisons, visualization drafts)

Supplementary Resources
-	LTER Scientific Computing Team website
-	NCEAS Resources for Working Groups
