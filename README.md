# Assessing the Resilience of Productivity to Climate Variability Across Management and Climate Gradients

Principal Investigators:
- David Hoover
- Olivia Hajek

## Workflow Strategy

1. Raw ANPP data collected in Drive
2. `pre-process` scripts handle pernicious idiosyncrasies
3. Pre-processed ANPP data created
4. `01_harmonize` script + "data key" used to harmonize pre-processed data across networks
5. '02_baseline_anpp_filter' - script for initial data filtering - including removing sites and treatments not of interest
6. `03_quality-control` script used to do QA/QC on harmonized data
7. `04_aggregation` script used to aggregate to site/year/treatment replicates
8. Tidy ANPP data created 
9. Management info sheet (by site/treatment), site attributes sheet (including environmental variables), and site-year attribute sheet (similar to management info but one year per row) all joined with tidy ANPP data (see step 7) in "grand join"

## Script Explanations

Briefly describe the purpose of each script (or folder of scripts) here as you create them!

- "retrieve-mswep-ppt-data.R" - script for retreiveing daily precipitation data for sites from MSWEP
- "01_harmonize.R" - script for harmonizing raw data into a single data table
- "02_baseline_anpp_filter.R" - script for initial data filtering - including removing sites and treatments not of interest
- "03_quality-control.R" - script for quality control operations and--eventually--integration of environmental covariates and management metadata
- "04_filter.R" - script for subsetting out treatments / sites / rows of data that are believed to not be of interest or not be comparable to remaining data
- "05_aggregate-to-site.R" - script for aggregating to site level (to standardize across space for each dataset)

## Supplementary Resources

LTER Scientific Computing Team [website](https://lter.github.io/scicomp/) & NCEAS' [Resources for Working Groups](https://www.nceas.ucsb.edu/working-group-resources)

