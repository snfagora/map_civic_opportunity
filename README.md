# The Unequal Landscape of Civic Opportunity in America

Authors: Milan de Vries, Jae Yeon Kim, and Hahrie Han (2023)

## Session information 

* R version 4.2.2 (2022-10-31)
* Platform: aarch64-apple-darwin20 (64-bit)
* Running under: macOS Ventura 13.4.1

## Data 

### Raw data 

* We collected the IRS tax returns and related website data (i.e., their "About" pages) using our own R package, called ["MapAgora"](https://snfagora.github.io/MapAgora/) (ver 0.08), developed by de Vries and Kim.

* We classified the organizational types based on the above data using our own R package, called ["autotextclassifier"](https://snfagora.github.io/autotextclassifier/) (ver 0.05), also developed by de Vries and Kim.

### Other data sources 

- [Kyne and Aldrich (2020)](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/IUNNZM)

- [Chetty, et al (2023)](https://www.socialcapital.org/?dimension=EconomicConnectednessIndividual&geoLevel=county&selectedId=&dim1=EconomicConnectednessIndividual&dim2=CohesivenessClustering&dim3=CivicEngagementVolunteeringRates&bigModalSection=&bigModalChart=scatterplot&showOutliers=false&colorBy=)

- [Rupasingha, et al (2006, with updates) (Penn State Index)](https://aese.psu.edu/nercrd/community/social-capital-resources)

- [Schlozman et al (2014) (Washington Representative Study)](https://www.icpsr.umich.edu/web/ICPSR/studies/35309/publications)

### Replication (processed) data 

All replication data are available at: tk 

* mma_sc_demo.csv

* unit_cor_df.csv

* map_trade_off.csv

* county_opc.rds

* la_zip_shp.shp

* la_zcta_opc.csv

* matched_mma_wa.csv (5,853 * 5)
  - `ein` (numeric): Employment Identification Number (IRS-assigned-organization IDs)
  - `state` (character): The state in which the organization is located
  - `lobby` (dummy): 1 = "lobbying organization," 0 = "non-lobbying organization"  
  - `predicted` (character): 15 predicted categories or `NA`
  - `ruling` (numeric) = IRS ruling year-month (e.g., `200303`)
  
* matched_mma_wa_summary.csv (30 * 6)
  - `class` (character): 15 predicted categories 
  - `n` (numeric): The number of classified organizations in each category
  - `freq` (numeric): The frequency of classified organizations in each category 
  - `typr` (character): "DC Organizations" or "Civic Organizations"
  
* org_flow_over_time.csv (30 * 6)
  - `period` (character): "Pre-1960" or "Post-2010"
  - `class` (character): 15 predicted categories 
  - `n` (numeric): The number of classified organizations in each category
  - `freq` (numeric): The frequency of classified organizations in each category 
  - `flow_change` (numeric): The frequency difference between the post-2010 and the pre-1960 cohorts
  - `dir` (character): "Increase" or "Decrease"
  
* org_volume_over_time.csv (15 * 4)
  - `class` (character): 15 predicted categories 
  - `all` (numeric): The total number of classified organizations in each category
  - `Post-2010` (numeric): The number of classified organizations in each category from the post-2010 cohort
  - `Pre-1960` (numeric): The number of classified organizations in each category from the pre-1960 cohort

* pred_eval.csv (135 * 5)
  - `.metric`: "Accuracy," "Balanced accuracy," or "F-Score"
  - `.estimator`: "Binary"
  - `.estimate`: Predicted probability
  - `model` (classifier): "Lasso," "Random forest", or "XGBoost"
  - `class` (character): 15 predicted categories 

## Code

- [Wrangling and joining the MMA and other outcomes and covariates](https://github.com/snfagora/map_civic_opportunity/blob/main/src/data_wrangling.Rmd) 
- [Analyzing the MMA and other outcomes and covariates](https://github.com/snfagora/map_civic_opportunity/blob/main/src/measurement_test.R) (Figures 1-3) 
- [Matching the MMA and Washington data](https://github.com/snfagora/map_civic_opportunity/blob/main/src/name_matching.R)
- [Analyzing the MMA and Washington data](https://github.com/snfagora/map_civic_opportunity/blob/main/src/org_type_analysis.R) (Figure 4)
- [Regression tables](https://github.com/snfagora/map_civic_opportunity/blob/main/src/regression_tables.R)
- [Classification evaluation tables](https://github.com/snfagora/map_civic_opportunity/blob/main/src/ml_performance_eval.R)

## Tables and Figures 

* All tables are in `/tables` 
* All figures are in `/figures` 