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

- [Pierri, et al. (2022)](https://github.com/osome-iu/CoVaxxy-Misinfo)

- [Rupasingha, et al (2006, with updates) (Penn State Index)](https://aese.psu.edu/nercrd/community/social-capital-resources)

- [Schlozman et al (2014) (Washington Representative Study)](https://www.icpsr.umich.edu/web/ICPSR/studies/35309/publications)

### Replication (processed) data 

All replication data are available at https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/TCXRTM 

* mma_sc_demo.csv (3127 * 8)
  - `FIPS` (character): County FIPS code
  - `opc` (numeric): Civic opportunity scores per capita
  - `socialcap` (numeric): Social capital measure from Kyne & Aldrich
  - `sk2014` (numeric): Social capital measure from Rupasingha, et al. 
  - `civicorganization_county` (numeric): Civic organization measure from Chetty, et al.
  - `race_per_white_nonhispaic` (numeric): The proportion of non-white Hispanic populations in counties 
  - `per_poverty` (numeric): The proportion of people below the federal poverty line in counties 
  - `college_educ` (numeric): The proportion of college educated people in counties 

* unit_cor_df.csv (4 * 5)
  - `term` (character): Civic opportunity dimensions (i.e., "Taking action," "Volunteering," "Membership," "Holding events")
  - `Binary index` (numeric): Binary index civic opportunity scores 
  - `Mean index` (numeric): Mean index civic opportunity scores 
  - `Inverse covariance matrix index` (numeric): Inverse covariance matrix index civic opportunity scores 
  - `Principal component first factor index` (numeric): PCA first factor index civic opportunity scores 
  
* map_trade_off.csv (51 * 3)
  - `State` (character): U.S. States 
  - `pct_higher_cnts` (numeric): The proportion of high civic opportunity counties in states 
  - `pct_lower_cnts` (numeric): The proportion of lower civic opportunity counties in states 

* county_opc.rds (3108 * 16)
  - `opc_tile` (numeric): Civic opportunity ranks (1-5)
  - The remaining columns are from the US shapefile, which is based on the [tigris package](https://cran.r-project.org/web/packages/tigris/index.html) in R (ver 2.03).

* la_zip_shp.shp: [LA TIGER/Line shapefile (2018)](https://catalog.data.gov/dataset/tiger-line-shapefile-2018-county-los-angeles-county-ca-all-roads-county-based-shapefile)

* la_zcta_opc.csv (300 * 2) 
  - `ZCTA` (character): 5 digit ZIPCODE
  - `opc_tile` (numeric): Civic opportunity ranks (1-5)

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
  
* regression_mutual_aid.csv (3112 * 17)
  - `FIPS` (character): County FIPS code
  - `has_hubs` (numeric): Presence of COVID-19 mutual aid hubs in the county
  - `hubs` (numeric): Number of COVID-19 mutual aid hubs in the county
  - `civic_opportunity_per_capita` (numeric): Civic opportunity scores per capita
  - `socialcap` (numeric): Social capital measure from Kyne & Aldrich
  - `civic_organizations_county` (numeric): Civic organization measure from Chetty, et al.
  - `sk2014` (numeric): Social capital measure from Rupasingha, et al. 

* regression_vaccine_hesitancy.csv (3107 * 16)
  - `FIPS` (character): County FIPS code
  - `vac.acceptance` (numeric): County vaccine acceptance rate. See Pierri, et al.
  - `civic_opportunity_per_capita` (numeric): Civic opportunity scores per capita
  - `sk2014` (numeric): Social capital measure from Rupasingha, et al. 
  - `misinfo` (numeric): COVID-19 misinformation. See Pierri, et al.
  - `covidmortality` (numeric): County-level COVID-19 mortality rate

* regression_vaccine_uptake.csv (3107 * 16)
  - `FIPS` (character): County FIPS code
  - `vpt.all` (numeric): County vaccine uptake rate as defined by the CDC
  - `civic_opportunity_per_capita` (numeric): Civic opportunity scores per capita
  - `sk2014` (numeric): Social capital measure from Rupasingha, et al. 
  - `Recip_State` (numeric): U.S. States
  - `covidmortality` (numeric): County-level COVID-19 mortality rate

* regression_zcta_mn.csv (869 * 14)
  - `GEOID` (character): ZCTA Geoid (Zip Code)
  - `vaccination_rate` (numeric): ZCTA vaccination rate
  - `civic_opportunity_per_capita` (numeric): Civic opportunity scores per capita
  - `d.share` (numeric): Democratic vote share, 2020 election

* regression_zcta_tx.csv (1855 * 14)
  - `GEOID` (character): ZCTA Geoid (Zip Code)
  - `vaccination_rate` (numeric): ZCTA vaccination rate
  - `civic_opportunity_per_capita` (numeric): Civic opportunity scores per capita
  - `d.share` (numeric): Democratic vote share, 2020 election

* regression_zcta_ny.csv (1793 * 14)
  - `GEOID` (character): ZCTA Geoid (Zip Code)
  - `vaccination_rate` (numeric): ZCTA vaccination rate
  - `civic_opportunity_per_capita` (numeric): Civic opportunity scores per capita
  - `d.share` (numeric): Democratic vote share, 2020 election

## Code

- [Wrangling and joining the MMA and other outcomes and covariates](https://github.com/snfagora/map_civic_opportunity/blob/main/src/data_wrangling.Rmd) 
- [Analyzing the MMA and other outcomes and covariates](https://github.com/snfagora/map_civic_opportunity/blob/main/src/measurement_test.R) (Figures 1-3, Figures S1-2) 
- [Matching the MMA and Washington data](https://github.com/snfagora/map_civic_opportunity/blob/main/src/name_matching.R)
- [Analyzing the MMA and Washington data](https://github.com/snfagora/map_civic_opportunity/blob/main/src/org_type_analysis.R) (Figure 4, Tables S15-16)
- [Regression tables](https://github.com/snfagora/map_civic_opportunity/blob/main/src/regression_tables.R)
- [Classification evaluation tables](https://github.com/snfagora/map_civic_opportunity/blob/main/src/ml_performance_eval.R) (Tables S2-3)

## Tables and Figures 

* All tables are in `/tables` 
* All figures are in `/figures` 