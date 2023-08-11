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
  - `ein` (numeric): Employment Identification Number (IRS assigned organization ID)
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

### Manuscript

* [Figure 1: The Geography of Civic Opportunity in the United States](https://github.com/snfagora/map_civic_opportunity/blob/main/plots/figure1.pdf)

* [Figure 2: Inequality of Civic Opportunity](https://github.com/snfagora/map_civic_opportunity/blob/main/plots/figure2.pdf)

* [Figure 3: Relationship of per capita civic opportunity scores (Panel A), Kyne and Aldrich's composite measure of social capital (Panel B), Chetty et al's "public good"" organizations per capita (Panel C), and Rupasingha et al's index (Panel D) with COVID-19 mutual aid instances at the county level](https://github.com/snfagora/map_civic_opportunity/blob/main/plots/figure3.pdf) TK

* [Figure 4: Sources of Civic Opportunity](https://github.com/snfagora/map_civic_opportunity/blob/main/plots/figure4.pdf)

### Supplemenatary materials 

* Table S1 (manually created codebook)

* [Table S2 Performance results for individual binary classifiers. The final models used were ensemble models that combined Lasso and XGBoost models](https://github.com/snfagora/map_civic_opportunity/blob/main/tables/table_s2.docx)

* [Table S3 Performance results for classifiers are pooled by models and metrics](https://github.com/snfagora/map_civic_opportunity/blob/main/tables/table_s3.docx)

* Table S4. Number of organizations in each category labeled by their availability of website data and IRS tax return data.  

* Table S6. Number and percentage of organizations identified by their activities, based on their websites. 

* Table S7 Regression results on the associations between three measures of inequality and civic opportunity scores per capita and the Penn State Index.  

* Table S9 Weighted least square regression of county-level emergence of mutual aid instances and covariates.  

* Table S10 Weighted least square regression of county-level vaccine acceptance and covariates. County-level social capital indices are taken from Rupasingha, et al. 

* Table S11 Weighted least square regression of county-level vaccine hesitancy and covariates with the inclusion of misinformation. County-level social capital indices are taken from Rupasingha, et al. 

* Table S12 Ordinary least square regression of vaccine uptake ??? county (n=2,728). For this analysis we excluded the states of New Hampshire, Colorado, Texas, and Hawaii, which had not fully reported vaccination data to the CDC at that time. A state-level term (not shown) was also included in the analysis. 

* Table S14 Ordinary least square regression for vaccine uptake in zip codes on civic opportunities per capita and other covariates. Vaccine uptake is expressed as the cumulative number of doses per thousand residents. (MN, n=792 ; TX, n=1582 ; NY, n=1516) 

* [Table S15 Organizations from the Washington Representative Study matched to the Civic Opportunity dataset](https://github.com/snfagora/map_civic_opportunity/blob/main/tables/table_s15.rtf)

* [Table S16 Distribution of civic opportunity organizations across organization types. Pre-1960 organization are those that received IRS non-profit designations prior to 1960](https://github.com/snfagora/map_civic_opportunity/blob/main/tables/table_s16.rtf)

* [Fig S1 Correlation matrix between index variables and their components](https://github.com/snfagora/map_civic_opportunity/blob/main/plots/figure_s1.pdf)

* [Fig S2 The percentage of high and low civic score counties across states](https://github.com/snfagora/map_civic_opportunity/blob/main/plots/figure_s2.pdf)