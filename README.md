# The Unequal Landscape of Civic Opportunity in America

Authors: Milan de Vries, Jae Yeon Kim, and Hahrie Han (2023)

Last update: August 7, 2023

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

### Processed data 

* org_measures.csv (organization-level, 1,363,701 * 15)

## Code

- Data Wrangling and Descriptive Analysis: `Figures 1-3`, `Figures S1-2`
- Matching the MMA and Washington data: `Figure 4`
- Regression tables
- Classification evaluation tables

## Tables and Figures 

### Manuscript

* [Figure 1: The Geography of Civic Opportunity in the United States](https://github.com/snfagora/map_civic_opportunity/blob/main/plots/figure1.pdf)

* [Figure 2: Inequality of Civic Opportunity](https://github.com/snfagora/map_civic_opportunity/blob/main/plots/figure2.pdf)

* [Figure 3: Relationship of per capita civic opportunity scores (Panel A), Kyne and Aldrich's composite measure of social capital (Panel B), Chetty et al's "public good"" organizations per capita (Panel C), and Rupasingha et al's index (Panel D) with COVID-19 mutual aid instances at the county level](https://github.com/snfagora/map_civic_opportunity/blob/main/plots/figure3.pdf) TK

* [Figure 4: Sources of Civic Opportunity](https://github.com/snfagora/map_civic_opportunity/blob/main/plots/figure4.pdf)

### Supplemenatary materials 

* Table S2 Performance results for individual binary classifiers. The final models used were ensemble models that combined Lasso and XGBoost models. 

* Table S4. Number of organizations in each category labeled by their availability of website data and IRS tax return data.  

* Table S6. Number and percentage of organizations identified by their activities, based on their websites. 

* Table S7 Regression results on the associations between three measures of inequality and civic opportunity scores per capita and the Penn State Index.  

* Table S9 Weighted least square regression of county-level emergence of mutual aid instances and covariates.  

* Table S10 Weighted least square regression of county-level vaccine acceptance and covariates. County-level social capital indices are taken from Rupasingha, et al. 

* Table S11 Weighted least square regression of county-level vaccine hesitancy and covariates with the inclusion of misinformation. County-level social capital indices are taken from Rupasingha, et al. 

* Table S12 Ordinary least square regression of vaccine uptake ??? county (n=2,728). For this analysis we excluded the states of New Hampshire, Colorado, Texas, and Hawaii, which had not fully reported vaccination data to the CDC at that time. A state-level term (not shown) was also included in the analysis. 

* Table S14 Ordinary least square regression for vaccine uptake in zip codes on civic opportunities per capita and other covariates. Vaccine uptake is expressed as the cumulative number of doses per thousand residents. (MN, n=792 ; TX, n=1582 ; NY, n=1516) 

* Table S15 Organizations from the Washington Representative Study matched to the Civic Opportunity dataset. 

* Table S16 Distribution of civic opportunity organizations across organization types. Pre-1960 organization are those that received IRS non-profit designations prior to 1960. 

* [Fig S1 Correlation matrix between index variables and their components](https://github.com/snfagora/map_civic_opportunity/blob/main/plots/figure_s1.pdf)

* [Fig S2 The percentage of high and low civic score counties across states](https://github.com/snfagora/map_civic_opportunity/blob/main/plots/figure_s2.pdf)