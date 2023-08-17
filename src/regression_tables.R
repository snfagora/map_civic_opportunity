$)C
if (!require(pacman)) install.packages("pacman")

pacman::p_load(dplyr, here, clipr, ggplot2, readr, gt, modelsummary, stringr, effectsize, datawizard, purrr)

ggplot2::theme_set(theme_minimal())

desc_files <- list.files(here("processed_data"))[stringr::str_detect(list.files(here("processed_data")), "txt")]

desc_files

rds_files <- list.files(here("processed_data"))[stringr::str_detect(list.files(here("processed_data")), "rds")]

################################################################################
# Supplementary Table S9 [Mutual Aid]
################################################################################

mut.aid <- read_csv(here("processed_data", "regression_mutual_aid.csv"))

opc_min <- lm(has_hubs ~ opc + RUCC + gop.tier, mut.aid %>% filter(!is.na(opc),!is.na(civic_organizations_county),!is.na(socialcap), !is.na(sk2014)))
kyne_min <- lm(has_hubs ~ socialcap + RUCC + gop.tier, mut.aid %>% filter(!is.na(opc),!is.na(civic_organizations_county),!is.na(socialcap), !is.na(sk2014)))
chetty_min <- lm(has_hubs ~ civic_organizations_county + RUCC + gop.tier, mut.aid %>% filter(!is.na(opc),!is.na(civic_organizations_county),!is.na(socialcap), !is.na(sk2014)))
penn_min <- lm(has_hubs ~ sk2014 + RUCC + gop.tier, mut.aid %>% filter(!is.na(opc),!is.na(civic_organizations_county),!is.na(socialcap), !is.na(sk2014)))

opc_full <- lm(has_hubs ~ opc + Age65AndOlderPct2010 + WhiteNonHispanicPct2010 + AsianNonHispanicPct2010 + BlackNonHispanicPct2010 + HispanicPct2010 + NativeAmericanNonHispanicPct2010 + per_col_ed + PCTPOVALL + RUCC + gop.tier, tt %>% filter(!is.na(opc),!is.na(civic_organizations_county),!is.na(socialcap), !is.na(sk2014)))
kyne_full<- lm(has_hubs ~ socialcap + Age65AndOlderPct2010 + WhiteNonHispanicPct2010 + AsianNonHispanicPct2010 + BlackNonHispanicPct2010 + HispanicPct2010 + NativeAmericanNonHispanicPct2010 + per_col_ed + PCTPOVALL + RUCC + gop.tier, tt %>% filter(!is.na(opc),!is.na(civic_organizations_county),!is.na(socialcap), !is.na(sk2014)))
chetty_full <- lm(has_hubs ~ civic_organizations_county + Age65AndOlderPct2010 + WhiteNonHispanicPct2010 + AsianNonHispanicPct2010 + BlackNonHispanicPct2010 + HispanicPct2010 + NativeAmericanNonHispanicPct2010 + per_col_ed + PCTPOVALL + RUCC + gop.tier, tt %>% filter(!is.na(opc),!is.na(civic_organizations_county),!is.na(socialcap), !is.na(sk2014)))
penn_full <- lm(has_hubs ~ sk2014 + Age65AndOlderPct2010 + AsianNonHispanicPct2010 + WhiteNonHispanicPct2010 + BlackNonHispanicPct2010 + HispanicPct2010 + NativeAmericanNonHispanicPct2010 + per_col_ed + PCTPOVALL + RUCC + gop.tier, tt %>% filter(!is.na(opc),!is.na(civic_organizations_county),!is.na(socialcap), !is.na(sk2014)))

mad <- list("opc_min" = opc_min, "kyne_min" = kyne_min, "chetty_min" = chetty_min, "penn_min" = penn_min, "opc_full" = opc_full, "kyne_full" = kyne_full, "chetty_full" = chetty_full, "penn_full" = penn_full)

mad_models <- list(
  "Civic Opportunity (base)" = mad$opc_min,
  "Kyne & Aldrich (base)" = mad$kyne_min,
  "Chetty, et al. (base)" = mad$chetty_min,
  "Rupasingha, et al. (base)" = mad$penn_min,
  "Civic Opportunity (full)" = mad$opc_full,
  "Kyne & Aldrich (full)" = mad$kyne_full,
  "Chetty, et al. (full)" = mad$chetty_full,
  "Rupasingha, et al. (full)" = mad$penn_full
)

mad_cm <- c(
  "opc" = "Civic opportunity",
  "socialcap" = "Social capital (Kyne & Aldrich)",
  "civic_organizations_county" = "Civic Organization Density",
  "sk2014" = "Social capital (Rupasingha, et al.)",
  "gop.tier" = "GOP presidential vote (%)",
  "Age65AndOlderPct2010" = "Age 65 or older (%)",
  "WhiteNonHispanicPct2010" = "White Non-hispanic (%)",
  "AsianNonHispanicPct2010" = "Asian (%)",
  "BlackNonHispanicPct2010" = "Black (%)",
  "HispanicPct2010" = "Hispanic (%)",
  "NativeAmericanNonHispanicPct2010" = "Native American (%)",
  "per_col_ed" = "College education rate",
  "PCTPOVALL" = "Poverty rate (%)",
  "RUCC" = "Rural/Urban continuity"
)

modelsummary(mad_models, 
             fmt = fmt_statistic("p.value" = fmt_sprintf("%.2e"),"estimate" = 3,
             vcov = "robust",
             estimate = c("{estimate}{stars} \n [{conf.low}, {conf.high}] \n p = {p.value}"),
             statistic = NULL,
             coef_omit = "Intercept",
             coef_map = mad_cm,
             output = here("outputs", "supplementary_table_9.docx")
)
             
################################################################################
# Supplementary Table S10 [Vaccine Acceptance]  
################################################################################

vac.hesitancy <- read_csv(here("processed_data", "regression_vaccine_hesitancy.csv"))

vac_accept_opc_min <- lm(vac.acceptance ~ opc + dec_gop, data=vac.hesitancy %>% filter(!is.na(vac.hesitancy)))
vac_accept_soccap_min <- lm(vac.acceptance ~ sk2014 + dec_gop, data=vac.hesitancy %>% filter(!is.na(vac.hesitancy)))

vac_accept_opc <- lm(vac.acceptance ~ opc + dec_gop + covidmortality + PCTPOVALL + Age65AndOlderPct2010 + WhiteNonHispanicPct2010 + per_col_ed + AsianNonHispanicPct2010 + BlackNonHispanicPct2010 + HispanicPct2010 + NativeAmericanNonHispanicPct2010 + RUCC, data=vac.hesitancy %>% filter(!is.na(vac.hesitancy)))
vac_accept_soccap <- lm(vac.acceptance ~ sk2014 + dec_gop + covidmortality + PCTPOVALL + Age65AndOlderPct2010 + WhiteNonHispanicPct2010 + per_col_ed + AsianNonHispanicPct2010 + BlackNonHispanicPct2010 + HispanicPct2010 + NativeAmericanNonHispanicPct2010 + RUCC, data=vac.hesitancy %>% filter(!is.na(vac.hesitancy)))

vac.hes <- list("opc_min" = vac_accept_opc_min, "soccap_min" = vac_accept_soccap_min, "opc_full" = vac_accept_opc, "soccap_full" = vac_accept_soccap)


vac_hes_models <- list(
  "Civic Opportunity (base)" = vac.hes$opc_min,
  "Civic Opportunity (full)" = vac.hes$opc_full,
  "Social Capital (base)" = vac.hes$soccap_min,
  "Social Capital (full)" = vac.hes$soccap_full
)

vac_hes_cm <- c(
  "opc" = "Civic opportunity",
  "sk2014" = "Social capital (Rupasingha, et al.)",
  "dec_gop" = "Decile GOP presidential vote (%)",
  "covidmortality" = "Covid mortality rate",
  "Age65AndOlderPct2010" = "Age 65 or older (%)",
  "WhiteNonHispanicPct2010" = "White Non-hispanic (%)",
  "AsianNonHispanicPct2010" = "Asian (%)",
  "BlackNonHispanicPct2010" = "Black (%)",
  "HispanicPct2010" = "Hispanic (%)",
  "NativeAmericanNonHispanicPct2010" = "Native American (%)",
  "PCTPOVALL" = "Percent Poverty",
  "per_col_ed" = "College education rate",
  "RUCC" = "Rural/Urban continuity"
)

modelsummary(vac_hes_models, 
             fmt = fmt_statistic("p.value" = fmt_sprintf("%.2e"),"estimate" = 3),
             vcov = "robust",
             estimate = c("{estimate}{stars} \n [{conf.low}, {conf.high}] \n p = {p.value}"),
             statistic = NULL,
             coef_omit = "Intercept",
             coef_map = vac_hes_cm,
             output = here("outputs", "supplementary_table_10.docx")

################################################################################
# Supplementary Table S11 [Vaccine Acceptance + Misinformation]   
################################################################################                          

vac_accept_opc_min_misinfo <- lm(vac.acceptance ~ opc + misinfo + dec_gop, data=vac.hesitancy %>% filter(!is.na(vac.hesitancy)))
vac_accept_soccap_min_misinfo <- lm(vac.acceptance ~ sk2014 + misinfo + dec_gop, data=vac.hesitancy %>% filter(!is.na(vac.hesitancy)))

vac_accept_opc_misinfo <- lm(vac.acceptance ~ opc + misinfo + dec_gop + covidmortality + PCTPOVALL + Age65AndOlderPct2010 + WhiteNonHispanicPct2010 + per_col_ed + AsianNonHispanicPct2010 + BlackNonHispanicPct2010 + HispanicPct2010 + NativeAmericanNonHispanicPct2010 + RUCC, data=vac.hesitancy %>% filter(!is.na(vac.hesitancy)))
vac_accept_soccap_misinfo <- lm(vac.acceptance ~ sk2014 + misinfo + dec_gop + covidmortality + PCTPOVALL + Age65AndOlderPct2010 + WhiteNonHispanicPct2010 + per_col_ed + AsianNonHispanicPct2010 + BlackNonHispanicPct2010 + HispanicPct2010 + NativeAmericanNonHispanicPct2010 + RUCC, data=vac.hesitancy %>% filter(!is.na(vac.hesitancy)))

vac.hes.misinfo <- list("opc_min_misinfo" = vac_accept_opc_min_misinfo, "soccap_min_misinfo" = vac_accept_soccap_min_misinfo, "opc_full_misinfo" = vac_accept_opc_misinfo, "soccap_full_misinfo" = vac_accept_soccap_misinfo)

vac_hes_misinfo_models <- list(
  "Civic Opportunity (base)" = vac.hes.misinfo$opc_min_misinfo,
  "Civic Opportunity (full)" = vac.hes.misinfo$opc_full_misinfo,
  "Social Capital (base)" = vac.hes.misinfo$soccap_min_misinfo,
  "Social Capital (full)" = vac.hes.misinfo$soccap_full_misinfo
)

vac_hes_misinfo_cm <- c(
  "opc" = "Civic opportunity",
  "sk2014" = "Social capital (Rupasingha, et al.)",
  "misinfo" = "Misinformation",
  "dec_gop" = "Decile GOP presidential vote (%)",
  "covidmortality" = "Covid mortality rate",
  "WhiteNonHispanicPct2010" = "White Non-hispanic (%)",
  "Age65AndOlderPct2010" = "Age 65 or older (%)",
  "AsianNonHispanicPct2010" = "Asian (%)",
  "BlackNonHispanicPct2010" = "Black (%)",
  "HispanicPct2010" = "Hispanic (%)",
  "NativeAmericanNonHispanicPct2010" = "Native American (%)",
  "PCTPOVALL" = "Percent Poverty",
  "per_col_ed" = "College education rate",
  "RUCC" = "Rural/Urban continuity"
)

modelsummary(vac_hes_misinfo_models, 
             fmt = fmt_statistic("p.value" = fmt_sprintf("%.2e"),"estimate" = 3),
             vcov = "robust",
             estimate = c("{estimate}{stars} \n [{conf.low}, {conf.high}] \n p = {p.value}"),
             statistic = NULL,
             coef_omit = "Intercept",
             coef_map = vac_hes_misinfo_cm,
             output = here("outputs", "supplementary_table_11.docx")

################################################################################
# Supplementary Table S12 [Vaccine Uptake]
################################################################################

vac.hesitancy <- read_csv(here("processed_data", "regression_vaccine_uptake.csv"))

vac_uptake_opc_min <- lm(vpt.all ~ opc + dec_gop + Recip_State, data=vac.uptake %>% filter(vpt.all > 0, Recip_State != 'NH', Recip_State != 'CO', Recip_State !='TX', Recip_State !='HI'))
vac_uptake_soccap_min <- lm(vpt.all ~ sk2014 + dec_gop + Recip_State, data=vac.uptake %>% filter(vpt.all > 0, Recip_State != 'NH', Recip_State != 'CO', Recip_State !='TX', Recip_State !='HI'))

vac_uptake_opc <- lm(vpt.all ~ opc + dec_gop + covidmortality + PCTPOVALL + Age65AndOlderPct2010 + WhiteNonHispanicPct2010 + per_col_ed + AsianNonHispanicPct2010 + BlackNonHispanicPct2010 + HispanicPct2010 + NativeAmericanNonHispanicPct2010 + RUCC + Recip_State, data=vac.uptake %>% filter(vpt.all > 0, Recip_State != 'NH', Recip_State != 'CO', Recip_State !='TX', Recip_State !='HI'))
vac_uptake_soccap <- lm(vpt.all ~ sk2014 + dec_gop + covidmortality + PCTPOVALL + Age65AndOlderPct2010 + WhiteNonHispanicPct2010 + per_col_ed + AsianNonHispanicPct2010 + BlackNonHispanicPct2010 + HispanicPct2010 + NativeAmericanNonHispanicPct2010 + RUCC + Recip_State, data=vac.uptake %>% filter(vpt.all > 0, Recip_State != 'NH', Recip_State != 'CO', Recip_State !='TX', Recip_State !='HI'))

vac.uptake <- list("uptake_opc_min" = vac_uptake_opc_min, "uptake_soccap_min" = vac_uptake_soccap_min, "uptake_opc_full" = vac_uptake_opc, "uptake_soccap_full" = vac_uptake_soccap)

vac_uptake_models <- list(
  "Civic Opportunity (base)" = vac.uptake$uptake_opc_min,
  "Civic Opportunity (full)" = vac.uptake$uptake_opc_full,
  "Social Capital (base)" = vac.uptake$uptake_soccap_min,
  "Social Capital (full)" = vac.uptake$uptake_soccap_full
)

vac_uptake_cm <- c(
  "opc" = "Civic opportunity",
  "sk2014" = "Social capital (Rupasingha, et al.)",
  "dec_gop" = "Decile GOP presidential vote (%)",
  "covidmortality" = "Covid mortality rate",
  "Age65AndOlderPct2010" = "Age 65 or older (%)",
  "AsianNonHispanicPct2010" = "Asian (%)",
  "BlackNonHispanicPct2010" = "Black (%)",
  "HispanicPct2010" = "Hispanic (%)",
  "NativeAmericanNonHispanicPct2010" = "Native American (%)",
  "PCTPOVALL" = "Percent Poverty",
  "per_col_ed" = "College education rate",
  "RUCC" = "Rural/Urban continuity",
  "Recip_State" = "State"
)

modelsummary(vac_uptake_models, 
             fmt = fmt_statistic("p.value" = fmt_sprintf("%.2e"),"estimate" = 3),
             vcov = "robust",
             estimate = c("{estimate}{stars} \n [{conf.low}, {conf.high}] \n p = {p.value}"),
             statistic = NULL,
             coef_omit = "Intercept",
             coef_map = vac_uptake_cm,
             output = here("outputs", "supplementary_table_12.docx")

################################################################################
# Supplementary Table S14 [Vaccine Uptake - ZCTA] 
################################################################################

df_mn <- read_csv(here("processed_data", "regression_zcta_mn.csv"))
df_tx <- read_csv(here("processed_data", "regression_zcta_mn.csv"))
df_ny <- read_csv(here("processed_data", "regression_zcta_mn.csv"))

mn_vac_uptake_opc <- lm(vaccination_rate ~ per_white_nonhispanic + per_hispanic + per_black + insured_rate + education_col + poverty_100_149_rate + housing_vacant + age_lt_5 + age_65p + log_density + d.share + opc, data=df_mn %>% filter(population_total>100, vaccination_rate>0))
tx_vac_uptake_opc <- lm(vaccination_rate ~ per_white_nonhispanic + per_hispanic + per_black + insured_rate + education_col + poverty_100_149_rate + housing_vacant + age_lt_5 + age_65p + log_density + d.share + opc, data=df_tx %>% filter(population_total>100, vaccination_rate>0))
ny_vac_uptake_opc <- lm(vaccination_rate ~ per_white_nonhispanic + per_hispanic + per_black + insured_rate + education_col + poverty_100_149_rate + housing_vacant + age_lt_5 + age_65p + log_density + d.share + opc, data=df_ny %>% filter(population_total>100, vaccination_rate>0))

zcta_vaccine_models <- list("mn_opc" = mn_vac_uptake_opc, "tx_opc" = tx_vac_uptake_opc, "ny_opc" = ny_vac_uptake_opc)

zcta_uptake_models <- list(
  "ZCTA - MN" = zcta_vaccine_models$mn_opc,
  "ZCTA - TX" = zcta_vaccine_models$tx_opc,
  "ZCTA - NY" = zcta_vaccine_models$ny_opc
)

zcta_uptake_cm <- c(
  "opc" = "Civic opportunity",
  "d.share" = "Democratic vote share presidential vote (%)",
  "log_density" = "population density (log scale)",
  "poverty_100_149" = "Poverty rate",
  "age_65p" = "Age 65 or older (%)",
  "age_lt_5" = "Age younger than 5 (%)",
  "education_col" = "College educated (%)",
  "insured_rate" = "Health insured rate (%)",
  "per_white_nonhispanic" = "White Non-Hispanic(%)",
  "per_hispanic" = "Black (%)",
  "per_black" = "Hispanic (%)"
)

modelsummary(zcta_uptake_models, 
             fmt = fmt_statistic("p.value" = fmt_sprintf("%.2e"),"estimate" = 3),
             #vcov = "robust",
             estimate = c("{estimate}{stars} \n [{conf.low}, {conf.high}] \n p = {p.value}"),
             statistic = NULL,
             coef_omit = "Intercept",
             coef_map = zcta_uptake_cm,
             output = "here("outputs", "supplementary_table_14.docx")
