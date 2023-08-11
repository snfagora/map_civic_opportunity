
if (!require(pacman)) install.packages("pacman")

pacman::p_load(dplyr, here, clipr, ggplot2, readr, gt, modelsummary, stringr, effectsize, datawizard, purrr)

ggplot2::theme_set(theme_minimal())

desc_files <- list.files(here("processed_data"))[stringr::str_detect(list.files(here("processed_data")), "txt")]

desc_files

rds_files <- list.files(here("processed_data"))[stringr::str_detect(list.files(here("processed_data")), "rds")]

# Mutual aid model 

mad <- read_rds(here("processed_data", "mutual_aid_models.rds"))

mad_models <- list(
   "Civic Opportunity (base)" = mad$opp_orgs_min,
   "Civic Opportunity (full)" = mad$opp_orgs_full,
   "Organizational density (base)" = mad$all_orgs_min,
   "Organizational density (full)" = mad$all_orgs_full,
   "Social capital (full)" = mad$soc_cap_full
)

mad_cm <- c(
  "opc" = "Civic opportunity",
  "apc" = "Organizational density",
  "socialcap" = "Social capital",
  "gop.tier" = "GOP presidential vote (%)",
  "Age65AndOlderPct2010" = "Age 65 or older (%)",
  "AsianNonHispanicPct2010" = "Asian (%)",
  "BlackNonHispanicPct2010" = "Black (%)",
  "HispanicPct2010" = "Hispanic (%)",
  "NativeAmericanNonHispanicPct2010" = "Native American (%)",
  "RUCC" = "Rural/Urban continuity"
)

modelsummary(mad_models, 
             fmt = 3,
             vcov = "robust",
             estimate = c("{estimate} ({std.error}){stars}"),
             statistic = NULL,
             coef_omit = "Intercept",
             coef_map = mad_cm,
             output = here("outputs", "reg_base.docx"))