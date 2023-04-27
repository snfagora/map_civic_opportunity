
if (!require(pacman)) install.packages("pacman")

pacman::p_load(dplyr, here, clipr, ggplot2, readr, gt, modelsummary)

ggplot2::theme_set(theme_minimal())

mods <- read_rds(here("processed_data", "mutual_aid_models.rds"))

modelsummary(mods)
