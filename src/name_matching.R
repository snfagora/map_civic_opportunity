# install pkgs
if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  tidyverse, 
  here, 
  glue, 
  haven, 
  vroom,
  fedmatch, 
  fastLink, 
  purrr, 
  future, 
  furrr,
  broom,
  MKinfer,
  modelsummary,
  gt,
  ggalluvial,
  ggrepel,
  patchwork
)

# load data

## sav file (Washington data)

dc_file_name <- list.files(here("raw_data"))[str_detect(list.files(here("raw_data")), "WR")]

washington <- haven::read_sav(here("raw_data", dc_file_name))

washington$lobby <- if_else(washington$lobbydum01 == 1 | washington$lobbydum11 == 1, 1, 0)
washington$lobby[is.na(washington$lobby)] <- 0

washington$lobby %>% mean()

washington$orgidno %>%
  unique() %>%
  length() # 40,782

washington %>%
  distinct(orgidno, lobby) %>%
  summarize(lobby_pct = mean(lobby)) # 33%

#' codebook
#' 101-don't know (876)
#' 201-corporation  (14409)
#' 202-American subsidiary of a foreign corp (612)
#' 203-cooperative (182)
#' 204-trade associations (2634)
#' 205-business coalition (1189)
#' 206-firm of professionals (1264)
#' 220-
#' 301-306 occupational associations
#' 303-professional associations
#' 401-407 unions
#' 501-606 farms
#' 701-702 health
#' 801-806 US government
#' 1001-1006 Foreign government and organizations
#' 1101-1116 public interest
#' 1201-PAC
#' 1202-Party
#' 1301-1302 Veterans
#' 1401-1417 Civil rights
#' 1501-1502 Age
#' 1601-1701 Gender
#' 1801-1802 Disabled
#' 1901-1903 Social welfare
#' 2001-Recreational
#' 2101-Arts/cultural
#' 2201-Charity/Philanthropy
#' 2301-2304 Think thank/research
#' 2401-Others

not_this <- c(
  101, # don't know
  201:202, # corp
  203, # coops
  501:606, # farms
  801:806, # US Govt
  1001:1006, # Foreign govt and orgs
  1201, # PAC
  1202, # Part
  2401
) # Others

washington <- washington %>%
  filter(!category %in% not_this) %>%
  filter(location != "")

washington %>%
  group_by(category) %>%
  summarize(cat_n = n()) %>%
  arrange(desc(cat_n))

washington <- washington %>%
  mutate(
    cat_union = if_else(category %in% c(401:407), 1, 0),
    cat_prof = if_else(category %in% c(204:303), 1, 0)
  )

washington %>%
  summarize(
    sum_union = sum(cat_union),
    sum_prof = sum(cat_prof)
  )

# 204 unions, 4,800 professional organizations (broadly construed)

## MMA data

mma <- vroom::vroom(here("raw_data", "irs_mbf.csv"))
pred <- vroom::vroom(here("raw_data", "predictions.csv"))
civic_orgs <- vroom::vroom(here("processed_data", "civic_orgs.csv"))

mma <- left_join(mma, pred %>%
  select(ein, predicted)) 

# inspect data

## summary

washington %>%
  group_by(location) %>%
  summarize(
    lobby_n = sum(lobby, na.rm = T),
    n = n()
  ) %>%
  mutate(freq = lobby_n / n) %>%
  filter(freq != 1) %>%
  arrange(desc(lobby_n))

# cleaning

washington$orgname <- tolower(washington$orgname) %>% trimws()
mma$name <- tolower(mma$name) %>% trimws()

washington$orgname <- clean_strings(washington$orgname)
mma$name <- clean_strings(mma$name)

# raw status

intersect(washington$orgname, mma$name) %>%
  unique() %>%
  length() # only 2,020 names are matched

# fuzzy name matching

states <- intersect(washington$location, mma$state)

## Using fedmatch

washington$orgidno[washington$orgidno %>% duplicated()]

washington_copy <- washington

washington <- washington %>%
  distinct(orgidno, orgname, lobby, location)

fuzzy_match <- function(state) {
  # state <- "DC"
  message(state)

  matched <- fedmatch::merge_plus(
    data1 = washington %>%
      filter(location == state),
    data2 = mma %>%
      filter(state == state),
    match_type = "fuzzy",
    by.x = "orgname",
    by.y = "name",
    unique_key_1 = "orgidno",
    unique_key_2 = "ein",
    suffixes = c("_1", "_2"),
    fuzzy_settings = build_fuzzy_settings(
      maxDist = .5,
      method = "wgt_jaccard",
      nthread = 2
    )
  )

  matched$matches <- matched$matches %>%
    distinct(name, ein, orgidno, predicted, lobby) %>%
    as_tibble() %>%
    mutate(state = state)

  return(matched$matches)
}

final_df <- map_dfr(states, fuzzy_match)

# Save the result

write_rds(final_df, here("processed_data", "matched.rds"))

final_df <- read_rds(here("processed_data", "matched.rds"))

5968 / 9673 # 62%
2020 / 9673 # 21%

# simplify by it one orgidno -> one org

set.seed(1234)

# final_df <- final_df %>%
#   filter(!is.na(predicted)) %>%
#   group_by(orgidno) %>%
#   slice_sample(n = 1)

final_df$lobby %>% mean() # 41%

# check the missingness

final_df %>%
  group_by(state) %>%
  summarize(
    lobby_n = sum(lobby, na.rm = T),
    n = n()
  ) %>%
  mutate(freq = lobby_n / n) %>%
  filter(freq != 1) %>%
  arrange(desc(lobby_n))

missed <- setdiff(washington$orgidno, final_df$orgidno)

washington <- washington %>%
  mutate(missed = if_else(orgidno %in% missed, 1, 0))

# join the two tables

combined_df <- left_join(final_df %>% distinct(ein, state, lobby, name), mma)

combined_df <- combined_df %>%
  select(ein, state, lobby, name, predicted, ruling)

vroom_write(combined_df, here("processed_data", "joined_df.csv"))

# reproducible data 

vroom_write(combined_df, here("data_outputs", "matched_mma_wa.csv"))

civic_mma <- civic_orgs %>% 
  left_join(mma)

civic_freq <- civic_mma %>%
  group_by(predicted) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  arrange(desc(freq))

civic_freq$freq[1] + civic_freq$freq[2]

mma_lobby_n <- civic_mma %>%
  filter(!is.na(predicted)) %>%
  group_by(predicted) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(source = "Civic Opportunity Organizations")

joined_lobby_df <- full_join(w_lobby_n, mma_lobby_n)

class_vec <- joined_lobby_df$predicted %>% unique()

joined_lobby_df <- joined_lobby_df %>%
  mutate(class = case_match(predicted,
                            class_vec[1] ~ "Arts & Cultural",
                            class_vec[2] ~ "Political",
                            class_vec[3] ~ "Community",
                            class_vec[4] ~ "Economic",
                            class_vec[5] ~ "Education",
                            class_vec[6] ~ "Foundations",
                            class_vec[7] ~ "Healthcare",
                            class_vec[8] ~ "Hobby & Sports",
                            class_vec[9] ~ "Housing",
                            class_vec[10] ~ "Professional",
                            class_vec[11] ~ "Religious",
                            class_vec[12] ~ "Research & Think Tank",
                            class_vec[13] ~ "Social & Fraternal",
                            class_vec[14] ~ "Unions",
                            class_vec[15] ~ "Youth")) %>%
  mutate(type = factor(source, levels = c("DC Organizations", "Civic Opportunity Organizations")))

joined_lobby_df <- joined_lobby_df %>%
  select(class, n, freq, type)

write_csv(joined_lobby_df, here("data_outputs", "matched_mma_wa_summary.csv"))

civic_mma <- civic_orgs %>% 
  left_join(mma)

civic_mma$irs_year <- substr(civic_mma$ruling, start = 1, stop = 4) %>% as.numeric()

civic_mma_filtered <- civic_mma %>%
  filter(irs_year != 0)

civic_flow_df <- civic_mma_filtered %>%
  mutate(period = case_when(
    irs_year  < 1960 ~ "Pre-1960",
    irs_year >= 2010 ~ "Post-2010",
    .default = NA)) %>%
  filter(!is.na(period)) %>%
  mutate(class = case_match(predicted,
                            class_vec[1] ~ "Arts & Cultural",
                            class_vec[2] ~ "Political",
                            class_vec[3] ~ "Community",
                            class_vec[4] ~ "Economic",
                            class_vec[5] ~ "Education",
                            class_vec[6] ~ "Foundations",
                            class_vec[7] ~ "Healthcare",
                            class_vec[8] ~ "Hobby & Sports",
                            class_vec[9] ~ "Housing",
                            class_vec[10] ~ "Professional",
                            class_vec[11] ~ "Religious",
                            class_vec[12] ~ "Research & Think Tank",
                            class_vec[13] ~ "Social & Fraternal",
                            class_vec[14] ~ "Unions",
                            class_vec[15] ~ "Youth")) 

civic_flow_sum <- civic_flow_df %>%
  filter(!is.na(predicted)) %>%
  group_by(period, class) %>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  mutate(period = factor(period, levels = c("Pre-1960", "Post-2010")))

options(ggrepel.max.overlaps = Inf)

civic_flow_sum$flow_change <- rep(civic_flow_sum$freq[1:15] - civic_flow_sum$freq[16:30], 2)

civic_flow_sum <- civic_flow_sum %>%
  mutate(class = fct_reorder(class, flow_change)) %>%
  mutate(dir = ifelse(flow_change > 0, "Increase", "Decrease"))

civic_flow_sum %>%
  filter(str_detect(period, "Post")) %>%
  arrange(desc(freq))

write_csv(civic_flow_sum, here("data_outputs", "org_flow_over_time.csv"))

all_period_table <- left_join(
  civic_flow_df %>%
    group_by(class) %>%
    count() %>%
    rename(all = n),
  
  civic_flow_df %>%
    group_by(class, period) %>%
    count() %>%
    pivot_wider(
      names_from = period, 
      values_from = n
    )) %>%
  filter(!is.na(class)) 

write_csv(all_period_table, here("data_outputs", "org_volume_over_time.csv"))