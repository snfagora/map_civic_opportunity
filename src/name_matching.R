# install pkgs 
if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse, here, haven, vroom, 
               fedmatch, fastLink, # fuzzy matching methods
               purrr, future, furrr,
               broom,
               MKinfer,
               modelsummary,
               gt)

# load data 
## sav file (Washington data)

dc_file_name <- list.files(here("raw_data"))[str_detect(list.files(here("raw_data")), "WR")]

washington <- haven::read_sav(here("raw_data", dc_file_name))

washington$lobby <- if_else(washington$lobbydum01 == 1 | washington$lobbydum11 == 1, 1, 0) 
washington$lobby[is.na(washington$lobby)] <- 0

washington$lobby %>% mean()

washington$orgidno %>% unique() %>% length() #40,782

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

not_this <- c(101, #don't know  
              201:202, #corp
              203, # coops 
              501:606, #farms
              801:806, #US Govt 
              1001:1006, #Foreign govt and orgs 
              1201, #PAC 
              1202, #Part
              2401) #Others

washington <- washington %>%
  filter(!category %in% not_this) %>%
  filter(location != "")

washington %>%
  group_by(category) %>%
  summarize(cat_n = n()) %>%
  arrange(desc(cat_n)) 

washington <- washington %>%
  mutate(cat_union = if_else(category %in% c(401:407), 1, 0),
         cat_prof = if_else(category %in% c(204:303), 1, 0)) 

washington %>%
  summarize(sum_union = sum(cat_union),
            sum_prof = sum(cat_prof))
#204 unions, 4,800 professional organizations (broadly construed)

## MMA data 

mma <- vroom::vroom(here("raw_data", "irs_mbf.csv"))
pred <- vroom::vroom(here("raw_data", "predictions.csv"))
activities <- vroom::vroom(here("raw_data", "activities.csv"))

mma <- left_join(mma, 
                 pred %>%
                   select(ein, predicted))

# inspect data 

## summary 

washington %>%
  group_by(location) %>%
  summarize(lobby_n = sum(lobby, na.rm = T), 
            n = n()) %>%
  mutate(freq = lobby_n/n) %>%
  filter(freq != 1) %>%
  arrange(desc(lobby_n))

# cleaning 

washington$orgname <- tolower(washington$orgname) %>% trimws()
mma$name <- tolower(mma$name) %>% trimws()

washington$orgname <- clean_strings(washington$orgname)
mma$name <- clean_strings(mma$name)

# raw status 
intersect(washington$orgname, mma$name) %>% unique() %>% length() # only 2,020 names are matched 

# fuzzy name matching
states <- intersect(washington$location, mma$state)

## Using fedmatch 

washington$orgidno[washington$orgidno %>% duplicated()]

washington_copy <- washington 

washington <- washington %>%
  distinct(orgidno, orgname, lobby, location)

fuzzy_match <- function(state) {
  
  #state <- "DC"
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
    fuzzy_settings = build_fuzzy_settings(maxDist = .5,
                                          method = "wgt_jaccard", 
                                          nthread = 2
    ))
  
  matched$matches <- matched$matches %>%
    distinct(name, ein, orgidno, predicted, lobby) %>%
    as_tibble() %>%
    mutate(state = state)
  
  return(matched$matches)
}

plan(multiprocess, workers = 7)
options(future.globals.maxSize= 1000000000)

final_df <- map_dfr(states, fuzzy_match)

# Save the result

write_rds(final_df, here("processed_data", "matched.rds"))

final_df <- read_rds(here("processed_data", "matched.rds"))

5968/9673 #62%
2020/9673 #21%

# simplify by it one orgidno -> one org

set.seed(1234)

# final_df <- final_df %>%
#   filter(!is.na(predicted)) %>%
#   group_by(orgidno) %>%
#   slice_sample(n = 1)

final_df$lobby %>% mean() #42%

# check the missingness 

final_df %>%
  group_by(state) %>%
  summarize(lobby_n = sum(lobby, na.rm = T),
            n = n()) %>%
  mutate(freq = lobby_n/n) %>%
  filter(freq != 1) %>%
  arrange(desc(lobby_n))

missed <- setdiff(washington$orgidno, final_df$orgidno)

washington <- washington %>%
  mutate(missed = if_else(orgidno %in% missed, 1, 0)) 

1 - (washington$missed %>% mean())

t.test.out <- washington %>%
  filter(location %in% states) %>%
  filter(location != "AS") %>%
  mutate(location = factor(location)) %>%
  group_by(location) %>%
  summarize(tidy(t.test(lobby, missed, data = .)))

t.test.out %>%
  ggplot(aes(x = fct_reorder(location, p.value), y = p.value)) +
  geom_point() +
  coord_flip() +
  geom_hline(yintercept = 0.01, linetype = "dashed", color = "red") +
  labs(x = "",
       subtitle = "The dotted vertical line indicates p.value = 0.01")

ggsave(here("outputs", "t_test.png"), height = 8, width = 10)

# join the two tables

combined_df <- left_join(final_df %>% distinct(ein, state, lobby, name), mma)

vroom_write(combined_df, here("processed_data", "joined_df.tsv.gz"))

# intersect 
dc_table <- combined_df %>%
  filter(!is.na(predicted)) %>%
  #mutate(lobby = tidyr::replace_na(lobby, 0)) %>%
  group_by(predicted) %>%
  summarize(n = n(),
            lobby_n = sum(lobby, na.rm = T)) %>%
  mutatea(lobby_pct = lobby_n/n) %>%
  select(predicted, lobby_n, lobby_pct)

dc_table

write_csv(dc_table, here("processed_data", "dc_table.csv"))

gt_dc_table <- dc_table %>%
  gt() %>%
  fmt_percent(
    columns = c("lobby_pct"),
    decimals = 1
  ) %>%
  cols_label(predicted = "Predicted", 
             lobby_n = "Lobby Org N",
             lobby_pct = "Lobby Org %")

gtsave(gt_dc_table, here("outputs", "summary.rtf"))
gtsave(gt_dc_table, here("outputs", "summary.png"))

w_lobby_n <- combined_df %>%
  filter(!is.na(predicted)) %>%
  group_by(predicted) %>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  mutate(source = "Washington")

mma_lobby_n <- mma %>%
  filter(!is.na(predicted)) %>%
  group_by(predicted) %>%
  summarize(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  mutate(source = "MMA")

full_join(w_lobby_n, mma_lobby_n) %>%
  ggplot(aes(x = fct_reorder(predicted, freq), y = freq, fill = source)) +
  geom_col(posiiton = position_dodge2(width = 2)) +
  theme(legend.position = "bottom") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "%")

ggsave(here("outputs", "lobby_civic_pct.png"), height = 12, width = 10)