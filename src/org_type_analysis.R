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

# read data 

combined_df <- vroom::vroom(here("data_outputs", "matched_mma_wa.csv"))

joined_lobby_df <- read_csv(here("data_outputs", "matched_mma_wa_summary.csv"))

civic_flow_sum <- read_csv(here("data_outputs", "org_flow_over_time.csv"))

all_period_table <- read_csv(here("data_outputs", "org_volume_over_time.csv"))

class_vec <- c("arts", "civic", "community", "econ", "education", "foundation", "health", "hobby", "housing", "professional", "religious", "research", "socialfraternal", "unions", "youth")

# summary tables

dc_table <- combined_df %>%
  filter(!is.na(predicted)) %>%
  # mutate(lobby = tidyr::replace_na(lobby, 0)) %>%
  group_by(predicted) %>%
  summarize(
    n = n(),
    lobby_n = sum(lobby, na.rm = T)
  ) %>%
  mutate(lobby_pct = lobby_n / sum(lobby_n)) %>%
  select(predicted, lobby_n, lobby_pct) 

write_csv(dc_table, here("processed_data", "dc_table.csv"))

dc_table <- dc_table %>%
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

gt_dc_table <- dc_table %>%
  arrange(desc(lobby_pct)) %>%
  select(class, lobby_n, lobby_pct) %>%
  gt() %>%
  fmt_percent(
    columns = c("lobby_pct"),
    decimals = 1
  ) %>%
  cols_label(
    class = "Predicted category",
    lobby_n = "The number of lobbying organizations",
    lobby_pct = "The percentage of lobbying organizations"
  )

################################################################################
# Table S15
################################################################################

gtsave(gt_dc_table, here("outputs", "dc_lobby_summary.rtf"))

# for publication
gtsave(gt_dc_table, here("tables", "table_s15.docx"))

w_lobby_n <- combined_df %>%
  filter(!is.na(predicted)) %>%
  group_by(predicted) %>%
  summarize(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  mutate(source = "DC Organizations")

cross_sec_plot <- joined_lobby_df %>%
  mutate(type = factor(type, levels = c("DC Organizations", "Civic Opportunity Organizations"))) %>%
  ggplot(aes(x = fct_reorder(class, freq), y = freq, fill = type)) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "", y = "Proportion", fill = "Type") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "bottom") 

cross_sec_plot

ggsave(here("outputs", "lobby_civic_pct.png"), height = 8, width = 5)

civic_flow_plot <- civic_flow_sum %>%
  mutate(period = factor(period, levels = c("Pre-1960", "Post-2010"))) %>%
  mutate(class = fct_reorder(class, flow_change)) %>%
  ggplot(aes(x = period,
             y = freq, 
             stratum = class, 
             alluvium = flow_change, 
             fill = dir,
             label = class)) + 
  geom_flow(stat = "alluvium",
            color = "black",
            cement.alluvia = TRUE,
            aes(fill = dir),
            aes.bind = "flows") +  
  geom_stratum(alpha = .25) +
  geom_label_repel(stat = "alluvium", 
                   size = 3.5, 
                   cement.alluvia = TRUE,
                   show.legend = FALSE) +
  labs(x = "", 
       y = "Proportion", 
       fill = "Proportion change",
       label = "Proportion change") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.position = "bottom")

civic_flow_plot

ggsave(here("outputs", "alluvial.png"), height = 8, width = 8)

# for publication

civic_flow_plot + cross_sec_plot + plot_annotation(tag_levels = "a")

ggsave(here("outputs", "cross_flow.png"),
       height = 7, width = 10)

################################################################################
# FIGURE 4
################################################################################

civic_flow_plot + cross_sec_plot + plot_annotation(tag_levels = "a")

ggsave(here("plots", "figure4.pdf"), 
       width = 10,
       height = 7,
       device = "pdf")

all_period_gt <- all_period_table %>%
  as_tibble() %>%
  select(class, all, `Pre-1960`, `Post-2010`) %>%
  gt() %>%
  cols_label(
    class = "Predicted category",
    all = "All civic opportunity organizations",
    `Post-2010` = "Pre-1960 civic opportunity organizations",
    `Pre-1960` = "Post-2010 civic opportunity organizations"
  )

################################################################################
# Table S16
################################################################################

gtsave(all_period_gt, here("outputs", "all_period_gt.rtf"))

# for publication
gtsave(all_period_gt, here("tables", "table_s16.docx"))