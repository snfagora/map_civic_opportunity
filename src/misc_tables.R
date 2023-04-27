
if (!require(pacman)) install.packages("pacman")

pacman::p_load(dplyr, here, clipr, ggplot2, readr, gt)

ggplot2::theme_set(theme_minimal())

dodge <- position_dodge(width = 0.9)

# Prediction evaluations 

eval_out <- read_csv(here("processed_data", "pred_eval_out.csv"))

eval_summary <- eval_out %>%
  mutate(.metric = case_match(.metric,
                              "accuracy" ~ "Accuracy",
                              "bal_accuracy" ~ "Balanced accuracy",
                              "f_meas" ~ "F-score" # Harmonic mean of precision and recall values
  )) %>%
  mutate(class = recode(class,
                        "civic" = "political")) %>%
  mutate(.estimate = round(.estimate, 2))

eval_summary %>%
  ggplot(aes(x = model, y = round(.estimate, 2), col = class)) +
  geom_point() +
  coord_flip() +
  facet_wrap(~.metric) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom")

ggsave(here("outputs", "summary.png"), width = 10, height = 8)

# Class vector

class_vec <- eval_summary$class %>% unique()

eval_summary %>%
  select(model, class, .metric, .estimate) %>%
  mutate(class = case_match(class,
                            class_vec[1] ~ "Arts & Cultural",
                            class_vec[2] ~ "Political",
                            class_vec[3] ~"Community",
                            class_vec[4] ~"Economic",
                            class_vec[5] ~"Education",
                            class_vec[6] ~"Foundations",
                            class_vec[7] ~"Healthcare",
                            class_vec[8] ~"Hobby & Sports",
                            class_vec[9] ~"Housing",
                            class_vec[10] ~"Professional",
                            class_vec[11] ~"Religious",
                            class_vec[12] ~"Research & Think Tank",
                            class_vec[13] ~"Social & Fraternal",
                            class_vec[14] ~"Unions",
                            class_vec[15] ~"Youth")) %>%
  rename("Model" = model,
         "Class" = class,
         "Metric" = .metric,
         "Estimate" = .estimate) %>%
  arrange(Class) %>%
  gt() %>%
  gtsave(here("outputs", "individual_eval_out.rtf"))

pooled_out <- eval_out %>%
  group_by(model, .metric) %>%
  summarize(mean = round(mean(.estimate), 2))

pooled_out %>%
  gt() %>%
  gtsave(here("outputs", "pooled_out.rtf"))

# Misc summary tables 

dist_org_cat <- read_csv(here("processed_data", "dist_org_cat.csv"))

dist_org_act <- read_csv(here("processed_data", "dist_org_act.csv"))

dist_org_cat %>%
  mutate(number = scales::comma(number)) %>%
  rename(Category = category,
         Number = number,
         Percentage = pct) %>%
  gt() %>%
  gtsave(here("outputs", "dist_org_cat.rtf"))

dist_org_act %>%
  rename(Category = category,
         Number = number,
         Percentage = pct) %>%
  gt() %>%
  gtsave(here("outputs", "dist_org_act.rtf"))