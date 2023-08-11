
if (!require(pacman)) install.packages("pacman")

pacman::p_load(dplyr, here, clipr, ggplot2, readr, gt)

ggplot2::theme_set(theme_minimal())

dodge <- position_dodge(width = 0.9)

# Prediction evaluations
eval_out <- read_csv(here("data_outputs", "pred_eval.csv"))

eval_summary <- eval_out %>%
  mutate(.metric = case_match(
    .metric,
    "accuracy" ~ "Accuracy",
    "bal_accuracy" ~ "Balanced accuracy",
    "f_meas" ~ "F-score" # Harmonic mean of precision and recall values
  )) %>%
  mutate(class = recode(class,
    "civic" = "political"
  )) %>%
  mutate(.estimate = round(.estimate, 2))

eval_summary %>%
  ggplot(aes(x = model, y = round(.estimate, 2), col = class)) +
  geom_point() +
  coord_flip() +
  facet_wrap(~.metric) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom")

ggsave(here("outputs", "summary.png"), width = 10, height = 8)

################################################################################
# Table S2
################################################################################

class_vec <- eval_summary$class %>% unique()

eval_table <- eval_summary %>%
  select(model, class, .metric, .estimate) %>%
  mutate(class = case_match(
    class,
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
    class_vec[15] ~ "Youth"
  )) %>%
  rename(
    "Model" = model,
    "Class" = class,
    "Metric" = .metric,
    "Estimate" = .estimate
  ) 

eval_table <- eval_table %>%
  arrange(Class) %>%
  gt() 

gtsave(eval_table, here("outputs", "individual_eval_out.docx"))

# for publication 
gtsave(eval_table, here("tables", "table_s2.docx"))

################################################################################
# Table S3
################################################################################

pooled_out <- eval_out %>%
  group_by(model, .metric) %>%
  summarize(mean = round(mean(.estimate), 2)) %>%
  gt()

gtsave(pooled_out, here("outputs", "pooled_out.docx"))

# for publication 
gtsave(pooled_out, here("tables", "table_s3.docx"))
