custom_theme <- function() {
  
  theme_bw(base_size = 13) +
    theme(
      aspect.ratio = 1.2,
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_text(margin = margin(t = 6)),
      plot.title = element_text(size = 12),
      plot.subtitle = element_text(size = 10),
      plot.caption = element_text(colour = "grey50", hjust = 0),
      legend.position = "right"
    )
  
}

cut_pop <- function(n_top) {
  
  top_fips <- dd %>%
    ungroup() %>%
    group_by(urban_suburban_rural) %>%
    slice_max(order_by = TotalPopEst2019, prop = n_top) %>%
    pull(FIPS)
  
  dd <- dd %>%
    mutate(top = ifelse(FIPS %in% top_fips, 1, 0))
  
  out <- dd %>%
    filter(top == 1) %>%
    ungroup() %>%
    group_by(urban_suburban_rural) %>% 
    #top50) %>%
    do(tidy(standardize(lm_robust(formula = opc_tier ~ race_per_white_nonhispanic + per_poverty + college_educ + foreign_born_pct, data = ., weights = TotalPopEst2019)))) %>% 
    filter(!str_detect(term, "(Intercept)")) %>%
    mutate(top_n = n_top)
  
  return(out)
  
}