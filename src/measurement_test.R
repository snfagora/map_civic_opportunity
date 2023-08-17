
# import pkgs 

if (!require(pacman)) install.packages("pacman")

pacman::p_load(
  tidyverse,
  here,
  modelsummary,
  sf,
  tigris,
  tmap,
  RColorBrewer,
  usdata
  )

custom_theme <- function(size = 13) {
  theme_bw(base_size = size) +
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
      legend.position = "bottom"
    )
}

ggplot2::theme_set(custom_theme())

# import data 

# summary tables 
unit_cor_df <- read_csv(here("data_outputs", "unit_cor_df.csv"))
map_trade_off <- read_csv(here("data_outputs", "map_trade_off.csv"))
dd <- read_csv(here("data_outputs", "mma_sc_demo.csv"))

# for the map 

combined_unique <- read_rds(here("data_outputs", "county_opc.rds"))
la_zip_shp <- st_read(here("data_outputs", "la_zip_shp.shp"))
la_zcta_opc <- read_csv(here("data_outputs", "la_zcta_opc.csv"))

# ad hoc data analysis

sd(unit_cor_df[, 2] %>% unlist()) # sd binary index
sd(unit_cor_df[, 3] %>% unlist()) # sd mean index
sd(unit_cor_df[, 4] %>% unlist()) # sd inverse covariance matrix index
sd(unit_cor_df[, 5] %>% unlist()) # sd principal component first factor index

# correlation test 

options(scipen = 999) 

## Pearson 

### Kyne and Aldrich 
cor.test(dd$opc, dd$socialcap) %>%
  tidy()

### Rupasingha 
cor.test(dd$opc, dd$sk2014) %>%
  tidy()

### Chetty et al 
cor.test(dd$opc, dd$civic_organizations_county) %>%
  tidy()

### Kyne and Aldrich 
cor.test(dd$opc, dd$socialcap) %>%
  tidy()

### Rupasingha 
cor.test(dd$opc, dd$sk2014) %>%
  tidy()

cor.test(dd$opc, dd$civic_organizations_county)
p.adjust(cor.test(dd$opc, dd$civic_organizations_county)$p.value, n = 3127, method = "bonferroni")

paste("Degrees of freedom:", nrow(dd) - 2)

################################################################################
# FIGURE s1 
################################################################################

con_plot <- unit_cor_df %>%
  pivot_longer(matches("index")) %>%
  ggplot(aes(
    x = term, y = name,
    fill = value,
    label = round(value, 2)
  )) +
  geom_tile() +
  labs(
    x = "",
    y = "",
    fill = "Correlation coefficient",
    label = "Correlation coefficient"
  ) +
  scale_fill_gradient2(
    low = "white",
    high = "green",
    midpoint = 0.4,
    limit = c(0, 1),
    space = "Lab"
  ) +
  geom_label(
    col = "black",
    show.legend = FALSE
  ) +
  theme(legend.position = "none") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

con_plot

ggsave(here("outputs", "con_plot.png"),
       height = 5, width = 5
)

# for publication 
ggsave(here("plots", "figure_s1.pdf"), 
       width = 5,
       height = 5,
       device = "pdf")

################################################################################
# FIGURE s2
################################################################################

map_trade_off %>%
  filter(State != "DC") %>%
  ggplot(aes(x = pct_lower_cnts, y = pct_higher_cnts, label = State)) +
  geom_text_repel() +
  coord_flip() +
  labs(
    x = "% of 1 or 2 graded counties",
    y = "% of 4 or 5 graded counties"
  ) +
  scale_x_continuous(label = scales::percent) +
  scale_y_continuous(label = scales::percent) +
  geom_vline(xintercept = 0.5, linetype = 2) +
  geom_hline(yintercept = 0.5, linetype = 2)

ggsave(here("outputs", "map_trade_off.png"), height = 6, width = 6)

# for publication 
ggsave(here("plots", "figure_s2.pdf"), 
       width = 6,
       height = 6,
       device = "pdf")

################################################################################
# FIGURE 2
################################################################################

pred_plot <- function(var, x_label) {
  
  ci95 <- predict(lm(opc ~ var, data = dd), dd, interval = "confidence", level = 0.95) %>%
    data.frame()
  
  dd %>%
    bind_cols(ci95) %>%
    ggplot(aes(y = opc, x = var)) +
    geom_jitter(alpha = 0.05) +
    geom_line(aes(y = fit, col = "OLS fit")) +
    geom_ribbon(aes(ymax = upr, ymin = lwr, col = "95% CIs"), alpha = 0.07) +
    labs(
      y = glue("Civic opportunity scores"),
      x = x_label,
      col = "Indicators"
    ) +
    scale_x_continuous(label = scales::percent, limits = c(0, 1)) +
    ylim(c(0, 5)) 
}

combined_plots <- wrap_plots(pred_plot(dd$per_poverty, "Federal poverty level"),
                             pred_plot(dd$college_educ, "College educated"),
                             pred_plot(dd$race_per_white_nonhispanic, "Non-Hispanic white"),
                             ncol = 3) +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect")

combined_plots

# Save the plots as images
ggsave(here("outputs", "civic_inequ_asso.png"), combined_plots, width = 9, height = 7)

# for publication 
ggsave(here("plots", "figure2.pdf"), 
       width = 9,
       height = 7,
       device = "pdf")

################################################################################
# FIGURE 1
################################################################################

sj <- tigris::counties(cb = TRUE) %>%
  mutate(id = row_number())

sj <- sj %>%
  filter(!(STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))) # Sorry Alaska, Hawaii, and US Territories

sj_state <- tigris::states(cb = TRUE) %>%
  filter(!(STATEFP %in% c("02", "15", "60", "66", "69", "72", "78"))) # Sorry Alaska, Hawaii, and US Territories

# Define the color palette
my_palette <- RColorBrewer::brewer.pal(5, "Greens")

national_map <- ggplot() +
  geom_sf(data = combined_unique, aes(fill = as.factor(opc_tile), geometry = geometry), color = "white", size = 0.2) +
  scale_fill_manual(labels = c(1:5, "No Data"), values = my_palette) +
  geom_sf(data = sj, aes(geometry = geometry), fill = "transparent", color = gray(0.8), inherit.aes = FALSE) +
  geom_sf(data = sj_state, aes(geometry = geometry), fill = "transparent", color = "black", inherit.aes = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom", axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(fill = "Civic opportuntiy index")

la_map_cnts <- merge(la_zcta_cnts, la_zip_shp) 

# Skip the islands in the LA map
la_map_cnts <- la_map_cnts %>%
  filter(!(ZCTA %in% c("90704", "90731")))

la_map <- ggplot() +
  geom_sf(data = la_map_cnts, aes(fill = as.factor(opc_tile), geometry = geometry), color = "white", size = 0.2) +
  scale_fill_manual(labels = c(1:5, "No Data"), values = my_palette) +
  geom_sf(data = la_zip_shp, aes(geometry = geometry), fill = "transparent", color = gray(0.8), inherit.aes = FALSE) +
  theme_bw() +
  theme(legend.position = "bottom", axis.line = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank()) +
  labs(fill = "Civic opportuntiy index")

national_map + la_map + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")

ggsave(file = here("outputs", "us_map.png"), width = 10, height = 7)

# for publication 
ggsave(here("plots", "figure1.pdf"), 
       width = 10,
       height = 7,
       device = "pdf")