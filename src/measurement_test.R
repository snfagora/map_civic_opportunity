
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
  usdata,
  ggrepel,
  patchwork,
  glue,
  vroom,
  scales
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

# ad-hoc data analysis

sd(unit_cor_df[, 2] %>% unlist()) # sd binary index
sd(unit_cor_df[, 3] %>% unlist()) # sd mean index
sd(unit_cor_df[, 4] %>% unlist()) # sd inverse covariance matrix index
sd(unit_cor_df[, 5] %>% unlist()) # sd principal component first factor index

# correlation test 

options(scipen = 999) 

## Pearson 

### Kyne and Aldrich 
cor.test(dd$opc, dd$socialcap) 

### Rupasingha 
cor.test(dd$opc, dd$sk2014) 

### Chetty et al 
cor.test(dd$opc, dd$civic_organizations_county)

### Kyne and Aldrich 
cor.test(dd$opc, dd$socialcap) 

### Rupasingha 
cor.test(dd$opc, dd$sk2014) 

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
# FIGURE 3
################################################################################

mutual_aid_models <- read_rds(here("processed_data", "mutual_aid_models.rds"))

fig2a <- mutual_aid_models$opc_full
fit.val.fig2a <- fitted(fig2a)

fig2b <- mutual_aid_models$kyne_full
fit.val.fig2b <- fitted(fig2b)

fig2c <- mutual_aid_models$chetty_full
fit.val.fig2c <- fitted(fig2c)

fig2d <- mutual_aid_models$penn_full
fit.val.fig2d <- fitted(fig2d)

fig2.plot.data <- tt %>% 
  filter(!is.na(opc),!is.na(apc),!is.na(civic_organizations_county),!is.na(socialcap)) %>% 
  mutate(fit.val.fig2a = fit.val.fig2a, fit.val.fig2b = fit.val.fig2b, fit.val.fig2c = fit.val.fig2c, fit.val.fig2d = fit.val.fig2d)

gg.fig2a <- ggplot(fig2.plot.data %>% filter(opc != 0),aes(y=fit.val.fig2a,x=(log(opc)-min(log(opc)))/(max(log(opc)) - min(log(opc))))) + geom_point(aes(size=log(TotalPopEst2019), alpha=0.7), show.legend=FALSE) + theme_bw() + theme(axis.title.y=element_text(size=18), axis.title.x=element_text(size=18), axis.text.y = element_text(size=16), axis.text.x = element_text(size=16)) + xlab("Civic Opportunities Per Capita") + ylab("Presence of Covid Mutual Aid") + geom_smooth(method="lm")

gg.fig2b <- ggplot(fig2.plot.data,aes(y=fit.val.fig2b,x=(log(socialcap)-min(log(socialcap)))/(max(log(socialcap))-min(log(socialcap))))) + geom_point(aes(size=log(TotalPopEst2019), alpha=0.7), show.legend=FALSE) + theme_bw() + theme(axis.title.y=element_text(size=18), axis.title.x=element_text(size=18), axis.text.y = element_text(size=16), axis.text.x = element_text(size=16)) + xlab("Social Capital (Kyne and Aldrich)") + ylab("Presence of Covid Mutual Aid") + geom_smooth(method="lm")

gg.fig2c <- ggplot(fig2.plot.data,aes(y=fit.val.fig2c,x=(log(civic_organizations_county)-min(log(civic_organizations_county)))/(max(log(civic_organizations_county))-min(log(civic_organizations_county))) )) + geom_point(aes(size=log(TotalPopEst2019), alpha=0.7), show.legend=FALSE) + theme_bw() + theme(axis.title.y=element_text(size=18), axis.title.x=element_text(size=18), axis.text.y = element_text(size=16), axis.text.x = element_text(size=16)) + xlab("'Public Goods' Organizations (Chetty, et al.)") + ylab("Presence of Covid Mutual Aid") + geom_smooth(method="lm")

gg.fig2d <- ggplot(fig2.plot.data,aes(y=fit.val.fig2d,x=(log(sk2014+4)-min(log(sk2014+4)))/(max(log(sk2014+4))-min(log(sk2014+4))))) + geom_point(aes(size=log(TotalPopEst2019), alpha=0.7), show.legend=FALSE) + theme_bw() + theme(axis.title.y=element_text(size=18), axis.title.x=element_text(size=18), axis.text.y = element_text(size=16), axis.text.x = element_text(size=16)) + xlab("Social Capital (Rupasingha, et al.)") + ylab("Presence of Covid Mutual Aid)") + geom_smooth(method="lm")

(gg.fig2a + gg.fig2b)/(gg.fig2c + gg.fig2d) + plot_annotation(tag_levels = 'a')

# Save the plots as images
ggsave(here("outputs", "figure3.png"), combined_plots, width = 14, height = 14)

# for publication 
ggsave(here("plots", "figure3.pdf"), 
       width = 14,
       height = 14,
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

mods <- list(
  "DV: Civic opportunity" = lm(opc ~ per_poverty, data = dd), 
  "DV: Civic opportunity" = lm(opc ~ college_educ, data = dd),
  "DV: Civic opportunity" = lm(opc ~ race_per_white_nonhispanic, data = dd)
  )

mod_cm <- c(
  "per_poverty" = "Federal poverty level (%)",
  "college_educ" = "College educated (%)",
  "race_per_white_nonhispanic" = "White Non-hispanic (%)"
)
  
modelsummary(mods, 
             estimate = c("{estimate}{stars} \n [{conf.low}, {conf.high}] \n p = {p.value}"),
             statistic = NULL,
             coef_omit = "Intercept",
             coef_map = mod_cm,
             output = here("tables", "additional_table.docx")
             )
             
combined_plots <- wrap_plots(pred_plot(dd$per_poverty, "Federal poverty level"),
                             pred_plot(dd$college_educ, "College educated"),
                             pred_plot(dd$race_per_white_nonhispanic, "Non-Hispanic white"),
                             ncol = 3) +
  plot_annotation(tag_levels = "a") +
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

la_map_cnts <- merge(la_zcta_opc, la_zip_shp) 

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

national_map + la_map + plot_annotation(tag_levels = "a") + plot_layout(guides = "collect")

ggsave(file = here("outputs", "us_map.png"), width = 10, height = 7)

# for publication 
ggsave(here("plots", "figure1.pdf"), 
       width = 10,
       height = 7,
       device = "pdf")

################## Mutual aids map ##################

mutual_aids <- read_csv(here("processed_data", "regression_mutual_aid.csv"))

ma_geo <- mutual_aids %>%
  left_join(sj, by = c("FIPS" = "GEOID"))

mutual_aid_map <- ggplot() +
  geom_sf(data = ma_geo, aes(fill = as.factor(has_hubs), geometry = geometry), color = "white", size = 0.2) +
  geom_sf(data = sj, aes(geometry = geometry), fill = "transparent", color = gray(0.8), inherit.aes = FALSE) +
  geom_sf(data = sj_state, aes(geometry = geometry), fill = "transparent", color = "black", inherit.aes = FALSE) +
  scale_fill_manual(values = c("0" = "white", "1" = "green")) +  # Specify colors for factor levels
  theme_bw() +
  theme(legend.position = "bottom",
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(fill = "Has COVID-19 Mutual Aid Hubs?")

ggsave(here("outputs", "mutual_aid_map.png"))