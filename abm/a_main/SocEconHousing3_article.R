### A socio-economic Model of Residential Segregation, Neighborhood Change and Housing Inequality
### Tables and Figures for the Article
### Malte Grönemann


# Documentation

# This script exports selected tables and figures from the analyses for the article.
# The data for the selected analyses come from multiple computational experiments.
# While the city and neighborhood data come from the main experiment, the individual 
# level analyses are only shown for plausible parameter values, therefore are based 
# on the "hypotheses" experiment.


# Libraries and Data
library(nanoparquet)
library(dplyr)
library(tidyr)
library(broom)
library(Hmisc)
library(ggplot2)
library(latex2exp)
library(ggh4x)

data_directory <- "./data/main/" # TODO change directory

unit_data <- read_parquet(paste(data_directory, "unit_data.parquet", sep = ""))
nb_data <- read_parquet(paste(data_directory, "nb_data.parquet", sep = ""))
city_data <- read_parquet(paste(data_directory, "city_data.parquet", sep = ""))


label_r <- c(`0` = "No Correlation bw. Income and Status", `0.5` = "Realistic Correlation (r = 0.71)", `1` = "Perfect Correlation")
label_a <- c(`0` = "Preferences: Only Housing Quality", `0.25` = "Realistic: Housing Quality > Neighborhood Status", `1` = "Only Neighborhood Status")

# colourblind-friendly and black-and-white compatible differentiation aesthetics
# adopted from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/#a-colorblind-friendly-palette
cb_palette <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9")
lines <- c("solid", "twodash", "longdash", "dotdash")
shapes <- c(15, 16, 17, 18)


# Figure Segregation
seg_data <- city_data %>%
  select(-ends_with("gini")) %>%
  pivot_longer(cols = ends_with("seg"),
               cols_vary = "slowest",
               names_to = "dimension",
               values_to = "segregation") %>%
  mutate(dimension = if_else(dimension == "inc_seg", "Income", dimension),
         dimension = if_else(dimension == "status_seg", "Status", dimension),
         dimension = if_else(dimension == "quality_seg", "Housing Quality", dimension),
         dimension = if_else(dimension == "rent_seg", "Rent", dimension),
         dimension = if_else(dimension == "vacancy_seg", "Vacancy", dimension)) %>%
  filter(dimension != "Vacancy" & b_inertia == 0.95 &
           r_correlation != 0.25 & r_correlation != 0.75) %>%
  group_by(a_preferences, r_correlation, dimension) %>%
  summarise(segregation = mean(segregation))

ggplot(seg_data) +
  aes(x = a_preferences,
      y = segregation,
      colour = dimension,
      shape = dimension,
      linetype = dimension) +
  at_panel(annotate(geom = "rect", xmin = 0.2, xmax = 0.3, ymin = -Inf, ymax = Inf,
                    fill = "orange", colour = "orange", alpha = 0.4),
           PANEL == 2) +
  geom_point(size = 2) +
  geom_line() +
  facet_wrap(~r_correlation,
             labeller = labeller(.cols = label_r)) +
  labs(x = "Importance of Neighborhood Status relative to Housing Quality",
       y = TeX("Rank-Order Information Theory Index $H^R$"),
       shape = "", colour = "", linetype = "") +
  scale_color_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) +
  scale_shape_manual(values = shapes) +
  scale_linetype_manual(values = lines) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("abm_segregation.pdf", height = 9, width = 24, units = "cm")


# Figure Housing Inequality
unit_data %>%
  filter(a_preferences == 0.25 & r_correlation == 0.5 & b_inertia == 0.95) %>%
  select(sample_id, hh_id, hh_income, housing_quality, rent, rent_to_income) %>%
  mutate(rent_to_income = rent_to_income / 20) %>%
  pivot_longer(cols = housing_quality:rent_to_income) %>%
  mutate(name = if_else(name == "housing_quality", "Housing Quality", name),
         name = if_else(name == "rent", "Rent", name),
         name = if_else(name == "rent_to_income", "Rent / Income", name)) %>%
  ggplot() +
  aes(x = hh_income,
      y = value,
      colour = name,
      linetype = name) +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 1/20, colour = "darkgrey", linetype = "dashed") +
  scale_y_continuous(sec.axis = sec_axis(~ . * 20, 
                                         name = "Rent to Income Ratio", 
                                         breaks = c(1, 3, 5))) +
  labs(x = "Income", y = "Housing Quality and Rent", colour = "", linetype = "") +
  scale_color_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) +
  scale_shape_manual(values = shapes) +
  scale_linetype_manual(values = lines) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("abm_inequality.pdf", height = 8, width = 15, units = "cm")


# Figure Housing Inequality by Status
unit_data %>%
  filter(r_correlation == 0.5 & b_inertia %in% c(0, 0.95, 1) & a_preferences < 1) %>%
  nest_by(a_preferences, b_inertia) %>%
  mutate(trivariate = list(lm(data = data, housing_quality ~ hh_status + hh_income))) %>%
  reframe(tidy(trivariate)) %>%
  filter(term == "hh_status") %>%
  select(-statistic, -p.value) %>%
  rename(beta = estimate,
         se = std.error) %>%
  ggplot() +
  aes(x = a_preferences,
      y = beta,
      ymin = beta - 3.1 * se,
      ymax = beta + 3.1 * se,
      colour = as.factor(b_inertia),
      linetype = as.factor(b_inertia)) +
  geom_line() +
  geom_errorbar(width = 0.01,
                linetype = "solid") +
  annotate(geom = "rect", xmin = 0.2, xmax = 0.3, ymin = -Inf, ymax = Inf,
           fill = "orange", colour = "orange", alpha = 0.4) +
  geom_hline(yintercept = 0,
             colour = "darkgrey") +
  labs(x = "Importance of Neighborhood Status relative to Housing Quality", 
       y = "Coefficient for Status",
       colour = "Inertia", linetype = "Inertia") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("abm_statusreg.pdf", height = 8, width = 15, units = "cm")


# Figure Residential Mobility
unit_data %>%
  filter(a_preferences == 0.25 & r_correlation == 0.5 & b_inertia == 0.95 & !is.na(hh_id)) %>%
  arrange(sample_id, hh_id, t) %>%
  mutate(pos = paste(x, y, sep = "_"),
         moved = if_else(pos == Lag(pos), "stayed", NA),
         moved = if_else(pos != Lag(pos) & Lag(rent > hh_income), "forced to move", moved),
         moved = if_else(pos != Lag(pos) & Lag(rent <= hh_income), "moved voluntarily", moved),
         inc_decile = cut(hh_income, 
                          quantile(hh_income, 
                                   seq(0, 1, by = .1),
                                   na.rm = TRUE),
                          1:10)) %>%
  filter(complete.cases(.)) %>%
  ggplot() +
  aes(x = inc_decile,
      fill = moved) +
  geom_bar(position = "dodge") +
  labs(x = "Income Decile", y = "Residency Status from t to t+1", fill = "") +
  scale_color_manual(values = cb_palette) +
  scale_fill_manual(values = cb_palette) +
  scale_shape_manual(values = shapes) +
  scale_linetype_manual(values = lines) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("abm_mobility.pdf", height = 8, width = 15, units = "cm")
