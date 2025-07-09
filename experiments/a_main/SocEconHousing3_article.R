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


## for some reason, loading the unit_data.parquet file created by the dataprep R script from the main experiment does not work.
## The file seems corrupted and every attempt at creating it new and saving it seems to fail as well.
## I therefore need to create the dataset again from the raw data for the analysis.
unit_data <- read_parquet(paste(data_directory,
                                "unit_raw.parquet", 
                                sep = "")) %>%
  filter(t >= 300)

parameters <- read_parquet(paste(data_directory, 
                                 "parameters.parquet", 
                                 sep = ""))

unit_data <- full_join(parameters, unit_data, 
                       by = "sample_id")

unit_data <- unit_data %>% 
  mutate(nb = paste(floor(x_coord / 5),
                    floor(y_coord / 5), 
                    sep = "_"),
         empty = is.na(hh_id),
         rent_to_income = rent / hh_income)

nb_data <- unit_data %>%
  group_by(sample_id, t, nb) %>%
  summarise(ave_inc = mean(hh_income, na.rm = TRUE),
            ave_status = mean(hh_status, na.rm = TRUE),
            ave_quality = mean(housing_quality, na.rm = TRUE),
            ave_rent = mean(rent, na.rm = TRUE),
            ave_vacant = mean(empty)) %>%
  mutate(rank_inc = length(unique(nb)) - rank(ave_inc) + 1, # inverting ranks to have the nb with highest ave as rank 1
         rank_status = length(unique(nb)) - rank(ave_status) + 1,
         rank_quality = length(unique(nb)) - rank(ave_quality) + 1,
         rank_rent = length(unique(nb)) - rank(ave_rent) + 1,
         rank_vacant = rank(ave_vacant)) # rank 1 is neighborhood with lowest vacancy rates

unit_data <- full_join(unit_data, nb_data, 
                       by = c("sample_id", "t", "nb"))


# loading uncorrupted files from storage
nb_data <- read_parquet(paste(data_directory, "nb_data.parquet", sep = ""))
city_data <- read_parquet(paste(data_directory, "city_data.parquet", sep = ""))


label_r <- c(`0` = "No Correlation bw. Income and Status", `0.5` = "Realistic Correlation (r = 0.71)", `1` = "Perfect Correlation")
label_a <- c(`0` = "Preferences: Only Housing Quality", `0.25` = "Realistic: Housing Quality > Neighborhood Status", `1` = "Only Neighborhood Status")


# Figure 1: Segregation
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
  filter(dimension != "Vacancy" & d_decay == 0.95 &
           r_correlation != 0.25 & r_correlation != 0.75) %>%
  group_by(a_preferences, r_correlation, dimension) %>%
  summarise(segregation = mean(segregation))

ggplot(seg_data) +
  aes(x = a_preferences,
      y = segregation,
      colour = dimension,
      shape = dimension) +
  at_panel(annotate(geom = "rect", xmin = 0.2, xmax = 0.3, ymin = -Inf, ymax = Inf,
                    fill = "orange", colour = "orange", alpha = 0.4),
           PANEL == 2) +
  geom_point(size = 2) +
  geom_line() +
  facet_wrap(~r_correlation,
             labeller = labeller(.cols = label_r)) +
  labs(x = "Importance of Neighborhood Status relative to Housing Quality",
       y = TeX("Rank-Order Information Theory Index $H^R$"),
       shape = "", colour = "") +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("abm_01_segregation.pdf", height = 9, width = 24, units = "cm")


# Figure 2: Neighborhood Stability by Income Rank
nb_data %>%
  filter(d_decay == 0.95 & 
           r_correlation != 0.25 & r_correlation != 0.75 &
           a_preferences != 0.5 & a_preferences != 0.75) %>%
  group_by(sample_id, a_preferences, r_correlation, nb) %>%
  summarise(ave_rank_inc = mean(rank_inc),
            sd_rank_inc = sd(rank_inc)) %>%
  ggplot() +
  aes(x = ave_rank_inc,
      y = sd_rank_inc) +
  at_panel(annotate(geom = "rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf,
                    fill = "orange", colour = "orange", alpha = 0.4),
           PANEL == 5) +
  geom_point(colour = "darkgrey") +
  geom_smooth(colour = "black") +
  facet_grid(rows = vars(r_correlation),
             cols = vars(a_preferences), 
             labeller = labeller(.rows = label_r,
                                 .cols = label_a)) +
  labs(x = "Mean of the Neighborhood Rank by Income over Time",
       y = "SD of the Neighborhood Rank by Income over Time") +
  theme_bw() +
  theme(legend.position = "none")
ggsave("abm_02_nbstability.pdf", height = 20, width = 30, units = "cm")

# no facet version
nb_data %>%
  filter(d_decay == 0.95 & 
           r_correlation == 0.5 &
           a_preferences == 0.25) %>%
  group_by(sample_id, a_preferences, r_correlation, nb) %>%
  summarise(ave_rank_inc = mean(rank_inc),
            sd_rank_inc = sd(rank_inc)) %>%
  ggplot() +
  aes(x = ave_rank_inc,
      y = sd_rank_inc) +
  geom_point(colour = "darkgrey") +
  geom_smooth(colour = "black") +
  labs(x = "Mean of the Neighborhood Rank by Income over Time",
       y = "SD of the Neighborhood Rank by Income over Time") +
  theme_bw() +
  theme(legend.position = "none")
ggsave("abm_02b_nbstability.pdf", height = 10, width = 20, units = "cm")


# Figure 3: Housing Inequality
unit_data %>%
  filter(a_preferences == 0.25 & r_correlation == 0.5 & d_decay == 0.95) %>%
  select(sample_id, hh_id, hh_income, housing_quality, rent, rent_to_income) %>%
  mutate(rent_to_income = rent_to_income / 20) %>%
  pivot_longer(cols = housing_quality:rent_to_income) %>%
  mutate(name = if_else(name == "housing_quality", "Housing Quality", name),
         name = if_else(name == "rent", "Rent", name),
         name = if_else(name == "rent_to_income", "Rent / Income", name)) %>%
  ggplot() +
  aes(x = hh_income,
      y = value,
      colour = name) +
  geom_smooth() +
  geom_hline(yintercept = 1/20, colour = "darkgrey", linetype = "dashed") +
  scale_y_continuous(sec.axis = sec_axis(~ . * 20, 
                                         name = "Rent to Income Ratio", 
                                         breaks = c(1, 3, 5))) +
  labs(x = "Income", y = "Housing Quality and Rent", colour = "") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("abm_03_inequality.pdf", height = 8, width = 15, units = "cm")


# Figure 4: Housing Inequality by Status # TODO add highlight
unit_data %>%
  filter(r_correlation == 0.5 & d_decay == 0.95) %>%
  nest_by(a_preferences) %>%
  mutate(trivariate = list(lm(data = data, housing_quality ~ hh_status + hh_income))) %>%
  reframe(tidy(trivariate)) %>%
  filter(term == "hh_status") %>%
  select(-statistic, -p.value) %>%
  rename(beta = estimate,
         se = std.error) %>%
  ggplot() +
  aes(x = as.factor(a_preferences),
      y = beta,
      ymin = beta - 3.1 * se,
      ymax = beta + 3.1 * se) +
  geom_errorbar(size = 2, width = 0) +
  geom_hline(yintercept = 0,
             colour = "darkgrey") +
  labs(x = "Importance of Neighborhood Status relative to Housing Quality", 
       y = "99.9% CI around the Status Coefficient") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("abm_04_statusreg.pdf", height = 8, width = 15, units = "cm")


# Figure 5: Residential Mobility
unit_data %>%
  filter(a_preferences == 0.25 & r_correlation == 0.5 & d_decay == 0.95 & !is.na(hh_id)) %>%
  arrange(sample_id, hh_id, t) %>%
  mutate(moved = if_else(obj_id == Lag(obj_id), "stayed", NA),
         moved = if_else(obj_id != Lag(obj_id) & Lag(rent > hh_income), "forced to move", moved),
         moved = if_else(obj_id != Lag(obj_id) & Lag(rent <= hh_income), "moved voluntarily", moved),
         inc_decile = cut(hh_income, 
                          quantile(hh_income, 
                                   seq(0, 1, by = .1),
                                   na.rm = TRUE),
                          1:10)) %>%
  filter(complete.cases(.)) %>%
  ggplot() +
  aes(x = inc_decile,
      fill = moved) + # TODO change colour palette?
  geom_bar(position = "dodge") +
  labs(x = "Income Decile", y = "Residency Status from t to t+1", fill = "") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("abm_05_mobility.pdf", height = 8, width = 15, units = "cm")
