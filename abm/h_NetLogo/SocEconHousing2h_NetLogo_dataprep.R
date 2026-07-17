### A socio-economic Model of Residential Segregation, Neighborhood Change and Housing Inequality
### Part 2h: Data Preparation in R for the NetLogo Model
### Malte Grönemann


## Documentation
#This script prepares the data outputted by the computational experiments of the model. 
# Depending on which data from a given experiment shall be transformed, the corresponding file needs to be loaded. 
# Additionally, several parameter choices for analysis are made.
# Please adapt the script accordingly depending on the data you want to transform and whether you want to change the analysis.
# - The analyses conducted for the main article are based on a slice of the data where the simulations have already reached the equilibrium state, i.e. it disregards the burn-in. Therefore, the first observations from each run are disregarded.
# - To calculate segregation indices and neighborhood averages, we need to subdivide the city into neighborhoods. A common size for neighborhoods in segregation ABMs is 5x5 (e.g. Bruch 2014, Yavas 2019).
# - The global parameters that have been varied in the computational experiment are saved in a separate file and need to be joined to the data.
# - The common measures of segregation are based on quantiles by (arbitrary) cut-off values. I use the deciles for all continuous variables.
# - Adjust the file path to the data directory to select the computational experiment.

# The script outputs several data sets that correspond to different levels of analysis and exports them as parquet files.
# - The city data contain aggregate measures of segregation and inequality.
# - The neighborhood data takes mean values of the housing units in each neighborhood and calculates a rank order within each city.
# - The unit data contain all variables concerning housing units and households occupying them as well as neighborhood averages.


## Packages, Working Directory and Parameters
library(nanoparquet)
library(readr)
library(dplyr)
library(tidyr)
library(DescTools)

#data_directory <- "./experiments/e_NetLogo/data/"
data_directory <- "./data/netlogo/" # TODO change path
t_min <- 100
size_nb <- 5

## Loading and reshaping Data into Long Format (specific to NetLogo experiment)
unit_data <- read_csv(paste(data_directory, 
                            "unit_raw.csv", 
                            sep = ""),
                      col_names = TRUE,
                      skip = 6)  %>%
  select(-`[reporter]`) %>%
  rename(sample_id = `[run number]`, # formely run
         t = `[step]`) %>% # formerly time
  filter(t >= t_min)


unit_data_params <- unit_data %>%
  select(sample_id:t, -turnover, -distribution)
unit_data_vals <- unit_data %>%
  select(-(sample_id:t))

vars <- c("x_coord", "y_coord", "housing_quality", "rent", "hh_id", "hh_income", "hh_status")
colnames <- c()
for (i in 1:(ncol(unit_data_vals) / 7)) {
  temp <- paste(vars, i, sep = ".")
  colnames <- append(colnames, temp)
}
colnames(unit_data_vals) <- colnames

unit_data <- cbind(unit_data_params, unit_data_vals)
remove(i, temp, colnames, vars, unit_data_params, unit_data_vals)

unit_data <- unit_data %>%
  pivot_longer(cols = contains("."),
               cols_vary = "slowest",
               names_to = c(".value", "set"),
               names_sep = "\\.",
               values_drop_na = TRUE) %>%
  select(-set)

unit_data <- unit_data %>% 
  mutate(nb = paste(floor(x_coord / size_nb),
                    floor(y_coord / size_nb), 
                    sep = "_"),
         empty = is.na(hh_id),
         rent_to_income = rent / hh_income)


## Aggregating
nb_data <- unit_data %>%
  group_by(sample_id, a_utility, b_inertia, r_correlation, t, nb) %>%
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
                       by = c("sample_id", "a_utility", "b_inertia", "r_correlation", "t", "nb"))


## Defining Segregation Measures
deciles <- function(variable) {
  quantile(variable, 
           probs = seq(.1, .9, by = .1),
           na.rm = TRUE)
}

entropy <- function(p) {
  e <- ifelse(p == 0 | p == 1, 
              0, # 0 * log2(1 / 0) := 0; see Reardon et al (2006), p. 11
              p * log2(1 / p) + (1 - p) * log2(1 / (1 - p)))
  return(e)
}

# entropy for the percentile below a threshold of a distribution 
# notation following Reardon and Bischoff (2011) p. 1110
E_p <- function(variable, threshold) {
  p <- mean(variable <= threshold, na.rm = TRUE)
  return(entropy(p))
}

# Theil index for a binary variable (coded 0/1 or FALSE/TRUE)
Theil <- function(variable, neighborhoods) {
  df <- data.frame(variable = variable, neighborhoods = neighborhoods)
  
  p <- mean(variable, na.rm = TRUE)
  entropy_total_E <- entropy(p)
  pop_total_T <- sum(is.na(variable) == FALSE)
  
  df_nb <- data.frame(nb_j = unique(neighborhoods),
                      pop_tj = NA,
                      entropy_Ej = NA)
  for (j in 1:length(df_nb$nb_j)) {
    temp <- subset(df, neighborhoods == df_nb$nb_j[j])
    df_nb$pop_tj[j] = sum(is.na(temp$variable) == FALSE)
    df_nb$entropy_Ej[j] = entropy(mean(temp$variable, na.rm = TRUE))
  }
  
  relative_entropies <- (df_nb$pop_tj * df_nb$entropy_Ej) / (pop_total_T * entropy_total_E)
  
  return(1 - sum(relative_entropies, na.rm = TRUE))
}


# Theil segregation index for a continuous variable and a single threshold
H_p <- function(variable, neighborhoods, threshold){
  # notation following Reardon and Bischoff (2011) p. 1111
  # this function assumes that we have a dataset with a continuous variable of individuals in neighborhoods
  df <- data.frame(variable = variable, neighborhoods = neighborhoods)
  
  entropy_total_E <- E_p(variable, threshold)
  pop_total_T <- sum(is.na(variable) == FALSE)
  
  df_nb <- data.frame(nb_j = unique(neighborhoods),
                      pop_tj = NA,
                      entropy_Ej = NA)
  for (j in 1:length(df_nb$nb_j)) {
    temp <- subset(df, neighborhoods == df_nb$nb_j[j])
    df_nb$pop_tj[j] = sum(is.na(temp$variable) == FALSE)
    df_nb$entropy_Ej[j] = E_p(variable = temp$variable, threshold = threshold)
  }
  
  relative_entropies <- (df_nb$pop_tj * df_nb$entropy_Ej) / (pop_total_T * entropy_total_E)
  H_p <- 1 - sum(relative_entropies, na.rm = TRUE)
  
  return(H_p)
}


# Reardon index
H_R <- function(variable, neighborhoods, thresholds) {
  p_i <- rep(NA, times = length(thresholds))
  E_p_i <- rep(NA, times = length(thresholds))
  H_p_i <- rep(NA, times = length(thresholds))
  
  for(i in 1:length(thresholds)) {
    p_i[i] <- mean(variable <= thresholds[i], na.rm = TRUE)
    E_p_i[i] <- E_p(variable, thresholds[i])
    H_p_i[i] <- H_p(variable, neighborhoods, thresholds[i])
  }
  
  E_p_model <- lm(E_p_i ~ p_i + I(p_i^2) + I(p_i^3) + I(p_i^4))
  E_p_fun <- function(p) {
    E_p_model$coefficients[1] + E_p_model$coefficients[2] * p + E_p_model$coefficients[3] * p^2 + E_p_model$coefficients[4] * p^3 + E_p_model$coefficients[5] * p^4
  }
  H_p_model <- lm(H_p_i ~ p_i + I(p_i^2) + I(p_i^3) + I(p_i^4))
  H_p_fun <- function(p) {
    H_p_model$coefficients[1] + H_p_model$coefficients[2] * p + H_p_model$coefficients[3] * p^2 + H_p_model$coefficients[4] * p^3 + H_p_model$coefficients[5] * p^4
  }
  integrand <- function(p) {H_p_fun(p) * E_p_fun(p)}
  
  integral <- integrate(integrand, 0, 1)
  H_R <- 2 * log(2) * integral$value
  
  return(H_R)
}


## City Aggregation and Segregation
city_data <- unit_data %>%
  group_by(sample_id, a_utility, b_inertia, r_correlation, t) %>%
  summarise(inc_gini = Gini(hh_income, na.rm = TRUE),
            status_gini = Gini(hh_status, na.rm = TRUE),
            quality_gini = Gini(housing_quality, na.rm = TRUE),
            rent_gini = Gini(rent, na.rm = TRUE),
            inc_seg = H_R(hh_income, nb, deciles(hh_income)),
            status_seg = H_R(hh_status, nb, deciles(hh_status)),
            quality_seg = H_R(housing_quality, nb, deciles(housing_quality)),
            rent_seg = H_R(rent, nb, deciles(rent)),
            vacancy_seg = Theil(empty, nb))


## Exporting Data
write_parquet(city_data, paste(data_directory, "city_data.parquet", sep = ""))
write_parquet(nb_data, paste(data_directory, "nb_data.parquet", sep = ""))
write_parquet(unit_data, paste(data_directory, "unit_data.parquet", sep = ""))
