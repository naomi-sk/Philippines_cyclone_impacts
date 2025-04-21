## Load libraries
library(tidyverse)
library(readxl)
library(lubridate)

## Set working directory
setwd(file.path(dirname(getwd()), "2a_0_data_cleaning"))

## Load datasets
storm_winds_ADM2 <- read_csv("storm_winds_ADM2.csv")
pp_mortality_ADM2 <- read_csv("pp_mortality_ADM2.csv")

## Reset working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### DATASET MERGING ###

# Aggregate storm data to monthly level (ADM2-month)
storm_monthly <- storm_winds_ADM2 %>%
  # Group by ADM2, year, month
  group_by(adm2_merge_key, year, month) %>%
  # Create binary indicators for cyclone exposure (≥34 knots)
  summarise(
    # 1 if any day in month had cyclone exposure (≥34 knots)
    cyclone_exposure = max(ifelse(cyclone_category %in% 
                                    c("Tropical Storm / Severe Tropical Storm", "Typhoon / Super Typhoon"), 1, 0)),
    # Maximum wind speed in the month
    max_wind_knots = max(vmax_sust_knots),
    # Count unique storms
    storm_count = n_distinct(storm_id),
    .groups = "drop"
  )

# Create lag variables for capturing delayed effects
storm_monthly <- storm_monthly %>%
  # Ensure data is properly sorted
  arrange(adm2_merge_key, year, month) %>%
  # Group by ADM2
  group_by(adm2_merge_key) %>%
  # Create lag variables for 1, 2, and 3 months
  mutate(
    cyclone_lag1 = lag(cyclone_exposure, n = 1, default = 0),
    cyclone_lag2 = lag(cyclone_exposure, n = 2, default = 0),
    cyclone_lag3 = lag(cyclone_exposure, n = 3, default = 0)
  ) %>%
  ungroup()

# Create exposure variables following Parks et al. approach
storm_monthly <- storm_monthly %>%
  mutate(
    # Month of or month after exposure (Parks et al. standard approach)
    cyclone_exposure_1month = ifelse(cyclone_exposure == 1 | cyclone_lag1 == 1, 1, 0),
    
    # Extended 3-month exposure for research question
    cyclone_exposure_3month = ifelse(cyclone_exposure == 1 | 
                                       cyclone_lag1 == 1 | 
                                       cyclone_lag2 == 1 | 
                                       cyclone_lag3 == 1, 1, 0)
  )

# Merge with mortality data
merged_data <- pp_mortality_ADM2 %>%
  # Join with storm exposure data
  left_join(
    storm_monthly %>%
      select(adm2_merge_key, year, month, 
             cyclone_exposure_1month,
             cyclone_exposure_3month,
             max_wind_knots, storm_count),
    by = c("adm2_merge_key", "year", "month")
  ) %>%
  # Replace NA values with 0 for months without cyclone exposure
  mutate(across(c(cyclone_exposure_1month, cyclone_exposure_3month), 
                ~replace_na(.x, 0)),
         max_wind_knots = replace_na(max_wind_knots, 0),
         storm_count = replace_na(storm_count, 0))

# For sex-stratified analysis 
sex_stratified <- merged_data %>%
  group_by(adm2_merge_key, year, month, sex, cyclone_exposure_1month) %>%
  summarize(
    deaths = sum(deaths),
    population = mean(population),  # Use mean to handle potential duplicates
    .groups = "drop"
  )

# Save the full merged dataset 
write_csv(merged_data, "cyclone_mortality_merged_data.csv")

# Save the sex-stratified dataset
write_csv(sex_stratified, "cyclone_mortality_sex_stratified.csv")
