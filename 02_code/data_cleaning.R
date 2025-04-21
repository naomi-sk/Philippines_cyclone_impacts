## Load libraries
library(tidyverse)
library(readxl)
library(lubridate)

## Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### DATA PREPARATION ###

# Load mortality data
pp_summary <- readRDS("Philippines_summary.rds")

# Load storm data
storm_winds <- read_csv("philippines_storm_winds.csv")

# Missing values check
colSums(is.na(storm_winds))

# Convert date column to Date format 
storm_winds <- storm_winds %>%
  mutate(date_time_max_wind = as.POSIXct(date_time_max_wind))

# Filter for data between 2006 and 2022
storm_winds <- storm_winds %>%
  filter(year(date_time_max_wind) >= 2006 & year(date_time_max_wind) <= 2022)

# Using vmax_sust - convert into knots
storm_winds <- storm_winds %>%
  mutate(vmax_sust_knots = vmax_sust * 1.94384)  # Convert from m/s to knots
# Source: https://www.xconvert.com/unit-converter/metres-per-second-to-knots 

# Reclassify storms using vmax_sust_knots
storm_winds <- storm_winds %>%
  mutate(cyclone_category = case_when(
    vmax_sust_knots >= 34 & vmax_sust_knots < 64 ~ "Tropical Storm / Severe Tropical Storm",
    vmax_sust_knots >= 64 ~ "Typhoon / Super Typhoon",
    TRUE ~ "Below Tropical Storm"
  ))

# Separate time data within storm dataset
storm_winds <- storm_winds %>%
  mutate(
    year = year(date_time_max_wind),
    month = month(date_time_max_wind),
    day = day(date_time_max_wind)
  )

## Assign ADM2 names to mortality data ##

# Read in mortality data with ADM2 names
psgc_data <- read_excel(
  "adm2_mortality.xlsx",
  col_types = c(
    "text",    # 10-digit PSGC
    "text",    # Name
    "text",    # Correspondence Code
    "text",    # Geographic Level
    "text",    # Old names
    "text",    # City Class
    "text",    # Income Classification
    "text",    # Urban / Rural
    "numeric", # 2020 Population
    "text"     # Status
  )
)

# Save CSV file
write_csv(psgc_data, "adm_names_PP.csv")

# Join by matching ADM2 code in pp_summary to 10-digit PSGC
# Clean ADM2 codes by removing "PH" prefix, then join
pp_summary_named <- pp_summary %>%
  mutate(ADM2_clean = str_remove(ADM2, "^PH")) %>%
  left_join(
    psgc_data %>% select(`Correspondence Code`, Name),
    by = c("ADM2_clean" = "Correspondence Code")
  ) %>%
  rename(adm2_name = Name) %>%
  select(-ADM2_clean)  # optional: drop cleaned ADM2 column

# Finding missing codes
missing_codes <- pp_summary_named %>%
  filter(is.na(adm2_name)) %>%
  mutate(Correspondence_Code = str_remove(ADM2, "^PH")) %>%
  distinct(Correspondence_Code)

# Create tibble with missing codes
manual_matches <- tribble(
  ~Correspondence_Code, ~manual_name,
  "153800000", "Maguindanao",
  "137600000", "Fourth District",
  "137500000", "Third District",
  "137400000", "Second District",
  "129800000", "Cotabato City"
)

# Patch NAs directly
pp_summary_named <- pp_summary_named %>%
  mutate(Correspondence_Code = str_remove(ADM2, "^PH")) %>%
  left_join(manual_matches, by = "Correspondence_Code") %>%
  mutate(adm2_name = coalesce(adm2_name, manual_name)) %>%
  select(-Correspondence_Code, -manual_name)

# Ensure no NAs
sum(is.na(pp_summary_named$adm2_name))


## Assign ADM2 names to storm data ##
# Read the GeoJSON
adm2 <- st_read("geoBoundaries-PHL-ADM2_simplified.geojson")

# Join storm_winds with adm2 shapefile attributes
storm_winds_named <- storm_winds %>%
  left_join(adm2 %>% 
              st_drop_geometry() %>% 
              select(shapeID, adm2_name = shapeName),
            by = c("ADM2_id" = "shapeID"))


## Check ADM2 naming between mortality data and storm data ##
common_adm2 <- intersect(storm_winds_named$adm2_name, pp_summary_named$adm2_name)
length(common_adm2)  # 80 ADM2s overlap

only_in_mortality <- setdiff(pp_summary_named$adm2_name, storm_winds_named$adm2_name)
# Only in mortality dataset: City of Manila, Second District; Third District;
# Fourth District; City of Isabela; Davao de Oro; Sultan Kudarat

only_in_storms <- setdiff(storm_winds_named$adm2_name, pp_summary_named$adm2_name)
# Only in storm dataset: City of Isabela; Compostela Valley; NCR, City of Manila, First District
# NCR, Second District; NCR, Fourth District; NCR, Third District



## Create merge key ##

pp_summary_named <- pp_summary_named %>%
  mutate(adm2_merge_key = case_when(
    adm2_name == "City of Manila" ~ "NCR, City of Manila, First District",
    adm2_name == "Second District" ~ "NCR, Second District",
    adm2_name == "Third District" ~ "NCR, Third District",
    adm2_name == "Fourth District" ~ "NCR, Fourth District",
    adm2_name == "City of Isabela (Not a Province)" ~ "City of Isabela",
    adm2_name == "Davao de Oro" ~ "Compostela Valley",
    TRUE ~ adm2_name
  ))

# Save CSV file
write_csv(pp_summary_named, "pp_summary_mapping.csv")

storm_winds_named <- storm_winds_named %>%
  mutate(adm2_merge_key = case_when(
    adm2_name == "Compostela Valley" ~ "Compostela Valley",  # keep same
    TRUE ~ adm2_name
  ))


### DATASET MERGING ###

# Aggregate storm data to monthly level (ADM2-month)
storm_monthly <- storm_winds_named %>%
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
merged_data <- pp_summary_named %>%
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
