## Load libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)

### DATA PREPARATION ###

# Load mortality data
pp_summary <- readRDS("01_data/1b_outcome_data/Philippines_summary.rds")

# Load storm data
storm_winds <- read_csv("01_data/1a_exposure_data/philippines_storm_winds.csv")

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
  "01_data/1c_supportive_data/adm2_mortality.xlsx",
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
# write_csv(psgc_data, "adm_names_PP.csv")

# Join by matching ADM2 code in pp_summary to 10-digit PSGC
# Clean ADM2 codes by removing "PH" prefix, then join
pp_mortality_ADM2 <- pp_summary %>%
  mutate(ADM2_clean = str_remove(ADM2, "^PH")) %>%
  left_join(
    psgc_data %>% select(`Correspondence Code`, Name),
    by = c("ADM2_clean" = "Correspondence Code")
  ) %>%
  rename(adm2_name = Name) %>%
  select(-ADM2_clean)  # optional: drop cleaned ADM2 column

# Finding missing codes
missing_codes <- pp_mortality_ADM2 %>%
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
pp_mortality_ADM2 <- pp_mortality_ADM2 %>%
  mutate(Correspondence_Code = str_remove(ADM2, "^PH")) %>%
  left_join(manual_matches, by = "Correspondence_Code") %>%
  mutate(adm2_name = coalesce(adm2_name, manual_name)) %>%
  select(-Correspondence_Code, -manual_name)

# Ensure no NAs
sum(is.na(pp_mortality_ADM2$adm2_name))


## Assign ADM2 names to storm data ##
# Read the GeoJSON
adm2 <- st_read("01_data/1c_supportive_data/geoBoundaries-PHL-ADM2_simplified.geojson")

# Join storm_winds with adm2 shapefile attributes
storm_winds_named <- storm_winds %>%
  left_join(adm2 %>% 
              st_drop_geometry() %>% 
              select(shapeID, adm2_name = shapeName),
            by = c("ADM2_id" = "shapeID"))


## Check ADM2 naming between mortality data and storm data ##
common_adm2 <- intersect(storm_winds_named$adm2_name, pp_mortality_ADM2$adm2_name)
length(common_adm2)  # 80 ADM2s overlap

only_in_mortality <- setdiff(pp_mortality_ADM2$adm2_name, storm_winds_named$adm2_name)
# Only in mortality dataset: City of Manila, Second District; Third District;
# Fourth District; City of Isabela; Davao de Oro; Sultan Kudarat

only_in_storms <- setdiff(storm_winds_named$adm2_name, pp_mortality_ADM2$adm2_name)
# Only in storm dataset: City of Isabela; Compostela Valley; NCR, City of Manila, First District
# NCR, Second District; NCR, Fourth District; NCR, Third District

## Create merge key ##
pp_mortality_ADM2 <- pp_mortality_ADM2 %>%
  mutate(adm2_merge_key = case_when(
    adm2_name == "City of Manila" ~ "NCR, City of Manila, First District",
    adm2_name == "Second District" ~ "NCR, Second District",
    adm2_name == "Third District" ~ "NCR, Third District",
    adm2_name == "Fourth District" ~ "NCR, Fourth District",
    adm2_name == "City of Isabela (Not a Province)" ~ "City of Isabela",
    adm2_name == "Davao de Oro" ~ "Compostela Valley",
    TRUE ~ adm2_name
  ))

# Save ADM2 named mortality dataset
write_csv(pp_mortality_ADM2, "03_output/3c_intermediate_cleaned/pp_mortality_ADM2.csv")

# Save ADM2 named storm dataset
storm_winds_named <- storm_winds_named %>%
  mutate(adm2_merge_key = case_when(
    adm2_name == "Compostela Valley" ~ "Compostela Valley",  # keep same
    TRUE ~ adm2_name
  ))
write_csv(storm_winds_named, "03_output/3c_intermediate_cleaned/storm_winds_ADM2.csv")


