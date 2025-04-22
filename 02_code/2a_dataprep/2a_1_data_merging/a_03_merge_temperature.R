## Load libraries
library(tidyverse)
library(lubridate)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Path to ERA5 .rds files
era5_folder <- "../../../01_data/1a_era_temp/"

# Years 
years <- 2006:2022

# Inspect columns
names(readRDS("../../../01_data/1a_era_temp/weighted_area_raster_PHL_2_t2m_daily_2006.rds"))

# Lookup table for ADM2 name
lookup <- readRDS(paste0(era5_folder, "weighted_area_raster_PHL_2_t2m_daily_2006.rds")) %>%
  select(ID_2, NAME_2) %>%
  distinct()

# Function: combine ERA5 files and compute monthly mean temperature
era5_monthly <- map_df(years, function(yr) {
  file_path <- paste0(era5_folder, "weighted_area_raster_PHL_2_t2m_daily_", yr, ".rds")
  df <- readRDS(file_path)
  
  df %>%
    mutate(
      date = as.Date(date),
      year = year(date),
      month = month(date)
    ) %>%
    group_by(ID_2, year, month) %>%
    summarise(mean_temp_c = mean(t2m, na.rm = TRUE), .groups = "drop")
}) %>%
  left_join(lookup, by = "ID_2")  

era5_monthly <- era5_monthly %>%
  rename(adm2_merge_key = NAME_2)

# Load merged cyclone and mortality data
cyclone_mortality_merged_data <- read_csv("cyclone_mortality_merged_data.csv")

