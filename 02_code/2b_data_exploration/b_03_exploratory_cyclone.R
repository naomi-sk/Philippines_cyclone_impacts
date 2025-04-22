# Load libraries
library(tidyverse)
library(ggplot2)

# Load cyclone data
storm_winds <- read_csv("01_data/1a_exposure_data/philippines_storm_winds.csv")

#### Data Inspection and processing

# Missing values check
colSums(is.na(storm_winds))

# Convert date column to Date format
storm_winds <- storm_winds %>%
  mutate(date_time_max_wind = as.POSIXct(date_time_max_wind))

# Filter for data between 2006 and 2022
storm_winds <- storm_winds %>%
  filter(year(date_time_max_wind) >= 2006 & year(date_time_max_wind) <= 2022)

# Unique categories for analysis
storm_winds %>% summarise(across(where(is.character), n_distinct))

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

# Check the updated category counts
storm_winds %>% count(cyclone_category)

# Separate time data
storm_winds <- storm_winds %>%
  mutate(
    year = year(date_time_max_wind),
    month = month(date_time_max_wind),
    day = day(date_time_max_wind)
  )


#### Exploratory Visualisations: Cyclone data

# 1. With Below Tropical Storm category
plot1 <- storm_winds %>%
  mutate(year = year(date_time_max_wind)) %>%
  count(year, cyclone_category) %>%
  ggplot(aes(x = factor(year), y = n, fill = cyclone_category)) +
  geom_col(position = "dodge") +
  labs(title = "Cyclone Category Distribution (2006-2022)",
       x = "Year", y = "Number of Cyclones", fill = "Cyclone Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("03_output/3a_eda_output/cyclone_trends/cyclone_dist_with_below.png", plot1, width = 8, height = 5)


# 2. Without Below Tropical Storm category
plot2 <- storm_winds %>%
  mutate(year = year(date_time_max_wind)) %>%
  filter(cyclone_category != "Below Tropical Storm") %>%
  count(year, cyclone_category) %>%
  ggplot(aes(x = factor(year), y = n, fill = cyclone_category)) +
  geom_col(position = "dodge") +
  labs(title = "Cyclone Category Distribution (2006-2022)",
       x = "Year", y = "Number of Cyclones", fill = "Cyclone Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("03_output/3a_eda_output/cyclone_trends/cyclone_dist_without_below.png", plot2, width = 8, height = 5)


# 3. Cyclone trends over time
plot3 <- storm_winds %>%
  mutate(year = year(date_time_max_wind)) %>%
  count(year, cyclone_category) %>%
  ggplot(aes(x = year, y = n, color = cyclone_category)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Philippines Cyclone trends (2006-2022)",
       x = "Year", y = "Number of Cyclones", color = "Cyclone Category") +
  theme_minimal()
ggsave("03_output/3a_eda_output/cyclone_trends/cyclone_trend_all.png", plot3, width = 8, height = 5)


# 4. Cyclone trends without Below Tropical Storm
plot4 <- storm_winds %>%
  mutate(year = year(date_time_max_wind)) %>%
  filter(cyclone_category != "Below Tropical Storm") %>%
  count(year, cyclone_category) %>%
  ggplot(aes(x = factor(year), y = n, color = cyclone_category, group = cyclone_category)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Trend of Cyclone Categories (2006-2022)",
       x = "Year", y = "Number of Cyclones", color = "Cyclone Category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("03_output/3a_eda_output/cyclone_trends/cyclone_trend_filtered.png", plot4, width = 8, height = 5)


# 5. Cyclone frequency by month
plot5 <- storm_winds %>%
  count(month) %>%
  ggplot(aes(x = factor(month), y = n)) +
  geom_col(fill = "blue") +
  labs(title = "Cyclone Frequency by Month",
       x = "Month", y = "Number of Cyclones") +
  theme_minimal()
ggsave("03_output/3a_eda_output/cyclone_trends/cyclone_freq_by_month.png", plot5, width = 8, height = 5)