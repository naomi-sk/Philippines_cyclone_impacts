# Load libraries
library(tidyverse)
library(ggplot2)

# Load mortality data
pp_summary <- readRDS("01_data/1b_outcome_data/Philippines_summary.rds")

# Mortality stats (annual)
mortality_stats <- pp_summary %>%
  group_by(year) %>%
  summarise(
    total_deaths = sum(deaths, na.rm = TRUE),
    mean_monthly_deaths = mean(deaths, na.rm = TRUE),
    # Using mid-year population (July) for more accurate crude death rate calculation
    mid_year_population = sum(population[month == 7], na.rm = TRUE),
    crude_death_rate = (total_deaths / mid_year_population) * 1000
  ) %>%
  ungroup()

# Sex-based mortality differences
sex_diff <- pp_summary %>%
  filter(month == 7) %>%  # Use July population for better accuracy
  group_by(sex, year) %>%
  summarise(
    deaths = sum(deaths, na.rm = TRUE),
    population = sum(population, na.rm = TRUE),
    death_rate = (deaths / population) * 1000
  ) %>%
  arrange(year, sex)

# Monthly mortality patterns

monthly_patterns <- pp_summary %>%
  group_by(month) %>%
  summarise(
    avg_deaths = mean(deaths, na.rm = TRUE),
    median_deaths = median(deaths, na.rm = TRUE),
    sd_deaths = sd(deaths, na.rm = TRUE),
    total_deaths = sum(deaths, na.rm = TRUE)
  ) %>%
  arrange(month)


## 1. Annual Mortality Trends
annual_mortality <- ggplot(mortality_stats, aes(x = year, y = total_deaths)) +
  geom_line(size = 1, color = "darkblue") +
  geom_point(size = 3, color = "darkblue") +
  scale_y_continuous(labels = scales::comma) +  # Show full numbers
  labs(title = "Total Deaths by Year (2006-2022)",
       x = "Year", 
       y = "Number of Deaths") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(mortality_stats$year), max(mortality_stats$year), by = 2))
ggsave("03_output/3a_eda_output/mortality_trends/total_deaths_by_year.png", width = 8, height = 5)

## 2. Crude Death Rate Trends
cdr_stats <- mortality_stats  # Reusing mortality_stats, which contains crude_death_rate
ggplot(cdr_stats, aes(x = year, y = crude_death_rate)) +
  geom_line(size = 1, color = "darkred") +
  geom_point(size = 3, color = "darkred") +
  labs(title = "Crude Death Rate (2006-2022)",
       x = "Year", 
       y = "Deaths per 1,000 Population") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(cdr_stats$year), max(cdr_stats$year), by = 2))
ggsave("03_output/3a_eda_output/mortality_trends/crude_death_rate_trends.png", width = 8, height = 5)

## 3. Monthly Mortality Patterns
monthly_mortality <- ggplot(monthly_patterns, aes(x = factor(month), y = avg_deaths)) +  
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Average Monthly Deaths (Seasonal Pattern)",
       x = "Month", 
       y = "Average Deaths") +
  theme_minimal() +
  scale_x_discrete(labels = month.abb)  # Use month abbreviations for clearer x-axis
ggsave("03_output/3a_eda_output/mortality_trends/average_monthly_deaths.png", width = 8, height = 5)


## 4. Death Rates by Sex Over Time
death_rate_sex <- ggplot(sex_diff, aes(x = year, y = death_rate, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Death Rates by Sex (2006-2022)",
       x = "Year", 
       y = "Death Rate (per 1,000 population)") +
  theme_minimal() +
  scale_color_manual(values = c("darkblue", "darkred")) +
  scale_x_continuous(breaks = seq(min(sex_diff$year), max(sex_diff$year), by = 2))
ggsave("03_output/3a_eda_output/mortality_trends/death_rate_sex.png", width = 8, height = 5)


## 5. Regional Variation in Death Rates (Top 10 Regions)
region_avg_mortality <- pp_summary %>%
  group_by(region) %>%
  summarise(avg_death_rate = mean(deaths / population * 1000, na.rm = TRUE)) %>%
  arrange(desc(avg_death_rate))

region_top10 <- region_avg_mortality %>% slice_max(avg_death_rate, n = 10)

regional_death_rates <- ggplot(region_top10, aes(x = reorder(region, avg_death_rate), y = avg_death_rate)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +  # horizontal bars
  labs(title = "Average Death Rates by Region (Top 10)",
       x = "", 
       y = "Death Rate (per 1,000 population)") +
  theme_minimal()
ggsave("03_output/3a_eda_output/mortality_trends/regional_death_rates.png", width = 8, height = 5)
