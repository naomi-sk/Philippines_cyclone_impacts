library(sf)
library(tidyverse)

## Map of crude death rates

# Load shapefile 
adm2_sf <- st_read("01_data/1c_supportive_data/geoBoundaries-PHL-ADM2_simplified.geojson")

# Load the mortality dataset
pp_mortality_adm2 <- read_csv("03_output/3c_intermediate_cleaned/pp_mortality_ADM2.csv")


# ADM2s in mortality data not found in the shapefile
setdiff(pp_mortality_adm2$adm2_name, adm2_sf$shapeName)

# shapefile regions not matched by mortality data
setdiff(adm2_sf$shapeName, pp_mortality_adm2$adm2_name)

# Ensure mortality data ADM2 names match shapefile ShapeName for ADM2
pp_mortality_adm2 <- pp_mortality_adm2 %>%
  mutate(adm2_name = case_when(
    adm2_name == "City of Manila" ~ "NCR, City of Manila, First District",
    adm2_name == "Second District" ~ "NCR, Second District",
    adm2_name == "Third District" ~ "NCR, Third District",
    adm2_name == "Fourth District" ~ "NCR, Fourth District",
    adm2_name == "City of Isabela (Not a Province)" ~ "City of Isabela",
    adm2_name == "Davao de Oro" ~ "Compostela Valley",
    TRUE ~ adm2_name
  ))

# Merge mortality data and shapefile
map_df <- adm2_sf %>%
  left_join(pp_mortality_adm2, by = c("shapeName" = "adm2_name"))

# Crude death rate per 1,000 calculated by summing total deaths for each ADM2 and year,
# and dividing by the mid-year (July) population, then multiplying by 1,000.
# July is used as a proxy for mid-year population
# ADM2s with zero or missing July population are excluded to avoid infinite or misleading values

map_df <- map_df %>%
  group_by(shapeName, year, geometry) %>%
  summarise(
    total_deaths = sum(deaths, na.rm = TRUE),
    mid_year_population = sum(population[month == 7], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(mid_year_population > 0) %>%  
  mutate(cdr_1000 = (total_deaths / mid_year_population) * 1000)

# Select necessary variables only
map_df <- map_df %>%
  select(shapeName, year, cdr_1000, geometry)

# Plot panelled map
cdr_map <- ggplot(map_df) +
  geom_sf(aes(fill = cdr_1000), color = "black", size = 0.15) +
  scale_fill_viridis_c(option = "magma", direction = -1,
                       name = "CDR per 1k",
                       limits = c(0, 9),
                       na.value = "grey90") +
  facet_wrap(~ year, ncol = 4) +
  labs(title = "Crude Death Rate by ADM2 (per 1,000)",
       caption = "Source: Philippine National Vital Registration Records + geoBoundaries") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# Save map
ggsave("03_output/3a_eda_output/maps/crude_death_rate_panel.png", plot = cdr_map, width = 16, height = 14, dpi = 300, bg = "white")

