library(sf)
library(tidyverse)

## Map of total death rates by ADM2 (2006-2022)

# Load ADM2 shapefile
adm2_sf <- st_read("01_data/1c_supportive_data/geoBoundaries-PHL-ADM2_simplified.geojson")

# Load cleaned mortality dataset
pp_mortality_adm2 <- read_csv("03_output/3c_intermediate_cleaned/pp_mortality_ADM2.csv")

# Aggregate total deaths across all years per ADM2
total_deaths_adm2 <- pp_mortality_adm2 %>%
  group_by(adm2_name) %>%
  summarise(total_deaths = sum(deaths, na.rm = TRUE), .groups = "drop")

# Merge with shapefile
map_total_deaths <- adm2_sf %>%
  left_join(total_deaths_adm2, by = c("shapeName" = "adm2_name"))

# Plot map
total_deaths_map <- ggplot(map_total_deaths) +
  geom_sf(aes(fill = total_deaths), color = "black", size = 0.15) +
  scale_fill_viridis_c(option = "plasma",
                       name = "Total Deaths",
                       labels = scales::comma,
                       na.value = "grey90") +
  labs(title = "Total Deaths by ADM2 (2006–2022)",
       caption = "Source: Philippines National Vital Registration Records + geoBoundaries") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# Save output
ggsave("03_output/3a_eda_output/maps/total_deaths_by_adm2_2006_2022.png",
       plot = total_deaths_map, width = 10, height = 11, dpi = 300, bg = "white")
