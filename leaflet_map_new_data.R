# Testing issues with Jess' map:

library(tidyverse)
library(rworldmap)
library(sf)
library(leaflet)
library(viridisLite)
library(janitor)


country_data_all <- read_csv("country_all_iso_all.csv", )

countriesLow <- countriesLow


#Temporarily removing air date (not sure how to animate/facet/etc. in Leaflet)

country_geom_full <- country_data_all %>%
  left_join(countryExData, by = c("iso3" = "ISO3V10")) %>%
  group_by(iso3) %>%
  mutate(mean_value = mean(value)) %>%
  add_tally(name = "count") %>%
  ungroup() %>%
  select(country, count, mean_value, iso3) %>%
  distinct() %>%
  mutate(iso3 = toupper(iso3)) %>%
  rename(ISO3 = iso3) 


country_geom_map_data <- country_geom_full %>%
  mutate(ISO3 = as.factor(ISO3)) %>%
  full_join(countriesLow) %>%
  clean_names() %>%
  st_as_sf



pal <- colorNumeric(
  palette = "Greens",
  domain = country_geom_map_data$count)



popup_info<- paste0("Country: ",
                    country_geom_map_data$name, "
",
"Population: ",
country_geom_map_data$pop_est, "
",
"Count: ",
country_geom_map_data$count, "
",
"Mean Value: ",
round(country_geom_map_data$mean_value, 1))

leaflet(country_geom_map_data) %>%
  addTiles() %>%
  addPolygons(color = ~pal(count), popup = popup_info)




