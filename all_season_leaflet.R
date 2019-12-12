#all seasons
library(tidyverse)
library(rworldmap)
library(sf)
library(leaflet)
library("dplyr")
library(viridisLite)
library(janitor)

country_data_all <- read_csv("country_all_iso_all.csv", )

countriesLow <- countriesLow %>%
  st_as_sf

#Temporarily removing air date (not sure how to animate/facet/etc. in Leaflet)

country_geom_full<- country_data_all %>%
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
  dplyr::full_join(countriesLow) %>%
  clean_names() %>%
  st_as_sf

pal <- colorNumeric(
  palette = "Greens",
  domain = country_geom_map_data$count)

popup_info<- paste0("<b>Country:</b> ",
                    country_geom_map_data$name, "<br/>",
                    "<b>Population:</b>",
                    country_geom_map_data$pop_est, "<br/>",
                    "<b>Count:</b>",
                    country_geom_map_data$count, "<br/>",
                    "<b>Mean Value:</b> ",
                    round(country_geom_map_data$mean_value))




leaflet(country_geom_map_data) %>%
  addTiles() %>%
  addPolygons(color = ~pal(count), popup = popup_info)



