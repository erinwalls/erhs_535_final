
library(leaflet)
library(tidyverse)
source("country_dataframes.R") #To get country_all_isoobject; can comment this pout if already loaded


test_geom <- countriesLow %>% st_as_sf


test_geom_full <- country_all_iso %>%
  group_by(iso3) %>%
  add_tally(name = "count") %>%
  ungroup() %>%
  mutate(iso3 = toupper(iso3),
         iso3 = as.factor(iso3)) %>%
  rename(ISO3 = iso3) %>%
  full_join(test_geom) %>%
  mutate(ISO3 = as.factor(ISO3)) %>%
  clean_names() %>%
  st_as_sf


pal <- colorNumeric(
  palette = 'Dark2',
  domain = test_geom_full$count
)

leaflet(test_geom_full) %>%
  addTiles() %>%
  addPolygons(color = ~pal(count))
