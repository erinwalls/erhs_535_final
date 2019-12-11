
library(leaflet)
library(tidyverse)
source("country_dataframes.R") #To get country_all_isoobject; can comment this pout if already loaded
library(rworldmap)
library(sf)

countries_low_res <- countriesLow %>% st_as_sf #from `rworldpackage`; I think they have higher-res versions as well ,and we could probably use different sources



jeopadry_countries_low_res <- country_all_iso %>%
  group_by(iso3) %>%
  add_tally(name = "count") %>%
  ungroup() %>%
  mutate(iso3 = toupper(iso3),
         iso3 = as.factor(iso3)) %>%
  rename(ISO3 = iso3) %>%
  full_join(countries_low_res) %>%
  mutate(ISO3 = as.factor(ISO3)) %>%
  clean_names() %>%
  st_as_sf


count_pal <- colorNumeric(
  palette = 'Dark2',
  domain = jeopadry_countries_low_res$count
)

leaflet(jeopadry_countries_low_res) %>%
  addTiles() %>%
  addPolygons(color = ~count_pal(count))
