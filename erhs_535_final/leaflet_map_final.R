library("ggplot2")
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

library("viridisLite")
pal <- colorNumeric(
  palette =  "Greens",
  domain = test_geom_full$count)

popup_info<- paste0("<b>Country:</b> ",
                      test_geom_full$name, "<br/>",
                    "<b>Population:</b> ",
                      test_geom_full$pop_est, "<br/>",
                    "<b>Rank:</b> ",
                      test_geom_full$scale_rank, "<br/>",
                    "<b>Air Date:</b> ",
                      test_geom_full$air_date)
                    
                    

leaflet(test_geom_full) %>%
  addTiles() %>%
  addPolygons(color = ~pal(count), popup = popup_info)

