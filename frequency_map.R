#############################
#R Script for frequency map
#############################

#Adding a quick demonstration for S1 using rworldmap (obviously could stand cleaning-up, use a different package, etc.):


source("country_dataframes.R")
library(tidyverse)
library(rworldmap)

iso3_tally %>%
  mutate(iso3 = toupper(iso3)) %>%
  rename(ISO3V10 = iso3) %>%
  joinCountryData2Map(joinCode = "ISO3", nameCountryColumn = "ISOV10") %>%
  mapCountryData(nameColumnToPlot = "count", catMethod = 'pretty')



################ Example tigris map; 
#importing map from `rworldmap` b/c it has ISO3 codes, 
#but might be able to use another source if we translate e.g. ISO3 to FIPS

library(rworldmap)
library(tigris)
library(sf)

options(tigris_class = "sf") #not sure if this step is necessary; leftover from ~template tutorial

countriesLow_sf <- countriesLow %>% st_as_sf #from rworldmap; I don't think I had to load it separately
  #Ideally could use another source if we want to control zooming, projection, etc, 
#but haven't looked into that option so far

frequency_map_test <- iso3_tally %>%
  mutate(iso3 = toupper(iso3),
         iso3 = as.factor(iso3)) %>%
  rename(ISO3 = iso3) %>%
  full_join(countriesLow_sf) %>%
  mutate(ISO3 = as.factor(ISO3)) %>%
  st_as_sf

ggplot() +
  geom_sf(data = frequency_map_test, aes(fill = count)) +
  scale_fill_viridis_c()

#Testing using other jeopardy data:

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


test_geom_full %>%
  group_by(country) %>%
  mutate(mean_value = purrr::map(value, .f = mean)) %>% 
  unnest(mean_value) %>%
  #mutate(category = fct_lump(category, n=8, other_level = "Other")) %>%
  ungroup(country) %>%
  ggplot() +
  geom_sf() +
  aes(fill = count, color = mean_value) +
  scale_fill_viridis_c() +
  scale_color_viridis_c(option = "plasma")

#Can't see where highest mean value is--

####