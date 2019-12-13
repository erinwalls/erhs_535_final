library(dplyr)
library(rworldmap)
library(tidyverse)
library(rvest)
library(magrittr)
library(janitor)
library(rworldmap)
library(tidyverse)
library(rvest)
library(magrittr)
library(janitor)
library(lubridate)
library(broom)

data("countryExData")
data("countryRegions")
data("countrySynonyms")


country_all_iso_all <- read.csv("country_all_iso_all.csv") #exported csv of countries matched by iso3 codes



jeopardy_all_season  <-  country_all_iso_all%>%
  mutate(year = year(air_date)) %>%
  group_by(iso3, year) %>%
  add_tally(name = "count") %>%
  ungroup() %>%
  mutate(iso3 = toupper(iso3),
         iso3 = as.factor(iso3)) %>%
  rename(ISO3V10 = iso3) %>%
  full_join(countryExData) %>%
  filter(!is.na(count)) %>%
  rename(iso3 = "ISO3V10") %>%
  clean_names() %>%
  mutate(ratio = count / landarea)



jeopardy_all_season %>%
  select(iso3, landarea, year, geo_subregion, count) %>%
  #filter(iso3 != 'USA') %>%
  distinct() %>%
  group_by(year) %>% #summarize(number = n()) %>% arrange(desc(number)) %>%
  drop_na() %>%
  nest() %>%
  mutate(model = purrr::map(data, ~lm(count ~ landarea, data = .x) %>% 
                              tidy() %>% select(term, estimate))) %>%
  unnest(c(model)) %>% #arrange(desc(p.value)) <-- all p values are <0.05
  pivot_wider(names_from = term, values_from = estimate) %>%
  clean_names %>%
  rename(slope = landarea) %>%
  unnest(data) %>%
  ggplot() +
  geom_point(aes(x = landarea, y = count, color = year)) +
  geom_abline(aes(slope = slope, intercept = intercept, color = year))



ggplotly(jeopardy_all_season %>%
  select(iso3, landarea, year, epi_regions, count) %>%
  #filter(iso3 != 'USA') %>%
  distinct() %>%
  group_by(year, epi_regions) %>% #summarize(number = n()) %>% arrange(desc(number)) %>%
  drop_na() %>%
  nest() %>%
  mutate(model = purrr::map(data, ~lm(count ~ landarea, data = .x) %>% 
                              tidy() %>% select(term, estimate))) %>%
  unnest(c(model)) %>% #arrange(desc(p.value)) <-- all p values are <0.05
  pivot_wider(names_from = term, values_from = estimate) %>%
  clean_names %>%
  rename(slope = landarea) %>%
  unnest(data) %>%
  ggplot() +
  geom_point(aes(x = landarea, y = count, frame = year, color = epi_regions, label = iso3)) +
  geom_abline(aes(slope = slope, intercept = intercept, frame = year, color = epi_regions))
)



ggplotly(jeopardy_all_season %>%
           select(iso3, gdp_capita_mrya, year, epi_regions, count) %>%
           #filter(iso3 != 'USA') %>%
           distinct() %>%
           group_by(year) %>% #summarize(number = n()) %>% arrange(desc(number)) %>%
           drop_na() %>%
           nest() %>%
           mutate(model = purrr::map(data, ~lm(count ~ gdp_capita_mrya, data = .x) %>% 
                                       tidy() %>% select(term, estimate))) %>%
           unnest(c(model)) %>% #arrange(desc(p.value)) <-- all p values are <0.05
           pivot_wider(names_from = term, values_from = estimate) %>%
           clean_names %>%
           rename(slope = gdp_capita_mrya) %>%
           unnest(data) %>%
           ggplot() +
           geom_point(aes(x = gdp_capita_mrya, y = count, frame = year, color = epi_regions, label = iso3)) +
           geom_abline(aes(slope = slope, intercept = intercept, frame = year))
)
