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

jeopardy_all_season    %>%
  mutate(Ratio = count / landarea) %>%
  joinCountryData2Map(joinCode = "ISO3", nameJoinColumn = "iso3", nameCountryColumn = "iso3") %>%
  mapCountryData(nameColumnToPlot = "Ratio")

library(dlnm)

jeopardy_all_season <- jeopardy_all_season  %>%
  mutate(Ratio = count / landarea) %>%
  mutate(year = year(air_date))

#view(jeopardy_all_season) 

mod_2 <- lm(count ~ landarea, data = jeopardy_all_season  )
mod_2

jeopardy_all_season %>%
  select(iso3, landarea, year, geo_subregion, count) %>%
  filter(iso3 != 'USA') %>%
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
  geom_point(aes(x = landarea, y = count, frame = year, color = epi_regions)) +
  geom_abline(aes(slope = slope, intercept = intercept, frame = year)) +
  facet_wrap(. ~epi_regions)
)


library(broom)
tidy(mod_2)

glance(mod_2)

augment(mod_2) %>%
  ggplot(aes(x = landarea, y = count)) + 
  geom_point(size = 0.8, alpha = 0.5, col = "gray") + 
  geom_line(aes(x = landarea, y = .fitted), color = "red", size = 2) + 
  theme_classic()  

