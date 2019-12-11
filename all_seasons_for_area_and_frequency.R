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


country_all_iso_all <- read.csv("country_all_iso_all.csv") #downloaded manually; couldn't figure out how to read w/ file being zipped


head(country_all_iso_all ) 


jeopardy_all_season  <-  country_all_iso_all%>%
  group_by(iso3) %>%
  add_tally(name = "count") %>%
  ungroup() %>%
  mutate(iso3 = toupper(iso3),
         iso3 = as.factor(iso3)) %>%
  rename(ISO3V10 = iso3) %>%
  full_join(countryExData) %>%
  filter(!is.na(count))

jeopardy_all_season    %>%
  mutate(Ratio = count / landarea) %>%
  joinCountryData2Map(joinCode = "ISO3", nameCountryColumn = "ISO3V10") %>%
  mapCountryData(nameColumnToPlot = "Ratio")

library(dlnm)

jeopardy_all_season <- jeopardy_all_season  %>%
  mutate(Ratio = count / landarea)

str(jeopardy_all_season) 

mod_2 <- lm(count ~ landarea, data = jeopardy_all_season  )
mod_2

library(broom)
tidy(mod_2)

glance(mod_2)

augment(mod_2) %>%
  ggplot(aes(x = landarea, y = count)) + 
  geom_point(size = 0.8, alpha = 0.5, col = "gray") + 
  geom_line(aes(x = landarea, y = .fitted), color = "red", size = 2) + 
  theme_classic()  

