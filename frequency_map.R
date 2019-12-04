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
  mapCountryData(nameColumnToPlot = "count")