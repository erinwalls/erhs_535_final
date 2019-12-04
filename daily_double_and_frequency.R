# R script for daily double & frequency analysis

# upload Jeopardy data
library(readr)
jeopardy_s1 <- read_tsv(
  "https://raw.githubusercontent.com/jwolle1/jeopardy_clue_dataset/master/season1.tsv")

# look at Jeopardy data
jeopardy_s1 # this data has character column which tells if clue is a daily double

# need to join with country tally or follow cleaning process to get which clues are
# daily doubles and have a country mentioned

#filter to clues that are daily doubles
library(magrittr)
library(dplyr)
daily_double <- jeopardy_s1 %>% 
  filter(daily_double == "yes") # 62 clues which are daily doubles in season 1

# load data and libraries needed for country name data
library(rworldmap)
library(tidyverse)
library(rvest)
library(magrittr)
library(janitor)

data("countryExData")
data("countryRegions")
data("countrySynonyms")

# load denonyms
webpage <- read_html("https://en.wikipedia.org/wiki/List_of_adjectival_and_demonymic_forms_for_countries_and_nations")

table <- webpage %>% 
  html_nodes("table") %>% 
  html_table(header = F)
table <- table[[1]] 

names(table) = table[1,]
table <- table %>% 
  slice(-1) %>% 
  clean_names()

# converting country synonyms to full list - one obs for each adjectival/demonym
denonym_table <- table %>% 
  as.tibble() %>% 
  mutate(country_entity_name = str_replace(country_entity_name, "\\[.\\]", ""),
         adjectivals = str_replace(adjectivals, "\\[.\\]", ""),
         demonyms = str_replace(demonyms, "\\[.\\]", "")) %>% 
  separate_rows(adjectivals, sep = ",\\s|/|\\sor\\s") %>% 
  separate_rows(demonyms, sep = ",\\s|/|\\sor\\s")

countrySynonyms_full <- countrySynonyms %>% 
  pivot_longer(name1:name8, names_to = "name", values_to = "country") %>% 
  filter(!is.na(country) & country != "")

country_names_full <- countrySynonyms_full %>% 
  select(-c(name, ID)) %>% 
  left_join(denonym_table, by = c("country" = "country_entity_name")) %>% 
  pivot_longer(country:demonyms, names_to = "name_type", values_to = "names") %>% 
  drop_na()

  