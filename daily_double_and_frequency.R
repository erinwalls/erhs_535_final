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
library(tidyr)

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
demonym_table <- table %>% 
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
  left_join(demonym_table, by = c("country" = "country_entity_name")) %>% 
  pivot_longer(country:demonyms, names_to = "name_type", values_to = "names") %>% 
  drop_na()

# filter answers & questions that have countries mentioned
country_answers <-  daily_double %>%
  filter(str_detect(string = answer, pattern = paste0(paste(country_names_full$names, collapse = "|"),"[^a-z]")) ) %>%
  mutate(country_a = str_extract_all(string = answer, pattern = paste0(paste(country_names_full$names, collapse = "|"),
                                                                       "[^a-z]"))) %>%
  unnest(country_a)

country_questions <-  daily_double %>%
  filter(str_detect(string = answer, pattern = paste0(paste(country_names_full$names, collapse = "|"),"[^a-z]")) ) %>%
  mutate(country_q = str_extract_all(string = question, pattern = paste0(paste(country_names_full$names, collapse = "|"),
                                                                         "[^a-z]"))) %>%
  unnest(country_q)

# joining iso codes to help reduce false positives
country_answers_iso <- country_answers %>%
  left_join(country_names_full, by = c("country_a" = "names")) %>%
  rename(country = country_a) %>%
  mutate(type = rep("answer", nrow(.)))

country_questions_iso <- country_questions %>%
  left_join(country_names_full, by = c("country_q" = "names")) %>%
  rename(country = country_q)%>%
  mutate(type = rep("question", nrow(.)))

country_all_iso <- full_join(country_answers_iso, country_questions_iso) %>%
  filter(!(category == "AMERICAN INDIANS" & ISO3 == "ind")) #getting rid of at least some false positives

iso3_tally <- country_all_iso %>%
  group_by(ISO3) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
  