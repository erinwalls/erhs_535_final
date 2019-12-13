# Attempting cleaner version of data as it stands

#comment out once s1 is loaded
#source("s1_download.R")

insta


library(rworldmap)
library(tidyverse)
library(rvest)
library(magrittr)
library(janitor)
library(lubridate)
data("countryExData")
data("countryRegions")
data("countrySynonyms")

jeopardy_all <- read_tsv("master_season1-35.tsv/master_season1-35.tsv") #downloaded manually; couldn't figure out how to read w/ file being zipped

#Loading demonyms

webpage <- read_html("https://en.wikipedia.org/wiki/List_of_adjectival_and_demonymic_forms_for_countries_and_nations")

table <- webpage %>%
  html_nodes("table") %>%
  html_table(header=F)
table <- table[[1]]

names(table) = table[1,]
table <- table %>%
  slice(-1) %>%
  clean_names()

#Converting country synoyms to full list

demonym_table <- table %>%
  as.tibble() %>%
  mutate(country_entity_name = str_replace(country_entity_name, "\\[.\\]", ""),
         adjectivals = str_replace(adjectivals, "\\[.\\]", ""),
         demonyms = str_replace(demonyms, "\\[.\\]", "")) %>%
  #
  separate_rows(adjectivals, sep = ",\\s|/|\\sor\\s") %>%
  separate_rows(demonyms, sep = ",\\s|/|\\sor\\s")



countrySynonyms_full <- countrySynonyms %>%
  pivot_longer(name1:name8, names_to = "name", values_to = "country") %>%
  filter(!is.na(country) & country != "") %>%
  drop_na()

country_names_full <- countrySynonyms_full %>%
  select(-c(name, ID)) %>%
  left_join(demonym_table, by = c("country" = "country_entity_name")) %>%
  pivot_longer(country:demonyms, names_to = "name_type", values_to = "names") %>%
  select(-name_type) %>% 
  distinct() %>%
  clean_names() %>%
  drop_na() %>%
  filter(iso3 != "")

country_questions_all <-  jeopardy_all %>%
  #filter(year(air_date) %in% 2004:2010) %>%
  mutate(country_q = str_extract_all(string = question, pattern = paste0(paste(country_names_full$names, collapse = "|"),
                                                                         "[^a-z]"))) %>%
  filter(!is.na(country_q)) %>%
  unnest(country_q)

country_answers_all <-  jeopardy_all %>%
  #filter(year(air_date) %in% 2004:2010) %>%
  mutate(country_a = str_extract_all(string = answer, pattern = paste0(paste(country_names_full$names, collapse = "|"),
                                                                         "[^a-z]"))) %>%
  filter(!is.na(country_a)) %>%
  unnest(country_a)

country_merge_all <- full_join(country_answers_all, country_questions_all) %>%
  pivot_longer(c(country_a, country_q), names_to = "type", values_to = "country") %>%
  drop_na()


#country_merge_all_4_10 <- country_merge_all

country_all_iso_all <- country_merge_all %>%
  left_join(country_names_full, by = c("country" = "names")) %>%
  filter(!(category %in% c("AMERICAN INDIANS", "AMERICAN INDIAN TRIBES") & iso3 == "ind"),
         !(iso3 %in% c("iot","atf"))) %>% #getting rid of some more fasle positives
  mutate(iso3 = toupper(iso3),
         iso3 = as.factor(iso3))
  
#Superfluous, but keeping as ~templates for tallies  
iso3_tally_all <- country_all_iso_all %>%
  group_by(iso3) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


iso3_tally_all_years <- country_all_iso_all %>%
  mutate(year = year(air_date)) %>% 
  group_by(iso3, year) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

jeopadry_country_merge_all <- country_all_iso_all %>%
  group_by(iso3) %>%
  add_tally(name = "count") %>%
  ungroup() %>%
  mutate(iso3 = toupper(iso3),
         iso3 = as.factor(iso3)) %>%
  rename(ISO3V10 = iso3) %>%
  full_join(countryExData) %>%
  filter(!is.na(count))


write_csv(country_all_iso_all, "country_all_iso_all.csv",row.names = FALSE) 
write_csv(jeopadry_country_merge_all, "jeopadry_country_merge_all.csv")


