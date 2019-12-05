# Attempting cleaner version of data as it stands

#comment out once s1 is loaded
#source("s1_download.R")

jeopardy_all <- read_tsv("master_season1-35.tsv/master_season1-35.tsv") #downloaded manually; couldn't figure out how to read w/ file being zipped


library(rworldmap)
library(tidyverse)
library(rvest)
library(magrittr)
library(janitor)
library(lubridate)
data("countryExData")
data("countryRegions")
data("countrySynonyms")

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
  filter(!is.na(country) & country != "") 

country_names_full <- countrySynonyms_full %>%
  select(-c(name, ID)) %>%
  left_join(demonym_table, by = c("country" = "country_entity_name")) %>%
  pivot_longer(country:demonyms, names_to = "name_type", values_to = "names") %>%
  drop_na()%>%
  clean_names()



country_ansmwers_all <-  jeopardy_all %>%
  filter(str_detect(string = answer, pattern = paste0(paste(country_names_full$names, collapse = "|"),"[^a-z]")) ) %>%
  mutate(country_a = str_extract_all(string = answer, pattern = paste0(paste(country_names_full$names, collapse = "|"),
                                                                       "[^a-z]"))) %>%
  unnest(country_a)


country_questions_all <-  jeopardy_all %>%
  filter(str_detect(string = answer, pattern = paste0(paste(country_names_full$names, collapse = "|"),"[^a-z]")) ) %>%
  mutate(country_q = str_extract_all(string = question, pattern = paste0(paste(country_names_full$names, collapse = "|"),
                                                                       "[^a-z]"))) %>%
  unnest(country_q)


country_ansmwers_iso_all <- country_ansmwers_all %>%
  left_join(country_names_full, by = c("country_a" = "names")) %>%
  rename(country = country_a) %>%
  mutate(type = rep("answer", nrow(.)))

country_questions_iso_all <- country_questions_all %>%
  left_join(country_names_full, by = c("country_q" = "names")) %>%
  rename(country = country_q)%>%
  mutate(type = rep("question", nrow(.)))

country_all_iso_all <- full_join(country_ansmwers_iso_all, country_questions_iso_all) %>%
  filter(!(category == "AMERICAN INDIANS" & iso3 == "ind")) #getting rid of at least some false positives


iso3_tally_all <- country_all_iso_all %>%
  group_by(iso3) %>%
  summarize(count = n()) %>%
  arrange(desc(count))



iso3_tally_all <- country_all_iso_all %>%
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

write.csv(iso3_tally_all, "iso3_tally_all.csv")




