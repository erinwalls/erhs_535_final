# Attempting cleaner version of data as it stands

#comment out once s1 is loaded
#source("s1_download.R")

library(rworldmap)
library(tidyverse)
library(rvest)
library(magrittr)
library(janitor)
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



country_ansmwers <-  jeopardy_s1 %>%
  filter(str_detect(string = answer, pattern = paste0(paste(country_names_full$names, collapse = "|"),"[^a-z]")) ) %>%
  mutate(country_a = str_extract_all(string = answer, pattern = paste0(paste(country_names_full$names, collapse = "|"),
                                                                       "[^a-z]"))) %>%
  unnest(country_a)


country_questions <-  jeopardy_s1 %>%
  filter(str_detect(string = answer, pattern = paste0(paste(country_names_full$names, collapse = "|"),"[^a-z]")) ) %>%
  mutate(country_q = str_extract_all(string = question, pattern = paste0(paste(country_names_full$names, collapse = "|"),
                                                                       "[^a-z]"))) %>%
  unnest(country_q)


country_ansmwers_iso <- country_ansmwers %>%
  left_join(country_names_full, by = c("country_a" = "names")) %>%
  rename(country = country_a) %>%
  mutate(type = rep("answer", nrow(.)))

country_questions_iso <- country_questions %>%
  left_join(country_names_full, by = c("country_q" = "names")) %>%
  rename(country = country_q)%>%
  mutate(type = rep("question", nrow(.)))

full_join(country_ansmwers_iso, country_questions_iso)
