#comment out once s1 is loaded
#source("s1_download.R")

library(rworldmap)
library(tidyverse)
library(rvest)
library(magrittr)
data("countryExData")
data("countryRegions")
data("countrySynonyms")

#Adding demonyms: 

demonyms <- read_csv("https://raw.githubusercontent.com/knowitall/chunkedextractor/master/src/main/resources/edu/knowitall/chunkedextractor/demonyms.csv") %>%
  dplyr::rename(demonym = Aalborgenser, country = Aalborg) #%>%
  filter(country %in% countrySynonyms$name1)

  
  #Noticed my current list seems spotty (e.g. "Korean" isn't included); trying to find my older one
  
  
  
  webpage <- read_html("https://en.wikipedia.org/wiki/List_of_adjectival_and_demonymic_forms_for_countries_and_nations")
  
  table <- webpage %>%
    html_nodes("table") %>%
    html_table(header=F)
  table <- table[[1]]
  
  names(table) = table[1,]
  table <- table %>%
    slice(-1) %>%
    clean_names()
  
  #table <- as_tibble(table) %>%
  
  
  
  
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
    drop_na()
  
  

jeopardy_s1_tallies_1 <- jeopardy_s1 %>%
  filter(str_detect(string = answer, pattern = paste0("(",paste(country_names_full$names, collapse = "|"),")","[[:punct:]|[:space:]|s]")) |
           str_detect(string = question, pattern = paste0("(",paste(country_names_full$names, collapse = "|"),")","[[:punct:]|[:space:]|s]"))) %>%
  mutate(country_a = gsub('[[:punct:] ]+','', str_extract(string = answer,
                                 pattern = paste0("(",paste(country_names_full$names, collapse = "|"),")","[[:punct:]|[:space:]|s]")))) %>%
  mutate(country_q = gsub('[[:punct:] ]+','', str_extract(string = question,
                                 pattern = paste0("(",paste(country_names_full$names, collapse = "|"),")","[[:punct:]|[:space:]|s]"))))
  
#Note: this doesn't detect all synonyms;
jeopardy_s1 %>% 
  mutate(US = str_detect(answer, "America|US|United States|United States of America|USA")) %>% 
  filter(US == TRUE)

jeopardy_s1_tallies_1 %>%
  select(country_a, country_q) %>%
  pivot_longer(c(country_q, country_a), names_to = "prompt", values_to = "country") %>%
  drop_na() %>%
  group_by(country) %>%
  tally(name = "count") %>%
  arrange(desc(count))


