#comment out once s1 is loaded
source("s1_download.R")

library(rworldmap)
data("countryExData")
data("countryRegions")
data("countrySynonyms")

#Aggregating all country names
countrySynonyms_full <- countrySynonyms %>%
  pivot_longer(name1:name8, names_to = "name", values_to = "country") %>%
  filter(!is.na(country) & country != "") 

#Adding demonyms: 
demonyms <- read_csv("https://raw.githubusercontent.com/knowitall/chunkedextractor/master/src/main/resources/edu/knowitall/chunkedextractor/demonyms.csv") %>%
  dplyr::rename(demonym = Aalborgenser, country = Aalborg) %>%
  filter(country %in% countrySynonyms_full$country)
#filtered to just those matching country names; this list also had state/city/regional names as well

countries_and_demonyms <- c(countrySynonyms_full$country, demonyms$demonym)

jeopardy_s1_tallies_1 <- jeopardy_s1 %>%
  filter(str_detect(string = answer, pattern = paste0("(",paste(countries_and_demonyms, collapse = "|"),")","[[:punct:]|[:space:]]")) |
           str_detect(string = question, pattern = paste0("(",paste(countries_and_demonyms, collapse = "|"),")","[[:punct:]|[:space:]]"))) %>%
  mutate(country_a = gsub('[[:punct:] ]+','', str_extract(string = answer,
                                 pattern = paste0("(",paste(countries_and_demonyms, collapse = "|"),")","[[:punct:]|[:space:]]")))) %>%
  mutate(country_q = gsub('[[:punct:] ]+','', str_extract(string = question,
                                 pattern = paste0("(",paste(countries_and_demonyms, collapse = "|"),")","[[:punct:]|[:space:]]"))))
  
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


