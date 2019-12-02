#comment out once s1 is loaded
source("s1_download.R")


jeopardy_s1 %>%
  filter(str_detect(string = answer, pattern = paste(countrySynonyms$name1, collapse = "|") ) | str_detect(string = question, pattern = paste(countrySynonyms$name1, collapse = "|") )) %>%
  mutate(country = str_extract(string = answer, pattern = paste(countrySynonyms$name1, collapse = "|") )) %>%
  mutate(country2 = str_extract(string = question, pattern = paste(countrySynonyms$name1, collapse = "|") )) %>%
  group_by(country) %>%
  add_tally(name = "country_1") %>%
  ungroup %>%
  group_by(country2) %>%
  add_tally(name = "country_2")

