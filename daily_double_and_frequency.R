# R script for daily double & frequency analysis

# libraries needed
library(readr)
library(tidyverse)
library(dplyr)
library(rworldmap)
library(rvest)
library(tidyr)
library(janitor)
library(ggplot2)
library(rgeos)
library(data.table)
library(lubridate)
library(ggthemes)
library(plotly)

# upload season 1 Jeopardy data
jeopardy_s1 <- read_tsv(
  "https://raw.githubusercontent.com/jwolle1/jeopardy_clue_dataset/master/season1.tsv")

# look at Jeopardy data
jeopardy_s1 # has character column which tells if clue is a daily double

#filter to clues that are daily doubles
# library(dplyr)
daily_double <- jeopardy_s1 %>% 
  filter(daily_double == "yes") # 62 clues which are daily doubles in season 1


data("countryExData")
data("countryRegions")
data("countrySynonyms")

# load denonyms
webpage <- read_html("https://en.wikipedia.org/wiki/List_of_adjectival_and_demonymic_forms_for_countries_and_nations")


# upload all seasons data
jeopardy_all <- read_tsv("master_season1-35.tsv/master_season1-35.tsv")

# filter data
daily_double_all <- jeopardy_all %>% 
  filter(daily_double == "yes")

# demonyms
table <- webpage %>% 
  html_nodes("table") %>% 
  html_table(header = F)
table <- table[[1]]

names(table) = table[1,]
table <- table %>% 
  slice(-1) %>% 
  clean_names()

# converting country synonyms to full list - one ob for each adjectival/demonym
demonym_table <- table %>% 
  as.tibble() %>% 
  mutate(country_entity_name = str_replace(country_entity_name, "\\[.\\]", ""),
         adjectivals = str_replace(adjectivals, "\\[.\\]", ""),
         demonyms = str_replace(demonyms, "\\[.\\]", "")) %>% 
  separate_rows(adjectivals, sep = ",\\s|/|\\sor\\s") %>% 
  separate_rows(demonyms, sep = ",\\s|/|\\sor\\s")


countrySynonyms_full <- countrySynonyms %>% 
  pivot_longer(name1:name8, names_to = "name", values_to = "country") %>% 
  filter(!is.na(country) & country != "") %>% 
  drop_na() %>% 
  mutate_all(toupper)

country_names_full <- countrySynonyms_full %>% 
  select(-c(name, ID)) %>% 
  left_join(demonym_table, by = c("country" = "country_entity_name")) %>% 
  pivot_longer(country:demonyms, names_to = "name_type", values_to = "names") %>% 
  select(-name_type) %>% 
  distinct() %>% 
  clean_names() %>% 
  drop_na() %>% 
  filter(iso3 != "")

# filter answers & questions that have countries mentioned
country_answers_all <-  daily_double_all %>%
  filter(str_detect(string = answer, pattern = paste0(paste(
    country_names_full$names, collapse = "|"),"[^a-z]")) ) %>%
  mutate(country_a = str_extract_all(string = answer, pattern = paste0(
    paste(country_names_full$names, collapse = "|"),"[^a-z]"))) %>% 
  unnest(country_a)

country_questions_all <-  daily_double_all %>%
  filter(str_detect(string = answer, pattern = paste0(paste(
    country_names_full$names, collapse = "|"),"[^a-z]")) ) %>%
  mutate(country_q = str_extract_all(string = question, pattern = paste0(
    paste(country_names_full$names, collapse = "|"),"[^a-z]"))) %>%
  unnest(country_q)

# joining iso codes
country_answers_iso_all <- country_answers_all %>%
  left_join(country_names_full, by = c("country_a" = "names")) %>%
  rename(country = country_a) %>%
  mutate(type = rep("answer", nrow(.)))

country_questions_iso_all <- country_questions_all %>%
  left_join(country_names_full, by = c("country_q" = "names")) %>%
  rename(country = country_q)%>%
  mutate(type = rep("question", nrow(.)))

country_all_iso_allszn <- full_join(country_answers_iso_all, 
                                    country_questions_iso_all) %>%
  filter(!(category == "AMERICAN INDIANS" & iso3 == "ind"),
         !(iso3 %in% c("iot", "atf"))) %>% 
  mutate(iso3 = toupper(iso3))


# map creation
wmap <- getMap(resolution = "low")
wmap <- spTransform(wmap, CRS("+proj=robin"))
# get centroids
centroids <- gCentroid(wmap, byid = TRUE, id = wmap@data$ISO3)
centroids <- data.frame(centroids)
setDT(centroids, keep.rownames = TRUE)[]
setnames(centroids, "rn", "country_iso3c")

all_country_iso <- country_all_iso_allszn %>% 
  mutate(date = ymd(air_date)) %>% 
  mutate(year = year(date)) %>% 
  select(c(round, value, daily_double, answer, question, country, iso3,
           type, date, year)) %>% 
  group_by(iso3) %>% 
  summarize(season_count = n()) %>% 
  left_join(countrySynonyms_full, by = c('iso3' = 'ISO3')) %>%  
  filter(name == "NAME1") %>% 
  mutate(country = str_to_title(country)) %>% 
  mutate(hover = with(all_country_iso, paste(country, '<br>',
                                                    "Total:", season_count)))


# join new data set to map
wmap_df <- fortify(wmap, region = "ISO3")
wmap_df <- left_join(wmap_df, all_country_iso, by = c('id' = 'iso3'))
wmap_df <- left_join(wmap_df, centroids, by = c('id' = 'country_iso3c'))

# attempt at gif creation
ggplot(data = wmap_df) +
  geom_polygon(aes(x = long, y = lat, group = group,fill = season_count)) +
  theme_void() +
  theme(legend.position = "bottom") +
  transition_states(year)

# plotly
p <- ggplot(data = wmap_df) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = season_count,
                   text = hover)) +
  labs(title = "Number of Country Mentions\nDaily Doubles Season 1 through 35",
       fill = "Number of Mentions") +
  theme_map()

plotly <- ggplotly(p, tooltip = "text")

plotly