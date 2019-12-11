library(dplyr)
library(rworldmap)
library(tidyverse)
library(rvest)
library(magrittr)
library(janitor)
library(rworldmap)
library(tidyverse)
library(rvest)
library(magrittr)
library(janitor)
library(lubridate)
library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(ggrepel)
library(readr)
library(ggplot2)
library(dlnm)
data("countryExData")
data("countryRegions")
data("countrySynonyms")


country_all_iso_all <- read.csv("country_all_iso_all.csv") #downloaded manually; couldn't figure out how to read w/ file being zipped


head(country_all_iso_all ) 


jeopardy_all_season  <-  country_all_iso_all%>%
  group_by(iso3) %>%
  add_tally(name = "count") %>%
  ungroup() %>%
  mutate(iso3 = toupper(iso3),
         iso3 = as.factor(iso3)) %>%
  rename(ISO3V10 = iso3) %>%
  full_join(countryExData) %>%
  filter(!is.na(count)) %>%
  mutate(Ratio = count / landarea)

all_season_worldmap <- jeopardy_all_season %>%
  joinCountryData2Map(joinCode = "ISO3", nameCountryColumn = "ISO3V10") %>%
  mapCountryData(nameColumnToPlot = "Ratio") 


all_season_simple <- jeopardy_all_season  %>%
  mutate(Ratio = count / landarea) %>%
  mutate(air_date = ymd(air_date))%>%
  separate(air_date, sep="-", into = c("year", "month", "day")) %>%
  mutate(season = year) %>%
  mutate(season = as_factor(x = season))%>%
  select(count,Ratio,season,landarea,ISO3V10,GDP_capita.MRYA)%>%
  rename(country=ISO3V10) %>%
  mutate(country = as_factor(x=country)) %>%
  filter (!is.na(country))

view(all_season_simple) 

all_season_unique <-all_season_simple %>%
  unique() %>%
  filter (!is.na(landarea)) %>%
  filter (!is.na(GDP_capita.MRYA)) 

head(all_season_unique) 

str(all_season_unique)

write.csv(all_season_unique, file = "all_season_unique.csv")

view(all_season_unique)



mod_2 <- lm(Ratio ~ season, data = all_season_unique)
Anova(mod_2, type = 3)



library(broom)
tidy(mod_2)

glance(mod_2)

map_count_season_landarea <- jeopardy_all_season_simple%>%
  ggplot() + 
  geom_point(aes(x = season, y = count, color = landarea),size=0.5) + 
  geom_text_repel(aes(x = season, y = count, color=landarea, label=country),
                  force = 0.5, direction = c("both"),size=2)+
  theme_classic(base_size = 7) +
  theme(plot.title = element_text(size = 7, face = "bold"),
        axis.title = element_text(size = 7), legend.position = "bottom") +
  labs(x = "Seasons",y = expression('Frequencey'),
       color = "Landarea") 


geom_line(aes(x = season, y = count, color = landarea, group=country), size = 0.5) 
  
map_count_season_landarea 
