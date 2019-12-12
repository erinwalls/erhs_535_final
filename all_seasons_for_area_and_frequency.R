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

#### Modify data for world maping
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
##### R world mapping
all_season_worldmap <- jeopardy_all_season %>%
  joinCountryData2Map(joinCode = "ISO3", nameCountryColumn = "ISO3V10") %>%
  mapCountryData(nameColumnToPlot = "Ratio") 


##### In following section, I modified the data in order to do statistic analysis
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

##### After viewing the data, I realize that there are some duplicated data row. In order to avoid them, I used unique() to extract rows

all_season_unique <-all_season_simple %>%
  unique() %>%
  filter (!is.na(landarea)) %>%
  filter (!is.na(GDP_capita.MRYA)) 

str(all_season_unique)

#####Then I write a CSV in case something will be wrong

write.csv(all_season_unique, file = "all_season_unique.csv")

###### Then I view it, and it looks okay
view(all_season_unique)

######## Before we seperate the analysis by season, we try to see whether season influence the Ratio (count/landarea)
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
