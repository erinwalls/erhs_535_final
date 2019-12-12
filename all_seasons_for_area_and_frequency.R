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
library(broom)
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

View(jeopardy_all_season)
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
  select(count,Ratio,season,landarea,ISO3V10,GDP_capita.MRYA, EPI_regions)%>%
  rename(country=ISO3V10) %>%
  mutate(country = as_factor(x=country)) %>%
  filter (!is.na(country)) %>%
  mutate(EPI_regions = as_factor(x=EPI_regions))

view(all_season_simple) 

##### After viewing the data, I realize that there are some duplicated data row. In order to avoid them, I used unique() to extract rows

all_season_unique <-all_season_simple %>%
  distinct() %>%
  filter (!is.na(landarea)) %>%
  filter (!is.na(GDP_capita.MRYA)) %>%
  rename(GDP = GDP_capita.MRYA) %>%
  rename(continent = EPI_regions)

#####Then I write a CSV in case something will be wrong

write.csv(all_season_unique, file = "all_season_unique.csv")

###### Then I view it, and it looks okay, but I realized that the count are same for same country, which means ......... we cann't run season here
view(all_season_unique)
str(all_season_unique)
all_season_onecountry <- all_season_unique %>%
  filter(season == "2018") 

 
view(all_season_onecountry)
######## Before we seperate the analysis by season, we try to see whether season or GDP (as we discussed) influence the Ratio (count/landarea)
mod_1 <- lm(count ~ landarea, data = all_season_onecountry)
plot(mod_1)
Anova(mod_1, type = 3)
augment(mod_1) %>% 
  ggplot(aes(x = landarea, y = count)) + 
  geom_point(size = 0.8, alpha = 0.5) + 
  geom_line(aes(x = landarea, y = .fitted), color = "red", size = 2) + 
  theme_classic()

mod_2 <- lm(count ~ GDP, data = all_season_onecountry)
plot(mod_2)
Anova(mod_2, type = 3)
augment(mod_2) %>% 
  ggplot(aes(x = GDP, y = count)) + 
  geom_point(size = 0.8, alpha = 0.5) + 
  geom_line(aes(x = GDP, y = .fitted), color = "red", size = 2) + 
  theme_classic()



####### Unfortunally, the ratio was not significantly influence by seanson, but it is still worth to try. Also, the Ratio is influenced by GDP

map_count_season_landarea1 <- all_season_onecountry%>%
  ggplot() + 
  geom_point(aes(x = landarea, y = count, color = GDP),size=3) + 
  geom_label(aes(x = landarea, y = count, color= GDP, label=country),
                  force = 0, direction = c("both"),size=3)+
  theme_classic(base_size = 12) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 12)) +
  labs(x = "Landarea",y = expression('Frequencey'),
       color = "GDP") 
map_count_season_landarea1


map_count_season_landarea2 <- all_season_onecountry%>%
  ggplot() + 
  geom_point(aes(x = landarea, y = count, color = GDP),size=2) + 
  geom_label(aes(x = landarea, y = count, color= GDP, label=country),
                  force = 0, direction = c("both"),size=2)+
  theme_classic(base_size = 8) +
  theme(plot.title = element_text(size = 8, face = "bold"),
        axis.title = element_text(size = 8), legend.position = "bottom") +
  labs(x = "Landarea",y = expression('Frequencey'),
       color = "Continent") +
  facet_wrap(facets = "continent", nrow = 2, scales = "free_y")
map_count_season_landarea2

library(scales)
map_count_season_landarea3 <- all_season_onecountry%>%
  mutate(country = fct_reorder(country,GDP))%>%
  ggplot(aes(x = country, fill = landarea)) + 
  geom_bar(aes(weight = count)) + 
  coord_flip() + 
  labs(x = "Coundry Ordered by GDP", y = "Frequency", fill = "Landarea")
map_count_season_landarea3


map_count_season_landarea4 <- all_season_onecountry%>%
  mutate(country = fct_reorder(country,landarea))%>%
  ggplot(aes(x = country, fill = GDP)) + 
  geom_bar(aes(weight = count)) + 
  coord_flip() + 
  labs(x = "Coundry Ordered by Landarea", y = "Frequency", fill = "GDP") +
  facet_wrap(facets = "continent", nrow = 2, scales = "free_y")
map_count_season_landarea4


map_count_season_landarea5 <- all_season_onecountry%>%
  mutate(country = fct_reorder(country,GDP))%>%
  ggplot(aes(x = country, fill = landarea)) + 
  geom_bar(aes(weight = count)) + 
  coord_flip() + 
  labs(x = "Coundry Ordered by GDP", y = "Frequency", fill = "Landarea") +
  facet_wrap(facets = "continent", nrow = 2, scales = "free_y")
map_count_season_landarea5
