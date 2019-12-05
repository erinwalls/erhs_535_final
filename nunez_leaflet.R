#comment out once s1 is loaded
#source("s1_download.R")

library(rworldmap)
data("countryExData")
data("countryRegions")
data("countrySynonyms")
library("leaflet")

library(readr)
jeopardy_s1 <- read_tsv("https://raw.githubusercontent.com/jwolle1/jeopardy_clue_dataset/master/season1.tsv")
write_tsv(jeopardy_s1, "season1.tsv")

df<-jeopardy_s1 %>%
  filter(str_detect(string = answer, pattern = paste(countrySynonyms$name1, collapse = "|") ) | str_detect(string = question, pattern = paste(countrySynonyms$name1, collapse = "|") )) %>%
  mutate(country = str_extract(string = answer, pattern = paste(countrySynonyms$name1, collapse = "|") )) %>%
  mutate(country2 = str_extract(string = question, pattern = paste(countrySynonyms$name1, collapse = "|") )) %>%
  group_by(daily_double,country) %>%
  add_tally(name = "country_1") %>%
  ungroup %>%
  group_by(daily_double, country2) %>%
  add_tally(name = "country_2") %>%
  view()

#jess nunez
library("ggmap")
test<- df %>%
  select(answer, question, country, country_1) %>%
  group_by(country) %>%
  mutate(lat=country, long=country2) %>%
  group_by(country, country_1) %>%
  nest()
as.numeric(test$country_1)

-------------------------
library("rnaturalearth")
library("sf")  
countries <- ne_countries(returnclass = "sf")  
france <- countries %>%
  filter(name== "France")
library("ggplot2")  

ggplot() +
  geom_sf(data = countries, aes(fill= pop_est)) 


library("viridisLite")

bins<- c(0,1,2,3,4,5,15,20, Inf)
pal<- colorBin("YlOrRd", domain = test$country_1, bins = bins)

labels<- sprintf("<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
                 test$country, test$country_1) %>% 
  lapply(htmltools::HTML)

leaflet(test) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(fillColor = ~pal(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
            position = "bottomright")
  



??MapBox





 ------ 
int<-leaflet() %>%
  addProviderTiles("Esri.NatGeoWorldMap") %>%
  addPolygons(data= countries, popup = polygon_popup,color = "#000000", fillColor = "969696", weight = 2) %>%
  addCircleMarkers(data = test, radius = 2, color = pal(test$country_1))

-----
int<-leaflet() %>%
  addProviderTiles("Esri.NatGeoWorldMap") %>%
  addPolygons(data= countries, color  = "#000000", fillColor = "969696", weight = 2) %>%
  addPolygons(data= test$country, fillColor = "~pal()", weight = 2) %>%
  addCircleMarkers(data = test, radius = 2, color = pal(test$country_1))





