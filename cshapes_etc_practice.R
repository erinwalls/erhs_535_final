### 

#Testing out `rworldmap` package

###


#install.packages("rworldmap")
library(rworldmap)
library(tidyverse)


vignette("rworldmap")

data("countryExData")
data("countryRegions")
data("countrySynonyms")

names(countryExData)

data(countryExData)
sPDF <- joinCountryData2Map(countryExData, joinCode = "ISO3", nameJoinColumn = "ISO3V10")

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i") 
mapCountryData( sPDF, nameColumnToPlot="BIODIVERSITY" )
mapCountryData( sPDF, nameColumnToPlot="ENVHEALTH")

countryExData %>%
  mutate(EXAMPLE = (GDP_capita.MRYA * Population2005) / landarea) %>%
  joinCountryData2Map(joinCode = "ISO3", nameCountryColumn = "ISO3V10") %>%
  mapCountryData(nameColumnToPlot = "EXAMPLE")
  

gridExData %>%
  mapGriddedData()


#Testing cshapes (from ~tutorial on main site)

#install.packages("cshapes")
library(cshapes)
dmat <- distmatrix(as.Date("2002-1-1"), type="capdist")
write.table(dmat, "distmat2002.txt", row.names=T, col.names=T)
adjmat <- ifelse(dmat > 900, 0, 1)
diag(adjmat) <- 0
write.table(adjmat, "adjmat2002.txt", row.names=T, col.names=T)



GW <- TRUE # wether to use the Gleditsch & Ward country codes
disttype <- "capdist" # what type of distance we want to compute
result <- distlist(as.Date("1946-6-30"), type=disttype, useGW=GW)
result <- result[result$ccode1 < result$ccode2,] # we drop duplicate dyads
result$year <- 1946 
for (year in 1947:2008) {
  date.current <- paste(year, "6", "30", sep="-")
  result.current <- distlist(as.Date(date.current), type=disttype, useGW=GW)
  result.current <- result.current[result.current$ccode1 < result.current$ccode2,]
  result.current$year <- year
  result <- rbind(result, result.current)
}
#write.table(result, filename, row.names=F) # save complete table to file




#~Rough draft of subsetting data; need to somehow replace literal strings w/ e.g. a list of names from the dataset


jeopardy_s1 %>%
  filter(str_detect(string = answer, pattern = paste(countrySynonyms$name1, collapse = "|") ) | str_detect(string = question, pattern = paste(countrySynonyms$name1, collapse = "|") )) %>%
  mutate(country = str_extract(string = answer, pattern = paste(countrySynonyms$name1, collapse = "|") )) %>%
  mutate(country2 = str_extract(string = question, pattern = paste(countrySynonyms$name1, collapse = "|") )) %>% 
  mutate(country3 = str_extract(string = answer, pattern = paste(countrySynonyms$name8, collapse = "|") )) %>%
  mutate(country4 = str_extract(string = question, pattern = paste(countrySynonyms$name8, collapse = "|") ))
  
  
jeopardy_s1 %>%
  str_count(string = answer, pattern = paste(countrySynonyms$name1, collapse = "|"))

jeopardy_s1 %>%
  filter(str_detect(string = answer, pattern = paste(countrySynonyms$name1, collapse = "|") ) | str_detect(string = question, pattern = paste(countrySynonyms$name1, collapse = "|") )) %>%



country_name_vector <- vector()
country_mention_count <- numeric()
country_mention_dataframe = as.data.frame(cbind(country_name_vector, country_mention_count))

for(i in 1:nrow(countrySynonyms)){
  country_mention_count = 0
  for(j in 1:nrow(jeopardy_s1)){
    country_name_vector <- c(country_name_vector, countrySynonyms[i,1])
    if(strdetect(jeopardy_s1$answer, countrySynonyms[,i]) | str_detect(jeopardy_s1$question, countrySynonyms[,i])){
      country_mention_count = country_mention_count + 1
    }
    country_mention_dataframe = cat(country_mention_dataframe, cbind())
  }
}



jeopardy_s1_countries 
