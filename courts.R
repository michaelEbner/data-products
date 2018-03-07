require(rjson)
require(tidyverse)
require(leaflet)


setwd("/Users/mebner/Documents/for_me/R_coursera/GitHub/data-products")

if (!file.exists("courts.csv")){
  fileURL <- "https://www.data.act.gov.au/api/views/igti-4f4a/rows.csv?accessType=DOWNLOAD"
  download.file(fileURL, "courts.csv", method="curl")
} 


courts <- read.csv("courts.csv") %>% select(ID, TYPE, DIVISION,LATITUDE, LONGITUDE)

logo <- makeIcon(
  iconUrl = "basketball-court-clipart-black-and-white-15.png",
  iconWidth = 31*215/230, iconHeight = 31,
  iconAnchorX = 31*215/230/2, iconAnchorY = 16
)

courts %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions(), icon = logo)
