require(rjson)


setwd("/Users/mickey/Documents/GitHub/data-products")

if (!file.exists("courts.csv")){
  fileURL <- "https://www.data.act.gov.au/api/views/igti-4f4a/rows.csv?accessType=DOWNLOAD"
  download.file(fileURL, "courts.csv", method="curl")
} 


courts <- read.csv("courts.csv") %>% select(ID, TYPE, DIVISION,LATITUDE, LONGITUDE)


courts %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())
