library(tidyverse)
library(leaflet)

load("listings.Rdata")
#listings <- listings[listings[, "room_type"]=="Entire home/apt", ]

df.20 <- listings


getColor <- function(listings) {
  sapply(listings$room_type, function(room_type) {
    if(room_type == "Entire home/apt")	 {
      "green"
    } else if(room_type == "Private room") {
      "orange"
    } else if (room_type == "Shared room"){
      "red"
    } else {
      "purple"
    } 
    })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(df.20)
)

leaflet(df.20) %>% addTiles() %>%
  addAwesomeMarkers(~longitude, ~latitude, icon=icons, label=~as.character(room_type), clusterOptions = markerClusterOptions()
)