library(tidyverse)
library(leaflet)


load("listings.Rdata")
load("listings_detailed.Rdata")

devtools::install_github("dkahle/ggmap", ref = "tidyup")

height <- max(listings$latitude) - min(listings$latitude)
width <- max(listings$longitude) - min(listings$longitude)
borders <- c(bottom  = min(listings$latitude)  - 0.1 * height,
             top     = max(listings$latitude)  + 0.1 * height,
             left    = min(listings$longitude) - 0.1 * width,
             right   = max(listings$longitude) + 0.1 * width)

map <- get_stamenmap(borders, zoom = 11, maptype = "toner-lite")
ggmap(map) +
  geom_point(data = listings, mapping = aes(x = longitude, y = latitude,
                                          colour = room_type))




