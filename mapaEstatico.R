list.of.packages <- c("devtools", "leaflet", "tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

devtools::install_github("dkahle/ggmap")


library(tidyverse)
library(ggmap)


#“terrain”, “terrain-background”, “terrain-labels”, “terrain-lines”, “toner”, 
#“toner-2010”, “toner-2011”, “toner-background”, “toner-hybrid”, “toner-labels”, 
#“toner-lines”, “toner-lite”, “watercolor”

load("listings.Rdata")
load("listings_detailed.Rdata")



height <- max(listings$latitude) - min(listings$latitude)
width <- max(listings$longitude) - min(listings$longitude)
borders <- c(bottom  = min(listings$latitude)  - 0.1 * height,
             top     = max(listings$latitude)  + 0.1 * height,
             left    = min(listings$longitude) - 0.1 * width,
             right   = max(listings$longitude) + 0.1 * width)

map <- get_stamenmap(borders, zoom = 11, maptype = "terrain")
ggmap(map) +
  geom_point(data = listings, mapping = aes(x = longitude, y = latitude,
                                          colour = room_type), alpha=0.5)


