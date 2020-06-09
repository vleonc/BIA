library(shinythemes)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)


shinyServer(function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    
    listings2 <- filter(listings, (room_type %in% input$roomType))
    listings2 <- listings[listings$price >= input$range[1] & listings$price <= input$range[2], ]
    return(listings2)
  })
  
  
  output$map <- renderLeaflet({
    
    leaflet(listings) %>% addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
  
  observe({
    req(input$tab_being_displayed == "NYC map")
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
      markerColor = getColor(filteredData())
    )
    
    
    leafletProxy("map", data = filteredData()) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addAwesomeMarkers(~longitude, ~latitude, icon=icons, label=~as.character(room_type), clusterOptions = markerClusterOptions()
      )
  })
  
  
  pal <- colorNumeric("RdYlGn", listings$price, reverse = T)
  
  observe({
    req(input$tab_being_displayed == "NYC map")
    proxy <- leafletProxy("map", data = listings)
    
    proxy %>% clearControls()
    if (input$legend) {
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~price
      )
    }
  })

  output$mockData <- renderPlot(
    ggplot(listings, aes(x = room_type, y = price)) + geom_violin() + scale_y_log10()
    + ggtitle("Price by room type") + xlab("Room type") + ylab("Price")
    + theme(
      plot.title = element_text(color="black", size=24, face="bold.italic"),
      axis.title.x = element_text(color="black", size=16, face="bold"),
      axis.title.y = element_text(color="black", size=16, face="bold")
    ),
    # get top 50 listings by price
    top_df <- listings %>% top_n(n = 50, wt = price),
    
    # get background map
    top_height <- max(top_df$latitude) - min(top_df$latitude),
    top_width <- max(top_df$longitude) - min(top_df$longitude),
    top_borders <- c(bottom  = min(top_df$latitude)  - 0.1 * top_height,
                     top     = max(top_df$latitude)  + 0.1 * top_height,
                     left    = min(top_df$longitude) - 0.1 * top_width,
                     right   = max(top_df$longitude) + 0.1 * top_width),
    
    top_map <- get_stamenmap(top_borders, zoom = 12, maptype = "toner-lite"),
    
    # map of top 50 most expensive
    ggmap(top_map) +
      geom_point(data = top_df, mapping = aes(x = longitude, y = latitude,
                                              col = price)) +
      scale_color_gradient(low = "blue", high = "red")
  )
    
  output$mockData2 <- renderTable({
    return(mtcars)
  })
  
})