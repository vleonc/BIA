library(shinythemes)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(ggmap)

shinyServer(function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    
    listings2 <- filter(listings, (room_type %in% input$roomType))
    listings2 <- listings2[listings2$price >= input$range[1] & listings2$price <= input$range[2], ]
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
  

  output$price1 <- renderPlot({
    ggplot(listings, aes(x = room_type, y = price)) + geom_boxplot() + 
      stat_summary(fun=mean, geom="point", shape=8, size=4, color="red", fill="red") + 
      theme_grey() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_y_log10() +
      ggtitle("Price by room type") + xlab("Room type") + ylab("Price") +
      theme(plot.title = element_text(color="black", size=24, face="bold.italic"),
            axis.title.x = element_text(color="black", size=16, face="bold"),
            axis.title.y = element_text(color="black", size=16, face="bold"))
  })
  
  output$price2 <- renderPlot({
    top_df <- listings %>% top_n(n = 100, wt = price)
    top_height <- max(top_df$latitude) - min(top_df$latitude)
    top_width <- max(top_df$longitude) - min(top_df$longitude)
    top_borders <- c(bottom  = min(top_df$latitude)  - 0.1 * top_height,
                     top     = max(top_df$latitude)  + 0.1 * top_height,
                     left    = min(top_df$longitude) - 0.1 * top_width,
                     right   = max(top_df$longitude) + 0.1 * top_width)
    top_map <- get_stamenmap(top_borders, zoom = 12, maptype = "toner-lite")
    ggmap(top_map) +
      geom_point(data = top_df, mapping = aes(x = longitude, y = latitude, col = price)) +
      scale_color_gradient(low = "green4", high = "red") +
      ggtitle("Map of the top 100 most expensive listings") +
      theme(plot.title = element_text(color="black", size=24, face="bold.italic")) +
      labs(colour="Price")
  })
  
  output$price3 <- renderPlot({
    listingsGroup <- listings %>%
      group_by(neighbourhood) %>%
      summarize(num_listings = n(),
                median_price = median(price),
                long = median(longitude),
                lat = median(latitude),
                borough = unique(neighbourhood_group))
    height <- max(listings$latitude) - min(listings$latitude)
    width <- max(listings$longitude) - min(listings$longitude)
    borders <- c(bottom  = min(listings$latitude)  - 0.1 * height,
                 top     = max(listings$latitude)  + 0.1 * height,
                 left    = min(listings$longitude) - 0.1 * width,
                 right   = max(listings$longitude) + 0.1 * width)
    map <- get_stamenmap(borders, zoom = 11, maptype = "toner-lite")
    ggmap(map) +
      geom_point(data = listingsGroup, mapping = aes(x = long, y = lat, col = median_price, size = num_listings)) +
      scale_color_gradient(low = "green4", high = "red") +
      ggtitle("Median price by neighborhood") +
      theme(plot.title = element_text(color="black", size=24, face="bold.italic")) +
      labs(colour="Median price", size="Agrupation")
  })
 

  output$roomTypeBarPlot <- renderPlot({
    ggplot(filteredData(), aes(x = fct_infreq(room_type), fill = room_type)) +
      geom_bar(aes(y = (..count..)/sum(..count..)), show.legend = FALSE) +
      labs(title = "% of listings per room type",
           x = "Room Type", y = "Percentage") +
      scale_y_continuous(labels = scales::percent) +
      theme(panel.background=element_rect(fill="white"),
            panel.grid=element_blank())
  })
  

  
  output$Hood <- renderPlot({
    ggplot(listings, aes(x = fct_infreq(neighbourhood_group), fill = room_type)) + theme_dark() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), ,axis.text.x = element_text(angle = -45)) +
      geom_bar() +
      labs(title = "Number of listings by district",
           x = "District", y = "Number of listings") +
      theme(legend.position = "bottom") +
      labs(fill="Room Type:")
      

  })
  
  output$Hood2 <- renderPlot({
    listings %>%
      group_by(neighbourhood) %>%
      summarize(num_listings = n(), 
                district = unique(neighbourhood_group)) %>%
      top_n(n = 20, wt = num_listings) %>%
      ggplot(aes(x = fct_reorder(neighbourhood, num_listings), 
                 y = num_listings, fill = district)) + theme_dark()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.text.x = element_text(angle = -45)) +
      geom_col() +
      theme(legend.position = "bottom") +
      labs(title = "Top 20 neighborhoods by number of listings",
           x = "Neighborhood", y = "Number of listings")
    
  })
})

