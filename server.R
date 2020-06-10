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
  
  output$mockData <- renderTable({
    return(mtcars)
  })
  
  
  output$mockData2 <- renderPlot({
    ggplot(listings, aes(x = fct_infreq(neighbourhood_group), fill = room_type)) + theme_dark() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), ,axis.text.x = element_text(angle = -45)) +
      geom_bar() +
      labs(title = "Number of listings by district",
           x = "District", y = "Number of listings") +
      theme(legend.position = "bottom") +
      labs(fill="Room Type:")
      
  })
  
  output$mockData3 <- renderPlot({
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

