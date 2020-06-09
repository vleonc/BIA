library(shinythemes)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)


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
  
  output$mockData <- renderPlot({
    ggplot(filteredData(), aes(x = fct_infreq(room_type), fill = room_type)) +
      geom_bar(aes(y = (..count..)/sum(..count..)), show.legend = FALSE) +
      labs(title = "No. of listings by borough",
           x = "Room Type", y = "Percentage") +
      scale_y_continuous(labels = scales::percent) +
      theme(panel.background=element_rect(fill="white"),
            panel.grid=element_blank())
  })
  
  output$mockData3 <- renderTable({
    return(mtcars)
    
  })
  output$mockData2 <- renderTable({
    return(mtcars)
  })
  
})