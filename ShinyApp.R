list.of.packages <- c("leaflet", "tidyverse", "shiny",'RColorBrewer')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()
                                   [,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(leaflet)
library(RColorBrewer)

load("listings.Rdata")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Magnitudes", 9, max(listings$price),
                            value = range(listings$price), step = 100
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  filteredData <- reactive({
    listings[listings$price >= input$range[1] & listings$price <= input$range[2],]
  })
  
  colorpal <- reactive({
    colorNumeric(input$colors, listings$price)
  })
  
  output$map <- renderLeaflet({
    leaflet(listings) %>% addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  

  observe({
    pal <- colorpal()
    
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
  
  observe({
    proxy <- leafletProxy("map", data = listings)
    
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~price
      )
    }
  })
}

shinyApp(ui, server)