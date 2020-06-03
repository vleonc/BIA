library(shiny)
library(leaflet)
library(RColorBrewer)

setwd("/home/victorio/BIA")
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
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    listings[listings$price >= input$range[1] & listings$price <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, listings$price)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(listings) %>% addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
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
      addAwesomeMarkers(~longitude, ~latitude, icon=icons, label=~as.character(room_type), clusterOptions = markerClusterOptions()
      #addAwesomeMarkers(~longitude, ~latitude, icon=icons, label=~as.character(room_type), clusterOptions = markerClusterOptions()
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = listings)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
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