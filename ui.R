library(shinythemes)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)

shinyUI(
  navbarPage(title = "Airbnb Visualisation", 
             id ="tab_being_displayed",
             
             theme = shinytheme("united"), #https://rstudio.github.io/shinythemes/
             
##### Overview ########## 
    tabPanel("Overview",
             br(),
             br(),
             br(),
             #img(src = "airbnb_overview.jpg", height = 600, weight =700, align="center")
             #use Shiny’s HTML tag functions to center the image
             #https://stackoverflow.com/questions/34663099/how-to-center-an-image-in-a-shiny-app
             HTML('<center><img src="airbnb_overview.jpg", height = 600, weight =700 ></center>')
             ),

##### Map ##########      
    tabPanel("NYC map",
      div(class="outer",
          tags$head(#customized CSS
            includeCSS("styles.css")),
          
      leafletOutput(outputId = "map", width = "100%", height = "100%"),
                          
      # Panel options: borough, Room Type, Price, Rating, Reviews
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE, 
                    top = 80, left = "auto", right = 20, bottom = "auto",
                    width = 320, height = "auto",
      h2("Airbnb in NYC"),
      sliderInput("range", "Price", 9, max(listings$price),
                  value = range(listings$price), step = 100
      ),
      selectInput("roomType","Select room type",
       c("Entire home/apt","Private room","Shared room","Hotel room"), multiple = T,
       selected =  c("Entire home/apt","Private room","Shared room","Hotel room")),
      checkboxInput("legend", "Show legend", TRUE)
      )
      ))
    
))
