list.of.packages <- c("leaflet", "tidyverse","shinythemes","shiny",
                      "RColorBrewer", "ggmap")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()
                                   [,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shinythemes)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(ggmap)

shinyUI(
  navbarPage(title = "Airbnb Visualisation", 
             id ="tab_being_displayed",
             
             theme = shinytheme("united"), 
             

    tabPanel("Overview",
             br(),
             br(),
             h2("New York City"),
             br(),
             br(),
             HTML('<center><img src="airbnb_overview.jpg", height = 600, weight =700 ></center>'),
             br(),
             br(),
             h2("G12"),
             h3("Xavier Heras, Víctor León, Lorenzo Paso"),
             ),

    tabPanel("NYC Map",
        div(class="outer",
            tags$head(#customized CSS
              includeCSS("styles.css")),
            
        leafletOutput(outputId = "map", width = "100%", height = "100%"),
                            

        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE, 
                      top = 80, left = "auto", right = 20, bottom = "auto",
                      width = 400, height = "auto",
        h2("Airbnb in NYC"),
        sliderInput("range", "Price", min(listings$price), max(listings$price),

                    value = range(listings$price), step = 1

        ),
        checkboxGroupInput("roomType","Select room type",
         c("Entire home/apt","Private room","Shared room","Hotel room"),
         selected =  c("Entire home/apt","Private room","Shared room","Hotel room")),
        plotOutput("roomTypeBarPlot", height = 200),
        checkboxInput("legend", "Show legend", TRUE)
        )
        )),

  tabPanel("Price Analysis",

    fluidPage(
      verticalLayout(
        column(width = 12, offset = 0, style='padding-left:0px; padding-right:0px; padding-top:5px; padding-bottom:5px',
          div(plotOutput("price1"), align = "left"),
          br(),
          div(plotOutput("price2"), align = "left"),
          br(),
          div(plotOutput("price3"), align = "left"))
        )
      )

  ),

tabPanel("Neighbourhood Analysis",
         fluidPage(
           verticalLayout(
             column(width = 12, offset = 0, style='padding-left:0px; padding-right:0px; padding-top:5px; padding-bottom:5px',
                    div(plotOutput("Hood"), align = "left"),
                    numericInput("topN",label= "Select the ranking of top neighbourhoods (Max = 50):", value=20, min=1, max=50),
                    div(plotOutput("Hood2"), align = "left"))
             
           )
         )
)
    
))
