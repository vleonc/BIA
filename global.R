library(shinythemes)
library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)

load("listings.Rdata")
listings <- listings[listings$price>0,]
