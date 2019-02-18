library(shiny)
library(shinythemes)
library(leaflet)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(geomnet)
library(ggnetwork)
library(sf)
library(ggmap)
library(sp)
library(rgdal)
library(leaflet)
library(xlsx)
library(data.table)


shinyUI(fluidPage(
  
  tags$script(HTML(
    "document.body.style.backgroundColor = 'sapphire';"
  )),
  tags$script(HTML(
    "document.body.style.fontFamily = 'Verdana';"
  )),
  
  
  
  theme = shinytheme("superhero"),
  themeSelector(),
  
  
  
  titlePanel("Input functions - Surveyer"),
  tags$b("Surveyer|R"),
  
  sidebarLayout(
    sidebarPanel(
      
      
      fileInput(inputId = "fileSHP_points", label = "Upload points vector file. Choose shapefile:",
                multiple = TRUE, accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
      
      fileInput(inputId = "fileSHP_observations", label = "Upload observations vector file. Choose shapefile:",
                multiple = TRUE, accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
      
      # fileInput(inputId = "fileKML_points", label = "Upload points vector KML file. Choose .kml:",
      #           multiple = TRUE, accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
      # 
      # fileInput(inputId = "fileKML_observations", label = "Upload observations vector KML file. Choose .kml:",
      #           multiple = TRUE, accept = c('.kml')),
      # 
      # fileInput(inputId = "fileXLSX_points", label = "Upload points xlsx file. Choose .xlsx:",
      #           multiple = TRUE, accept = c('.xlsx')),
      # 
      # fileInput(inputId = "fileXLSX_observations", label = "Upload observations xlsx file. Choose .xlsx:",
      #           multiple = TRUE, accept = c('.xlsx')),
      
      

      
      p("Made by:", a("R Shiny", href = "http://shiny.rstudio.com"),"."),
      img(src = "https://www.bu.edu/library/files/2016/03/RShiny-logo.png", width = "70px", height = "70px")
      
      
      
      
      
    ),
    
    
    mainPanel(
      
      leafletOutput("mymap", height = 550),
      p("Points"),
      verbatimTextOutput("points_shp"),
      p("Observations"),
      verbatimTextOutput("observations_shp"),
      p("Spatial view - Geodetic network and observations"),
      plotlyOutput("net_spatial_view_shp")
      
      
      
    ))
))