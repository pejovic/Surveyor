source(here("R/input_functions.r"))
source(here("R/inputFunction_withObservations.R"))
source(here("R/functions.r"))

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
library(readxl)
library(data.table)
library(plotly)
library(mapview)
library(mapedit)
library(shinycssloaders)
library(here)
library(matlib)
library(nngeo)
library(shinyWidgets)
library(dplyr)
library(DT)
library(leaflet.extras)
library(rhandsontable)

shinyUI(
  tagList(
    tags$script(HTML(
      "document.body.style.backgroundColor = 'sapphire';"
    )),
    tags$script(HTML(
      "document.body.style.fontFamily = 'Verdana';"
    )),
    navbarPage(
      "Surveyer|R",
      theme = shinytheme("flatly"),
      tabPanel("InputData_surveynet",
               mainPanel(
                 tabsetPanel(
                   tabPanel("InputData_xlsx",
                            #sidebarPanel(
                            #  fileInput(inputId = "fileXLSX", label = "Upload points and observations file. Choose Excel - xlsx file:",
                            #            multiple = TRUE, accept = c('.xlsx')),
                            #  numericInput(inputId = "epsg_xlsx", "Destination CRS [EPSG code]: ", value = 3857)
                            #),
                            mainPanel(
                              #leafletOutput("mymap2", height = 550) %>% withSpinner(color="#0dc5c1"),
                              #actionButton(inputId ='go2', label='Calculate', class = "btn-primary btn-block"),
                              navlistPanel(
                                #tabPanel("Points", verbatimTextOutput(outputId = "points_xlsx_3")%>% withSpinner(color="#0dc5c1")),
                                #tabPanel("Observations", verbatimTextOutput(outputId = "observations_xlsx_3")%>% withSpinner(color="#0dc5c1"))
                                #tabPanel("netSpatialView", plotOutput("netSpatialView_xlsx")%>% withSpinner(color="#0dc5c1"))
                              )
                            )
                   ),
                   tabPanel("InputData_kml",
                            sidebarPanel(
                              fileInput(inputId = "fileKML_points", label = "Upload points vector file. Choose kml file:",
                                        multiple = TRUE, accept = c('.kml')),
                              fileInput(inputId = "fileKML_observations", label = "Upload observations vector file. Choose kml file:",
                                        multiple = TRUE, accept = c('.kml')),
                              numericInput(inputId = "st_dir_kml", "StDev direction: ", value = 3),
                              numericInput(inputId = "st_dist_kml", "StDev distance: ", value = 3),
                              numericInput(inputId = "epsg_kml", "Destination CRS [EPSG code]: ", value = 3857),
                              textInput(inputId = "fix_x_kml", "Datum [fix X for Points] (e.g. A,B)"),
                              textInput(inputId = "fix_y_kml", "Datum [fix Y for Points] (e.g. A,B)"),
                              textInput(inputId = "points_obj_kml", "Geodetic network points at object (e.g. A,B)")
                            ),
                            mainPanel(
                              leafletOutput("mymap1", height = 550) %>% withSpinner(color="#0dc5c1"),
                              actionButton(inputId ='go1', label='Calculate', class = "btn-primary btn-block"),
                              navlistPanel(
                                tabPanel("Points", verbatimTextOutput(outputId = "points_kml_3") %>% withSpinner(color="#0dc5c1")),
                                tabPanel("Observations", verbatimTextOutput(outputId = "observations_kml_3") %>% withSpinner(color="#0dc5c1")),
                                tabPanel("netSpatialView", plotOutput("netSpatialView_kml") %>% withSpinner(color="#0dc5c1"))
                              )
                            )
                   ),
                   tabPanel("InputData_shp",
                            sidebarPanel(
                              fileInput(inputId = "fileSHP_points", label = "Upload points vector file. Choose shapefile:",
                                        multiple = TRUE, accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
                              fileInput(inputId = "fileSHP_observations", label = "Upload observations vector file. Choose shapefile:",
                                        multiple = TRUE, accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
                              numericInput(inputId = "st_dir_shp", "StDev direction: ", value = 3),
                              numericInput(inputId = "st_dist_shp", "StDev distance: ", value = 3),
                              numericInput(inputId = "epsg_shp", "Destination CRS [EPSG code]: ", value = 3857),
                              textInput(inputId = "fix_x_shp", "Datum [fix X for Points] (e.g. A,B)"),
                              textInput(inputId = "fix_y_shp", "Datum [fix Y for Points] (e.g. A,B)"),
                              textInput(inputId = "points_obj_shp", "Geodetic network points at object (e.g. A,B)")
                            ),
                            mainPanel(
                              leafletOutput("mymap", height = 550) %>% withSpinner(color="#0dc5c1"),
                              actionButton(inputId ='go', label='Calculate', class = "btn-primary btn-block"),
                              navlistPanel(
                                tabPanel("Points", verbatimTextOutput(outputId = "points_shp_3") %>% withSpinner(color="#0dc5c1")),
                                tabPanel("Observations", verbatimTextOutput(outputId = "observations_shp_3") %>% withSpinner(color="#0dc5c1")),
                                tabPanel("netSpatialView",plotOutput("netSpatialView_shp") %>% withSpinner(color="#0dc5c1"))
                              )
                            )
                   ),
                   tabPanel("InputData_mapEdit",
                            sidebarPanel(
                              numericInput(inputId = "st_dir_me", "StDev direction: ", value = 3),
                              numericInput(inputId = "st_dist_me", "StDev distance: ", value = 3),
                              numericInput(inputId = "epsg_me", "Destination CRS [EPSG code]: ", value = 3857),
                              textInput(inputId = "fix_x_me", "Datum [fix X for Points] (e.g. A,B)"),
                              textInput(inputId = "fix_y_me", "Datum [fix Y for Points] (e.g. A,B)"),
                              textInput(inputId = "points_obj_me", "Geodetic network points at object (e.g. A,B)")
                            ),
                            mainPanel(
                              editModUI("map_me", height=550),
                              #actionButton(inputId ='go_me_draw', label='Get data [Points]', class = "btn-primary btn-block"),
                              #verbatimTextOutput(outputId = "primer") %>% withSpinner(color="#0dc5c1"),
                              actionButton(inputId ='go_me_edit_o', label='Edit data [Observations]', class = "btn-primary btn-block"),
                              DT::dataTableOutput("primer1") %>% withSpinner(color="#0dc5c1"),
                              actionButton(inputId ='delete_b', label='Delete rows', class = "btn-danger btn-block"),
                              navlistPanel(
                                tabPanel("Observational plan", rHandsontableOutput('OldObs')),
                                tabPanel("Button", actionButton(inputId ='run_table', label='Update', class = "btn-danger btn-block")),
                                tabPanel("Observational plan [Updated]", DT::dataTableOutput("primer4")%>% withSpinner(color="#0dc5c1"))
                              ),
                              actionButton(inputId ='go_me', label='Calculate', class = "btn-primary btn-block"),
                              leafletOutput("map_me_out", height = 550) %>% withSpinner(color="#0dc5c1"),
                              navlistPanel(
                                #tabPanel("Points", verbatimTextOutput(outputId = "points_me_3") %>% withSpinner(color="#0dc5c1")),
                                #tabPanel("Observations", verbatimTextOutput(outputId = "observations_me_3") %>% withSpinner(color="#0dc5c1")),
                                tabPanel("netSpatialView", plotOutput("netSpatialView_me") %>% withSpinner(color="#0dc5c1"))
                              )
                            )
                   ),
                   tabPanel("InputData_withObservations",
                            sidebarPanel(
                              fileInput(inputId = "fileXLSX_wO", label = "Upload points and observations file. Choose Excel - xlsx file:",
                                        multiple = TRUE, accept = c('.xlsx')),
                              numericInput(inputId = "epsg_xlsx_wO", "Destination CRS [EPSG code]: ", value = 3857)
                            ),
                            mainPanel(
                              actionButton(inputId ='calc_obs', label='Calculate', class = "btn-primary btn-block"),
                              navlistPanel(
                                tabPanel("Points",
                                         DT::dataTableOutput("points_wO") %>% withSpinner(color="#0dc5c1")),
                                tabPanel("Observations",
                                         p("Edit observations"),
                                         rHandsontableOutput('OldObs_wO'),
                                         actionButton(inputId ='edit_wO', label='Edit observations', class = "btn-danger btn-block"),
                                         DT::dataTableOutput("observations_wO") %>% withSpinner(color="#0dc5c1")),
                                tabPanel("netSpatialView", plotOutput("netSpatialView_xlsx_wO")%>% withSpinner(color="#0dc5c1"))
                              )
                            )
                   )
                 )
                 , width = 12 )
      ),
      tabPanel("DESIGN",
               mainPanel(
                 tabsetPanel(type="pills",
                   tabPanel("1D NET DESIGN",
                            mainPanel(
                              tabsetPanel(
                                tabPanel("XLSX - INPUT DATA"
                                ),
                                tabPanel("MAP - INPUT DATA"

                                )
                              )
                            )
                          ),
                   tabPanel("2D NET DESIGN",
                            mainPanel(
                              tabsetPanel(type="pills",
                                tabPanel("XLSX - INPUT DATA",
                                         sidebarPanel(
                                           fileInput(inputId = "fileXLSX", label = "Upload points and observations file. Choose Excel - xlsx file:",
                                                     multiple = TRUE, accept = c('.xlsx')),
                                           numericInput(inputId = "epsg_xlsx", "Destination CRS [EPSG code]: ", value = 3857),
                                           actionButton(inputId ='go2', label='Calculate', class = "btn-primary btn-block")
                                         ),

                                         mainPanel(

                                         mainPanel(width = 6,
                                           tabsetPanel(type="pills",
                                             tabPanel("POINTS",
                                                      rHandsontableOutput('p_des_xlsx') %>% withSpinner(color="#0dc5c1")
                                             ),
                                             tabPanel("OBSERVATIONS",
                                                      rHandsontableOutput('o_des_xlsx') %>% withSpinner(color="#0dc5c1")
                                             ),
                                             tabPanel("MAP",
                                                      leafletOutput("mymap2", height = 550) %>% withSpinner(color="#0dc5c1"),
                                                      plotOutput("netSpatialView_xlsx")%>% withSpinner(color="#0dc5c1")
                                             )
                                           ),
                                           actionButton(inputId ='update_design_2d_xlsx', label='Update', class = "btn-primary"),
                                           actionButton(inputId ='design_adjust', label='Calculate', class = "btn-danger btn-block")
                                         ),
                                         mainPanel(width = 6,
                                           sidebarPanel(
                                                        tabsetPanel(type="pills",

                                                          tabPanel("MAP VISUALIZATION"

                                                          ),
                                                          tabPanel("TAB VISUALIZATION"

                                                          )
                                                        )
                                         )
                                ))),
                                tabPanel("MAP - INPUT DATA",
                                         sidebarPanel(
                                           tabsetPanel(
                                             tabPanel("POINTS"
                                             ),
                                             tabPanel("OBSERVATIONS"

                                             )
                                           ),
                                           actionButton(inputId ='design_adjust', label='Calculate', class = "btn-danger btn-block")
                                         ),
                                         mainPanel(

                                         )

                                )
                              )
                            )
                          )
                 )
                )




























      ),
      tabPanel("ADJUSTMENT", "Blank"
      ),
      tabPanel("2D Optimization",
               mainPanel(
                 tabsetPanel(
                   tabPanel("InputData",
                            sidebarPanel(
                              prettyRadioButtons(inputId = "rb",  label = "Input data [points and observational plan]: ",
                                                 choices = c("Input_xlsx" = "i_xlsx", "Input_shp"= "i_shp", "Input_kml"= "i_kml", "Input_mapEdit"= "i_mapEdit"),
                                                 shape = "round", status = "danger",
                                                 fill = TRUE),
                              actionButton(inputId ="data_list_get", label='Get data', class = "btn-primary btn-block")
                            ),
                            mainPanel(
                              verbatimTextOutput(outputId = "data_list_in") %>% withSpinner(color="#0dc5c1")
                            )
                   ),
                   tabPanel("Results",
                            sidebarPanel(
                              actionButton(inputId ="adjust_1", label='Adjust geodetic network', class = "btn-primary btn-block"),
                              textInput(inputId = "adjust_1_units", "Result units: " , value = "mm"),
                              numericInput(inputId = "adjust_1_ell_scale", "Ellipse scale: ", value = 10)
                              ),
                            mainPanel(
                              navlistPanel(
                                tabPanel("Error ellipse", DT::dataTableOutput("ellipse_error") %>% withSpinner(color="#0dc5c1")),
                                tabPanel("Net points", DT::dataTableOutput('net_points_adj') %>% withSpinner(color="#0dc5c1")),
                                tabPanel("Obseravtions", DT::dataTableOutput('net_observations_adj') %>% withSpinner(color="#0dc5c1")),
                                tabPanel("Plot error ellipses", plotOutput("netSpatialView_ell") %>% withSpinner(color="#0dc5c1"))
                              )
                            )
                   ),
                   tabPanel("Visualization",
                            sidebarPanel(
                            ),
                            mainPanel(
                              leafletOutput("map_ellipses_opt", height = 550) %>% withSpinner(color="#0dc5c1")
                            )
                   ),
                   tabPanel("Compare 2D net adjustments",
                            sidebarPanel(
                            ),
                            mainPanel(
                            )
                   )
                 )
                 , width = 12)

      ),
      tabPanel("DEFORMATION ANALYSIS", "Blank"
      ),
      tabPanel("COORDINATE TRANSFORMATION", "Blank"
      )
    )
  )
)
















