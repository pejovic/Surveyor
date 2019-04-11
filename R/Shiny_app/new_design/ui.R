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
library(shinyBS)


shinyUI(
  tagList(
    tags$script(HTML(
      "document.body.style.backgroundColor = 'sapphire';"
    )),
    tags$script(HTML(
      "document.body.style.fontFamily = 'Verdana';"
    )),
    navbarPage(fluid = TRUE,"Surveyer|R",
      theme = shinytheme("flatly"),
                           tabPanel("MAIN",
                                 fluidRow(
                                   column(width = 6,
                                          div(img(src ="Grb_Gradjevinski.png", height = 370, width = 297), style="text-align: center;")
                                            ),
                                   column(width = 6,
                                          h3("Project: Surveyer"),
                                          p("Description: Package of Land and Engineering Surveying utilities"),
                                          p("Authors: Milutin Pejovic, Petar Bursac, Milan Kilibarda, Branislav Bajat and Aleksandar Sekulic"),
                                          p("University of Belgrade, Faculty of Civil Engineering, Department of geodesy and geoinformatics")
                                   )

                                 )
                           ),
                           tabPanel("DESIGN",
                                    tabsetPanel(type = "pills",
                                                tabPanel("1D DESIGN",
                                                         p(""),
                                                           tabsetPanel(type = "pills",
                                                             tabPanel("XLSX INPUT DATA"
                                                                      ),
                                                             tabPanel("MAP INPUT DATA"
                                                                      )
                                                           )

                                                ),
                                                tabPanel("2D DESIGN",
                                                         p(""),
                                                           tabsetPanel(type = "pills",
                                                             tabPanel("XLSX INPUT DATA",
                                                                      p(""),
                                                                      fluidRow(
                                                                        column(width = 6, "DATA PREPARATION",
                                                                               tabsetPanel(
                                                                                 tabPanel("MAIN",
                                                                                          p(""),
                                                                                          fileInput(inputId = "fileXLSX", label = "Upload points and observations file. Choose Excel - xlsx file:",
                                                                                                    multiple = TRUE, accept = c('.xlsx')),
                                                                                          p(""),
                                                                                          navlistPanel(
                                                                                            tabPanel("OBSERVATION ACCURACY",
                                                                                                     numericInput(inputId = "st_dir_design_xlsx", "Standard deviation direction: ", value = 3),
                                                                                                     numericInput(inputId = "st_dist_design_xlsx", "Standard deviation distance: ", value = 3),
                                                                                                     numericInput(inputId = "st_apriori_design_xlsx", "'a priori' Standard deviation: ", value = 1)
                                                                                                     ),
                                                                                            tabPanel("COORDINATE REFERENCE SYSTEM",
                                                                                                     numericInput(inputId = "epsg_xlsx", "Destination CRS [EPSG code]: ", value = 3857)
                                                                                                     ),
                                                                                            tabPanel("RESULT UNITS AND SCALE",
                                                                                                     textInput(inputId = "adjust_1_units", "Result units: " , value = "mm"),
                                                                                                     numericInput(inputId = "adjust_1_ell_scale", "Ellipse scale: ", value = 10)
                                                                                                     ),
                                                                                            tabPanel("CRITERIA"
                                                                                                     )
                                                                                          ),
                                                                                          p(""),
                                                                                          actionButton(inputId ='go2', label='PREPROCESS DATA', class = "btn-primary btn-block"),
                                                                                          p(""),
                                                                                          actionButton(inputId ='update_design_2d_xlsx', label='Update 2D net design', class = "btn-primary"),
                                                                                          p(""),
                                                                                          actionButton(inputId ='design_adjust_xlsx', label='ADJUST', class = "btn-danger btn-block")
                                                                                          ),
                                                                                 tabPanel("POINTS",
                                                                                          rHandsontableOutput('p_des_xlsx') %>% withSpinner(color="#0dc5c1")
                                                                                          ),
                                                                                 tabPanel("OBSERVATIONS",
                                                                                          rHandsontableOutput('o_des_xlsx') %>% withSpinner(color="#0dc5c1")
                                                                                          ),
                                                                                 tabPanel("MAP",
                                                                                          tabsetPanel(
                                                                                            tabPanel("WEB MAP",
                                                                                                     #leafletOutput("web_map_xlsx", height = 550) %>% withSpinner(color="#0dc5c1"),
                                                                                                     leafletOutput("web_map_xlsx_updated", height = 550) %>% withSpinner(color="#0dc5c1")
                                                                                                     ),
                                                                                            tabPanel("PLOT",
                                                                                                     #plotOutput("netSpatialView_xlsx")%>% withSpinner(color="#0dc5c1"),
                                                                                                     plotOutput("netSpatialView_xlsx_updated")%>% withSpinner(color="#0dc5c1")
                                                                                                     )
                                                                                          )
                                                                                          )
                                                                               )
                                                                               ),
                                                                        column(width = 6, "DESIGN NET RESULTS",
                                                                               tabsetPanel(
                                                                                 tabPanel("MAP RESULTS",
                                                                                          p(""),
                                                                                          leafletOutput("map_ellipses_opt", height = 550) %>% withSpinner(color="#0dc5c1")
                                                                                          ),
                                                                                 tabPanel("TAB RESULTS",
                                                                                          p(""),
                                                                                          navlistPanel(
                                                                                            tabPanel("Error ellipse", DT::dataTableOutput("ellipse_error") %>% withSpinner(color="#0dc5c1")),
                                                                                            tabPanel("Net points", DT::dataTableOutput('net_points_adj') %>% withSpinner(color="#0dc5c1")),
                                                                                            tabPanel("Obseravtions", DT::dataTableOutput('net_observations_adj') %>% withSpinner(color="#0dc5c1")),
                                                                                            tabPanel("Plot error ellipses", plotOutput("netSpatialView_ell") %>% withSpinner(color="#0dc5c1"))
                                                                                          )

                                                                                          ),
                                                                                 tabPanel("EXPORT RESULTS",
                                                                                          actionButton(inputId ='modal_plot', label='PLOT', class = "btn-danger"),
                                                                                          bsModal("modalExample", "Plot error ellipses - 2D net design", "modal_plot", size = "large", plotOutput("netSpatialView_ell11"), downloadButton('downloadPlot', 'Download'))
                                                                                          )
                                                                               )

                                                                               )
                                                                      )
                                                                      ),
                                                             tabPanel("MAP INPUT DATA",
                                                                      p(""),
                                                                      fluidRow(
                                                                        column(width = 6, "DATA PREPARATION",
                                                                               tabsetPanel(
                                                                                 tabPanel("MAIN",
                                                                                          p(""),
                                                                                          navlistPanel(
                                                                                            tabPanel("OBSERVATION ACCURACY",
                                                                                                     numericInput(inputId = "st_dir_me", "Standard deviation direction: ", value = 3),
                                                                                                     numericInput(inputId = "st_dist_me", "Standard deviation distance: ", value = 3),
                                                                                                     numericInput(inputId = "st_apriori_design_map", "'a priori' Standard deviation: ", value = 1)
                                                                                            ),
                                                                                            tabPanel("COORDINATE REFERENCE SYSTEM",
                                                                                                     numericInput(inputId = "epsg_me", "Destination CRS [EPSG code]: ", value = 3857)
                                                                                            ),
                                                                                            tabPanel("RESULT UNITS AND SCALE",
                                                                                                     textInput(inputId = "adjust_1_units_me", "Result units: " , value = "mm"),
                                                                                                     numericInput(inputId = "adjust_1_ell_scale_me", "Ellipse scale: ", value = 10)
                                                                                            ),
                                                                                            tabPanel("CRITERIA"
                                                                                            )
                                                                                          ),
                                                                                          p(""),
                                                                                          actionButton(inputId ='map_edit_result', label='PREPROCESS DATA', class = "btn-primary btn-block"),
                                                                                          p(""),
                                                                                          actionButton(inputId ='update_design_2d_map', label='Update 2D net design', class = "btn-primary"),
                                                                                          p(""),
                                                                                          actionButton(inputId ='design_adjust_map', label='ADJUST', class = "btn-danger btn-block")
                                                                                          ),
                                                                                 tabPanel("EDIT MAP",
                                                                                          h4("Interactive web carthography maps."),
                                                                                          editModUI("map_me", height=550)
                                                                                 ),
                                                                                 tabPanel("POINTS",
                                                                                          rHandsontableOutput('p_des_map') %>% withSpinner(color="#0dc5c1")
                                                                                 ),
                                                                                 tabPanel("OBSERVATIONS",
                                                                                          rHandsontableOutput('o_des_map') %>% withSpinner(color="#0dc5c1")
                                                                                 ),
                                                                                 tabPanel("MAP",
                                                                                          tabsetPanel(
                                                                                            tabPanel("WEB MAP",
                                                                                                     leafletOutput("map_me_out", height = 550) %>% withSpinner(color="#0dc5c1")
                                                                                                     ),
                                                                                            tabPanel("PLOT",
                                                                                                     plotOutput("netSpatialView_me") %>% withSpinner(color="#0dc5c1")
                                                                                                     )
                                                                                          )
                                                                                          )
                                                                              )
                                                                        ),
                                                                        column(width = 6, "DESIGN NET RESULTS",
                                                                               tabsetPanel(
                                                                                 tabPanel("MAP RESULTS",
                                                                                          p(""),
                                                                                          leafletOutput("map_ellipses_opt_me", height = 550) %>% withSpinner(color="#0dc5c1")
                                                                                 ),
                                                                                 tabPanel("TAB RESULTS",
                                                                                          p(""),
                                                                                          navlistPanel(
                                                                                            tabPanel("Error ellipse", DT::dataTableOutput("ellipse_error_me") %>% withSpinner(color="#0dc5c1")),
                                                                                            tabPanel("Net points", DT::dataTableOutput('net_points_adj_me') %>% withSpinner(color="#0dc5c1")),
                                                                                            tabPanel("Obseravtions", DT::dataTableOutput('net_observations_adj_me') %>% withSpinner(color="#0dc5c1")),
                                                                                            tabPanel("Plot error ellipses", plotOutput("netSpatialView_ell_me") %>% withSpinner(color="#0dc5c1"))
                                                                                          )
                                                                                 ),
                                                                                 tabPanel("EXPORT RESULTS",
                                                                                          actionButton(inputId ='modal_plot1', label='PLOT', class = "btn-danger"),
                                                                                          bsModal("modalExample1", "Plot error ellipses - 2D net design", "modal_plot1", size = "large", plotOutput("netSpatialView_ell_me11"), downloadButton('downloadPlot1', 'Download'))
                                                                                 )
                                                                               )

                                                                        )
                                                                      )
                                                                      )


                                                           )

                                                )
                                    )



                           ),
                           tabPanel("ADJUSTMENT"


                           ),
                           tabPanel("DEFORMATION ANALYSIS"


                           ),
                           tabPanel("COORDINATE TRANSFORMATION"


                           )



      )
    )
)

