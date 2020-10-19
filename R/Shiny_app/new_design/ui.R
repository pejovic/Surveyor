options(encoding="UTF-8")
source(here::here("R/deprecated/input_functions.r"))
source(here::here("R/deprecated/inputFunction_withObservations.R"))
source(here::here("R/functions.r"))


library(shiny)
library(shinythemes)
library(leaflet)
library(tidyverse)
library(magrittr)
library(ggplot2)
#library(geomnet)
#library(ggnetwork)
library(sf)
library(ggmap)
library(sp)
library(rgdal)
library(leaflet)
#library(xlsx)
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
library(kableExtra)


shinyUI(
  tagList(
    tags$script(HTML(
      "document.body.style.backgroundColor = 'sapphire';"
    )),
    tags$script(HTML(
      "document.body.style.fontFamily = 'Verdana';"
    )),
    navbarPage(fluid = TRUE,"Surveyor|R",
      theme = shinytheme("flatly"),
                           tabPanel("MAIN",
                                 fluidRow(
                                   column(width = 6, div(img(src ="Grb_Gradjevinski.png", height = 370, width = 297), style="text-align: center;")),
                                   column(width = 6,
                                          h3("Project: Surveyor"),
                                          p("Description: Package of Land and Engineering Surveying utilities"),
                                          p("Authors: Milutin Pejovic, Petar Bursac, Milan Kilibarda, Aleksandar Sekulic and Branislav Bajat"),
                                          p("University of Belgrade, Faculty of Civil Engineering, Department of geodesy and geoinformatics")
                                   )

                                 )
                           ),
                           tabPanel("DESIGN",
                                    tabsetPanel(type = "pills",
                                                tabPanel("1D DESIGN",
                                                         p(""),
                                                           tabsetPanel(type = "pills",
                                                             tabPanel("XLSX INPUT DATA",
                                                                      p(""),
                                                                      fluidRow(
                                                                        column(width = 6, "DATA PREPARATION",
                                                                               tabsetPanel(
                                                                                 tabPanel("MAIN",
                                                                                          p(""),
                                                                                          fileInput(inputId = "fileXLSX_1d", label = "Upload points and observations file. Choose Excel - xlsx file:",
                                                                                                    multiple = TRUE, accept = c('.xlsx')),
                                                                                          p(""),
                                                                                          navlistPanel(
                                                                                            tabPanel("STOCHACTIC MODEL",
                                                                                                     fluidRow(
                                                                                                       p("Please select one to define the weights", style="text-align: left; font-weight: bold; font-colour: red")
                                                                                                     ),
                                                                                                     fluidRow(
                                                                                                       p(""),
                                                                                                       checkboxGroupInput(inputId = "dh.s.model", label = "Stochastic model:",choices =  c("Standard deviation for height difference [mm]" = "sd_dh",
                                                                                                                                                               "Leveling distances [km]" = "d_dh",
                                                                                                                                                               "Number of stations" = "n_dh",
                                                                                                                                                               "Unit weights - identity matrix" = "E"))
                                                                                                     ),
                                                                                                     fluidRow(
                                                                                                       p("The selected model must be identical to the input data.", style="text-align: left; font-weight: bold; color: red")
                                                                                                     )
                                                                                            ),
                                                                                            tabPanel("OBSERVATION ACCURACY",
                                                                                                     numericInput(inputId = "sd_dir_dh", "Standard deviation for height difference [mm]: ", value = 1),
                                                                                                     numericInput(inputId = "sd_dist_rdh", "Standard deviation for reference height difference [mm]: ", value = 1),
                                                                                                     numericInput(inputId = "sd_apriori_dh", "'a priori' Standard deviation for height differences: ", value = 0.2)
                                                                                            ),
                                                                                            tabPanel("COORDINATE REFERENCE SYSTEM",
                                                                                                     numericInput(inputId = "epsg_1d", "Destination CRS [EPSG code]: ", value = NA)
                                                                                            ),
                                                                                            tabPanel("RESULT UNITS AND SCALE",
                                                                                                     textInput(inputId = "units_1d", "Result units: " , value = "mm"),
                                                                                                     numericInput(inputId = "scale_1d", "Result scale: ", value = 10)
                                                                                            ),
                                                                                            tabPanel("CRITERIA",
                                                                                                     # p("STOCHACTIC MODEL", style="text-align: center; font-weight: bold;"),
                                                                                                     # fluidRow(
                                                                                                     #   checkboxGroupInput("dh.s.model", "Stochastic model:", c("Standard deviation for height difference [mm]" = "sd_dh",
                                                                                                     #                                                           "Leveling distances [km]" = "d_dh",
                                                                                                     #                                                           "Number of stations" = "n_dh",
                                                                                                     #                                                           "Unit weights - identity matrix" = "E"))
                                                                                                     # ),
                                                                                                     p(""),
                                                                                                     p("MEASURMENT ACCURACY", style="text-align: center; font-weight: bold;"),
                                                                                                     fluidRow(
                                                                                                       # column(width = 6, numericInput("sd_H", "sd_H: ", value = 0.50)),
                                                                                                       column(width = 6, numericInput("sd_h", "sd_h: ", value = 0.3))
                                                                                                     ),
                                                                                                     p(""),
                                                                                                     p("MEASURMENT RELIABILITY", style="text-align: center; font-weight: bold;"),
                                                                                                     fluidRow(
                                                                                                       column(width = 6, numericInput("rii_1d", "rii: ", value = 0.3))
                                                                                                     )
                                                                                            )
                                                                                          ),
                                                                                          p(""),
                                                                                          actionButton(inputId ='design_1d_result', label='PREPROCESS DATA', class = "btn-primary btn-block"),
                                                                                          p(""),
                                                                                          actionButton(inputId ='update_design_1d', label='Update 1D net design', class = "btn-primary"),
                                                                                          p(""),
                                                                                          actionButton(inputId ='design_adjust_1d', label='ADJUST', class = "btn-danger btn-block")
                                                                                 ),
                                                                                 tabPanel("POINTS",
                                                                                          rHandsontableOutput('p_1d') %>% withSpinner(color="#0dc5c1")
                                                                                 ),
                                                                                 tabPanel("OBSERVATIONS",
                                                                                          rHandsontableOutput('o_1d') %>% withSpinner(color="#0dc5c1")
                                                                                 ),
                                                                                 tabPanel("MAP",
                                                                                          tabsetPanel(
                                                                                            tabPanel("PLOT",
                                                                                                     plotlyOutput("netSpatialView_1d") %>% withSpinner(color="#0dc5c1")
                                                                                            )
                                                                                          )
                                                                                 )
                                                                               )
                                                                        ),
                                                                        column(width = 6, "DESIGN 1D NET RESULTS",
                                                                               tabsetPanel(
                                                                                 tabPanel("SUMMARY",
                                                                                          p(""),
                                                                                          navlistPanel(
                                                                                            tabPanel("Network design", tableOutput("design1d.summ.des")%>% withSpinner(color="#0dc5c1")),
                                                                                            tabPanel("Stations", tableOutput("design1d.summ.stations")%>% withSpinner(color="#0dc5c1")),
                                                                                            tabPanel("Observations", tableOutput("design1d.summ.observations")%>% withSpinner(color="#0dc5c1")),
                                                                                            tabPanel("Unknowns", tableOutput("design1d.summ.unknowns")%>% withSpinner(color="#0dc5c1")),
                                                                                            tabPanel("Degrees of freedom", tableOutput("design1d.summ.degrees")%>% withSpinner(color="#0dc5c1"))
                                                                                          )
                                                                                 ),
                                                                                 tabPanel("MAP RESULTS",
                                                                                          p(""),
                                                                                          plotlyOutput("netSpatialView_1d_design") %>% withSpinner(color="#0dc5c1")
                                                                                 ),
                                                                                 tabPanel("TAB RESULTS",
                                                                                          p(""),
                                                                                          navlistPanel(
                                                                                            tabPanel("Net points", DT::dataTableOutput('1d_points_des') %>% withSpinner(color="#0dc5c1")),
                                                                                            tabPanel("Obseravtions", DT::dataTableOutput('1d_observations_des') %>% withSpinner(color="#0dc5c1")),
                                                                                            tabPanel("Plots", plotlyOutput("netSpatialView_1d_des") %>% withSpinner(color="#0dc5c1"))
                                                                                          )
                                                                                 ),
                                                                                 tabPanel("EXPORT RESULTS",
                                                                                          actionButton(inputId ='modal_plot_1d', label='PLOT', class = "btn-danger"),
                                                                                          bsModal("modalExample1d", "Plot - 1D net design", "modal_plot_1d", size = "large", plotOutput("netSpatialView_1d_modal"), downloadButton('downloadPlot1d', 'Download')),
                                                                                          downloadButton("report1Ddesign", "Generate report")
                                                                                 )
                                                                               )

                                                                        )

                                                                      )


                                                                    )#,
                                                             #tabPanel("MAP INPUT DATA",
                                                                      #p("")#,
                                                                      # fluidRow(
                                                                      #  column(width = 6, "DATA PREPARATION",
                                                                      #         tabsetPanel(
                                                                      #           tabPanel("MAIN",
                                                                      #                    p(""),
                                                                      #                    navlistPanel(
                                                                      #                      tabPanel("OBSERVATION ACCURACY",
                                                                      #                               numericInput(inputId = "sd_dir_dh .me", "Standard deviation for height difference [mm]: ", value = 1),
                                                                      #                               numericInput(inputId = "sd_dist_rdh.me", "Standard deviation for reference height difference [mm]: ", value = 1),
                                                                      #                               numericInput(inputId = "sd_apriori_dh.me", "'a priori' Standard deviation for height differences: ", value = 0.2)
                                                                      #                      ),
                                                                      #                      tabPanel("COORDINATE REFERENCE SYSTEM",
                                                                      #                               numericInput(inputId = "epsg_me_1d", "Destination CRS [EPSG code]: ", value = 3857)
                                                                      #                      ),
                                                                      #                      tabPanel("RESULT UNITS AND SCALE",
                                                                      #                               textInput(inputId = "units_me_1d", "Result units: " , value = "mm"),
                                                                      #                               numericInput(inputId = "scale_me_1d", "Result scale: ", value = 10)
                                                                      #                      ),
                                                                      #                      tabPanel("CRITERIA",
                                                                      #                               p("STOCHACTIC MODEL", style="text-align: center; font-weight: bold;"),
                                                                      #                               fluidRow(
                                                                      #                                 checkboxGroupInput("dh.s.model.me", "Stochastic model:", c("Standard deviation for height difference [mm]" = "sd_dh",
                                                                      #                                                                                         "Leveling distances [km]" = "d_dh",
                                                                      #                                                                                         "Number of stations" = "n_dh",
                                                                      #                                                                                         "Unit weights - identity matrix" = "E"))
                                                                      #                               ),
                                                                      #                               #p("POINT ACCURACY", style="text-align: center; font-weight: bold;"),
                                                                      #                               #fluidRow(
                                                                      #                               #  column(width = 4, numericInput("sx_map", "Sx: ", value = 1.5)),
                                                                      #                               #  column(width = 4, numericInput("sy_map", "Sy: ", value = 1.5)),
                                                                      #                               #  column(width = 4, numericInput("sp_map", "Sp: ", value = 2))
                                                                      #                               #),
                                                                      #                               #fluidRow(
                                                                      #                               #  column(width = 12, numericInput("ab_map", "A/B: ", value = 0))
                                                                      #                               #),
                                                                      #                               #fluidRow(
                                                                      #                               #  column(width = 4, numericInput("dp_map", "dP: ", value = 0)),
                                                                      #                               #  column(width = 4, numericInput("dpteta_map", "dPT: ", value = 0)),
                                                                      #                               #  column(width = 4, numericInput("teta_map", "T: ", value = 0))
                                                                      #                               #),
                                                                      #                               #p(""),
                                                                      #                               p("MEASURMENT ACCURACY", style="text-align: center; font-weight: bold;"),
                                                                      #                               fluidRow(
                                                                      #                                 column(width = 6, numericInput("sdh_map", "Sdh: ", value = 0)),
                                                                      #                                 column(width = 6, numericInput("sH_map", "SH: ", value = 0))
                                                                      #                               )#,
                                                                      #                               #p(""),
#
                                                                      #                               #p("MEASURMENT RELIABILITY", style="text-align: center; font-weight: bold;"),
                                                                      #                               #fluidRow(
                                                                      #                               #  column(width = 6, numericInput("rii_map", "rii: ", value = 0.3)),
                                                                      #                               #  column(width = 6, numericInput("gii_map", "Gii: ", value = 0))
                                                                      #                               #)
                                                                      #                      )
                                                                      #                    ),
                                                                      #                    p(""),
                                                                      #                    actionButton(inputId ='map_edit_1d_result', label='PREPROCESS DATA', class = "btn-primary btn-block"),
                                                                      #                    p(""),
                                                                      #                    actionButton(inputId ='update_design_1d_map', label='Update 1D net design', class = "btn-primary"),
                                                                      #                    p(""),
                                                                      #                    actionButton(inputId ='design_adjust_1d_map', label='ADJUST', class = "btn-danger btn-block")
                                                                      #           ),
                                                                      #           tabPanel("EDIT MAP",
                                                                      #                    h4("Interactive web carthography maps."),
                                                                      #                    editModUI("map_me_1d", height=550)
                                                                      #           ),
                                                                      #           tabPanel("POINTS"#,
                                                                      #                    #rHandsontableOutput('p_des_map_1d') %>% withSpinner(color="#0dc5c1")
                                                                      #           ),
                                                                      #           tabPanel("OBSERVATIONS"#,
                                                                      #                    #rHandsontableOutput('o_des_map_1d') %>% withSpinner(color="#0dc5c1")
                                                                      #           ),
                                                                      #           tabPanel("MAP",
                                                                      #                    tabsetPanel(
                                                                      #                      tabPanel("WEB MAP"#,
                                                                      #                               #leafletOutput("map_me_out_1d", height = 550) %>% withSpinner(color="#0dc5c1")
                                                                      #                      ),
                                                                      #                      tabPanel("PLOT"#,
                                                                      #                               #plotOutput("netSpatialView_me_1d") %>% withSpinner(color="#0dc5c1")
                                                                      #                      )
                                                                      #                    )
                                                                      #           )
                                                                      #         )
                                                                      #  ),
                                                                      #  column(width = 6, "DESIGN 1D NET RESULTS",
                                                                      #         tabsetPanel(
                                                                      #           tabPanel("MAP RESULTS",
                                                                      #                    p("")#,
                                                                      #                    #leafletOutput("map_opt_me_1d", height = 550) %>% withSpinner(color="#0dc5c1")
                                                                      #           ),
                                                                      #           tabPanel("TAB RESULTS",
                                                                      #                    p(""),
                                                                      #                    navlistPanel(
                                                                      #                      #tabPanel("Error ellipse", DT::dataTableOutput("ellipse_error_me") %>% withSpinner(color="#0dc5c1")),
                                                                      #                      #tabPanel("Net points", DT::dataTableOutput('net_points_adj_me') %>% withSpinner(color="#0dc5c1")),
                                                                      #                      #tabPanel("Obseravtions", DT::dataTableOutput('net_observations_adj_me') %>% withSpinner(color="#0dc5c1")),
                                                                      #                      #tabPanel("Plot error ellipses", plotOutput("netSpatialView_ell_me") %>% withSpinner(color="#0dc5c1"))
                                                                      #                    )
                                                                      #           ),
                                                                      #           tabPanel("EXPORT RESULTS",
                                                                      #                    actionButton(inputId ='modal_plot_1d', label='PLOT', class = "btn-danger"),
                                                                      #                    bsModal("modalExample1d", "Plot - 1D net design", "modal_plot_1d", size = "large", plotOutput("netSpatialView_me_1d"), downloadButton('downloadPlot1d', 'Download'))
                                                                      #           )
                                                                      #         )
#
                                                                      #  )
#
                                                                      #)
                                                                    #)
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
                                                                                                     numericInput(inputId = "st_dir_design_xlsx", "Standard deviation for angle measurments ['']: ", value = 3),
                                                                                                     numericInput(inputId = "st_dist_design_xlsx", "Standard deviation for distance measurments [mm]: ", value = 3),
                                                                                                     numericInput(inputId = "st_apriori_design_xlsx", "'a priori' Standard deviation: ", value = 1)
                                                                                                     ),
                                                                                            tabPanel("COORDINATE REFERENCE SYSTEM",
                                                                                                     numericInput(inputId = "epsg_xlsx", "Destination CRS [EPSG code]: ", value = 3857)
                                                                                                     ),
                                                                                            tabPanel("RESULT UNITS AND SCALE",
                                                                                                     textInput(inputId = "adjust_1_units", "Result units: " , value = "mm"),
                                                                                                     numericInput(inputId = "adjust_1_ell_scale", "Ellipse scale: ", value = 10)
                                                                                                     ),
                                                                                            tabPanel("CRITERIA",
                                                                                                     p("POINT ACCURACY", style="text-align: center; font-weight: bold;"),
                                                                                                     fluidRow(
                                                                                                       column(width = 4, numericInput("sx_xlsx", "Sx: ", value = 1.5)),
                                                                                                       column(width = 4, numericInput("sy_xlsx", "Sy: ", value = 1.5)),
                                                                                                       column(width = 4, numericInput("sp_xlsx", "Sp: ", value = 2))
                                                                                                     ),
                                                                                                     fluidRow(
                                                                                                       column(width = 12, numericInput("ab_xlsx", "A/B: ", value = 0))
                                                                                                     ),
                                                                                                     fluidRow(
                                                                                                       column(width = 4, numericInput("dp_xlsx", "dP: ", value = 0)),
                                                                                                       column(width = 4, numericInput("dpteta_xlsx", "dPT: ", value = 0)),
                                                                                                       column(width = 4, numericInput("teta_xlsx", "T: ", value = 0))
                                                                                                     ),
                                                                                                     p(""),
                                                                                                     p("MEASURMENT ACCURACY", style="text-align: center; font-weight: bold;"),
                                                                                                     fluidRow(
                                                                                                       column(width = 6, numericInput("sdir_xlsx", "Sdir: ", value = 0)),
                                                                                                       column(width = 6, numericInput("sdist_xlsx", "Sdist: ", value = 0))
                                                                                                     ),
                                                                                                     p(""),
                                                                                                     p("MEASURMENT RELIABILITY", style="text-align: center; font-weight: bold;"),
                                                                                                     fluidRow(
                                                                                                       column(width = 6, numericInput("rii_xlsx", "rii: ", value = 0.3)),
                                                                                                       column(width = 6, numericInput("gii_xlsx", "Gii: ", value = 0))
                                                                                                     )
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
                                                                                                     leafletOutput("web_map_xlsx_updated", height = 550) %>% withSpinner(color="#0dc5c1")
                                                                                                     ),
                                                                                            tabPanel("PLOT",
                                                                                                     plotOutput("netSpatialView_xlsx_updated")%>% withSpinner(color="#0dc5c1")
                                                                                                     )
                                                                                          )
                                                                                          )
                                                                               )
                                                                               ),
                                                                        column(width = 6, "DESIGN NET RESULTS",
                                                                               tabsetPanel(
                                                                                 tabPanel("SUMMARY",
                                                                                          p(""),
                                                                                          navlistPanel(
                                                                                            tabPanel("Network design", tableOutput("deisgn2d.summ.des")%>% withSpinner(color="#0dc5c1")),
                                                                                            tabPanel("Stations", tableOutput("design2d.summ.stations")%>% withSpinner(color="#0dc5c1")),
                                                                                            tabPanel("Observations", tableOutput("design2d.summ.observations")%>% withSpinner(color="#0dc5c1")),
                                                                                            tabPanel("Unknowns", tableOutput("design2d.summ.unknowns")%>% withSpinner(color="#0dc5c1")),
                                                                                            tabPanel("Degrees of freedom", tableOutput("design2d.summ.degrees")%>% withSpinner(color="#0dc5c1"))
                                                                                          )
                                                                                 ),
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
                                                                                          bsModal("modalExample", "Plot error ellipses - 2D net design", "modal_plot", size = "large", plotOutput("netSpatialView_ell11"), downloadButton('downloadPlot', 'Download')),
                                                                                          downloadButton("report2Ddesign_xlsx", "Generate report")
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
                                                                                                     numericInput(inputId = "st_dir_me", "Standard deviation for angle measurments ['']: ", value = 3),
                                                                                                     numericInput(inputId = "st_dist_me", "Standard deviation for distance measurments [mm]: ", value = 3),
                                                                                                     numericInput(inputId = "st_apriori_design_map", "'a priori' Standard deviation: ", value = 1)
                                                                                            ),
                                                                                            tabPanel("COORDINATE REFERENCE SYSTEM",
                                                                                                     numericInput(inputId = "epsg_me", "Destination CRS [EPSG code]: ", value = 3857)
                                                                                            ),
                                                                                            tabPanel("RESULT UNITS AND SCALE",
                                                                                                     textInput(inputId = "adjust_1_units_me", "Result units: " , value = "mm"),
                                                                                                     numericInput(inputId = "adjust_1_ell_scale_me", "Ellipse scale: ", value = 10)
                                                                                            ),
                                                                                            tabPanel("CRITERIA",
                                                                                                     p("POINT ACCURACY", style="text-align: center; font-weight: bold;"),
                                                                                                     fluidRow(
                                                                                                       column(width = 4, numericInput("sx_map", "Sx: ", value = 1.5)),
                                                                                                       column(width = 4, numericInput("sy_map", "Sy: ", value = 1.5)),
                                                                                                       column(width = 4, numericInput("sp_map", "Sp: ", value = 2))
                                                                                                     ),
                                                                                                     fluidRow(
                                                                                                       column(width = 12, numericInput("ab_map", "A/B: ", value = 0))
                                                                                                     ),
                                                                                                     fluidRow(
                                                                                                       column(width = 4, numericInput("dp_map", "dP: ", value = 0)),
                                                                                                       column(width = 4, numericInput("dpteta_map", "dPT: ", value = 0)),
                                                                                                       column(width = 4, numericInput("teta_map", "T: ", value = 0))
                                                                                                     ),
                                                                                                     p(""),
                                                                                                     p("MEASURMENT ACCURACY", style="text-align: center; font-weight: bold;"),
                                                                                                     fluidRow(
                                                                                                       column(width = 6, numericInput("sdir_map", "Sdir: ", value = 0)),
                                                                                                       column(width = 6, numericInput("sdist_map", "Sdist: ", value = 0))
                                                                                                     ),
                                                                                                     p(""),
                                                                                                     p("MEASURMENT RELIABILITY", style="text-align: center; font-weight: bold;"),
                                                                                                     fluidRow(
                                                                                                       column(width = 6, numericInput("rii_map", "rii: ", value = 0.3)),
                                                                                                       column(width = 6, numericInput("gii_map", "Gii: ", value = 0))
                                                                                                     )
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
                                                                                 tabPanel("SUMMARY",
                                                                                          p(""),
                                                                                          navlistPanel(
                                                                                            tabPanel("Network design", tableOutput("deisgn2dme.summ.des")%>% withSpinner(color="#0dc5c1")),
                                                                                            tabPanel("Stations", tableOutput("design2dme.summ.stations")%>% withSpinner(color="#0dc5c1")),
                                                                                            tabPanel("Observations", tableOutput("design2dme.summ.observations")%>% withSpinner(color="#0dc5c1")),
                                                                                            tabPanel("Unknowns", tableOutput("design2dme.summ.unknowns")%>% withSpinner(color="#0dc5c1")),
                                                                                            tabPanel("Degrees of freedom", tableOutput("design2dme.summ.degrees")%>% withSpinner(color="#0dc5c1"))
                                                                                          )
                                                                                 ),
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
                                                                                          bsModal("modalExample1", "Plot error ellipses - 2D net design", "modal_plot1", size = "large", plotOutput("netSpatialView_ell_me11"), downloadButton('downloadPlot1', 'Download')),
                                                                                          downloadButton("report2Ddesign_me_input", "Generate report")
                                                                                 )
                                                                               )

                                                                        )
                                                                      )
                                                                      )


                                                           )

                                                )
                                    )



                           ),
                           tabPanel("ADJUSTMENT",
                                    tabsetPanel(type = "pills",
                                                tabPanel("1D ADJUSTMENT",
                                                         p(""),
                                                         tabsetPanel(type = "pills",
                                                           tabPanel("XLSX INPUT DATA",
                                                                    p(""),
                                                                    fluidRow(
                                                                      column(width = 6, "DATA PREPARATION",
                                                                             tabsetPanel(
                                                                               tabPanel("MAIN",
                                                                                        p(""),
                                                                                        fileInput(inputId = "fileXLSX_1d_adj", label = "Upload points and measurments data file. Choose Excel - xlsx file:",
                                                                                                  multiple = TRUE, accept = c('.xlsx')),
                                                                                        p(""),
                                                                                        navlistPanel(
                                                                                          tabPanel("STOCHACTIC MODEL",
                                                                                                   fluidRow(
                                                                                                     p("Please select one to define the weights", style="text-align: left; font-weight: bold; font-colour: red")
                                                                                                   ),
                                                                                                   fluidRow(
                                                                                                     p(""),
                                                                                                     checkboxGroupInput("dh.s.model.a", "Stochastic model:", c("Standard deviation for height difference [mm]" = "sd_dh",
                                                                                                                                                             "Leveling distances [km]" = "d_dh",
                                                                                                                                                             "Number of stations" = "n_dh",
                                                                                                                                                             "Unit weights - identity matrix" = "E"))
                                                                                                   ),
                                                                                                   fluidRow(
                                                                                                     p("The selected model must be identical to the input data.", style="text-align: left; font-weight: bold; color: red")
                                                                                                   )
                                                                                          ),
                                                                                          tabPanel("OBSERVATION ACCURACY",
                                                                                                   numericInput(inputId = "sd_dh.a", "Standard deviation for height difference [mm]: ", value = 1),
                                                                                                   numericInput(inputId = "sd_rdh.a", "Standard deviation for reference height difference [mm]: ", value = 1),
                                                                                                   numericInput(inputId = "sd_apriori_dh.a", "'a priori' Standard deviation for height differences: ", value = 0.2)
                                                                                          ),
                                                                                          tabPanel("COORDINATE REFERENCE SYSTEM",
                                                                                                   numericInput(inputId = "epsg_1d.a", "Destination CRS [EPSG code]: ", value = NA)
                                                                                          ),
                                                                                          tabPanel("RESULT UNITS AND SCALE",
                                                                                                   textInput(inputId = "units_1d.a", "Result units: " , value = "mm"),
                                                                                                   numericInput(inputId = "scale_1d.a", "Result scale: ", value = 10)
                                                                                          ),
                                                                                          tabPanel("CRITERIA",
                                                                                                   p(""),
                                                                                                   p("MEASURMENT ACCURACY", style="text-align: center; font-weight: bold;"),
                                                                                                   fluidRow(
                                                                                                     #column(width = 6, numericInput("sd_h.a", "sd_h: ", value = 0.50)),
                                                                                                     column(width = 6, numericInput("sd_h.a", "sd_h: ", value = 0.3))
                                                                                                   ),
                                                                                                   p(""),
                                                                                                   p("MEASURMENT RELIABILITY", style="text-align: center; font-weight: bold;"),
                                                                                                   fluidRow(
                                                                                                     column(width = 6, numericInput("rii_1d.a", "rii: ", value = 0.3))
                                                                                                   )
                                                                                          )
                                                                                        ),
                                                                                        p(""),
                                                                                        actionButton(inputId ='adjust_1d_result.a', label='PREPROCESS DATA', class = "btn-primary btn-block"),
                                                                                        p(""),
                                                                                        actionButton(inputId ='update_adjust_1d.a', label='Update data for 1D net adjustment', class = "btn-primary"),
                                                                                        p(""),
                                                                                        actionButton(inputId ='adjust_1d.a', label='ADJUST', class = "btn-danger btn-block")
                                                                               ),
                                                                               tabPanel("POINTS",
                                                                                        rHandsontableOutput('p_1d.a') %>% withSpinner(color="#0dc5c1")
                                                                               ),
                                                                               tabPanel("OBSERVATIONS",
                                                                                        rHandsontableOutput('o_1d.a') %>% withSpinner(color="#0dc5c1")
                                                                               ),
                                                                               tabPanel("MAP",
                                                                                        tabsetPanel(
                                                                                          tabPanel("PLOT",
                                                                                                   plotlyOutput("netSpatialView_1d.a") %>% withSpinner(color="#0dc5c1")
                                                                                          )
                                                                                        )
                                                                               )
                                                                             )
                                                                      ),
                                                                      column(width = 6, "ADJUSTMENT 1D NET RESULTS",
                                                                             tabsetPanel(
                                                                               tabPanel("SUMMARY",
                                                                                        p(""),
                                                                                        navlistPanel(
                                                                                          tabPanel("Network adjustment", tableOutput("adj1d.summ.adj")%>% withSpinner(color="#0dc5c1")),
                                                                                          tabPanel("Stations", tableOutput("adj1d.summ.stations")%>% withSpinner(color="#0dc5c1")),
                                                                                          tabPanel("Observations", tableOutput("adj1d.summ.observations")%>% withSpinner(color="#0dc5c1")),
                                                                                          tabPanel("Unknowns", tableOutput("adj1d.summ.unknowns")%>% withSpinner(color="#0dc5c1")),
                                                                                          tabPanel("Degrees of freedom", tableOutput("adj1d.summ.degrees")%>% withSpinner(color="#0dc5c1"))
                                                                                        )
                                                                               ),
                                                                               tabPanel("MAP RESULTS",
                                                                                        p(""),
                                                                                        plotlyOutput("netSpatialView_1d_a") %>% withSpinner(color="#0dc5c1")
                                                                               ),
                                                                               tabPanel("TAB RESULTS",
                                                                                        p(""),
                                                                                        navlistPanel(
                                                                                          tabPanel("Net points", DT::dataTableOutput('1d_points_a') %>% withSpinner(color="#0dc5c1")),
                                                                                          tabPanel("Obseravtions", DT::dataTableOutput('1d_observations_a') %>% withSpinner(color="#0dc5c1")),
                                                                                          tabPanel("Plots", plotlyOutput("netSpatialView_1d_adj") %>% withSpinner(color="#0dc5c1"))
                                                                                        )
                                                                               ),
                                                                               tabPanel("EXPORT RESULTS",
                                                                                        downloadButton("report1Dadjust_xlsx", "Generate report")
                                                                                        #actionButton(inputId ='modal_plot_1d.a', label='PLOT', class = "btn-danger"),
                                                                                        #bsModal("modalExample1d", "Plot - 1D net design", "modal_plot_1d.a", size = "large", plotOutput("netSpatialView_1d_modal.a"), downloadButton('downloadPlot1d', 'Download'))
                                                                               )
                                                                             )

                                                                      )

                                                                    )


                                                                    )
                                                         )
                                                ),
                                                tabPanel("2D ADJUSTMENT",
                                                         p(""),
                                                         tabsetPanel(type = "pills",
                                                           tabPanel("XLSX INPUT DATA",
                                                                     p(""),
                                                                     fluidRow(
                                                                       column(width = 6, "DATA PREPARATION",
                                                                              tabsetPanel(
                                                                                tabPanel("MAIN",
                                                                                         p(""),
                                                                                         fileInput(inputId = "fileXLSX_adj", label = "Upload points and measurments data file. Choose Excel - xlsx file:",
                                                                                                   multiple = TRUE, accept = c('.xlsx')),
                                                                                         p(""),
                                                                                         navlistPanel(
                                                                                           tabPanel("OBSERVATION ACCURACY",
                                                                                                    numericInput(inputId = "st_dir_adj_xlsx", "Standard deviation for angle measurments ['']: ", value = 3),
                                                                                                    numericInput(inputId = "st_dist_adj_xlsx", "Standard deviation for distance measurments [mm]: ", value = 3),
                                                                                                    numericInput(inputId = "st_apriori_adj_xlsx", "'a priori' Standard deviation: ", value = 1)
                                                                                           ),
                                                                                           tabPanel("COORDINATE REFERENCE SYSTEM",
                                                                                                    numericInput(inputId = "epsg_xlsx_adj", "Destination CRS [EPSG code]: ", value = 3857)
                                                                                           ),
                                                                                           tabPanel("RESULT UNITS AND SCALE",
                                                                                                    textInput(inputId = "adjust_2_units", "Result units: " , value = "mm"),
                                                                                                    numericInput(inputId = "adjust_2_ell_scale", "Ellipse scale: ", value = 10)
                                                                                           ),
                                                                                           tabPanel("CRITERIA",
                                                                                                    p("POINT ACCURACY", style="text-align: center; font-weight: bold;"),
                                                                                                    fluidRow(
                                                                                                      column(width = 4, numericInput("sx_xlsx_adj", "Sx: ", value = 1.5)),
                                                                                                      column(width = 4, numericInput("sy_xlsx_adj", "Sy: ", value = 1.5)),
                                                                                                      column(width = 4, numericInput("sp_xlsx_adj", "Sp: ", value = 2))
                                                                                                    ),
                                                                                                    fluidRow(
                                                                                                      column(width = 12, numericInput("ab_xlsx_adj", "A/B: ", value = 0))
                                                                                                    ),
                                                                                                    fluidRow(
                                                                                                      column(width = 4, numericInput("dp_xlsx_adj", "dP: ", value = 0)),
                                                                                                      column(width = 4, numericInput("dpteta_xlsx_adj", "dPT: ", value = 0)),
                                                                                                      column(width = 4, numericInput("teta_xlsx_adj", "T: ", value = 0))
                                                                                                    ),
                                                                                                    p(""),
                                                                                                    p("MEASURMENT ACCURACY", style="text-align: center; font-weight: bold;"),
                                                                                                    fluidRow(
                                                                                                      column(width = 6, numericInput("sdir_xlsx_adj", "Sdir: ", value = 0)),
                                                                                                      column(width = 6, numericInput("sdist_xlsx_adj", "Sdist: ", value = 0))
                                                                                                    ),
                                                                                                    p(""),
                                                                                                    p("MEASURMENT RELIABILITY", style="text-align: center; font-weight: bold;"),
                                                                                                    fluidRow(
                                                                                                      column(width = 6, numericInput("rii_xlsx_adj", "rii: ", value = 0.3)),
                                                                                                      column(width = 6, numericInput("gii_xlsx_adj", "Gii: ", value = 0))
                                                                                                    )
                                                                                           )
                                                                                         ),
                                                                                         p(""),
                                                                                         actionButton(inputId ='preprocess_2d_adj', label='PREPROCESS DATA', class = "btn-primary btn-block"),
                                                                                         p(""),
                                                                                         actionButton(inputId ='update_adj_2d_xlsx', label='Update data for 2D net adjustment', class = "btn-primary"),
                                                                                         p(""),
                                                                                         actionButton(inputId ='adj_2d_adjust_xlsx', label='ADJUST', class = "btn-danger btn-block")
                                                                                ),
                                                                                tabPanel("POINTS",
                                                                                         rHandsontableOutput('p_adj_xlsx') %>% withSpinner(color="#0dc5c1")
                                                                                ),
                                                                                tabPanel("OBSERVATIONS",
                                                                                         rHandsontableOutput('o_adj_xlsx') %>% withSpinner(color="#0dc5c1")
                                                                                ),
                                                                                tabPanel("MAP",
                                                                                         tabsetPanel(
                                                                                           tabPanel("WEB MAP",
                                                                                                    leafletOutput("web_map_xlsx_2d_adj", height = 550) %>% withSpinner(color="#0dc5c1")
                                                                                           ),
                                                                                           tabPanel("PLOT",
                                                                                                    plotOutput("netSpatialView_xlsx_2d_adj")%>% withSpinner(color="#0dc5c1")
                                                                                           )
                                                                                         )
                                                                                )
                                                                              )
                                                                       ),
                                                                       column(width = 6, "ADJUSTMENT RESULTS",
                                                                              tabsetPanel(
                                                                                tabPanel("SUMMARY",
                                                                                         p(""),
                                                                                         navlistPanel(
                                                                                           tabPanel("Network adjustment", tableOutput("adj2d.summ.adj") %>% withSpinner(color="#0dc5c1")),
                                                                                           tabPanel("Stations", tableOutput("adj2d.summ.stations")%>% withSpinner(color="#0dc5c1")),
                                                                                           tabPanel("Observations", tableOutput("adj2d.summ.observations")%>% withSpinner(color="#0dc5c1")),
                                                                                           tabPanel("Unknowns", tableOutput("adj2d.summ.unknowns")%>% withSpinner(color="#0dc5c1")),
                                                                                           tabPanel("Degrees of freedom", tableOutput("adj2d.summ.degrees")%>% withSpinner(color="#0dc5c1"))
                                                                                         )
                                                                                ),
                                                                                tabPanel("MAP RESULTS",
                                                                                         p(""),
                                                                                         #verbatimTextOutput('tekstputanja'),
                                                                                         leafletOutput("map_ellipses_2d_adj", height = 550) %>% withSpinner(color="#0dc5c1")

                                                                                ),
                                                                                tabPanel("TAB RESULTS",
                                                                                         p(""),
                                                                                         navlistPanel(
                                                                                           tabPanel("Error ellipse", DT::dataTableOutput("ellipse_error_2d_adj") %>% withSpinner(color="#0dc5c1")
                                                                                                    ),
                                                                                           tabPanel("Net points", DT::dataTableOutput('net_points_adj_2d_adj') %>% withSpinner(color="#0dc5c1")
                                                                                                    ),
                                                                                           tabPanel("Obseravtions", DT::dataTableOutput('net_observations_adj_2d_adj') %>% withSpinner(color="#0dc5c1")
                                                                                                    ),
                                                                                           tabPanel("Plot error ellipses", plotOutput("netSpatialView_ell_2d_adj") %>% withSpinner(color="#0dc5c1")
                                                                                                    )
                                                                                         )

                                                                                ),
                                                                                tabPanel("EXPORT RESULTS",
                                                                                         actionButton(inputId ='modal_plot_2d_adj', label='PLOT', class = "btn-danger"),
                                                                                         bsModal("modalExample_2d_adj", "Plot error ellipses - 2D net design", "modal_plot_2d_adj", size = "large", plotOutput("netSpatialView_ell11_2d_adj"), downloadButton('downloadPlot_2d_adj', 'Download')),
                                                                                         downloadButton("report2Dadjust_xlsx", "Generate report")
                                                                                )
                                                                              )

                                                                       )
                                                                     )














                                                                     )
                                                           )
                                                )


                                    )


                           ),
                           tabPanel("DEFORMATION ANALYSIS"


                           ),
                           tabPanel("COORDINATE TRANSFORMATION"


                           )



      )
    )
)

