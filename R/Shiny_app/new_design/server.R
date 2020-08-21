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
library(shinycssloaders)
library(here)
library(matlib)
library(nngeo)
library(dplyr)
library(mapedit)
library(DT)
library(leaflet.extras)
library(rhandsontable)
library(shinyBS)
library(shinyWidgets)
library(knitr)
library(rmarkdown)
library(kableExtra)

shinyServer(function(input, output){
  # ==================================================================
  # 2D NET DESIGN
  # ==================================================================

  # XLSX INPUT DATA
  # xlsx_points <- reactive({
  #   req(input$fileXLSX)
  #   map_xlsx_points <- read_surveynet(path = input$fileXLSX$datapath, dest_crs = NA)#, col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
  #   map_xlsx_points$points
  # })

  # xlsx_observations <- reactive({
  #   req(input$fileXLSX)
  #   map_xlsx_observations <- read_surveynet(path = input$fileXLSX$datapath, dest_crs = NA)#, col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
  #   map_xlsx_observations
  # })

  xlsx_list <- reactive({#(input$go2, {
    req(input$fileXLSX)
    #p_xlsx <- xlsx_points()
    #o_xlsx <- xlsx_observations()
    dest_crs_xlsx = as.numeric(input$epsg_xlsx)
    output_xlsx <- read_surveynet(file = input$fileXLSX$datapath, dest_crs = dest_crs_xlsx)
    # output_xlsx$points %<>%
    #   dplyr::mutate_if(is.numeric, ~replace(., is.na(.), "NULL"))
    # output_xlsx$observations %<>%
    #   dplyr::mutate_if(is.numeric, ~replace(., is.na(.), "NULL"))
    output_xlsx
  })

  values_p <- reactiveValues()
  values_o <- reactiveValues()

  output$p_des_xlsx <- renderRHandsontable({
    rhandsontable(as.data.frame(xlsx_list()[[1]] %>%
                                  st_drop_geometry() %>%
                                  mutate(id = as.numeric(round(id, 1)))),
                  width = 650, height = 650)
  })

  output$o_des_xlsx <- renderRHandsontable({
    # values_p$data <- hot_to_r(input$p_des_xlsx)
    # p_df <- as.data.frame(values_p$data)
    rhandsontable(as.data.frame(xlsx_list()[[2]] %>% st_drop_geometry()), width = 650, height = 650)
  })

  updated_xlsx_list <- reactive({#(input$update_design_2d_xlsx,{
    values_p$data <- hot_to_r(input$p_des_xlsx)
    p_df <- as.data.frame(values_p$data)
    values_o$data <- hot_to_r(input$o_des_xlsx)
    o_df <- as.data.frame(values_o$data)
    dest_crs_xlsx = as.numeric(input$epsg_xlsx)
    p_xlsx <- xlsx_list()[[1]]
    output_xlsx <- import_surveynet2D_updated(points = p_df, observations = o_df, dest_crs = dest_crs_xlsx, raw_points = p_xlsx)
    output_xlsx
  })

  #output$netSpatialView_xlsx <- renderPlot({
  #  out_points_xlsx <- xlsx_list()[[1]]
  #  out_observations_xlsx <- xlsx_list()[[2]]
  #  output_view_xlsx <- net_spatial_view(points = out_points_xlsx, observations = out_observations_xlsx)
  #  output_view_xlsx
  #})

  #output$web_map_xlsx <- renderLeaflet({
  #  out_points_xlsx <- xlsx_list()[[1]]
  #  out_observations_xlsx <- xlsx_list()[[2]]
  #  web_map_xlsx <- net_spatial_view_web(points = out_points_xlsx, observations = out_observations_xlsx)
  #  web_map_xlsx@map
  #})


  # TO DO:: ZAMENITI SA plot_surveynet

  output$netSpatialView_xlsx_updated <- renderPlot({
    # out_points_xlsx <- updated_xlsx_list()[[1]]
    # out_observations_xlsx <- updated_xlsx_list()[[2]]
    snet <- updated_xlsx_list()
    output_view_xlsx <- plot_surveynet(snet = snet, webmap = FALSE, net.1D = FALSE, net.2D = TRUE)
    output_view_xlsx
  },
  width = 800,
  height = 800)

  output$web_map_xlsx_updated <- renderLeaflet({
    # out_points_xlsx <- updated_xlsx_list()[[1]]
    # out_observations_xlsx <- updated_xlsx_list()[[2]]
    snet <- updated_xlsx_list()
    web_map_xlsx <- plot_surveynet(snet = snet, webmap = TRUE, net.1D = FALSE, net.2D = TRUE)
    web_map_xlsx@map
  })

  #output$p_acc_design_xlsx <- renderRHandsontable({
  #  rhandsontable(data.frame(Sx = 0, Sy = 0, Sp = 0, A_B = 0, dP = 0, dPTeta = 0, Teta = 0))
  #})

  #output$mes_acc_design_xlsx <- renderRHandsontable({
  #  rhandsontable(data.frame(Sdir = 0, Sdist = 0))
  #})

  #output$mes_rel_design_xlsx <- renderRHandsontable({
  #  rhandsontable(data.frame(rii = 0, Gii = 0))
  #})


  # MAPEDIT INPUT DATA

  ns <- shiny::NS("map_me")
  lf <- leaflet() %>%
    addTiles() %>%
    addProviderTiles("Esri.WorldImagery",group="Esri.WorldImagery") %>%
    addProviderTiles("OpenStreetMap.Mapnik",group="OpenStreetMap") %>%
    addProviderTiles("Esri.DeLorme",group="Esri.DeLorme") %>%
    addProviderTiles("Esri.WorldTopoMap",group="Esri.WorldTopoMap") %>%
    setView(20.4580456, 44.8195306, zoom=12) %>%

    addLayersControl(baseGroups = c("Esri.WorldImagery", "OpenStreetMap", "Esri.DeLorme","Esri.WorldTopoMap"))

  editmapx <- callModule(editMod, "map_me", lf )

  po_me <- reactive({
    points_raw_me <- editmapx()$finished
    points_me <- surveynet.mapedit_points(points = points_raw_me)
    points_me
  })

  ob_example <- reactive({#(input$map_edit_result, {
    p_me <- po_me()
    obs_example <- surveynet.mapedit_observations_edit(points = p_me, st_dir = input$st_dir_me, st_dist = input$st_dist_me)
    obs_example
  })

  values_p_map <- reactiveValues()
  values_o_map <- reactiveValues()

  output$p_des_map <- renderRHandsontable({
    rhandsontable(as.data.frame(po_me() %>% st_drop_geometry() %>%
                                  dplyr::mutate(
                                    FIX_2D = FALSE,
                                    FIX_1D = NA,
                                    h = as.numeric(NA),
                                    Point_object = FALSE
                                  )), width = 650, height = 650)
  })

  output$o_des_map <- renderRHandsontable({
    rhandsontable(as.data.frame(ob_example()), width = 650, height = 650)
  })

  mapEdit_list <- reactive({#(input$update_design_2d_map, {
    points_raw <- po_me()
    values_p_map$data <- hot_to_r(input$p_des_map)
    p_df <- as.data.frame(values_p_map$data)
    values_o_map$data <- hot_to_r(input$o_des_map)
    o_df <- as.data.frame(values_o_map$data)
    dest_crs_me = as.numeric(input$epsg_me)
    output_me <- surveynet.mapedit(points_raw = points_raw, points = p_df, observations = o_df, dest_crs = dest_crs_me)
    output_me
  })

 output$netSpatialView_me <- renderPlot({
   # out_points_me <- mapEdit_list()[[1]]
   # out_observations_me <- mapEdit_list()[[2]]
   snet <- mapEdit_list()
   output_view_me <- plot_surveynet(snet = snet, webmap = FALSE, net.1D = FALSE, net.2D = TRUE)
   output_view_me
 },
 width = 800,
 height = 800)

 output$map_me_out <- renderLeaflet({
   # out_points_me <- mapEdit_list()[[1]]
   # out_observations_me <- mapEdit_list()[[2]]
   snet <- mapEdit_list()
   web_map_me <- plot_surveynet(snet = snet, webmap = TRUE, net.1D = FALSE, net.2D = TRUE)
   web_map_me@map
 })

 #output$p_acc_design_map <- renderRHandsontable({
 #  rhandsontable(data.frame(Sx = 0, Sy = 0, Sp = 0, A_B = 0, dP = 0, dPTeta = 0, Teta = 0))
 #})

 #output$mes_acc_design_map <- renderRHandsontable({
 #  rhandsontable(data.frame(Sdir = 0, Sdist = 0))
 #})

 #output$mes_rel_design_map <- renderRHandsontable({
 #  rhandsontable(data.frame(rii = 0, Gii = 0))
 #})

  ##############################
  # Input data with observations
  ##############################

  # xlsx_points_wO <- reactive({
  #   req(input$fileXLSX_adj)
  #   map_xlsx_points_wO <- readxl::read_xlsx(path = input$fileXLSX_adj$datapath, sheet = "Points")#, col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
  #   map_xlsx_points_wO
  # })

  # xlsx_observations_wO <- reactive({
  #   req(input$fileXLSX_adj)
  #   map_xlsx_observations_wO <- readxl::read_xlsx(path = input$fileXLSX_adj$datapath, sheet = "Observations")#, col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
  #   map_xlsx_observations_wO
  # })

  surveynet.wO <- reactive({#(input$preprocess_2d_adj, {
    # p_xlsx_wO <- xlsx_points_wO()
    # o_xlsx_wO <- xlsx_observations_wO()
    dest_crs_xlsx_wO = as.numeric(input$epsg_xlsx_adj)
    output_xlsx_wO <- read_surveynet(file = input$fileXLSX_adj$datapath, dest_crs = dest_crs_xlsx_wO)
    output_xlsx_wO
  })

  values_p_2d_adj <- reactiveValues()
  values_m_2d_adj <- reactiveValues()

  output$p_adj_xlsx <- renderRHandsontable({
    rhandsontable({
      surveynet.wO()[[1]] %>%
      st_drop_geometry() %>%
      as.data.frame() %>%
      mutate(id = round(id, 0))
      },
      width = 650,
      height = 650)
  })

  output$o_adj_xlsx <- renderRHandsontable(
    rhandsontable({
      surveynet.wO()[[2]] %>%
      st_drop_geometry() %>%
      as.data.frame() %>%
      mutate(use = TRUE)
      },
      width = 700,
      height = 700
  ))

  edited_wO <- reactive({#(input$update_adj_2d_xlsx,{
    values_p_2d_adj$data <- hot_to_r(input$p_adj_xlsx)
    values_m_2d_adj$data <- hot_to_r(input$o_adj_xlsx)

    p_up_2d_adj <- as.data.frame(values_p_2d_adj$data)
    m_up_2d_adj <- as.data.frame(values_m_2d_adj$data)
    m_up_2d_adj <- subset(m_up_2d_adj, use == TRUE)

    return_data <- list(p_up_2d_adj, m_up_2d_adj)
    names(return_data) <- c("points", "measurments")
    return_data
  })

  output$netSpatialView_xlsx_2d_adj <- renderPlot({
    out_points_xlsx_wO <- surveynet.wO()[[1]]
    out_observations_xlsx_wO <- surveynet.wO()[[2]]

    edited_observations_xlsx_wO <- edited_wO()$measurments
    edited_observations_xlsx_wO$geometry <- out_observations_xlsx_wO$geometry[match(edited_observations_xlsx_wO$ID, out_observations_xlsx_wO$ID )]
    edited_observations_xlsx_wO <- st_as_sf(edited_observations_xlsx_wO)

    edited_points_xlsx_wO <- edited_wO()$points
    edited_points_xlsx_wO$geometry <- out_points_xlsx_wO$geometry[match(edited_points_xlsx_wO$id, out_points_xlsx_wO$id )]
    edited_points_xlsx_wO <- st_as_sf(edited_points_xlsx_wO)


    snet <- list("points" = edited_points_xlsx_wO, "observations" = edited_observations_xlsx_wO)
    # output_view_xlsx_wO <- net_spatial_view_2DAdjustment_Import(points = out_points_xlsx_wO, observations = edited_observations_xlsx_wO)
    output_view_xlsx_wO <- plot_surveynet(snet = snet, webmap = FALSE, net.1D = FALSE, net.2D = TRUE)
    output_view_xlsx_wO
  }, width = 800, height = 800
  )

  output$web_map_xlsx_2d_adj <- renderLeaflet({
    out_points_xlsx_wO <- surveynet.wO()[[1]]
    out_observations_xlsx_wO <- surveynet.wO()[[2]]

    edited_observations_xlsx_wO <- edited_wO()$measurments
    edited_observations_xlsx_wO$geometry <- out_observations_xlsx_wO$geometry[match(edited_observations_xlsx_wO$ID, out_observations_xlsx_wO$ID )]
    edited_observations_xlsx_wO <- st_as_sf(edited_observations_xlsx_wO)

    edited_points_xlsx_wO <- edited_wO()$points
    edited_points_xlsx_wO$geometry <- out_points_xlsx_wO$geometry[match(edited_points_xlsx_wO$id, out_points_xlsx_wO$id )]
    edited_points_xlsx_wO <- st_as_sf(edited_points_xlsx_wO)


    snet <- list("points" = edited_points_xlsx_wO, "observations" = edited_observations_xlsx_wO)
    # web_map_xlsx <- net_spatial_view_web(points =  out_points_xlsx_wO, observations = edited_observations_xlsx_wO)
    web_map_xlsx <- plot_surveynet(snet = snet, webmap = TRUE, net.1D = FALSE, net.2D = TRUE)
    web_map_xlsx@map
  })

  #######################
  # 2D NET DESIGN RESULTS
  #######################

  # 2D net design - xlsx input data

  adjusted_net_design <- eventReactive(input$design_adjust_xlsx,{
    data <- xlsx_list()
    data_up <- updated_xlsx_list()
    result_units <- input$adjust_1_units
    ellipse_scale <- input$adjust_1_ell_scale

    if(length(data_up) == 0){
      design_net_out <- adjust.snet(adjust = FALSE, survey.net = data, dim_type = "2D", result.units = result_units, ellipse.scale = ellipse_scale, use.sd.estimated = FALSE, all = FALSE)
      design_net_out
    } else{
      design_net_out <- adjust.snet(adjust = FALSE, survey.net = data_up, dim_type = "2D", result.units = result_units, ellipse.scale = ellipse_scale, use.sd.estimated = FALSE, all = FALSE)
      design_net_out
    }
  })

  output$deisgn2d.summ.des <- eventReactive(input$design_adjust_xlsx,{

    data <- xlsx_list()
    data_up <- updated_xlsx_list()


    if(length(data_up) == 0){
      summary.adjustment <- data.frame(Parameter = c("Type: ", "Dimension: ", "Number of iterations: ", "Max. coordinate correction in last iteration: ", "Datum definition: "),
                                       Value = c("Weighted", "2D", 1, "0.0000 m",
                                                 if(all(data$points$FIX_2D == FALSE)){
                                                   "Datum defined with a minimal trace of the matrix Qx"
                                                 }else{"Fixed parameters - classically defined datum"}
                                       ))

      summary.adjustment %>%
        kable(caption = "Network design", digits = 4, align = "c", col.names = NULL) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)

    } else{
      summary.adjustment <- data.frame(Parameter = c("Type: ", "Dimension: ", "Number of iterations: ", "Max. coordinate correction in last iteration: ", "Datum definition: "),
                                       Value = c("Weighted", "2D", 1, "0.0000 m",
                                                 if(all(data_up$points$FIX_2D == FALSE)){
                                                   "Datum defined with a minimal trace of the matrix Qx"
                                                 }else{"Fixed parameters - classically defined datum"}
                                       ))

      summary.adjustment %>%
        kable(caption = "Network design", digits = 4, align = "c", col.names = NULL) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)
      }

  })


  output$design2d.summ.stations <- eventReactive(input$design_adjust_xlsx,{

    data <- xlsx_list()
    data_up <- updated_xlsx_list()

    if(length(data_up) == 0){
      summary.stations <- data.frame(Parameter = c("Number of (partly) known stations: ", "Number of unknown stations: ", "Total: "),
                                     Value = c(sum(data$points$FIX_2D == TRUE),
                                               sum(data$points$FIX_2D == FALSE),
                                               sum(data$points$FIX_2D == TRUE) + sum(data$points$FIX_2D == FALSE)))

      summary.stations %>%
        kable(caption = "Stations", digits = 4, align = "c", col.names = NULL) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)

    } else{
      summary.stations <- data.frame(Parameter = c("Number of (partly) known stations: ", "Number of unknown stations: ", "Total: "),
                                     Value = c(sum(data_up$points$FIX_2D == TRUE),
                                               sum(data_up$points$FIX_2D == FALSE),
                                               sum(data_up$points$FIX_2D == TRUE) + sum(data_up$points$FIX_2D == FALSE)))

      summary.stations %>%
        kable(caption = "Stations", digits = 4, align = "c", col.names = NULL) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)
    }

  })

  output$design2d.summ.observations <- eventReactive(input$design_adjust_xlsx,{

    data <- xlsx_list()
    data_up <- updated_xlsx_list()

    if(length(data_up) == 0){
      summary.observations <- data.frame(Parameter = c("Directions: ", "Distances: ", "Known coordinates: ", "Total: "),
                                         Value = c(sum(data$observations$direction == TRUE),
                                                   sum(data$observations$distance == TRUE),
                                                   sum(data$points$FIX_2D == TRUE)*2,
                                                   sum(data$observations$direction == TRUE)+sum(data$observations$distance == TRUE)+(sum(data$points$FIX_2D == TRUE)*2)))

      summary.observations %>%
        kable(caption = "Observations", digits = 4, align = "c", col.names = NULL) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)


    } else{
      summary.observations <- data.frame(Parameter = c("Directions: ", "Distances: ", "Known coordinates: ", "Total: "),
                                         Value = c(sum(data_up$observations$direction == TRUE),
                                                   sum(data_up$observations$distance == TRUE),
                                                   sum(data_up$points$FIX_2D == TRUE)*2,
                                                   sum(data_up$observations$direction == TRUE)+sum(data_up$observations$distance == TRUE)+(sum(data_up$points$FIX_2D == TRUE)*2)))

      summary.observations %>%
        kable(caption = "Observations", digits = 4, align = "c", col.names = NULL) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)
    }

  })

  output$design2d.summ.unknowns <- eventReactive(input$design_adjust_xlsx,{

    data <- xlsx_list()
    data_up <- updated_xlsx_list()

    if(length(data_up) == 0){
      summary.unknowns <- data.frame(Parameter = c("Coordinates: ", "Orientations: ", "Total: "),
                                     Value = c(sum(data$points$FIX_2D == FALSE)*2,
                                               length(data$observations %>% dplyr::filter(direction == TRUE) %>% .$from %>% unique()),
                                               (sum(data$points$FIX_2D == FALSE)*2)+length(data$observations %>% dplyr::filter(direction == TRUE) %>% .$from %>% unique())))

      summary.unknowns %>%
        kable(caption = "Unknowns", digits = 4, align = "c", col.names = NULL) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)



    } else{
      summary.unknowns <- data.frame(Parameter = c("Coordinates: ", "Orientations: ", "Total: "),
                                     Value = c(sum(data_up$points$FIX_2D == FALSE)*2,
                                               length(data_up$observations %>% dplyr::filter(direction == TRUE) %>% .$from %>% unique()),
                                               (sum(data_up$points$FIX_2D == FALSE)*2)+length(data_up$observations %>% dplyr::filter(direction == TRUE) %>% .$from %>% unique())))

      summary.unknowns %>%
        kable(caption = "Unknowns", digits = 4, align = "c", col.names = NULL) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)

    }

  })

  output$design2d.summ.degrees <- eventReactive(input$design_adjust_xlsx,{

    data <- xlsx_list()
    data_up <- updated_xlsx_list()

    if(length(data_up) == 0){
      summary.degrees <- data.frame(Parameter = "Degrees of freedom: ", Value = (sum(data$observations$direction == TRUE)+sum(data$observations$distance == TRUE)+(sum(data$points$FIX_2D == TRUE)*2)) - ((sum(data$points$FIX_2D == FALSE)*2)+length(data$observations %>% dplyr::filter(direction == TRUE) %>% .$from %>% unique())))

      summary.degrees %>%
        kable(caption = "Degrees of freedom: ", digits = 4, align = "c", col.names = NULL) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)




    } else{
      summary.degrees <- data.frame(Parameter = "Degrees of freedom: ", Value = (sum(data_up$observations$direction == TRUE)+sum(data_up$observations$distance == TRUE)+(sum(data_up$points$FIX_2D == TRUE)*2)) - ((sum(data_up$points$FIX_2D == FALSE)*2)+length(data_up$observations %>% dplyr::filter(direction == TRUE) %>% .$from %>% unique())))

      summary.degrees %>%
        kable(caption = "Degrees of freedom: ", digits = 4, align = "c", col.names = NULL) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)


    }

  })



  output$ellipse_error <- DT::renderDataTable({
    DT::datatable(adjusted_net_design()[[1]]$ellipse.net %>%
                    st_drop_geometry() %>%
                    as.data.frame() %>%
                    mutate(
                      A = round(A, 1),
                      B = round(B, 1),
                      teta = round(teta, 1),
                      sx = round(sx, 1),
                      sy = round(sy, 1),
                      sp = round(sp, 1)
                    ),escape=F,
                  extensions = list('Buttons', 'Scroller'),
                  options = list(dom = 'Bfrtip', buttons = I('colvis'),
                                 deferRender = TRUE,
                                 scrollY = 500,
                                 scrollX = 300,
                                 scroller = TRUE)) %>%
      formatStyle(
        'sx',
        color = styleInterval(c(input$sx_xlsx), c('black', 'red'))#,
        #backgroundColor = styleInterval(input$sx_xlsx, c('lightGray', 'tomato'))
      ) %>%
      formatStyle(
        'sy',
        color = styleInterval(c(input$sy_xlsx), c('black', 'red'))#,
        #backgroundColor = styleInterval(input$sy_xlsx, c('lightGray', 'tomato'))
      ) %>%
      formatStyle(
        'sp',
        color = styleInterval(c(input$sp_xlsx), c('black', 'red'))#,
        #backgroundColor = styleInterval(input$sp_xlsx, c('lightGray', 'tomato'))
      )
  })

  output$netSpatialView_ell <- renderPlot({
    # ellipses_1 <- adjusted_net_design()[[1]]$ellipse.net
    # observations_1 <- adjusted_net_design()[[2]]
    snet.adj <- adjusted_net_design()
    adj_output_view <- plot_surveynet(snet.adj = snet.adj, webmap = FALSE, net.1D = FALSE, net.2D = TRUE, ellipse.scale = input$adjust_1_ell_scale, epsg = input$epsg_xlsx)
    adj_output_view
  })

  plotInput_design.xlsx <- function(){
    snet.adj <- adjusted_net_design()
    plot_surveynet(snet.adj = snet.adj, webmap = FALSE, net.1D = FALSE, net.2D = TRUE, ellipse.scale = input$adjust_1_ell_scale, epsg = input$epsg_xlsx)
  }

  output$netSpatialView_ell11 <- renderPlot({
    snet.adj <- adjusted_net_design()
    adj_output_view <- plot_surveynet(snet.adj = snet.adj, webmap = FALSE, net.1D = FALSE, net.2D = TRUE, ellipse.scale = input$adjust_1_ell_scale, epsg = input$epsg_xlsx)
    adj_output_view
  })

  output$downloadPlot <- downloadHandler(
    filename = "plot.png",
    content = function(file) {
      ggsave(file, plotInput_design.xlsx())
    })

  output$net_points_adj <- DT::renderDataTable({
    DT::datatable(
        adjusted_net_design()[[1]]$net.points %>%
        st_drop_geometry() %>%
        as.data.frame() %>%
        mutate(
          A = round(A, 1),
          B = round(B, 1),
          teta = round(teta, 1),
          sx = round(sx, 1),
          sy = round(sy, 1),
          sp = round(sp, 1)
        ) %>%
          dplyr:: select(Name, FIX_2D, Point_object, sx, sy, sp),
        escape=F,
        extensions = list('Buttons', 'Scroller'),
        options = list(dom = 'Bfrtip', buttons = I('colvis'),
                       deferRender = TRUE,
                       scrollY = 500,
                       scrollX = 300,
                       scroller = TRUE)
      ) %>%
      formatStyle(
        'sx',
        color = styleInterval(c(input$sx_xlsx), c('black', 'red'))#,
        #backgroundColor = styleInterval(input$sx_xlsx, c('lightGray', 'tomato'))
      ) %>%
      formatStyle(
        'sy',
        color = styleInterval(c(input$sy_xlsx), c('black', 'red'))#,
        #backgroundColor = styleInterval(input$sy_xlsx, c('lightGray', 'tomato'))
      ) %>%
      formatStyle(
        'sp',
        color = styleInterval(c(input$sp_xlsx), c('black', 'red'))#,
        #backgroundColor = styleInterval(input$sp_xlsx, c('lightGray', 'tomato'))
      )
  })

  output$net_observations_adj <- DT::renderDataTable({
    DT::datatable(
      adjusted_net_design()[[2]] %>%
        st_drop_geometry() %>%
        as.data.frame() %>%
        mutate(
          Kl = round(Kl, 2),
          Kv = round(Kv, 2),
          rii = round(rii, 2)
        ) %>%
        dplyr::select(from, to, type, Kl, Kv, rii),
      escape=F,
      extensions = list('Buttons', 'Scroller'),
      options = list(dom = 'Bfrtip', buttons = I('colvis'),
                     deferRender = TRUE,
                     scrollY = 500,
                     scrollX = 300,
                     scroller = TRUE)
    )%>%
      formatStyle(
        'rii',
        color = styleInterval(c(input$rii_xlsx), c('red', 'black')),
        background = styleColorBar(adjusted_net_design()[[2]]$rii, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  # DA li elipse plotovati sa centrom u pribliznim ili ocenjenim koordinatama?
  output$map_ellipses_opt <- renderLeaflet({
    snet.adj <- adjusted_net_design()
    adj.net_map <- plot_surveynet(snet.adj = snet.adj, webmap = TRUE, net.1D = FALSE, net.2D = TRUE, ellipse.scale = input$adjust_1_ell_scale, result.units = input$adjust_1_units, sp_bound = input$sp_xlsx, rii_bound = input$rii_xlsx, epsg = input$epsg_xlsx)
    adj.net_map@map
  })


 # :::::::::::::::::::::::::::::::::::
 # 2D net design - map_edit input data
 # :::::::::::::::::::::::::::::::::::

  adjusted_net_design_me <- eventReactive(input$design_adjust_map,{
    data <- mapEdit_list()
    result_units <- input$adjust_1_units_me
    ellipse_scale <- input$adjust_1_ell_scale_me
    design_net_out <- adjust.snet(adjust = FALSE, survey.net = data, dim_type = "2D", result.units = result_units, ellipse.scale = ellipse_scale, use.sd.estimated = FALSE, all = FALSE)
    design_net_out
  })

  output$deisgn2dme.summ.des <- eventReactive(input$design_adjust_map,{
    data <- mapEdit_list()
    summary.adjustment <- data.frame(Parameter = c("Type: ", "Dimension: ", "Number of iterations: ", "Max. coordinate correction in last iteration: ", "Datum definition: "),
                                       Value = c("Weighted", "2D", 1, "0.0000 m",
                                                 if(all(data$points$FIX_2D == FALSE)){
                                                   "Datum defined with a minimal trace of the matrix Qx"
                                                 }else{"Fixed parameters - classically defined datum"}
                                       ))

    summary.adjustment %>%
        kable(caption = "Network design", digits = 4, align = "c", col.names = NULL) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)
  })


  output$design2dme.summ.stations <- eventReactive(input$design_adjust_map,{
    data <- mapEdit_list()
    summary.stations <- data.frame(Parameter = c("Number of (partly) known stations: ", "Number of unknown stations: ", "Total: "),
                                     Value = c(sum(data$points$FIX_2D == TRUE),
                                               sum(data$points$FIX_2D == FALSE),
                                               sum(data$points$FIX_2D == TRUE) + sum(data$points$FIX_2D == FALSE)))

    summary.stations %>%
        kable(caption = "Stations", digits = 4, align = "c", col.names = NULL) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)
  })

  output$design2dme.summ.observations <- eventReactive(input$design_adjust_map,{
    data <- mapEdit_list()
    summary.observations <- data.frame(Parameter = c("Directions: ", "Distances: ", "Known coordinates: ", "Total: "),
                                         Value = c(sum(data$observations$direction == TRUE),
                                                   sum(data$observations$distance == TRUE),
                                                   sum(data$points$FIX_2D == TRUE)*2,
                                                   sum(data$observations$direction == TRUE)+sum(data$observations$distance == TRUE)+(sum(data$points$FIX_2D == TRUE)*2)))

    summary.observations %>%
        kable(caption = "Observations", digits = 4, align = "c", col.names = NULL) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)

  })

  output$design2dme.summ.unknowns <- eventReactive(input$design_adjust_map,{
    data <- mapEdit_list()
    summary.unknowns <- data.frame(Parameter = c("Coordinates: ", "Orientations: ", "Total: "),
                                     Value = c(sum(data$points$FIX_2D == FALSE)*2,
                                               length(data$observations %>% dplyr::filter(direction == TRUE) %>% .$from %>% unique()),
                                               (sum(data$points$FIX_2D == FALSE)*2)+length(data$observations %>% dplyr::filter(direction == TRUE) %>% .$from %>% unique())))

    summary.unknowns %>%
        kable(caption = "Unknowns", digits = 4, align = "c", col.names = NULL) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)
  })

  output$design2dme.summ.degrees <- eventReactive(input$design_adjust_map,{
    data <- mapEdit_list()
    summary.degrees <- data.frame(Parameter = "Degrees of freedom: ", Value = (sum(data$observations$direction == TRUE)+sum(data$observations$distance == TRUE)+(sum(data$points$FIX_2D == TRUE)*2)) - ((sum(data$points$FIX_2D == FALSE)*2)+length(data$observations %>% dplyr::filter(direction == TRUE) %>% .$from %>% unique())))

    summary.degrees %>%
        kable(caption = "Degrees of freedom: ", digits = 4, align = "c", col.names = NULL) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)
  })



  output$ellipse_error_me <- DT::renderDataTable({
    DT::datatable(
        adjusted_net_design_me()[[1]]$ellipse.net %>%
        st_drop_geometry() %>%
        as.data.frame() %>%
        mutate(
          A = round(A, 1),
          B = round(B, 1),
          teta = round(teta, 1),
          sx = round(sx, 1),
          sy = round(sy, 1),
          sp = round(sp, 1)
        ), escape = FALSE,
        extensions = list('Buttons', 'Scroller'),
        options = list(dom = 'Bfrtip', buttons = I('colvis'),
                       deferRender = TRUE,
                       scrollY = 500,
                       scrollX = 300,
                       scroller = TRUE)) %>%
      formatStyle(
        'sx',
        color = styleInterval(c(input$sx_map), c('black', 'red'))#,
        #backgroundColor = styleInterval(input$sx_map, c('lightGray', 'tomato'))
      ) %>%
      formatStyle(
        'sy',
        color = styleInterval(c(input$sy_map), c('black', 'red'))#,
        #backgroundColor = styleInterval(input$sy_map, c('lightGray', 'tomato'))
      ) %>%
      formatStyle(
        'sp',
        color = styleInterval(c(input$sp_map), c('black', 'red'))#,
        #backgroundColor = styleInterval(input$sp_map, c('lightGray', 'tomato'))
      )
  })

  output$netSpatialView_ell_me <- renderPlot({
    snet.adj <- adjusted_net_design_me()
    adj_output_view <- plot_surveynet(snet.adj = snet.adj, webmap = FALSE, net.1D = FALSE, net.2D = TRUE, ellipse.scale = input$adjust_1_ell_scale_me)
    adj_output_view
  })

  plotInput_me <- function(){
    snet.adj <- adjusted_net_design_me()
    plot_surveynet(snet.adj = snet.adj, webmap = FALSE, net.1D = FALSE, net.2D = TRUE, ellipse.scale = input$adjust_1_ell_scale_me)
  }

  output$netSpatialView_ell_me11 <- renderPlot({
    snet.adj <- adjusted_net_design_me()
    adj_output_view <- plot_surveynet(snet.adj = snet.adj, webmap = FALSE, net.1D = FALSE, net.2D = TRUE, ellipse.scale = input$adjust_1_ell_scale_me)
    adj_output_view
    })

  output$downloadPlot1 <- downloadHandler(
    filename = "plot.png",
    content = function(file) {
      ggsave(file, plotInput_me())
    })

  output$net_points_adj_me <- DT::renderDataTable({
    DT::datatable(
        adjusted_net_design_me()[[1]]$net.points %>%
        st_drop_geometry() %>%
        as.data.frame() %>%
        mutate(
          A = round(A, 1),
          B = round(B, 1),
          teta = round(teta, 1),
          sx = round(sx, 1),
          sy = round(sy, 1),
          sp = round(sp, 1)
        ) %>%
          dplyr:: select(Name, FIX_2D, Point_object, sx, sy, sp),
        escape = FALSE,
        extensions = list('Buttons', 'Scroller'),
        options = list(dom = 'Bfrtip', buttons = I('colvis'),
                       deferRender = TRUE,
                       scrollY = 500,
                       scrollX = 300,
                       scroller = TRUE))%>%
      formatStyle(
        'sx',
        color = styleInterval(c(input$sx_map), c('black', 'red'))#,
        #backgroundColor = styleInterval(input$sx_map, c('lightGray', 'tomato'))
      ) %>%
      formatStyle(
        'sy',
        color = styleInterval(c(input$sy_map), c('black', 'red'))#,
        #backgroundColor = styleInterval(input$sy_map, c('lightGray', 'tomato'))
      ) %>%
      formatStyle(
        'sp',
        color = styleInterval(c(input$sp_map), c('black', 'red'))#,
        #backgroundColor = styleInterval(input$sp_map, c('lightGray', 'tomato'))
      )
  })

  output$net_observations_adj_me <- DT::renderDataTable({
    DT::datatable(
        adjusted_net_design_me()[[2]] %>%
        st_drop_geometry() %>%
        as.data.frame() %>%
        mutate(
          Kl = round(Kl, 2),
          Kv = round(Kv, 2),
          rii = round(rii, 2)
        ) %>%
          dplyr::select(from, to, type, Kl, Kv, rii),
        escape = FALSE,
        extensions = list('Buttons', 'Scroller'),
        options = list(dom = 'Bfrtip', buttons = I('colvis'),
                       deferRender = TRUE,
                       scrollY = 500,
                       scrollX = 300,
                       scroller = TRUE))%>%
      formatStyle(
        'rii',
        color = styleInterval(c(input$rii_map), c('red', 'black')),
        background = styleColorBar(adjusted_net_design_me()[[2]]$rii, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
 # DA li elipse plotovati sa centrom u pribliznim ili ocenjenim koordinatama?
  output$map_ellipses_opt_me <- renderLeaflet({
    snet.adj <- adjusted_net_design_me()
    adj.net_map <- plot_surveynet(snet.adj = snet.adj, webmap = TRUE, net.1D = FALSE, net.2D = TRUE, ellipse.scale = input$adjust_1_ell_scale_me, result.units = input$adjust_1_units_me, sp_bound = input$sp_map, rii_bound = input$rii_map)
    adj.net_map@map
  })


  ########################################
  # REPORT 2D design - mapedit input data
  ########################################

  output$report2Ddesign_me_input <- downloadHandler(
    filename = "report2D_design.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path("D:/R_projects/Surveyer/R/Shiny_app/new_design/Reports/Report_2D_design.R")
      #file.copy("report.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      data <- mapEdit_list()
      data_up <- mapEdit_list()
      ellipses <- adjusted_net_design_me()[[1]]$ellipse.net
      observations <- adjusted_net_design_me()[[2]]
      sp_bound = input$sp_map
      rii_bound = input$rii_map
      sx_bound <- input$sx_map
      sy_bound <- input$sy_map
      ellipse_scale <- input$adjust_1_ell_scale_me
      result_units <- input$adjust_1_units_me
      points <- adjusted_net_design_me()[[1]]$net.points
      adjusted_net_design <- adjusted_net_design_me()
      epsg <- input$epsg_me


      params <- list(data = data,
                     data_up = data_up,
                     ellipses = ellipses,
                     observations = observations,
                     sp_bound = sp_bound,
                     rii_bound = rii_bound,
                     sx_bound = sx_bound,
                     sy_bound = sy_bound,
                     points = points,
                     adjusted_net_design = adjusted_net_design,
                     ellipse_scale = ellipse_scale,
                     result_units = result_units,
                     epsg = epsg)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )


  ########################################
  # REPORT 2D design - xlsx inuput data
  ########################################

  output$report2Ddesign_xlsx <- downloadHandler(
    filename = "report2D_design.html",
    content = function(file) {
      tempReport <- file.path("D:/R_projects/Surveyer/R/Shiny_app/new_design/Reports/Report_2D_design.R")

      # Set up parameters to pass to Rmd document
      data <- xlsx_list()
      data_up <- updated_xlsx_list()
      ellipses <- adjusted_net_design()[[1]]$ellipse.net
      observations <- adjusted_net_design()[[2]]
      sp_bound = input$sp_xlsx
      rii_bound = input$rii_xlsx
      sx_bound <- input$sx_xlsx
      sy_bound <- input$sy_xlsx
      points <- adjusted_net_design()[[1]]$net.points
      adjusted_net_design <- adjusted_net_design()
      ellipse_scale <- input$adjust_1_ell_scale
      result_units <- input$adjust_1_units
      epsg <- input$epsg_xlsx

      params <- list(data = data,
                     data_up = data_up,
                     ellipses = ellipses,
                     observations = observations,
                     sp_bound = sp_bound,
                     rii_bound = rii_bound,
                     sx_bound = sx_bound,
                     sy_bound = sy_bound,
                     points = points,
                     adjusted_net_design = adjusted_net_design,
                     ellipse_scale = ellipse_scale,
                     result_units = result_units,
                     epsg = epsg)

      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )


  ###########################
  # 2D NET ADJUSTMENT RESULTS
  ###########################

  # 2D net adjustment - xlsx input data

  adjusted_net_adj <- eventReactive(input$adj_2d_adjust_xlsx,{
    #data <- surveynet.wO()
    #out_points_xlsx_wO <- surveynet.wO()[[1]]
    #out_observations_xlsx_wO <- surveynet.wO()[[2]]
    #edited_observations_xlsx_wO <- edited_wO()$measurments
    #edited_observations_xlsx_wO$geometry <- out_observations_xlsx_wO$geometry[match(edited_observations_xlsx_wO$ID, out_observations_xlsx_wO$ID )]
    #edited_observations_xlsx_wO <- st_as_sf(edited_observations_xlsx_wO)
    out_points_xlsx_wO <- surveynet.wO()[[1]]
    out_observations_xlsx_wO <- surveynet.wO()[[2]]

    edited_observations_xlsx_wO <- edited_wO()$measurments
    edited_observations_xlsx_wO$geometry <- out_observations_xlsx_wO$geometry[match(edited_observations_xlsx_wO$ID, out_observations_xlsx_wO$ID )]
    edited_observations_xlsx_wO <- st_as_sf(edited_observations_xlsx_wO)

    edited_points_xlsx_wO <- edited_wO()$points
    edited_points_xlsx_wO$geometry <- out_points_xlsx_wO$geometry[match(edited_points_xlsx_wO$id, out_points_xlsx_wO$id )]
    edited_points_xlsx_wO <- st_as_sf(edited_points_xlsx_wO)

    snet <- list("points" = edited_points_xlsx_wO, "observations" = edited_observations_xlsx_wO)
    #data_up <- surveynet.wO()
    #data_up[[2]] <- edited_observations_xlsx_wO

    result_units <- input$adjust_2_units
    ellipse_scale <- input$adjust_2_ell_scale

    adjusted_net_out <- adjust.snet(adjust = TRUE, survey.net = snet,  dim_type = "2D", result.units = input$adjust_2_units, ellipse.scale = input$adjust_2_ell_scale, sd.apriori = input$st_apriori_adj_xlsx, all = FALSE)
    adjusted_net_out
    #if(length(data_up) == 0){
    #  design_net_out <- design.snet(survey.net = data, result.units = result_units, ellipse.scale = ellipse_scale, all = FALSE)
    #  design_net_out
    #} else{
    #  design_net_out <- design.snet(survey.net = data_up, result.units = result_units, ellipse.scale = ellipse_scale, all = FALSE)
    #  design_net_out
    #}
  })





  output$adj2d.summ.adj <- eventReactive(input$adj_2d_adjust_xlsx,{

    if(is.null(adjusted_net_adj()$test$df)){
      summary.adjustment <- data.frame(Observation = adjusted_net_adj()$Observation,
                                       Statistics =  adjusted_net_adj()$statistics
      )

      summary.adjustment %>%
        kable(caption = "Check the statistics for individual observations. Suggestion: Remove the observation with the highest value of statistics:", digits = 4, align = "c") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)


    }else{

    out_points_xlsx_wO <- surveynet.wO()[[1]]
    out_observations_xlsx_wO <- surveynet.wO()[[2]]

    edited_observations_xlsx_wO <- edited_wO()$measurments
    edited_observations_xlsx_wO$geometry <- out_observations_xlsx_wO$geometry[match(edited_observations_xlsx_wO$ID, out_observations_xlsx_wO$ID )]
    edited_observations_xlsx_wO <- st_as_sf(edited_observations_xlsx_wO)

    edited_points_xlsx_wO <- edited_wO()$points
    edited_points_xlsx_wO$geometry <- out_points_xlsx_wO$geometry[match(edited_points_xlsx_wO$id, out_points_xlsx_wO$id )]
    edited_points_xlsx_wO <- st_as_sf(edited_points_xlsx_wO)

    data <- list("points" = edited_points_xlsx_wO, "observations" = edited_observations_xlsx_wO)
    sd.estimated <- adjusted_net_adj()$test$sd.aposteriori
    df <- adjusted_net_adj()$test$df

    model <- model_adequacy_test.shiny(sd.apriori = input$st_apriori_adj_xlsx, sd.estimated = sd.estimated, df = df, prob = 0.95)

    summary.adjustment <- data.frame(Parameter = c("Type: ", "Dimension: ", "Number of iterations: ", "Max. coordinate correction in last iteration: ", "Datum definition: ", "Sd apriori: ", "Sd aposteriori: ", "Probability: ", "F estimated: ", "F quantile: ", "Model adequacy test: "),
                                     Value = c("Weighted", "2D", 1, "0.0000 m",
                                               if(all(data$points$FIX_2D == FALSE)){
                                                 "Datum defined with a minimal trace of the matrix Qx"
                                               }else{"Fixed parameters - classically defined datum"},
                                               input$st_apriori_adj_xlsx,
                                               round(adjusted_net_adj()$test$sd.aposteriori,5),
                                               0.95,
                                               round(model$F.estimated, 5),
                                               round(model$F.quantile, 5),
                                               model$model
                                     ))

    summary.adjustment %>%
      kable(caption = "Network adjustment", digits = 4, align = "c", col.names = NULL) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE) %>%
      row_spec(11, bold = T, color = "white", background = "#D7261E")
    }
  })


  output$adj2d.summ.stations <- eventReactive(input$adj_2d_adjust_xlsx,{
    out_points_xlsx_wO <- surveynet.wO()[[1]]
    out_observations_xlsx_wO <- surveynet.wO()[[2]]

    edited_observations_xlsx_wO <- edited_wO()$measurments
    edited_observations_xlsx_wO$geometry <- out_observations_xlsx_wO$geometry[match(edited_observations_xlsx_wO$ID, out_observations_xlsx_wO$ID )]
    edited_observations_xlsx_wO <- st_as_sf(edited_observations_xlsx_wO)

    edited_points_xlsx_wO <- edited_wO()$points
    edited_points_xlsx_wO$geometry <- out_points_xlsx_wO$geometry[match(edited_points_xlsx_wO$id, out_points_xlsx_wO$id )]
    edited_points_xlsx_wO <- st_as_sf(edited_points_xlsx_wO)

    data <- list("points" = edited_points_xlsx_wO, "observations" = edited_observations_xlsx_wO)

    summary.stations <- data.frame(Parameter = c("Number of (partly) known stations: ", "Number of unknown stations: ", "Total: "),
                                   Value = c(sum(data$points$FIX_2D == TRUE),
                                             sum(data$points$FIX_2D == FALSE),
                                             sum(data$points$FIX_2D == TRUE) + sum(data$points$FIX_2D == FALSE)))

    summary.stations %>%
      kable(caption = "Stations", digits = 4, align = "c", col.names = NULL) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)
  })

  output$adj2d.summ.observations <- eventReactive(input$adj_2d_adjust_xlsx,{
    out_points_xlsx_wO <- surveynet.wO()[[1]]
    out_observations_xlsx_wO <- surveynet.wO()[[2]]

    edited_observations_xlsx_wO <- edited_wO()$measurments
    edited_observations_xlsx_wO$geometry <- out_observations_xlsx_wO$geometry[match(edited_observations_xlsx_wO$ID, out_observations_xlsx_wO$ID )]
    edited_observations_xlsx_wO <- st_as_sf(edited_observations_xlsx_wO)

    edited_points_xlsx_wO <- edited_wO()$points
    edited_points_xlsx_wO$geometry <- out_points_xlsx_wO$geometry[match(edited_points_xlsx_wO$id, out_points_xlsx_wO$id )]
    edited_points_xlsx_wO <- st_as_sf(edited_points_xlsx_wO)

    data <- list("points" = edited_points_xlsx_wO, "observations" = edited_observations_xlsx_wO)

    summary.observations <- data.frame(Parameter = c("Directions: ", "Distances: ", "Known coordinates: ", "Total: "),
                                       Value = c(sum(data$observations$direction == TRUE),
                                                 sum(data$observations$distance == TRUE),
                                                 sum(data$points$FIX_2D == TRUE)*2,
                                                 sum(data$observations$direction == TRUE)+sum(data$observations$distance == TRUE)+(sum(data$points$FIX_2D == TRUE)*2)))

    summary.observations %>%
      kable(caption = "Observations", digits = 4, align = "c", col.names = NULL) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)

  })

  output$adj2d.summ.unknowns <- eventReactive(input$adj_2d_adjust_xlsx,{
    out_points_xlsx_wO <- surveynet.wO()[[1]]
    out_observations_xlsx_wO <- surveynet.wO()[[2]]

    edited_observations_xlsx_wO <- edited_wO()$measurments
    edited_observations_xlsx_wO$geometry <- out_observations_xlsx_wO$geometry[match(edited_observations_xlsx_wO$ID, out_observations_xlsx_wO$ID )]
    edited_observations_xlsx_wO <- st_as_sf(edited_observations_xlsx_wO)

    edited_points_xlsx_wO <- edited_wO()$points
    edited_points_xlsx_wO$geometry <- out_points_xlsx_wO$geometry[match(edited_points_xlsx_wO$id, out_points_xlsx_wO$id )]
    edited_points_xlsx_wO <- st_as_sf(edited_points_xlsx_wO)

    data <- list("points" = edited_points_xlsx_wO, "observations" = edited_observations_xlsx_wO)

    summary.unknowns <- data.frame(Parameter = c("Coordinates: ", "Orientations: ", "Total: "),
                                   Value = c(sum(data$points$FIX_2D == FALSE)*2,
                                             length(data$observations %>% dplyr::filter(direction == TRUE) %>% .$from %>% unique()),
                                             (sum(data$points$FIX_2D == FALSE)*2)+length(data$observations %>% dplyr::filter(direction == TRUE) %>% .$from %>% unique())))

    summary.unknowns %>%
      kable(caption = "Unknowns", digits = 4, align = "c", col.names = NULL) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)
  })

  output$adj2d.summ.degrees <- eventReactive(input$adj_2d_adjust_xlsx,{
    out_points_xlsx_wO <- surveynet.wO()[[1]]
    out_observations_xlsx_wO <- surveynet.wO()[[2]]

    edited_observations_xlsx_wO <- edited_wO()$measurments
    edited_observations_xlsx_wO$geometry <- out_observations_xlsx_wO$geometry[match(edited_observations_xlsx_wO$ID, out_observations_xlsx_wO$ID )]
    edited_observations_xlsx_wO <- st_as_sf(edited_observations_xlsx_wO)

    edited_points_xlsx_wO <- edited_wO()$points
    edited_points_xlsx_wO$geometry <- out_points_xlsx_wO$geometry[match(edited_points_xlsx_wO$id, out_points_xlsx_wO$id )]
    edited_points_xlsx_wO <- st_as_sf(edited_points_xlsx_wO)

    data <- list("points" = edited_points_xlsx_wO, "observations" = edited_observations_xlsx_wO)

    summary.degrees <- data.frame(Parameter = "Degrees of freedom: ", Value = (sum(data$observations$direction == TRUE)+sum(data$observations$distance == TRUE)+(sum(data$points$FIX_2D == TRUE)*2)) - ((sum(data$points$FIX_2D == FALSE)*2)+length(data$observations %>% dplyr::filter(direction == TRUE) %>% .$from %>% unique())))

    summary.degrees %>%
      kable(caption = "Degrees of freedom: ", digits = 4, align = "c", col.names = NULL) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)
  })



  # output$tekstputanja <- renderPrint(adjusted_net_adj())
  output$ellipse_error_2d_adj <- DT::renderDataTable({
    #data <- adjusted_net_design()[[1]]
    #data %<>%
    #  st_drop_geometry() %>%
    #  as.data.frame() %>%
    #  mutate(
    #    A = round(A, 4),
    #    B = round(B, 4),
    #    teta = round(teta, 4),
    #    sx = round(sx, 4),
    #    sy = round(sy, 4),
    #    sp = round(sp, 4)
    #  )
    DT::datatable(adjusted_net_adj()[[1]]$ellipse.net %>%
                    st_drop_geometry() %>%
                    as.data.frame() %>%
                    mutate(
                      A = round(A, 1),
                      B = round(B, 1),
                      teta = round(teta, 1),
                      sx = round(sx, 1),
                      sy = round(sy, 1),
                      sp = round(sp, 1)
                    ),escape=F,
                  extensions = list('Buttons', 'Scroller'),
                  options = list(dom = 'Bfrtip', buttons = I('colvis'),
                                 deferRender = TRUE,
                                 scrollY = 500,
                                 scrollX = 300,
                                 scroller = TRUE)) %>%
      formatStyle(
        'sx',
        color = styleInterval(c(input$sx_xlsx_adj), c('black', 'red'))#,
        #backgroundColor = styleInterval(input$sx_xlsx, c('lightGray', 'tomato'))
      ) %>%
      formatStyle(
        'sy',
        color = styleInterval(c(input$sy_xlsx_adj), c('black', 'red'))#,
        #backgroundColor = styleInterval(input$sy_xlsx, c('lightGray', 'tomato'))
      ) %>%
      formatStyle(
        'sp',
        color = styleInterval(c(input$sp_xlsx_adj), c('black', 'red'))#,
        #backgroundColor = styleInterval(input$sp_xlsx, c('lightGray', 'tomato'))
      )
  })

  output$netSpatialView_ell_2d_adj <- renderPlot({
    snet.adj <- adjusted_net_adj()
    adj_output_view <- plot_surveynet(snet.adj = snet.adj, webmap = FALSE, net.1D = FALSE, net.2D = TRUE, ellipse.scale = input$adjust_2_ell_scale, epsg = input$epsg_xlsx_adj)
    adj_output_view
  })

  plotInput_adj <- function(){
    snet.adj <- adjusted_net_adj()
    plot_surveynet(snet.adj = snet.adj, webmap = FALSE, net.1D = FALSE, net.2D = TRUE, ellipse.scale = input$adjust_2_ell_scale, epsg = input$epsg_xlsx_adj)
  }

  output$netSpatialView_ell11_2d_adj <- renderPlot({
    snet.adj <- adjusted_net_adj()
    adj_output_view <- plot_surveynet(snet.adj = snet.adj, webmap = FALSE, net.1D = FALSE, net.2D = TRUE, ellipse.scale = input$adjust_2_ell_scale, epsg = input$epsg_xlsx_adj)
    adj_output_view
  })

  output$downloadPlot_2d_adj <- downloadHandler(
    filename = "plot.png",
    content = function(file) {
      ggsave(file, plotInput_adj())
    })

  output$net_points_adj_2d_adj <- DT::renderDataTable({
    DT::datatable(
      adjusted_net_adj()[[1]]$net.points %>%
        st_drop_geometry() %>%
        as.data.frame() %>%
        mutate(
          A = round(A, 1),
          B = round(B, 1),
          teta = round(teta, 1),
          sx = round(sx, 1),
          sy = round(sy, 1),
          sp = round(sp, 1),
          `dx [mm]` = round(dx, 2),
          `dy [mm]` = round(dy, 2),
          X = round(x, 2),
          Y = round(y, 2)
        ) %>%
        dplyr:: select(Name, `dx [mm]`, `dy [mm]`, X, Y, sx, sy, sp),
      escape=F,
      extensions = list('Buttons', 'Scroller'),
      options = list(dom = 'Bfrtip', buttons = I('colvis'),
                     deferRender = TRUE,
                     scrollY = 500,
                     scrollX = 300,
                     scroller = TRUE)
    ) %>%
      formatStyle(
        'sx',
        color = styleInterval(c(input$sx_xlsx_adj), c('black', 'red'))#,
        #backgroundColor = styleInterval(input$sx_xlsx, c('lightGray', 'tomato'))
      ) %>%
      formatStyle(
        'sy',
        color = styleInterval(c(input$sy_xlsx_adj), c('black', 'red'))#,
        #backgroundColor = styleInterval(input$sy_xlsx, c('lightGray', 'tomato'))
      ) %>%
      formatStyle(
        'sp',
        color = styleInterval(c(input$sp_xlsx_adj), c('black', 'red'))#,
        #backgroundColor = styleInterval(input$sp_xlsx, c('lightGray', 'tomato'))
      )
  })

  output$net_observations_adj_2d_adj <- DT::renderDataTable({
    DT::datatable(
      adjusted_net_adj()[[2]] %>%
        st_drop_geometry() %>%
        as.data.frame() %>%
        mutate(
          v = round(v, 2),
          Kl = round(Kl, 2),
          Kv = round(Kv, 2),
          rii = round(rii, 2)
        ) %>%
        dplyr::select(from, to, type, v, Kl, Kv, rii, used),
      escape=F,
      extensions = list('Buttons', 'Scroller'),
      options = list(dom = 'Bfrtip', buttons = I('colvis'),
                     deferRender = TRUE,
                     scrollY = 500,
                     scrollX = 300,
                     scroller = TRUE)
    )%>%
      formatStyle(
        'rii',
        color = styleInterval(c(input$rii_xlsx_adj), c('red', 'black')),
        background = styleColorBar(adjusted_net_adj()[[2]]$rii, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })

  output$map_ellipses_2d_adj <- renderLeaflet({
    snet.adj <- adjusted_net_adj()
    adj.net_map <- plot_surveynet(snet.adj = snet.adj, webmap = TRUE, net.1D = FALSE, net.2D = TRUE, ellipse.scale = input$adjust_2_ell_scale, sp_bound = input$sp_xlsx_adj, rii_bound = input$rii_xlsx_adj, result.units = input$adjust_2_units, epsg = input$epsg_xlsx_adj)
    adj.net_map@map
  })


  ########################################
  # REPORT 2D ADJUSTMENT - xlsx inuput data
  ########################################

  output$report2Dadjust_xlsx <- downloadHandler(
    filename = "report2D_adjustment.html",
    content = function(file) {
      tempReport <- file.path("D:/R_projects/Surveyer/R/Shiny_app/new_design/Reports/Report_2D_adjust.R")

      # Set up parameters to pass to Rmd document

      out_points_xlsx_wO <- surveynet.wO()[[1]]
      out_observations_xlsx_wO <- surveynet.wO()[[2]]

      edited_observations_xlsx_wO <- edited_wO()$measurments
      edited_observations_xlsx_wO$geometry <- out_observations_xlsx_wO$geometry[match(edited_observations_xlsx_wO$ID, out_observations_xlsx_wO$ID )]
      edited_observations_xlsx_wO <- st_as_sf(edited_observations_xlsx_wO)

      edited_points_xlsx_wO <- edited_wO()$points
      edited_points_xlsx_wO$geometry <- out_points_xlsx_wO$geometry[match(edited_points_xlsx_wO$id, out_points_xlsx_wO$id )]
      edited_points_xlsx_wO <- st_as_sf(edited_points_xlsx_wO)

      data <- list("points" = edited_points_xlsx_wO, "observations" = edited_observations_xlsx_wO)
      ellipses <- adjusted_net_adj()[[1]]$ellipse.net
      observations <- adjusted_net_adj()[[2]]
      sp_bound = input$sp_xlsx_adj
      rii_bound = input$rii_xlsx_adj
      sx_bound <- input$sx_xlsx_adj
      sy_bound <- input$sy_xlsx_adj
      points <- adjusted_net_adj()[[1]]$net.points
      adjusted_net_adj <- adjusted_net_adj()
      ellipse_scale <- input$adjust_2_ell_scale
      result_units <- input$adjust_2_units
      epsg <- input$epsg_xlsx_adj

      sd.estimated <- adjusted_net_adj()$test$sd.aposteriori
      df <- adjusted_net_adj()$test$df
      sd.apriori <- input$st_apriori_adj_xlsx

      params <- list(data = data,
                     ellipses = ellipses,
                     observations = observations,
                     sp_bound = sp_bound,
                     rii_bound = rii_bound,
                     sx_bound = sx_bound,
                     sy_bound = sy_bound,
                     points = points,
                     adjusted_net_adj = adjusted_net_adj,
                     ellipse_scale = ellipse_scale,
                     result_units = result_units,
                     epsg = epsg,
                     sd.estimated = sd.estimated,
                     sd.apriori = sd.apriori,
                     df = df)

      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

  # ===================================================================
  # 1D DESIGN
  # ===================================================================

  # XLSX INPUT DATA

  xlsx_list_1d <- reactive({
    req(input$fileXLSX_1d)
    dest_crs_xlsx = as.numeric(input$epsg_1d)
    output_xlsx <- read_surveynet(file = input$fileXLSX_1d$datapath, dest_crs = dest_crs_xlsx)
    output_xlsx
  })

  values_p1d <- reactiveValues()
  values_o1d <- reactiveValues()

  output$p_1d <- renderRHandsontable({
   rhandsontable(as.data.frame(xlsx_list_1d()[[1]] %>%
                                 mutate(id = as.numeric(round(id, 1)))),
                 width = 650, height = 650)
  })

  output$o_1d <- renderRHandsontable({
    rhandsontable(as.data.frame(xlsx_list_1d()[[2]]),
                  width = 650, height = 650)
  })

  updated_xlsx_list_1d <- reactive({
    values_p1d$data <- hot_to_r(input$p_1d)
    p_df <- as.data.frame(values_p1d$data)
    values_o1d$data <- hot_to_r(input$o_1d)
    o_df <- as.data.frame(values_o1d$data)
    dest_crs_xlsx = as.numeric(input$epsg_1d)
    p_xlsx <- xlsx_list_1d()[[1]]
    output_xlsx <- import_surveynet2D_updated(points = p_df, observations = o_df, dest_crs = dest_crs_xlsx, raw_points = p_xlsx)
    output_xlsx
  })

  output$netSpatialView_1d <- renderPlotly({
    snet <- updated_xlsx_list_1d()
    output_view_xlsx <- plot_surveynet(snet = snet, webmap = FALSE, net.1D = TRUE, net.2D = FALSE)
    output_view_xlsx
  })


  # 1D XLSX DESIGN RESULTS

    adjusted_1d.net_design <- eventReactive(input$design_adjust_1d,{
    data <- xlsx_list_1d()
    data_up <- updated_xlsx_list_1d()
    result_units <- input$units_1d

    if(length(data_up) == 0){
      design_net_out <- adjust.snet(adjust = FALSE, survey.net = data, dim_type = "1D", wdh_model = input$dh.s.model, result.units = result_units, sd.apriori = input$sd_apriori_dh, all = FALSE)
      design_net_out
    } else{
      design_net_out <- adjust.snet(adjust = FALSE, survey.net = data_up, dim_type = "1D", wdh_model = input$dh.s.model, result.units = result_units, sd.apriori = input$sd_apriori_dh, all = FALSE)
      design_net_out
    }
  })




    output$design1d.summ.des <- eventReactive(input$design_adjust_1d,{

      data <- xlsx_list_1d()
      data_up <- updated_xlsx_list_1d()

      if(length(data_up) == 0){
        summary.adjustment <- data.frame(Parameter = c("Type: ", "Dimension: ", "Number of iterations: ", "Max. coordinate correction in last iteration: ", "Datum definition: ", "Stochastic model: "),
                                         Value = c("Weighted", "1D", 1, "0.0000 m",
                                                   if(length(which(data$points$FIX_1D))==1 || length(which(data$points$FIX_1D))==0){
                                                     "Free 1D geodetic network"
                                                   }else{"Unfree 1D geodetic network"},
                                                   input$dh.s.model
                                         ))

        summary.adjustment %>%
          kable(caption = "Network design", digits = 4, align = "c", col.names = NULL) %>%
          kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)

      } else{
        summary.adjustment <- data.frame(Parameter = c("Type: ", "Dimension: ", "Number of iterations: ", "Max. coordinate correction in last iteration: ", "Datum definition: ", "Stochastic model: "),
                                         Value = c("Weighted", "1D", 1, "0.0000 m",
                                                   if(length(which(data_up$points$FIX_1D))==1 || length(which(data_up$points$FIX_1D))==0){
                                                     "Free 1D geodetic network"
                                                   }else{"Unfree 1D geodetic network"},
                                                   input$dh.s.model
                                         ))

        summary.adjustment %>%
          kable(caption = "Network design", digits = 4, align = "c", col.names = NULL) %>%
          kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)
      }

    })


    output$design1d.summ.stations <- eventReactive(input$design_adjust_1d,{

      data <- xlsx_list_1d()
      data_up <- updated_xlsx_list_1d()

      if(length(data_up) == 0){
        summary.stations <- data.frame(Parameter = c("Number of (partly) known stations: ", "Number of unknown stations: ", "Total: "),
                                       Value = c(sum(data$points$FIX_1D == TRUE),
                                                 sum(data$points$FIX_1D == FALSE),
                                                 sum(data$points$FIX_1D == TRUE) + sum(data$points$FIX_1D == FALSE)))

        summary.stations %>%
          kable(caption = "Stations", digits = 4, align = "c", col.names = NULL) %>%
          kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)

      } else{
        summary.stations <- data.frame(Parameter = c("Number of (partly) known stations: ", "Number of unknown stations: ", "Total: "),
                                       Value = c(sum(data_up$points$FIX_1D == TRUE),
                                                 sum(data_up$points$FIX_1D == FALSE),
                                                 sum(data_up$points$FIX_1D == TRUE) + sum(data_up$points$FIX_1D == FALSE)))

        summary.stations %>%
          kable(caption = "Stations", digits = 4, align = "c", col.names = NULL) %>%
          kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)
      }

    })

    output$design1d.summ.observations <- eventReactive(input$design_adjust_1d,{

      data <- xlsx_list_1d()
      data_up <- updated_xlsx_list_1d()

      if(length(data_up) == 0){
        summary.observations <- data.frame(Parameter = c("Leveled Height Differences: ", "Known coordinates: ", "Total: "),
                                           Value = c(sum(data$observations$diff_level == TRUE),
                                                     sum(data$points$FIX_1D == TRUE)*1,
                                                     sum(data$observations$diff_level == TRUE)+sum(data$points$FIX_1D == TRUE)*1))

        summary.observations %>%
          kable(caption = "Observations", digits = 4, align = "c", col.names = NULL) %>%
          kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)


      } else{
        summary.observations <- data.frame(Parameter = c("Leveled Height Differences: ", "Known coordinates: ", "Total: "),
                                           Value = c(sum(data_up$observations$diff_level == TRUE),
                                                     sum(data_up$points$FIX_1D == TRUE)*1,
                                                     sum(data_up$observations$diff_level == TRUE)+sum(data$points$FIX_1D == TRUE)*1))
        summary.observations %>%
          kable(caption = "Observations", digits = 4, align = "c", col.names = NULL) %>%
          kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)
      }

    })

    output$design1d.summ.unknowns <- eventReactive(input$design_adjust_1d,{

      data <- xlsx_list_1d()
      data_up <- updated_xlsx_list_1d()

      if(length(data_up) == 0){
        summary.unknowns <- data.frame(Parameter = c("Coordinates: ", "Total: "),
                                       Value = c(sum(data$points$FIX_1D == FALSE)*1,
                                                 sum(data$points$FIX_1D == FALSE)*1))

        summary.unknowns %>%
          kable(caption = "Unknowns", digits = 4, align = "c", col.names = NULL) %>%
          kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)



      } else{
        summary.unknowns <- data.frame(Parameter = c("Coordinates: ", "Total: "),
                                       Value = c(sum(data_up$points$FIX_1D == FALSE)*1,
                                                 sum(data_up$points$FIX_1D == FALSE)*1))
        summary.unknowns %>%
          kable(caption = "Unknowns", digits = 4, align = "c", col.names = NULL) %>%
          kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)

      }

    })

    output$design1d.summ.degrees <- eventReactive(input$design_adjust_1d,{

      data <- xlsx_list_1d()
      data_up <- updated_xlsx_list_1d()

      if(length(data_up) == 0){
        summary.degrees <- data.frame(Parameter = "Degrees of freedom: ",
                                      Value = (sum(data$observations$diff_level == TRUE)+sum(data$points$FIX_1D == TRUE))-sum(data$points$FIX_1D == FALSE))

        summary.degrees %>%
          kable(caption = "Degrees of freedom: ", digits = 4, align = "c", col.names = NULL) %>%
          kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)




      } else{
        summary.degrees <- data.frame(Parameter = "Degrees of freedom: ",
                                      Value = (sum(data_up$observations$diff_level == TRUE)+sum(data_up$points$FIX_1D == TRUE))-sum(data_up$points$FIX_1D == FALSE))

        summary.degrees %>%
          kable(caption = "Degrees of freedom: ", digits = 4, align = "c", col.names = NULL) %>%
          kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)


      }

    })


  # output$netSpatialView_ell <- renderPlot({
  #   # ellipses_1 <- adjusted_net_design()[[1]]$ellipse.net
  #   # observations_1 <- adjusted_net_design()[[2]]
  #   snet.adj <- adjusted_net_design()
  #   adj_output_view <- plot_surveynet(snet.adj = snet.adj, webmap = FALSE, net.1D = FALSE, net.2D = TRUE, ellipse.scale = input$adjust_1_ell_scale)
  #   adj_output_view
  # })
  #
  # plotInput_design.xlsx <- function(){
  #   snet.adj <- adjusted_net_design()
  #   plot_surveynet(snet.adj = snet.adj, webmap = FALSE, net.1D = FALSE, net.2D = TRUE, ellipse.scale = input$adjust_1_ell_scale)
  # }
  #
  # output$netSpatialView_ell11 <- renderPlot({
  #   snet.adj <- adjusted_net_design()
  #   adj_output_view <- plot_surveynet(snet.adj = snet.adj, webmap = FALSE, net.1D = FALSE, net.2D = TRUE, ellipse.scale = input$adjust_1_ell_scale)
  #   adj_output_view
  # })

  # output$downloadPlot <- downloadHandler(
  #   filename = "plot.png",
  #   content = function(file) {
  #     ggsave(file, plotInput_design.xlsx())
  #   })

  output$netSpatialView_1d_design <- renderPlotly({
    snet_adj <- adjusted_1d.net_design()
    output_des_plot <- plot_surveynet(snet.adj = snet_adj, webmap = FALSE, net.1D = TRUE, net.2D = FALSE)
    output_des_plot
  })

  output$netSpatialView_1d_des <- renderPlotly({
    snet_adj <- adjusted_1d.net_design()
    output_des_plot <- plot_surveynet(snet.adj = snet_adj, webmap = FALSE, net.1D = TRUE, net.2D = FALSE)
    output_des_plot
  })



  output$`1d_points_des` <- DT::renderDataTable({
    DT::datatable(
      adjusted_1d.net_design()[[1]] %>%
        as.data.frame() %>%
        mutate(
          h = round(h, 2),
          sd_h = round(sd_h, 2)
        ) %>%
        dplyr:: select(Name, FIX_1D, Point_object, h, sd_h),
      escape=F,
      extensions = list('Buttons', 'Scroller'),
      options = list(dom = 'Bfrtip', buttons = I('colvis'),
                     deferRender = TRUE,
                     scrollY = 500,
                     scrollX = 300,
                     scroller = TRUE)
    ) %>%
      formatStyle(
        'sd_h',
        color = styleInterval(c(input$sd_h), c('black', 'aqua')),
        backgroundColor = styleInterval(input$sd_h, c('lightGray', '#FF6347'))
      )
  })

  output$`1d_observations_des` <- DT::renderDataTable({
    DT::datatable(
      adjusted_1d.net_design()[[2]] %>%
        as.data.frame() %>%
        mutate(
          Kl = round(Kl, 2),
          Kv = round(Kv, 2),
          rii = round(rii, 2)
        ) %>%
        dplyr::select(from, to, type, Kl, Kv, rii),
      escape=F,
      extensions = list('Buttons', 'Scroller'),
      options = list(dom = 'Bfrtip', buttons = I('colvis'),
                     deferRender = TRUE,
                     scrollY = 500,
                     scrollX = 300,
                     scroller = TRUE)
    )%>%
      formatStyle(
        'rii',
        color = styleInterval(c(input$rii_1d), c('red', 'black')),
        background = styleColorBar(adjusted_1d.net_design()[[2]]$rii, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })


  # MAPEDIT INPUT DATA

  ns_1d <- shiny::NS("map_me_1d")
  lf_1d <- leaflet() %>%
    addTiles() %>%
    addProviderTiles("Esri.WorldImagery",group="Esri.WorldImagery") %>%
    addProviderTiles("OpenStreetMap.Mapnik",group="OpenStreetMap") %>%
    addProviderTiles("Esri.DeLorme",group="Esri.DeLorme") %>%
    addProviderTiles("Esri.WorldTopoMap",group="Esri.WorldTopoMap") %>%
    setView(20.4580456, 44.8195306, zoom=12) %>%
    addLayersControl(baseGroups = c("Esri.WorldImagery", "OpenStreetMap", "Esri.DeLorme","Esri.WorldTopoMap"))

  editmapx_1d <- callModule(editMod, "map_me_1d", lf_1d )

  po_me_1d <- reactive({
    points_raw_me_1d <- editmapx_1d()$finished
    points_me_1d <- surveynet.mapedit_points_1d(points = points_raw_me_1d)
    points_me_1d
  })

  #ob_example_1d <- reactive({#(input$map_edit_result, {
  #  p_me_1d <- po_me_1d()
  #  obs_example_1d <- surveynet.mapedit_observations_edit_1d(points = p_me_1d)
  #  obs_example_1d
  #})

  values_p_map_1d <- reactiveValues()
  #values_o_map_1d <- reactiveValues()

  output$p_des_map_1d <- renderRHandsontable({
    rhandsontable(as.data.frame(po_me_1d() %>% st_drop_geometry() %>%
                                  mutate(
                                    FIX_1D = FALSE,
                                    Point_object = FALSE
                                  )), width = 650, height = 650)
  })




  ########################################
  # REPORT 1D design - xlsx inuput data
  ########################################

  output$report1Ddesign <- downloadHandler(
    filename = "report1D_design.html",
    content = function(file) {
      tempReport <- file.path("D:/R_projects/Surveyer/R/Shiny_app/new_design/Reports/Report_1D_design.R")

      # Set up parameters to pass to Rmd document
      model <- input$dh.s.model
      data <- xlsx_list_1d()
      data_up <- updated_xlsx_list_1d()
      net1d_design <- adjusted_1d.net_design()
      sd_h_bound <- input$sd_h
      rii_bound <- input$rii_1d

      params <- list(model = model,
                     data = data,
                     data_up = data_up,
                     net1d_design = net1d_design,
                     sd_h_bound = sd_h_bound,
                     rii_bound = rii_bound)

      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )


# ===================================================================
# 1D ADJUSTMENT
# ===================================================================

# XLSX INPUT DATA

xlsx_list_1d_adj <- reactive({
  req(input$fileXLSX_1d_adj)
  dest_crs_xlsx = as.numeric(input$epsg_1d.a)
  output_xlsx <- read_surveynet(file = input$fileXLSX_1d_adj$datapath, dest_crs = dest_crs_xlsx)
  output_xlsx
})

values_p1d.a <- reactiveValues()
values_o1d.a <- reactiveValues()

output$p_1d.a <- renderRHandsontable({
  rhandsontable(as.data.frame(xlsx_list_1d_adj()[[1]] %>%
                                mutate(id = as.numeric(round(id, 1)))),
                width = 650, height = 650)
})

output$o_1d.a <- renderRHandsontable({
  rhandsontable(as.data.frame(xlsx_list_1d_adj()[[2]]),
                width = 650, height = 650)
})

updated_xlsx_list_1d.a <- reactive({
  values_p1d.a$data <- hot_to_r(input$p_1d.a)
  p_df.a <- as.data.frame(values_p1d.a$data)
  values_o1d.a$data <- hot_to_r(input$o_1d.a)
  o_df.a <- as.data.frame(values_o1d.a$data)
  dest_crs_xlsx.a = as.numeric(input$epsg_1d.a)
  p_xlsx.a <- xlsx_list_1d_adj()[[1]]
  output_xlsx <- import_surveynet2D_updated(points = p_df.a, observations = o_df.a, dest_crs = dest_crs_xlsx.a, raw_points = p_xlsx.a)
  output_xlsx
})

output$netSpatialView_1d.a <- renderPlotly({
  snet <- updated_xlsx_list_1d.a()
  output_view_xlsx <- plot_surveynet(snet = snet, webmap = FALSE, net.1D = TRUE, net.2D = FALSE)
  output_view_xlsx
})


# 1D XLSX ADJUSTMENT RESULTS

adjusted_1d.net_a <- eventReactive(input$adjust_1d.a,{
  data <- xlsx_list_1d_adj()
  data_up <- updated_xlsx_list_1d.a()
  result_units <- input$units_1d.a

  if(length(data_up) == 0){
    design_net_out <- adjust.snet(adjust = TRUE, survey.net = data, dim_type = "1D", wdh_model = input$dh.s.model.a, result.units = result_units, sd.apriori = input$sd_apriori_dh.a, all = FALSE)
    design_net_out
  } else{
    design_net_out <- adjust.snet(adjust = TRUE, survey.net = data_up, dim_type = "1D", wdh_model = input$dh.s.model.a, result.units = result_units, sd.apriori = input$sd_apriori_dh.a, all = FALSE)
    design_net_out
  }
})



output$adj1d.summ.adj <- eventReactive(input$adjust_1d.a,{

  #data <- xlsx_list_1d_adj()
  #data_up <- updated_xlsx_list_1d.a()


  #sd.estimated <- adjusted_1d.net_a()$test$sd.aposteriori
  #df <- adjusted_1d.net_a()$test$df
  #model <- model_adequacy_test.shiny(sd.apriori = input$sd_apriori_dh.a, sd.estimated = sd.estimated, df = df, prob = 0.95)

  if(is.null(adjusted_1d.net_a()$test$df)){
    summary.adjustment <- data.frame(Observation = adjusted_1d.net_a()$Observation,
                                     Statistics =  adjusted_1d.net_a()$statistics
                                     )

    summary.adjustment %>%
      kable(caption = "Check the statistics for individual observations. Suggestion: Remove the observation with the highest value of statistics:", digits = 4, align = "c") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)


  }else{
    data <- xlsx_list_1d_adj()
    data_up <- updated_xlsx_list_1d.a()


    sd.estimated <- adjusted_1d.net_a()$test$sd.aposteriori
    df <- adjusted_1d.net_a()$test$df
    model <- model_adequacy_test.shiny(sd.apriori = input$sd_apriori_dh.a, sd.estimated = sd.estimated, df = df, prob = 0.95)

  if(length(data_up) == 0){
    summary.adjustment <- data.frame(Parameter = c("Type: ", "Dimension: ", "Number of iterations: ", "Max. coordinate correction in last iteration: ", "Datum definition: ", "Stochastic model: ", "Sd apriori: ", "Sd aposteriori: ", "Probability: ", "F estimated: ", "F quantile: ", "Model adequacy test: "),
                                     Value = c("Weighted", "1D", 1, "0.0000 m",
                                               if(length(which(data$points$FIX_1D))==1 || length(which(data$points$FIX_1D))==0){
                                                 "Free 1D geodetic network"
                                               }else{"Unfree 1D geodetic network"},
                                               input$dh.s.model.a,
                                               input$sd_apriori_dh.a,
                                               round(adjusted_1d.net_a()$test$sd.aposteriori,5),
                                               0.95,
                                               round(model$F.estimated, 5),
                                               round(model$F.quantile, 5),
                                               model$model

                                     ))

    summary.adjustment %>%
      kable(caption = "Network adjustment", digits = 4, align = "c", col.names = NULL) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)%>%
      row_spec(12, bold = T, color = "white", background = "#D7261E")

  } else{
    summary.adjustment <- data.frame(Parameter = c("Type: ", "Dimension: ", "Number of iterations: ", "Max. coordinate correction in last iteration: ", "Datum definition: ", "Stochastic model: ", "Sd apriori: ", "Sd aposteriori: ", "Probability: ", "F estimated: ", "F quantile: ", "Model adequacy test: "),
                                     Value = c("Weighted", "1D", 1, "0.0000 m",
                                               if(length(which(data_up$points$FIX_1D))==1 || length(which(data_up$points$FIX_1D))==0){
                                                 "Free 1D geodetic network"
                                               }else{"Unfree 1D geodetic network"},
                                               input$dh.s.model.a,
                                               input$sd_apriori_dh.a,
                                               round(adjusted_1d.net_a()$test$sd.aposteriori,5),
                                               0.95,
                                               round(model$F.estimated, 5),
                                               round(model$F.quantile, 5),
                                               model$model
                                     ))

    summary.adjustment %>%
      kable(caption = "Network adjustment", digits = 4, align = "c", col.names = NULL) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)%>%
      row_spec(12, bold = T, color = "white", background = "#D7261E")
  }
  }
})


output$adj1d.summ.stations <- eventReactive(input$adjust_1d.a,{

  data <- xlsx_list_1d_adj()
  data_up <- updated_xlsx_list_1d.a()

  if(length(data_up) == 0){
    summary.stations <- data.frame(Parameter = c("Number of (partly) known stations: ", "Number of unknown stations: ", "Total: "),
                                   Value = c(sum(data$points$FIX_1D == TRUE),
                                             sum(data$points$FIX_1D == FALSE),
                                             sum(data$points$FIX_1D == TRUE) + sum(data$points$FIX_1D == FALSE)))

    summary.stations %>%
      kable(caption = "Stations", digits = 4, align = "c", col.names = NULL) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)

  } else{
    summary.stations <- data.frame(Parameter = c("Number of (partly) known stations: ", "Number of unknown stations: ", "Total: "),
                                   Value = c(sum(data_up$points$FIX_1D == TRUE),
                                             sum(data_up$points$FIX_1D == FALSE),
                                             sum(data_up$points$FIX_1D == TRUE) + sum(data_up$points$FIX_1D == FALSE)))

    summary.stations %>%
      kable(caption = "Stations", digits = 4, align = "c", col.names = NULL) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)
  }

})

output$adj1d.summ.observations <- eventReactive(input$adjust_1d.a,{

  data <- xlsx_list_1d_adj()
  data_up <- updated_xlsx_list_1d.a()

  if(length(data_up) == 0){
    summary.observations <- data.frame(Parameter = c("Leveled Height Differences: ", "Known coordinates: ", "Total: "),
                                       Value = c(sum(data$observations$diff_level == TRUE),
                                                 sum(data$points$FIX_1D == TRUE)*1,
                                                 sum(data$observations$diff_level == TRUE)+sum(data$points$FIX_1D == TRUE)*1))

    summary.observations %>%
      kable(caption = "Observations", digits = 4, align = "c", col.names = NULL) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)


  } else{
    summary.observations <- data.frame(Parameter = c("Leveled Height Differences: ", "Known coordinates: ", "Total: "),
                                       Value = c(sum(data_up$observations$diff_level == TRUE),
                                                 sum(data_up$points$FIX_1D == TRUE)*1,
                                                 sum(data_up$observations$diff_level == TRUE)+sum(data$points$FIX_1D == TRUE)*1))
    summary.observations %>%
      kable(caption = "Observations", digits = 4, align = "c", col.names = NULL) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)
  }

})

output$adj1d.summ.unknowns <- eventReactive(input$adjust_1d.a,{

  data <- xlsx_list_1d_adj()
  data_up <- updated_xlsx_list_1d.a()

  if(length(data_up) == 0){
    summary.unknowns <- data.frame(Parameter = c("Coordinates: ", "Total: "),
                                   Value = c(sum(data$points$FIX_1D == FALSE)*1,
                                             sum(data$points$FIX_1D == FALSE)*1))

    summary.unknowns %>%
      kable(caption = "Unknowns", digits = 4, align = "c", col.names = NULL) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)



  } else{
    summary.unknowns <- data.frame(Parameter = c("Coordinates: ", "Total: "),
                                   Value = c(sum(data_up$points$FIX_1D == FALSE)*1,
                                             sum(data_up$points$FIX_1D == FALSE)*1))
    summary.unknowns %>%
      kable(caption = "Unknowns", digits = 4, align = "c", col.names = NULL) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)

  }

})

output$adj1d.summ.degrees <- eventReactive(input$adjust_1d.a,{

  data <- xlsx_list_1d_adj()
  data_up <- updated_xlsx_list_1d.a()

  if(length(data_up) == 0){
    summary.degrees <- data.frame(Parameter = "Degrees of freedom: ",
                                  Value = (sum(data$observations$diff_level == TRUE)+sum(data$points$FIX_1D == TRUE))-sum(data$points$FIX_1D == FALSE))

    summary.degrees %>%
      kable(caption = "Degrees of freedom: ", digits = 4, align = "c", col.names = NULL) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)




  } else{
    summary.degrees <- data.frame(Parameter = "Degrees of freedom: ",
                                  Value = (sum(data_up$observations$diff_level == TRUE)+sum(data_up$points$FIX_1D == TRUE))-sum(data_up$points$FIX_1D == FALSE))

    summary.degrees %>%
      kable(caption = "Degrees of freedom: ", digits = 4, align = "c", col.names = NULL) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = TRUE)


  }

})


# output$netSpatialView_ell <- renderPlot({
#   # ellipses_1 <- adjusted_net_design()[[1]]$ellipse.net
#   # observations_1 <- adjusted_net_design()[[2]]
#   snet.adj <- adjusted_net_design()
#   adj_output_view <- plot_surveynet(snet.adj = snet.adj, webmap = FALSE, net.1D = FALSE, net.2D = TRUE, ellipse.scale = input$adjust_1_ell_scale)
#   adj_output_view
# })
#
# plotInput_design.xlsx <- function(){
#   snet.adj <- adjusted_net_design()
#   plot_surveynet(snet.adj = snet.adj, webmap = FALSE, net.1D = FALSE, net.2D = TRUE, ellipse.scale = input$adjust_1_ell_scale)
# }
#
# output$netSpatialView_ell11 <- renderPlot({
#   snet.adj <- adjusted_net_design()
#   adj_output_view <- plot_surveynet(snet.adj = snet.adj, webmap = FALSE, net.1D = FALSE, net.2D = TRUE, ellipse.scale = input$adjust_1_ell_scale)
#   adj_output_view
# })

# output$downloadPlot <- downloadHandler(
#   filename = "plot.png",
#   content = function(file) {
#     ggsave(file, plotInput_design.xlsx())
#   })

output$netSpatialView_1d_a <- renderPlotly({
  snet_adj <- adjusted_1d.net_a()
  output_des_plot <- plot_surveynet(snet.adj = snet_adj, webmap = FALSE, net.1D = TRUE, net.2D = FALSE)
  output_des_plot
})

output$netSpatialView_1d_adj <- renderPlotly({
  snet_adj <- adjusted_1d.net_a()
  output_des_plot <- plot_surveynet(snet.adj = snet_adj, webmap = FALSE, net.1D = TRUE, net.2D = FALSE)
  output_des_plot
})


output$`1d_points_a` <- DT::renderDataTable({
  DT::datatable(
    adjusted_1d.net_a()[[1]] %>%
      as.data.frame() %>%
      mutate(
        h0 = round(h0, 2),
        dh = round(dh, 2),
        h = round(h, 2),
        sd_h = round(sd_h, 2)
      ) %>%
      dplyr:: select(Name, FIX_1D, Point_object, h0, h, dh, sd_h),
    escape=F,
    extensions = list('Buttons', 'Scroller'),
    options = list(dom = 'Bfrtip', buttons = I('colvis'),
                   deferRender = TRUE,
                   scrollY = 500,
                   scrollX = 300,
                   scroller = TRUE)
  ) %>%
    formatStyle(
      'sd_h',
      color = styleInterval(c(input$sd_h.a), c('black', 'aqua')),
      backgroundColor = styleInterval(input$sd_h.a, c('lightGray', '#FF6347'))
    )
})

output$`1d_observations_a` <- DT::renderDataTable({
  DT::datatable(
    adjusted_1d.net_a()[[2]] %>%
      as.data.frame() %>%
      mutate(
        f = round(f, 2),
        Kl = round(Kl, 2),
        Kv = round(Kv, 2),
        rii = round(rii, 2)
      ) %>%
      dplyr::select(from, to, type, f, Kl, Kv, rii),
    escape=F,
    extensions = list('Buttons', 'Scroller'),
    options = list(dom = 'Bfrtip', buttons = I('colvis'),
                   deferRender = TRUE,
                   scrollY = 500,
                   scrollX = 300,
                   scroller = TRUE)
  )%>%
    formatStyle(
      'rii',
      color = styleInterval(c(input$rii_1d.a), c('red', 'black')),
      background = styleColorBar(adjusted_1d.net_a()[[2]]$rii, 'steelblue'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )
})



########################################
# REPORT 1D adjustment - xlsx input data
########################################

output$report1Dadjust_xlsx <- downloadHandler(
  filename = "report1D_adjustment.html",
  content = function(file) {
    tempReport <- file.path("D:/R_projects/Surveyer/R/Shiny_app/new_design/Reports/Report_1D_adjust.R")

    # Set up parameters to pass to Rmd document
    model <- input$dh.s.model.a
    data <- xlsx_list_1d_adj()
    data_up <- updated_xlsx_list_1d.a()
    net1d_adj <- adjusted_1d.net_a()
    sd_h_bound <- input$sd_h.a
    rii_bound <- input$rii_1d.a
    sd.estimated <- adjusted_1d.net_a()$test$sd.aposteriori
    df <- adjusted_1d.net_a()$test$df
    sd.apriori <- input$sd_apriori_dh.a


    params <- list(model = model,
                   data = data,
                   data_up = data_up,
                   net1d_adj = net1d_adj,
                   sd_h_bound = sd_h_bound,
                   rii_bound = rii_bound,
                   sd.estimated = sd.estimated,
                   df = df,
                   sd.apriori = sd.apriori)

    rmarkdown::render(tempReport, output_file = file,
                      params = params,
                      envir = new.env(parent = globalenv())
    )
  }
)




})
