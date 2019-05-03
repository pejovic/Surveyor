source(here("R/input_functions.r"))
source(here("R/inputFunction_withObservations.R"))
source(here("R/functions.r"))

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


shinyServer(function(input, output){
  # ==================================================================
  # 2D NET DESIGN
  # ==================================================================

  # XLSX INPUT DATA
  xlsx_points <- reactive({
    req(input$fileXLSX)
    map_xlsx_points <- readxl::read_xlsx(path = input$fileXLSX$datapath, sheet = "Points", col_types = c("text", "numeric", "numeric", "logical", "logical", "logical"))
    map_xlsx_points
  })

  xlsx_observations <- reactive({
    req(input$fileXLSX)
    map_xlsx_observations <- readxl::read_xlsx(path = input$fileXLSX$datapath, sheet = "Observations", col_types = c("numeric", "text", "text", "logical", "logical", "numeric", "numeric"))
    map_xlsx_observations
  })

  xlsx_list <- reactive({#(input$go2, {
    p_xlsx <- xlsx_points()
    o_xlsx <- xlsx_observations()
    dest_crs_xlsx = as.numeric(input$epsg_xlsx)
    output_xlsx <- surveynet.xlsx(points = p_xlsx, observations = o_xlsx, dest_crs = dest_crs_xlsx)
    output_xlsx
  })

  values_p <- reactiveValues()
  values_o <- reactiveValues()

  output$p_des_xlsx <- renderRHandsontable({
    rhandsontable(as.data.frame(xlsx_list()[[1]] %>% st_drop_geometry()), width = 650, height = 650)
  })

  output$o_des_xlsx <- renderRHandsontable({
    values_p$data <- hot_to_r(input$p_des_xlsx)
    p_df <- as.data.frame(values_p$data)
    rhandsontable(as.data.frame(xlsx_list()[[2]] %>% st_drop_geometry()), width = 650, height = 650)
  })

  updated_xlsx_list <- reactive({#(input$update_design_2d_xlsx,{
    values_p$data <- hot_to_r(input$p_des_xlsx)
    p_df <- as.data.frame(values_p$data)
    values_o$data <- hot_to_r(input$o_des_xlsx)
    o_df <- as.data.frame(values_o$data)
    dest_crs_xlsx = as.numeric(input$epsg_xlsx)
    p_xlsx <- xlsx_points()
    output_xlsx <- surveynet.xlsx_updated(points = p_df, observations = o_df, dest_crs = dest_crs_xlsx, raw_points = p_xlsx)
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

  output$netSpatialView_xlsx_updated <- renderPlot({
    out_points_xlsx <- updated_xlsx_list()[[1]]
    out_observations_xlsx <- updated_xlsx_list()[[2]]
    output_view_xlsx <- net_spatial_view(points = out_points_xlsx, observations = out_observations_xlsx)
    output_view_xlsx
  })

  output$web_map_xlsx_updated <- renderLeaflet({
    out_points_xlsx <- updated_xlsx_list()[[1]]
    out_observations_xlsx <- updated_xlsx_list()[[2]]
    web_map_xlsx <- net_spatial_view_web(points = out_points_xlsx, observations = out_observations_xlsx)
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

  me_points_print <- eventReactive(input$go_me_draw, {
    points_raw_me <- editmapx()$finished
    points_me <- surveynet.mapedit_points(points = points_raw_me)
    points_me
  })

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
                                  mutate(
                                    FIX_X = FALSE,
                                    FIX_Y = FALSE,
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
   out_points_me <- mapEdit_list()[[1]]
   out_observations_me <- mapEdit_list()[[2]]
   output_view_me <- net_spatial_view(points = out_points_me, observations = out_observations_me)
   output_view_me
 },
 width = 600,
 height = 600)

 output$map_me_out <- renderLeaflet({
   out_points_me <- mapEdit_list()[[1]]
   out_observations_me <- mapEdit_list()[[2]]
   web_map_me <- net_spatial_view_web(points = out_points_me, observations = out_observations_me)
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

  xlsx_points_wO <- reactive({
    req(input$fileXLSX_adj)
    map_xlsx_points_wO <- readxl::read_xlsx(path = input$fileXLSX_adj$datapath, sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
    map_xlsx_points_wO
  })

  xlsx_observations_wO <- reactive({
    req(input$fileXLSX_adj)
    map_xlsx_observations_wO <- readxl::read_xlsx(path = input$fileXLSX_adj$datapath, sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
    map_xlsx_observations_wO
  })

  surveynet.wO <- reactive({#(input$preprocess_2d_adj, {
    p_xlsx_wO <- xlsx_points_wO()
    o_xlsx_wO <- xlsx_observations_wO()
    dest_crs_xlsx_wO = as.numeric(input$epsg_xlsx_adj)
    output_xlsx_wO <- surveynet2DAdjustment_Import_fun.xlsx(points = p_xlsx_wO, observations = o_xlsx_wO, dest_crs = dest_crs_xlsx_wO)
    output_xlsx_wO
  })

  values_p_2d_adj <- reactiveValues()
  values_m_2d_adj <- reactiveValues()

  output$p_adj_xlsx <- renderRHandsontable({
    rhandsontable({
      surveynet.wO()[[1]] %>%
      st_drop_geometry() %>%
      as.data.frame()
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
    edited_observations_xlsx_wO$geometry <- out_observations_xlsx_wO$geometry[match(edited_observations_xlsx_wO$id, out_observations_xlsx_wO$id )]
    edited_observations_xlsx_wO <- st_as_sf(edited_observations_xlsx_wO)
    output_view_xlsx_wO <- net_spatial_view_2DAdjustment_Import(points = out_points_xlsx_wO, observations = edited_observations_xlsx_wO)
    output_view_xlsx_wO
  }, width = 600, height = 600
  )

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
      design_net_out <- design.snet(survey.net = data, result.units = result_units, ellipse.scale = ellipse_scale, all = FALSE)
      design_net_out
    } else{
      design_net_out <- design.snet(survey.net = data_up, result.units = result_units, ellipse.scale = ellipse_scale, all = FALSE)
      design_net_out
    }
  })

  output$ellipse_error <- DT::renderDataTable({
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
    DT::datatable(adjusted_net_design()[[1]] %>%
                    st_drop_geometry() %>%
                    as.data.frame() %>%
                    mutate(
                      A = round(A, 4),
                      B = round(B, 4),
                      teta = round(teta, 4),
                      sx = round(sx, 4),
                      sy = round(sy, 4),
                      sp = round(sp, 4)
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
    ellipses_1 <- adjusted_net_design()[[1]]
    observations_1 <- adjusted_net_design()[[3]]
    adj_output_view <- adj_net_spatial_view(ellipses_1, observations_1)
    adj_output_view
  })

  plotInput <- function(){
    adj_net_spatial_view(adjusted_net_design()[[1]], adjusted_net_design()[[3]])
  }

  output$netSpatialView_ell11 <- renderPlot({
    ellipses_1 <- adjusted_net_design()[[1]]
    observations_1 <- adjusted_net_design()[[3]]
    adj_output_view <- adj_net_spatial_view(ellipses_1, observations_1)
    adj_output_view
  })

  output$downloadPlot <- downloadHandler(
    filename = "plot.png",
    content = function(file) {
      ggsave(file, plotInput())
    })

  output$net_points_adj <- DT::renderDataTable({
    DT::datatable(
        adjusted_net_design()[[2]] %>%
        st_drop_geometry() %>%
        as.data.frame() %>%
        mutate(
          A = round(A, 4),
          B = round(B, 4),
          teta = round(teta, 4),
          sx = round(sx, 4),
          sy = round(sy, 4),
          sp = round(sp, 4)
        ) %>%
          dplyr:: select(Name, FIX_X, FIX_Y, Point_object, sx, sy, sp),
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
      adjusted_net_design()[[3]] %>%
        st_drop_geometry() %>%
        as.data.frame() %>%
        mutate(
          Ql = round(Ql, 4),
          Qv = round(Qv, 4),
          rii = round(rii, 4)
        ) %>%
        dplyr::select(from, to, type, Ql, Qv, rii),
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
        background = styleColorBar(adjusted_net_design()[[3]]$rii, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })

  output$map_ellipses_opt <- renderLeaflet({
    ellipses <- adjusted_net_design()$ellipse.net
    observations <- adjusted_net_design()$observations
    points <- updated_xlsx_list()[[1]]
    adj.net_map <- adj.net_spatial_view_web(ellipses = ellipses, observations = observations, points = points, sp_bound = input$sp_xlsx, rii_bound = input$rii_xlsx)
    adj.net_map@map
  })

 # 2D net design - map_edit input data

  adjusted_net_design_me <- eventReactive(input$design_adjust_map,{
    data <- mapEdit_list()
    result_units <- input$adjust_1_units_me
    ellipse_scale <- input$adjust_1_ell_scale_me
    design_net_out <- design.snet(survey.net = data, result.units = result_units, ellipse.scale = ellipse_scale, all = FALSE)
    design_net_out
  })

  output$ellipse_error_me <- DT::renderDataTable({
    DT::datatable(
        adjusted_net_design_me()[[1]] %>%
        st_drop_geometry() %>%
        as.data.frame() %>%
        mutate(
          A = round(A, 4),
          B = round(B, 4),
          teta = round(teta, 4),
          sx = round(sx, 4),
          sy = round(sy, 4),
          sp = round(sp, 4)
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
    ellipses_1 <- adjusted_net_design_me()[[1]]
    observations_1 <- adjusted_net_design_me()[[3]]
    adj_output_view <- adj_net_spatial_view(ellipses_1, observations_1)
    adj_output_view
  })

  plotInput_me <- function(){
    adj_net_spatial_view(adjusted_net_design_me()[[1]],adjusted_net_design_me()[[3]])
  }

  output$netSpatialView_ell_me11 <- renderPlot({
    ellipses_1 <- adjusted_net_design_me()[[1]]
    observations_1 <- adjusted_net_design_me()[[3]]
    adj_output_view <- adj_net_spatial_view(ellipses_1, observations_1)
    adj_output_view
  })

  output$downloadPlot1 <- downloadHandler(
    filename = "plot.png",
    content = function(file) {
      ggsave(file, plotInput_me())
    })

  output$net_points_adj_me <- DT::renderDataTable({
    DT::datatable(
        adjusted_net_design_me()[[2]] %>%
        st_drop_geometry() %>%
        as.data.frame() %>%
        mutate(
          A = round(A, 4),
          B = round(B, 4),
          teta = round(teta, 4),
          sx = round(sx, 4),
          sy = round(sy, 4),
          sp = round(sp, 4)
        ) %>%
          dplyr:: select(Name, FIX_X, FIX_Y, Point_object, sx, sy, sp),
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
        adjusted_net_design_me()[[3]] %>%
        st_drop_geometry() %>%
        as.data.frame() %>%
        mutate(
          Ql = round(Ql, 4),
          Qv = round(Qv, 4),
          rii = round(rii, 4)
        ) %>%
          dplyr::select(from, to, type, Ql, Qv, rii),
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
        background = styleColorBar(adjusted_net_design_me()[[3]]$rii, 'steelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })

  output$map_ellipses_opt_me <- renderLeaflet({
    ellipses <- adjusted_net_design_me()$ellipse.net
    observations <- adjusted_net_design_me()$observations
    points <- mapEdit_list()[[1]]
    adj.net_map <- adj.net_spatial_view_web(ellipses = ellipses, observations = observations, points = points, sp_bound = input$sp_map, rii_bound = input$rii_map)
    adj.net_map@map
  })


})



