source(here("R/input_functions.R"))
source(here("R/inputFunction_withObservations.R"))

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
library(mapview)
library(mapedit)
library(leaflet.extras)
library(rhandsontable)
library(readxl)
library(here)

shinyServer(function(input, output){

  shp_points <- reactive({
    req(input$fileSHP_points)
    shpdf <- input$fileSHP_points
    tempdirname <- dirname(shpdf$datapath[1])

    for(i in 1:nrow(shpdf)){
      file.rename(shpdf$datapath[i], paste0(tempdirname, "/", shpdf$name[i]))
    }

    map_shp_points <- readOGR(paste(tempdirname, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep = "/"))
    map_shp_points <- st_as_sf(map_shp_points)
    map_shp_points
  })

  shp_observations <- reactive({
    req(input$fileSHP_observations)
    shpdf_1 <- input$fileSHP_observations
    tempdirname_1 <- dirname(shpdf_1$datapath[1])

    for(i in 1:nrow(shpdf_1)){
      file.rename(shpdf_1$datapath[i], paste0(tempdirname_1, "/", shpdf_1$name[i]))
    }

    map_shp_observations <- readOGR(paste(tempdirname_1, shpdf_1$name[grep(pattern = "*.shp$", shpdf_1$name)], sep = "/"))
    map_shp_observations <- st_as_sf(map_shp_observations)
    map_shp_observations

  })

  observeEvent(input$go, {

    p <- shp_points()
    o <- shp_observations()
    st_dir_shp = as.numeric(input$st_dir_shp)
    st_dist_shp = as.numeric(input$st_dist_shp)
    dest_crs_shp = as.numeric(input$epsg_shp)
    fix_x_shp <- as.list(strsplit(as.character(input$fix_x_shp), ",")[[1]])
    fix_y_shp <- as.list(strsplit(as.character(input$fix_y_shp), ",")[[1]])
    points_obj_shp <- as.list(strsplit(as.character(input$points_obj_shp), ",")[[1]])

    output_shp <- surveynet.shp(points = p, observations = o, fix_x = fix_x_shp, fix_y = fix_y_shp, st_dir = st_dir_shp, st_dist = st_dist_shp, dest_crs = dest_crs_shp, points_object = points_obj_shp )
    out_points <- output_shp[[1]]
    out_observations <- output_shp[[2]]

    output_view_shp <- net_spatial_view(points = out_points, observations = out_observations)

    output$points_shp_3 <- renderPrint({
      out_points
    })

    output$observations_shp_3 <- renderPrint({
      out_observations
    })

    output$netSpatialView_shp <- renderPlot({
      output_view_shp
    })

    web_map_shp <- net_spatial_view_web(points = out_points, observations = out_observations)

    output$mymap <- renderLeaflet({
      web_map_shp@map
    })
  })

  kml_points <- reactive({
    req(input$fileKML_points)
    map_kml_points <- readOGR(input$fileKML_points$datapath)
    map_kml_points <- st_as_sf(map_kml_points)
    map_kml_points
  })

  kml_observations <- reactive({
    req(input$fileKML_observations)
    map_kml_observations <- readOGR(input$fileKML_observations$datapath)
    map_kml_observations <- st_as_sf(map_kml_observations)
    map_kml_observations
  })

  observeEvent(input$go1, {
    p_kml <- kml_points()
    o_kml <- kml_observations()
    st_dir_kml = as.numeric(input$st_dir_kml)
    st_dist_kml = as.numeric(input$st_dist_kml)
    dest_crs_kml = as.numeric(input$epsg_kml)

    fix_x_kml <- as.list(strsplit(as.character(input$fix_x_kml), ",")[[1]])
    fix_y_kml <- as.list(strsplit(as.character(input$fix_y_kml), ",")[[1]])
    points_obj_kml <- as.list(strsplit(as.character(input$points_obj_kml), ",")[[1]])

    output_kml <- surveynet.kml(points = p_kml, observations = o_kml, fix_x = fix_x_kml, fix_y = fix_y_kml, st_dir = st_dir_kml, st_dist = st_dist_kml, dest_crs = dest_crs_kml, points_object = points_obj_kml)

    out_points_kml <- output_kml[[1]]
    out_observations_kml <- output_kml[[2]]

    output_view_kml <- net_spatial_view(points = out_points_kml, observations = out_observations_kml)

    output$points_kml_3 <-  renderPrint({
      out_points_kml
    })

    output$observations_kml_3 <- renderPrint({
      out_observations_kml
    })

    output$netSpatialView_kml <- renderPlot({
      output_view_kml
    })

    web_map_kml <- net_spatial_view_web(points = out_points_kml, observations = out_observations_kml)

    output$mymap1 <- renderLeaflet({
      web_map_kml@map
    })
  })


  xlsx_points <- reactive({
    req(input$fileXLSX)
    map_xlsx_points <- read.xlsx(input$fileXLSX$datapath, sheetName = "Points")
    map_xlsx_points
  })

  xlsx_observations <- reactive({
    req(input$fileXLSX)
    map_xlsx_observations <- read.xlsx(input$fileXLSX$datapath, sheetName = "Observations")
    map_xlsx_observations
  })

  observeEvent(input$go2, {
    p_xlsx <- xlsx_points()
    o_xlsx <- xlsx_observations()
    dest_crs_xlsx = as.numeric(input$epsg_xlsx)

    output_xlsx <- surveynet.xlsx(points = p_xlsx, observations = o_xlsx, dest_crs = dest_crs_xlsx)

    out_points_xlsx <- output_xlsx[[1]]
    out_observations_xlsx <- output_xlsx[[2]]

    output_view_xlsx <- net_spatial_view(points = out_points_xlsx, observations = out_observations_xlsx)

    output$points_xlsx_3 <- renderPrint({
      out_points_xlsx
    })

    output$observations_xlsx_3 <- renderPrint({
      out_observations_xlsx
    })

    output$netSpatialView_xlsx <- renderPlot({
      output_view_xlsx
    })

    web_map_xlsx <- net_spatial_view_web(points = out_points_xlsx, observations = out_observations_xlsx)

    output$mymap2 <- renderLeaflet({
      web_map_xlsx@map
    })


  })

  # mapedit

  ns <- shiny::NS("map_me")

  lf <- leaflet() %>%
    addTiles() %>%
    addProviderTiles("OpenStreetMap.Mapnik",group="OpenStreetMap") %>%
    addProviderTiles("Esri.WorldImagery",group="Esri.WorldImagery") %>%
    addProviderTiles("Esri.DeLorme",group="Esri.DeLorme") %>%
    addProviderTiles("Esri.WorldTopoMap",group="Esri.WorldTopoMap") %>%
    setView(20.4580456, 44.8195306, zoom=12) %>%

    addLayersControl(baseGroups = c("OpenStreetMap","Esri.WorldImagery","Esri.DeLorme","Esri.WorldTopoMap"))

  editmapx <- callModule(editMod, "map_me", lf )

  observeEvent(input$go_me_draw, {
    points_raw_me <- editmapx()$finished
    points_me <- surveynet.mapedit_points(points = points_raw_me)
    observations_me <- surveynet.mapedit_observations(points = points_me)
    output$primer <- renderPrint({
      points_me
    })
  })

  po_me <- reactive({
    points_raw_me <- editmapx()$finished
    points_me <- surveynet.mapedit_points(points = points_raw_me)
    points_me
  })

  ob_me <- reactive({
    points_me <- po_me()
    observations_me <- surveynet.mapedit_observations(points = points_me)
    observations_me
  })

  ob_example <- eventReactive(input$go_me_edit_o, {
    p_me <- po_me()
    o_me <- ob_me()
    obs_example <- surveynet.mapedit_observations_edit(points = p_me, st_dir = input$st_dir_me, st_dist = input$st_dist_me)
    obs_example
  })

  output$primer1 <- DT::renderDataTable(
    ob_example(),
    extensions = 'Buttons',
    options = list(dom = 'Bfrtip', buttons = I('colvis'))

  )

  del_r <- eventReactive(input$delete_b,{
    observations <- ob_example()
    d <- input$primer1_rows_selected
    observations <- observations[-d, ]
    observations %<>% select(id = id ,
                             from = from,
                             to = to,
                             standard_dir = standard_dir,
                             standard_dist = standard_dist
    )
    observations
  })

  values <- reactiveValues()

  output$OldObs <- renderRHandsontable({
    rhandsontable(as.data.frame(del_r()), width = 550, height = 550)
  })

  del_row <- eventReactive(input$run_table,{
    values$data <-  hot_to_r(input$OldObs)
    a <- as.data.frame(values$data)
    a
  })


  # helper function for making checkbox
  shinyInput = function(FUN, len, id, ...) {
    inputs = character(len)
    for (i in seq_len(len)) {
      inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
    }
    inputs
  }

  # datatable with checkbox
  output$primer4 <- DT::renderDataTable({
    data.frame(del_row(),
               distance=shinyInput(checkboxInput,nrow(del_row()),"cbox_"),
               direction=shinyInput(checkboxInput,nrow(del_row()),"cbox_1"))
  },
  selection = 'none',
  escape = FALSE,
  extensions = 'Scroller',

  options = list(
    deferRender = TRUE,
    scrollY = 500,
    scrollX = 300,
    scroller = TRUE,
    preDrawCallback = JS('function() {
                         Shiny.unbindAll(this.api().table().node()); }'),
    drawCallback = JS('function() {
                      Shiny.bindAll(this.api().table().node()); } ')
    )
    )

  # helper function for reading checkbox
  shinyValue = function(id, len) {
    unlist(lapply(seq_len(len), function(i) {
      value = input[[paste0(id, i)]]
      if (is.null(value)) NA else value
    }))
  }

  observations_edited <- reactive({o_e <- data.frame(id = del_row()$id,
                                                     from = del_row()$from,
                                                     to = del_row()$to,
                                                     distance = shinyValue("cbox_",nrow(del_row())),
                                                     direction = shinyValue("cbox_1",nrow(del_row())),
                                                     standard_dir = del_row()$standard_dir,
                                                     standard_dist = del_row()$standard_dist)
  })

  observeEvent(input$go_me, {
    p_me <- po_me()
    o_me <- observations_edited()

    dest_crs_me = as.numeric(input$epsg_me)

    fix_x_me <- as.list(strsplit(as.character(input$fix_x_me), ",")[[1]])
    fix_y_me <- as.list(strsplit(as.character(input$fix_y_me), ",")[[1]])
    points_obj_me <- as.list(strsplit(as.character(input$points_obj_me), ",")[[1]])

    output_me <- surveynet.mapedit(points = p_me, observations = o_me, fix_x = fix_x_me, fix_y = fix_y_me, dest_crs = dest_crs_me, points_object = points_obj_me)

    out_points_me <- output_me[[1]]
    out_observations_me <- output_me[[2]]

    output_view_me <- net_spatial_view(points = out_points_me, observations = out_observations_me)

    output$points_me_3 <- renderPrint({
      out_points_me
    })

    output$observations_me_3 <- renderPrint({
      out_observations_me
    })

    output$netSpatialView_me <- renderPlot({
      output_view_me
    },
    width = 600,
    height = 600)

    web_map_me <- net_spatial_view_web(points = out_points_me, observations = out_observations_me)

    output$map_me_out <- renderLeaflet({
      web_map_me@map
    })

  })

  ##############################
  # Input data with observations
  ##############################

  xlsx_points_wO <- reactive({
    req(input$fileXLSX_wO)
    map_xlsx_points_wO <- readxl::read_xlsx(path = input$fileXLSX_wO$datapath, sheet = "Points")
    map_xlsx_points_wO
  })

  xlsx_observations_wO <- reactive({
    req(input$fileXLSX_wO)
    map_xlsx_observations_wO <- readxl::read_xlsx(path = input$fileXLSX_wO$datapath, sheet = "Observations")
    map_xlsx_observations_wO
  })

  surveynet.wO <- eventReactive(input$calc_obs, {
    p_xlsx_wO <- xlsx_points_wO()
    o_xlsx_wO <- xlsx_observations_wO()
    dest_crs_xlsx_wO = as.numeric(input$epsg_xlsx_wO)
    output_xlsx_wO <- surveynet2DAdjustment_Import.xlsx(points = p_xlsx_wO, observations = o_xlsx_wO, dest_crs = dest_crs_xlsx_wO)
    output_xlsx_wO
  })

  output$points_wO <- DT::renderDataTable({
    out_points_xlsx_wO <- surveynet.wO()[[1]]
    out_points_xlsx_wO %<>%
      st_drop_geometry() %>%
      as.data.frame()},
    extensions = 'Buttons',
    options = list(dom = 'Bfrtip', buttons = I('colvis'))
  )

  values_wO <- reactiveValues()

  output$OldObs_wO <- renderRHandsontable(rhandsontable({
    out_observations_xlsx_wO <- surveynet.wO()[[2]]
    out_observations_xlsx_wO %<>%
      st_drop_geometry() %>%
      as.data.frame()
    out_observations_xlsx_wO %<>% mutate( use = TRUE)
  },
  width = 800,
  height = 800
  ))

  edited_wO <- eventReactive(input$edit_wO,{
    values_wO$data <-  hot_to_r(input$OldObs_wO)
    wO <- as.data.frame(values_wO$data)
    wO <- subset(wO, use == TRUE)
    wO
  })

  output$observations_wO <- DT::renderDataTable(
    edited_wO(),
    extensions = list('Buttons', 'Scroller'),
    options = list(dom = 'Bfrtip', buttons = I('colvis'),
                   deferRender = TRUE,
                   scrollY = 500,
                   scrollX = 300,
                   scroller = TRUE)
  )

  output$netSpatialView_xlsx_wO <- renderPlot({
    out_points_xlsx_wO <- surveynet.wO()[[1]]
    out_observations_xlsx_wO <- surveynet.wO()[[2]]
    edited_observations_xlsx_wO <- edited_wO()
    edited_observations_xlsx_wO$geometry <- out_observations_xlsx_wO$geometry[match(edited_observations_xlsx_wO$id, out_observations_xlsx_wO$id )]
    edited_observations_xlsx_wO <- st_as_sf(edited_observations_xlsx_wO)
    output_view_xlsx_wO <- net_spatial_view_2DAdjustment_Import(points = out_points_xlsx_wO, observations = edited_observations_xlsx_wO)
    output_view_xlsx_wO
  }, width = 650, height = 600)

    })
