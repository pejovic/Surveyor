source("D:/R_projects/Shiny_app/Test_6/inputFunction_shp.R")
source("D:/R_projects/Shiny_app/Test_6/function_netSpatialView.R")
source("D:/R_projects/Shiny_app/Test_6/inputFunction_kml.R")
source("D:/R_projects/Shiny_app/Test_6/inputFunction_xlsx.R")
source("D:/R_projects/Shiny_app/Test_6/function_netSpatialViewWeb.R")
#source("D:/R_projects/Shiny_app/Test_6/inputFunction_mapEdit.R")

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

    output_xlsx <- surveynet.xlsx(points = p_xlsx, observations = o_xlsx, fix_x = list(), fix_y = list(), st_dir = 3, st_dist = 3, dest_crs = dest_crs_xlsx, points_object = list())

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






  ### MapEdit
  #observeEvent(input$go_me_draw, {
  #  output$map_me <- surveynet.mapedit_add()
  #
  #  })
  #
  #points_me <- surveynet.mapedit_points(points = net_points)
  #
  #observations_me <- surveynet.mapedit_observations(points = points_me)
  #observeEvent(input$go_me, {
  #  p_me <- points_me
  #  o_me <- observations_me
  #  dest_crs_xlsx = as.numeric(input$epsg_me)
  #  st_dir_me = as.numeric(input$st_dir_me)
  #  st_dist_me = as.numeric(input$st_dist_me)
  #  dest_crs_me = as.numeric(input$epsg_me)
  #
  #  fix_x_me <- as.list(strsplit(as.character(input$fix_x_me), ",")[[1]])
  #  fix_y_me <- as.list(strsplit(as.character(input$fix_y_me), ",")[[1]])
  #  points_obj_me <- as.list(strsplit(as.character(input$points_obj_me), ",")[[1]])
  #
  #
  #  output_me <- surveynet.mapedit(points = p_me, observations = o_me, fix_x = fix_x_me, fix_y = fix_y_me, st_dir = st_dir_me, st_dist = st_dist_me, dest_crs = dest_crs_me, points_object = points_obj_me)
  #
  #  out_points_me <- output_me[[1]]
  #  out_observations_me <- output_me[[2]]
  #
  #  output_view_me <- net_spatial_view(points = out_points_me, observations = out_observations_me)
  #
  #  output$points_me_3 <- renderPrint({
  #    out_points_me
  #  })
  #
  #  output$observations_me_3 <- renderPrint({
  #    out_observations_me
  #  })
  #
  #  output$netSpatialView_me <- renderPlot({
  #    output_view_me
  #  })
  #
  #  web_map_me <- net_spatial_view_web(points = out_points_me, observations = out_observations_me)
  #
  #  output$mymap2 <- renderLeaflet({
  #    web_map_me@map
  #  })
  #
  #
  #
  #})







})





