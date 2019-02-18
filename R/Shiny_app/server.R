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




shinyServer(function(input, output){
  
  map_shp_points <- reactive({
    req(input$fileSHP_points)
    # shpdf is a data.frame with the name, size, type and datapath of the uploaded files
    shpdf <- input$fileSHP_points
    
    # The files are uploaded with names 0.dbf, 1.prj, 2.shp, 3.xml, 4.shx
    # (path/names are in column datapath)
    # We need to rename the files with the actual names: fe_2007_39_county.dbf, etc.
    # (these are in column name)
    
    # Name of the temporary directory where files are uploaded
    tempdirname <- dirname(shpdf$datapath[1])
    
    # Rename files
    for(i in 1:nrow(shpdf)){
      file.rename(shpdf$datapath[i], paste0(tempdirname, "/", shpdf$name[i]))
    }
    
    # Now we read the shapefile with readOGR() of rgdal package
    # passing the name of the file with .shp extension.
    
    # We use the function grep() to search the pattern "*.shp$"
    # within each element of the character vector shpdf$name.
    # grep(pattern="*.shp$", shpdf$name)
    # ($ at the end denote files that finish with .shp, not only that contain .shp)
    map_shp_points <- st_read(paste(tempdirname, shpdf$name[grep(pattern = "*.shp$", shpdf$name)], sep = "/"))
    
  })
  
  
  #st_read(dsn='Primer_1_ulazni_podaci_Ikea_Beograd','Tacke_mreza_objekat_1')
  
  
  map_shp_observations <- reactive({
    
    req(input$fileSHP_observations)
    
    shpdf_1 <- input$fileSHP_observations
    
    tempdirname_1 <- dirname(shpdf_1$datapath[1])
    
    for(i in 1:nrow(shpdf_1)){
      file.rename(shpdf_1$datapath[i], paste0(tempdirname_1, "/", shpdf_1$name[i]))
    }
    
    map_shp_observations <- st_read(paste(tempdirname_1, shpdf_1$name[grep(pattern = "*.shp$", shpdf_1$name)], sep = "/"))
    
  })
  
  ###############
  # surveynet.shp
  ###############
  
  surveynet.shp <- function(points, observations, fix_x = list(), fix_y = list(), st_dir, st_dist, dest_crs = NA, points_object = list()){
    for(i in names(points)){
      if(i == "Naziv"){
        points <- points %>% rename("Name" = "Naziv")
      }
    }
    for(i in names(observations)){
      if(i == "Name"){
        observations <- observations %>% rename("id" = "Name")
      }
    }
    
    # TODO Check funkcija ide ovde
    
    # Check function for point names, that can not be just numbers -> must contain letter
    points$Name <- as.character(points$Name)
    vec <- c(1:99999)
    j = 1
    for(i in points$Name){
      if(i %in% vec){
        points$Name[j] <- paste("T",i, sep = "")
        j = j +1
        
      }
    }
    
    # Transformation to the destination CRS
    if (is.na(dest_crs)){
      dest_crs <- 3857
    }
    if (st_crs(points)$epsg == "4326"){
      points <- points %>% st_transform(dest_crs)
    }
    if (st_crs(observations)$epsg == "4326"){
      observations <- observations %>% st_transform(dest_crs)
    }
    
    if(is.na(observations$id[[1]])){
      observations$id <- (1:length(observations$geometry))
    }
    
    # Defining datum for points
    points %<>% mutate(FIX_X = FALSE, FIX_Y = FALSE)
    
    points$FIX_X[points$Name %in% fix_x] <- TRUE
    points$FIX_Y[points$Name %in% fix_y] <- TRUE
    
    points %<>% mutate(Point_object = FALSE)
    points$Point_object[points$Name %in% points_object] <- TRUE
    
    # Observational plan - adding new columns
    observations %<>% mutate(from = NA,
                             to = NA,
                             distance = TRUE,
                             direction = TRUE,
                             standard_dir = st_dir,
                             standard_dist = st_dist
    )
    
    # Observational plan - defining and ading names for first and last points for observations
    # Creating data frame from sf class object observations, with goal to extract names for first and last points
    observations_1 <- data.frame(station = NA, obs.point = NA, x_station = NA,y_station = NA, x_obs.point = NA, y_obs.point = NA)
    
    # Line string to multipoint
    ob_plan_first_point <- st_line_sample(observations,sample = 0)
    ob_plan_last_point <- st_line_sample(observations,sample = 1)
    coord_1 <- as.data.frame(st_coordinates(ob_plan_first_point))
    coord_2 <- as.data.frame(st_coordinates(ob_plan_last_point))
    
    # Multipoint to point
    pnts_1 = st_cast(ob_plan_first_point, "POINT")
    pnts_2 = st_cast(ob_plan_last_point, "POINT")
    # X- East Y - North
    observations_1 <- data.frame(station = NA, obs.point = NA, x_station = st_coordinates(pnts_1)[,1], y_station = st_coordinates(pnts_1)[,2], x_obs.point = st_coordinates(pnts_2)[,1], y_obs.point = st_coordinates(pnts_2)[,2])
    
    # Adding columns Names for stations and observation point with values from constraint exactly match coordinates
    # TODO srediti da radi i za y
    
    observations_1$station <- points$Name[match(observations_1$x_station, st_coordinates(points)[,1])]
    observations_1$obs.point <- points$Name[match(observations_1$x_obs.point, st_coordinates(points)[,1])]
    observations_1$id <- coord_1$L1
    
    observations$from <- observations_1$station[match(observations$id, observations_1$id)]
    observations$to <- observations_1$obs.point[match(observations$id, observations_1$id)]
    
    # Creating list
    survey_net <- list(points,observations)
    
    return(survey_net)
  }
  
  
  
  
  myReactivePoints_shp <- reactive({
    u1 <- surveynet.shp(points = input$fileSHP_points, observations = input$fileSHP_observations, fix_x = list("A","B"), fix_y = list("A","B"), st_dir = 3, st_dist = 3, dest_crs = NA, points_object = list("T1","T2","T3","T4") )
    u1[[1]]
  })
  
  
  
  output$points_shp <- renderText({
    points_shp <- myReactivePoints_shp();
    
  })
  
  # 
  
  myReactiveObservations_shp <- reactive({
    u1 <- surveynet.shp(points = input$fileSHP_points, observations = input$fileSHP_observations, fix_x = list("A","B"), fix_y = list("A","B"), st_dir = 3, st_dist = 3, dest_crs = NA, points_object = list("T1","T2","T3","T4") )
    u1[[2]]
  })
  
  
  
  output$observations_shp <- renderText({
    observations_shp <- myReactiveObservations_shp();
  })
  

  
  
  
  # Leaflet mapa na kojoj treba da budu prikazane deltaH
  output$mymap <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(providers$MtbMap) %>%
      addMiniMap(
        tiles = providers$MtbMap,
        toggleDisplay = TRUE)%>%
      setView(lng =20.7541367, 
              lat =44.1871144, 
              zoom = 7)
  })
  


  
  
  
  
  
  
})































































