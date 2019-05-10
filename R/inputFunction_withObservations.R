# Project: Surveyer
# Description: Package of Land and Engineering Surveying utilities
# Authors: Milutin Pejovic, Milan Kilibarda, Branislav Bajat, Aleksandar Sekulic and Petar Bursac


# Packages
#library(tidyverse)
#library(magrittr)
#library(ggplot2)
#library(geomnet)
#library(ggnetwork)
#library(sf)
#library(ggmap)
#library(sp)
#library(rgdal)
#library(leaflet)
#library(xlsx)
#library(data.table)
#library(readxl)

# Parameters:
#    1. points -  Excel file sheet with attributes related to points - geodetic network [Example: Data/Input/With_observations]
#    2. observations - Excel file sheet with attributes related to observations [Example: Data/Input/With_observations]
#    3. dest_crs - destination Coordinate Reference System - set EPSG code [default: 3857 - Web Mercator projection coordinate system]

surveynet2DAdjustment_Import.xlsx <- function(points = points, observations = observations, dest_crs = NA){

  # If column "Description" is necessary, delete it;
  j=1
  for(i in names(points)){
    if (i == "NA."){
      points <- subset(points, select = -c(j))
    }
    j=j+1
  }

  j_1=1
  for(i in names(observations)){
    if (i == "NA."){
      observations <- subset(observations, select = -c(j_1))
    }
    j_1 = j_1 + 1
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

  observations$from <- as.character(observations$from)
  vec <- c(1:99999)
  j = 1
  for(i in observations$from){
    if(i %in% vec){
      observations$from[j] <- paste("T",i, sep = "")
      j = j +1

    }
  }

  observations$to <- as.character(observations$to)
  vec <- c(1:99999)
  j = 1
  for(i in observations$to){
    if(i %in% vec){
      observations$to[j] <- paste("T",i, sep = "")
      j = j +1

    }
  }

  # Conversion values of distances - Slope. raw [from factor to numeric without lossing infromation]
  observations$`Slope (raw)` <- as.character(observations$`Slope (raw)`)

  j <- 1
  for(i in observations$`Slope (raw)`){
    if(i == "NA"){
      observations$`Slope (raw)`[j] <- NA
    }
    j <- j+1

  }

  observations$`Slope (raw)` <- as.numeric(observations$`Slope (raw)`)

  # Replacing NA values for distances with 0
  j <- 1
  for(i in observations$`Slope (raw)`){

    if(is.na(i)){
      observations$`Slope (raw)`[j] <- 0
    }
    j <- j+1

  }

  # Separate direction - angle observations in separate columns
  # [Degress, minutes, seconds]
  observations %<>%
    mutate(HzC = str_replace_all(`H. Circle`, "([°'])", "_")) %>%
    mutate(HzC = str_replace_all(`HzC`, '(["])', "")) %>%
    separate(HzC, "_", into = c("HzD", "HzM", "HzS")) %>%
    mutate(Vc = str_replace_all(`V. Circle`, "([°'])", "_")) %>%
    mutate(Vc = str_replace_all(`Vc`, '(["])', "")) %>%
    separate(Vc, "_", into = c("VcD", "VcM", "VcS")) %>%
    mutate(Ha = str_replace_all(`H. Angle`, "([°'])", "_")) %>%
    mutate(Ha = str_replace_all(`Ha`, '(["])', "")) %>%
    separate(Ha, "_", into = c("HaD", "HaM", "HaS")) %>%
    separate(`V. Angle`,"Z", into = c("z", "V_Angle")) %>%
    mutate(Va = str_replace_all(`V_Angle`, "([°'])", "_")) %>%
    mutate(Va = str_replace_all(`Va`, '(["])', "")) %>%
    separate(Va, "_", into = c("VaD", "VaM", "VaS")) %>%
    mutate(Az = str_replace_all(`Semi-major Azimuth`, "([°'])", "_")) %>%
    mutate(Az = str_replace_all(`Az`, '(["])', "")) %>%
    separate(Az, "_", into = c("AzD", "AzM", "AzS"))

  # Transforming from character to numeric, without lossing informations
  observations$HzD <- as.numeric(observations$HzD)
  observations$HzM <- as.numeric(observations$HzM)
  observations$HzS <- as.numeric(observations$HzS)

  observations$VcD <- as.numeric(observations$VcD)
  observations$VcM <- as.numeric(observations$VcM)
  observations$VcS <- as.numeric(observations$VcS)

  observations$HaD <- as.numeric(observations$HaD)
  observations$HaM <- as.numeric(observations$HaM)
  observations$HaS <- as.numeric(observations$HaS)

  observations$VaD <- as.numeric(observations$VaD)
  observations$VaM <- as.numeric(observations$VaM)
  observations$VaS <- as.numeric(observations$VaS)

  observations$AzD <- as.numeric(observations$AzD)
  observations$AzM <- as.numeric(observations$AzM)
  observations$AzS <- as.numeric(observations$AzS)

  # Creating decimal columns
  observations$HC_dec <- observations$HzD + observations$HzM/60 + observations$HzS/3600
  observations$VC_dec <- observations$VcD + observations$VcM/60 + observations$VcS/3600
  observations$HA_dec <- observations$HaD + observations$HaM/60 + observations$HaS/3600
  observations$VA_dec <- observations$VaD + observations$VaM/60 + observations$VaS/3600
  observations$AZ_dec <- observations$AzD + observations$AzM/60 + observations$AzS/3600

  # Create geometry columns for points
  if (is.na(dest_crs)){
    dest_crs <- 3857
  }

  observations$x_station <- points$x[match(observations$from, points$Name)]
  observations$y_station <- points$y[match(observations$from, points$Name)]
  observations$x_obs.point <- points$x[match(observations$to, points$Name)]
  observations$y_obs.point <- points$y[match(observations$to, points$Name)]

  points <- points %>% as.data.frame %>% sf::st_as_sf(coords = c("x","y")) %>% sf::st_set_crs(dest_crs)

  dt <- as.data.table(observations)
  dt_1 <- dt[
    , {
      geometry <- sf::st_linestring(x = matrix(c(x_station, x_obs.point, y_station, y_obs.point), ncol = 2))
      geometry <- sf::st_sfc(geometry)
      geometry <- sf::st_sf(geometry = geometry)
    }
    , by = id
    ]
  dt_1 <- sf::st_as_sf(dt_1)
  dt_1 %<>% mutate(id = observations$id,
                   from = observations$from,
                   to = observations$to,
                   distance = observations$distance,
                   direction = observations$direction,
                   standard_dir = observations$standard_dir,
                   standard_dist = observations$standard_dist,
                   HC_deg = observations$HzD,
                   HC_min = observations$HzM,
                   HC_sec = observations$HzS,
                   HC_dec = observations$HC_dec,
                   VC_deg = observations$VcD,
                   VC_min = observations$VcM,
                   VC_sec = observations$VcS,
                   VC_dec = observations$VC_dec,
                   Dist = observations$`Slope (raw)`,
                   HA_deg = observations$HaD,
                   HA_min = observations$HaM,
                   HA_sec = observations$HaS,
                   HA_dec = observations$HA_dec,
                   VA_deg = observations$VaD,
                   VA_min = observations$VaM,
                   VA_sec = observations$VaS,
                   VA_dec = observations$VA_dec,
                   AZ_deg = observations$AzD,
                   AZ_min = observations$AzM,
                   AZ_sec = observations$AzS,
                   AZ_dec = observations$AZ_dec,
                   Face = observations$Face,
                   Instrument_Height = observations$`True Instrument Height`,
                   Target_Height = observations$`True Target Height`,
                   Prism_constant = observations$`Prism Constant`,
                   Backsight = observations$Backsight
  )

  dt_1 <- dt_1 %>% sf::st_set_crs(dest_crs)
  observations <- dt_1

  # Creating list
  survey_net <- list(points,observations)

  return(survey_net)

}

##################
# net_spatial_view
##################

# Function for spatial data visualisation trough package ggplot2

# Function for spatial data visualisation trough package ggplot2
# Parameters:
#    1. points -  sf object with geometry type POINT and related attributes as product from surveynet2DAdjustment_Import.___ function
#    2. observations - sf object with geometry type LINESTRING and related attributes as product from ssurveynet2DAdjustment_Import.___ function

net_spatial_view_2DAdjustment_Import <- function(points = points, observations = observations){

  points$fill_p <- "red"
  points$fill_p[points$Point_object == TRUE] <- "DeepSkyBlue"

  #if(any(is.na(st_coordinates(observations$geometry)[,1]) == FALSE) == TRUE){
  #  net_view <- ggplot(data=points) +
  #    geom_sf(data=points, shape = 24, fill = points$fill_p, size=2.5, stroke=2) +
  #    geom_sf_text(data=points, aes(label=Name,hjust = 1.5, vjust =1.5))+
  #    xlab("\nLongitude [deg]") +
  #    ylab("Latitude [deg]\n") +
  #    ggtitle("Points [geodetic network and object points]")+
  #    guides(col = guide_legend())+
  #    theme_bw()
  #} else{
    net_view <- ggplot(data=observations) +
      geom_sf(size=1,stroke=1)+
      geom_sf(data=points, shape = 24, fill = points$fill_p, size=2.5, stroke=2) +
      geom_sf_text(data=points, aes(label=Name,hjust = 1.5, vjust =1.5))+
      xlab("\nLongitude [deg]") +
      ylab("Latitude [deg]\n") +
      ggtitle("Observational plan and points [geodetic network and object points]")+
      guides(col = guide_legend())+
      theme_bw()
  #}
  return(net_view)
}


# Parameters:
#    1. points -  Excel file sheet with attributes related to points - geodetic network [Example: Data/Input/With_observations]
#    2. observations - Excel file sheet with attributes related to observations [Example: Data/Input/With_observations]
#    3. dest_crs - destination Coordinate Reference System - set EPSG code [default: 3857 - Web Mercator projection coordinate system]

surveynet2DAdjustment_Import_fun.xlsx <- function(points = points, observations = observations, dest_crs = NA){

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

  vec <- c(1:99999)
  j = 1
  for(i in observations$from){
    if(i %in% vec){
      observations$from[j] <- paste("T",i, sep = "")
      j = j +1
    }
  }

  vec <- c(1:99999)
  j = 1
  for(i in observations$to){
    if(i %in% vec){
      observations$to[j] <- paste("T",i, sep = "")
      j = j +1
    }
  }


  # Create geometry columns for points
  if (is.na(dest_crs)){
    dest_crs <- 3857
  }

  observations$x_station <- points$x[match(observations$from, points$Name)]
  observations$y_station <- points$y[match(observations$from, points$Name)]
  observations$x_obs.point <- points$x[match(observations$to, points$Name)]
  observations$y_obs.point <- points$y[match(observations$to, points$Name)]

  points <- points %>% as.data.frame() %>% sf::st_as_sf(coords = c("x","y")) %>% sf::st_set_crs(dest_crs)

  dt <- as.data.table(observations)
  dt$id <- c(1:length(dt$from))
  dt_1 <- dt[
    , {
      geometry <- sf::st_linestring(x = matrix(c(x_station, x_obs.point, y_station, y_obs.point), ncol = 2))
      geometry <- sf::st_sfc(geometry)
      geometry <- sf::st_sf(geometry = geometry)
    }
    , by = id
    ]
  dt_1 <- sf::st_as_sf(dt_1)
  dt_1 %<>% mutate(From = observations$from,
                   To = observations$to,
                   HzD = observations$HzD,
                   HzM = observations$HzM,
                   HzS = round(observations$HzS, 2),
                   SD = round(observations$SD, 3),
                   VzD = observations$VzD,
                   VzM = observations$VzM,
                   VzS = round(observations$VzS, 2)
  )

  dt_1 <- dt_1 %>% sf::st_set_crs(dest_crs)
  observations <- dt_1

  # Creating list
  survey_net <- list(points,observations)

  return(survey_net)
}










