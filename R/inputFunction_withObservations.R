# Project: Surveyer
# Description: Package of Land and Engineering Surveying utilities
# Authors: Milutin Pejovic, Milan Kilibarda, Branislav Bajat, Aleksandar Sekulic and Petar Bursac
#
rm(list = ls())

# Packages
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

# Radni direktorijum
wDir <- 'D:/R_projects/Surveyer_podaci/R_Surveyor/Radni_6_ulaz_sa_merenjima/'
setwd(wDir)
getwd()




surveynet2DAdjustment_Import.xlsx <- function(points = points, observations = observations, fix_x = list(), fix_y = list(), st_dir, st_dist, dest_crs = NA, points_object = list()){

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
  observations$Slope..raw. <- as.character(observations$Slope..raw.)

  j <- 1
  for(i in observations$Slope..raw.){
    if(i == "NA"){
      observations$Slope..raw.[j] <- NA
    }
    j <- j+1

  }

  observations$Slope..raw. <- as.numeric(observations$Slope..raw.)

  # Replacing NA values for distances with 0
  j <- 1
  for(i in observations$Slope..raw.){

    if(is.na(i)){
      observations$Slope..raw.[j] <- 0
    }
    j <- j+1

  }
  # Conversion from factor to character
  observations$H..Circle <- as.character(observations$H..Circle)
  observations$V..Circle <- as.character(observations$V..Circle)
  observations$H..Angle <- as.character(observations$H..Angle)
  observations$V..Angle <- as.character(observations$V..Angle)
  observations$Semi.major.Azimuth <- as.character(observations$Semi.major.Azimuth)

  # Separate angle observations to degrees, minutes and seconds
  observations <- observations %>% separate(H..Circle, c("HC_deg", "A"), "Â°")
  observations <- observations %>% separate(A, c("HC_min", "B"), "'")
  observations <- observations %>% separate(B, c("HC_sec", 'C'), '"')

  observations <- observations %>% separate(V..Circle, c("VC_deg", "A1"), "Â°")
  observations <- observations %>% separate(A1, c("VC_min", "B1"), "'")
  observations <- observations %>% separate(B1, c("VC_sec", 'C1'), '"')

  observations <- observations %>% separate(H..Angle, c("HA_deg", "A2"), "Â°")
  observations <- observations %>% separate(A2, c("HA_min", "B2"), "'")
  observations <- observations %>% separate(B2, c("HA_sec", 'C2'), '"')

  observations <- observations %>% separate(V..Angle, c("VA_deg1", "VA_deg"), "Z")
  observations <- observations %>% separate(VA_deg, c("VA_deg", "A3"), "Â°")
  observations <- observations %>% separate(A3, c("VA_min", "B3"), "'")
  observations <- observations %>% separate(B3, c("VA_sec", 'C3'), '"')

  observations <- observations %>% separate(Semi.major.Azimuth, c("AZ_deg", "A4"), "Â°")
  observations <- observations %>% separate(A4, c("AZ_min", "B4"), "'")
  observations <- observations %>% separate(B4, c("AZ_sec", 'C4'), '"')

  observations <- observations[, !(colnames(observations) %in% c("C","C1","C2","C3","C4","VA_deg1"))]

  # Transforming from character to numeric, without lossing informations
  observations$HC_deg <- as.numeric(observations$HC_deg)
  observations$HC_min <- as.numeric(observations$HC_min)
  observations$HC_sec <- as.numeric(observations$HC_sec)

  observations$VC_deg <- as.numeric(observations$VC_deg)
  observations$VC_min <- as.numeric(observations$VC_min)
  observations$VC_sec <- as.numeric(observations$VC_sec)

  observations$HA_deg <- as.numeric(observations$HA_deg)
  observations$HA_min <- as.numeric(observations$HA_min)
  observations$HA_sec <- as.numeric(observations$HA_sec)

  observations$VA_deg <- as.numeric(observations$VA_deg)
  observations$VA_min <- as.numeric(observations$VA_min)
  observations$VA_sec <- as.numeric(observations$VA_sec)

  observations$AZ_deg <- as.numeric(observations$AZ_deg)
  observations$AZ_min <- as.numeric(observations$AZ_min)
  observations$AZ_sec <- as.numeric(observations$AZ_sec)

  # Creating decimal columns
  observations$HC_dec <- observations$HC_deg + observations$HC_min/60 + observations$HC_sec/3600
  observations$VC_dec <- observations$VC_deg + observations$VC_min/60 + observations$VC_sec/3600
  observations$HA_dec <- observations$HA_deg + observations$HA_min/60 + observations$HA_sec/3600
  observations$VA_dec <- observations$VA_deg + observations$VA_min/60 + observations$VA_sec/3600
  observations$AZ_dec <- observations$AZ_deg + observations$AZ_min/60 + observations$AZ_sec/3600

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
  dt_1 %<>% mutate(from = observations$from,
                   to = observations$to,
                   distance = observations$distance,
                   direction = observations$direction,
                   standard_dir = observations$standard_dir,
                   standard_dist = observations$standard_dist,
                   HC_deg = observations$HC_deg,
                   HC_min = observations$HC_min,
                   HC_sec = observations$HC_sec,
                   HC_dec = observations$HC_dec,
                   VC_deg = observations$VC_deg,
                   VC_min = observations$VC_min,
                   VC_sec = observations$VC_sec,
                   VC_dec = observations$VC_dec,
                   Dist = observations$Slope..raw,
                   HA_deg = observations$HA_deg,
                   HA_min = observations$HA_min,
                   HA_sec = observations$HA_sec,
                   HA_dec = observations$HA_dec,
                   VA_deg = observations$VA_deg,
                   VA_min = observations$VA_min,
                   VA_sec = observations$VA_sec,
                   VA_dec = observations$VA_dec,
                   AZ_deg = observations$AZ_deg,
                   AZ_min = observations$AZ_min,
                   AZ_sec = observations$AZ_sec,
                   AZ_dec = observations$AZ_dec,
                   Face = observations$Face,
                   Instrument_Height = observations$True.Instrument.Height,
                   Target_Height = observations$True.Target.Height,
                   Prism_constant = observations$Prism.Constant,
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

net_spatial_view_2DAdjustment_Import <- function(points, observations){

  points$fill_p <- "red"
  points$fill_p[points$Point_object == TRUE] <- "DeepSkyBlue"

  # Example to add little different type of observations
  # observations$distance[1:3] <- FALSE
  # observations$direction[5:7] <- FALSE
  # observations$fill_o <- "LightGoldenRodYellow"

  observations$fill_o <- ifelse(observations$distance == TRUE & observations$direction == FALSE,"LightGoldenRodYellow", ifelse(observations$distance == FALSE & observations$direction == TRUE, "Khaki","orange"))

  net_view <- ggplot(data=observations) +
    geom_sf(size=1,stroke=1, color = observations$fill_o)+
    geom_sf(data=points, shape = 24, fill = points$fill_p, size=2.5, stroke=2) +
    geom_sf_text(data=points, aes(label=Name,hjust = 1.5, vjust =1.5))+
    xlab("\nLongitude [deg]") +
    ylab("Latitude [deg]\n") +
    ggtitle("Observational plan and points [geodetic network and object points]")+
    guides(col = guide_legend())+
    theme_bw()
  return(net_view)

}

# Examples
points_xlsx <- read.xlsx(file = "Merenja_Toranj_Avala/Avala_geodetic_network_observations.xlsx", sheetName = "Points")
observations_xlsx <- read.xlsx(file = "Merenja_Toranj_Avala/Avala_geodetic_network_observations.xlsx", sheetName = "Observations")

xlsx_Avala <- surveynet.xlsx(points = points_xlsx, observations = observations_xlsx, fix_x = list(), fix_y = list(), st_dir = 3, st_dist = 3, dest_crs = 3857, points_object = list())

net_spatial_view(points = xlsx_Avala[[1]], observations = xlsx_Avala[[2]])


points_xlsx
observations_xlsx

rastojanja <- data.frame( Name = points_xlsx$Name, x = points_xlsx$x , y = points_xlsx$y, DS1 = NA, DS2 = NA, DS3 = NA, DS4 = NA, DS5 = NA, DS6 = NA, DS7 = NA)

rastojanja$DS1 <- sqrt((rastojanja$x - rastojanja$x[[1]])^2 + (rastojanja$y - rastojanja$y[[1]])^2)
rastojanja$DS2 <- sqrt((rastojanja$x - rastojanja$x[[2]])^2 + (rastojanja$y - rastojanja$y[[2]])^2)
rastojanja$DS3 <- sqrt((rastojanja$x - rastojanja$x[[3]])^2 + (rastojanja$y - rastojanja$y[[3]])^2)
rastojanja$DS4 <- sqrt((rastojanja$x - rastojanja$x[[4]])^2 + (rastojanja$y - rastojanja$y[[4]])^2)
rastojanja$DS5 <- sqrt((rastojanja$x - rastojanja$x[[5]])^2 + (rastojanja$y - rastojanja$y[[5]])^2)
rastojanja$DS6 <- sqrt((rastojanja$x - rastojanja$x[[6]])^2 + (rastojanja$y - rastojanja$y[[6]])^2)
rastojanja$DS7 <- sqrt((rastojanja$x - rastojanja$x[[7]])^2 + (rastojanja$y - rastojanja$y[[7]])^2)

#################################
points_xlsx.1 <- read.xlsx(file = "Merenja_Toranj_Avala/Avala_geodetic_network_observations - good coord.xlsx", sheetName = "Points")
observations_xlsx.1 <- read.xlsx(file = "Merenja_Toranj_Avala/Avala_geodetic_network_observations.xlsx", sheetName = "Observations", as.data.frame=TRUE)

xlsx_Avala.1 <- surveynet2DAdjustment_Import.xlsx(points = points_xlsx.1, observations = observations_xlsx.1, fix_x = list(), fix_y = list(), st_dir = 3, st_dist = 3, dest_crs = 3857, points_object = list())

net_spatial_view_2DAdjustment_Import(points = xlsx_Avala.1[[1]], observations = xlsx_Avala.1[[2]])


observations_xlsx.1$Slope..raw. <- as.character(observations_xlsx.1$Slope..raw.)

j <- 1
for(i in observations_xlsx.1$Slope..raw.){
  if(i == "NA"){
    observations_xlsx.1$Slope..raw.[j] <- NA
  }
  j <- j+1

}

observations_xlsx.1$Slope..raw. <- as.numeric(observations_xlsx.1$Slope..raw.)

# Replacing NA values for distances with 0
j <- 1
for(i in observations_xlsx.1$Slope..raw.){

  if(is.na(i)){
    observations_xlsx.1$Slope..raw.[j] <- 0
  }
  j <- j+1

}

















