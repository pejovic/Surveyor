# Project: Surveyer
# Description: Package of Land and Engineering Surveying utilities
# Authors: Milutin Pejovic, Milan Kilibarda, Branislav Bajat, Aleksandar Sekulic and Petar Bursac

source(here("R/inputFunction_withObservations.r"))

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
library(readxl)
library(here)

# IMPORTANT:
# If you get next:
# Warning messages:
#   1: Expected 3 pieces. Missing pieces filled with `NA` in 128 rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

# Must
# In file inputFunction_withObservations.r
# Part
# Separate direction - angle observations in separate columns
# [Degress, minutes, seconds]
# values ([?']) must be replaced with ([°'])
# ?problem with file encoding



# surveynet2DAdjustment_Import.xlsx
# Parameters:
#    1. points -  Excel file sheet with attributes related to points - geodetic network [Example: Data/Input/With_observations]
#    2. observations - Excel file sheet with attributes related to observations [Example: Data/Input/With_observations]
#    3. dest_crs - destination Coordinate Reference System - set EPSG code [default: 3857 - Web Mercator projection coordinate system]


# net_spatial_view_2DAdjustment_Import
# Function for spatial data visualisation trough package ggplot2
# Parameters:
#    1. points -  sf object with geometry type POINT and related attributes as product from surveynet2DAdjustment_Import.___ function
#    2. observations - sf object with geometry type LINESTRING and related attributes as product from ssurveynet2DAdjustment_Import.___ function

# Examples

rastojanja <- data.frame( Name = points_xlsx$Name, x = points_xlsx$x , y = points_xlsx$y, DS1 = NA, DS2 = NA, DS3 = NA, DS4 = NA, DS5 = NA, DS6 = NA, DS7 = NA)

rastojanja$DS1 <- sqrt((rastojanja$x - rastojanja$x[[1]])^2 + (rastojanja$y - rastojanja$y[[1]])^2)
rastojanja$DS2 <- sqrt((rastojanja$x - rastojanja$x[[2]])^2 + (rastojanja$y - rastojanja$y[[2]])^2)
rastojanja$DS3 <- sqrt((rastojanja$x - rastojanja$x[[3]])^2 + (rastojanja$y - rastojanja$y[[3]])^2)
rastojanja$DS4 <- sqrt((rastojanja$x - rastojanja$x[[4]])^2 + (rastojanja$y - rastojanja$y[[4]])^2)
rastojanja$DS5 <- sqrt((rastojanja$x - rastojanja$x[[5]])^2 + (rastojanja$y - rastojanja$y[[5]])^2)
rastojanja$DS6 <- sqrt((rastojanja$x - rastojanja$x[[6]])^2 + (rastojanja$y - rastojanja$y[[6]])^2)
rastojanja$DS7 <- sqrt((rastojanja$x - rastojanja$x[[7]])^2 + (rastojanja$y - rastojanja$y[[7]])^2)

#################################
#points_xlsx.1 <- read.xlsx(file = "Merenja_Toranj_Avala/Avala_geodetic_network_observations - good coord.xlsx", sheetName = "Points")
#observations_xlsx.1 <- read.xlsx(file = "Merenja_Toranj_Avala/Avala_geodetic_network_observations - good coord.xlsx", sheetName = "Observations", as.data.frame=TRUE)

points_xlsx.1 <- readxl::read_xlsx(path = here::here("Data/Input/With_observations/Geodetic_observations_Tower_Avala/Avala_geodetic_network_observations - good coord.xlsx"), sheet = "Points")
observations_xlsx.2 <- readxl::read_xlsx(path = here::here("Data/Input/With_observations/Geodetic_observations_Tower_Avala/Avala_geodetic_network_observations - good coord.xlsx"), sheet = "Observations")

xlsx_Avala.1 <- surveynet2DAdjustment_Import.xlsx(points = points_xlsx.1, observations = observations_xlsx.2, dest_crs = 3857)

net_spatial_view_2DAdjustment_Import(points = xlsx_Avala.1[[1]], observations = xlsx_Avala.1[[2]])


# ===========================================================================================================
# NEW DESIGN

avala_points <- readxl::read_xlsx(path = here::here("Data/Input/With_observations/Avala/Avala_observations.xlsx"), sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
avala_obs <- readxl::read_xlsx(path = here::here("Data/Input/With_observations/Avala/Avala_observations.xlsx"), sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
avala <- surveynet2DAdjustment_Import_fun.xlsx(points = avala_points, observations = avala_obs, dest_crs = 3857)

net_spatial_view_2DAdjustment_Import(points = avala[[1]], observations = avala[[2]])

avala_1 <- surveynet.xlsx_1(points = avala_points, observations = avala_obs, dest_crs = 3857, obs = TRUE)
net_spatial_view_2DAdjustment_Import(points = avala_1[[1]], observations = avala[[2]])


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# UPDATED
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
avala_points <- readxl::read_xlsx(path = here::here("Data/Input/With_observations/Avala/Avala_observations.xlsx"), sheet = "Points")
avala_obs <- readxl::read_xlsx(path = here::here("Data/Input/With_observations/Avala/Avala_observations.xlsx"), sheet = "Observations")

net_2d_AVALA <- import_surveynet2D(points = avala_points, observations = avala_obs, dest_crs = 3857)
net_spatial_view_2DAdjustment_Import(points = net_2d_AVALA[[1]], observations = net_2d_AVALA[[2]])



