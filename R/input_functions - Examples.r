# Project: Surveyer
# Description: Package of Land and Engineering Surveying utilities
# Authors: Milutin Pejovic, Milan Kilibarda, Branislav Bajat, Aleksandar Sekulic and Petar Bursac

source(here("R/input_functions.r"))
source(here("R/functions.r"))

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
library(mapview)
library(mapedit)
library(leaflet.extras)
library(here)

################
# surveynet.xlsx
################

# Parameters:
#    1. points -  Excel file sheet with attributes related to points - geodetic network [Example: Data/Input/xlsx]
#    2. observations - Excel file sheet with attributes related to observations [Example: Data/Input/xlsx]
#    3. dest_crs - destination Coordinate Reference System - set EPSG code [default: 3857 - Web Mercator projection coordinate system]

# Examples
points_xlsx <- readxl::read_xlsx(path = here::here("Data/Input/Without_observations/xlsx/IB.xlsx"), sheet = "Points")
observations_xlsx <- readxl::read_xlsx(path = here::here("Data/Input/Without_observations/xlsx/IB.xlsx"), sheet = "Observations")

xlsx1 <- surveynet.xlsx(points = points_xlsx, observations = observations_xlsx, dest_crs = 3857)

points_xlsx_1 <- readxl::read_xlsx(path = here::here("Data/Input/Without_observations/xlsx/VB.xlsx"), sheet = "Points")
observations_xlsx_1 <- readxl::read_xlsx(path = here::here("Data/Input/Without_observations/xlsx/VB.xlsx"), sheet = "Observations")

xlsx2 <- surveynet.xlsx(points = points_xlsx_1, observations = observations_xlsx_1, dest_crs = 3857)

# Examples
net_spatial_view(points = xlsx1[[1]], observations = xlsx1[[2]])
net_spatial_view(points = xlsx2[[1]], observations = xlsx2[[2]])

# Examples with
xlsx3 <- surveynet.xlsx_1(points = points_xlsx, observations = observations_xlsx, dest_crs = 3857, obs = FALSE)
net_spatial_view(points = xlsx3[[1]], observations = xlsx3[[2]])


xlsx4 <- surveynet.xlsx_1(points = points_xlsx_1, observations = observations_xlsx_1, dest_crs = 3857, obs = FALSE)
net_spatial_view(points = xlsx4[[1]], observations = xlsx4[[2]])

###############
# surveynet.shp
###############

# Parameters:
#    1. points -  sf object with geometry type POINT - geodetic network [Example: Data/Input/shp]
#    2. observations - sf object with geometry type LINESTRING [Example: Data/Input/shp]
#    3. fix_x - list with names of points that define datum - X coordinate
#    3. fix_y - list with names of points that define datum - Y coordinate
#    4. st_dir - "a priori" standard deviation for direction observations ["]
#    5. st_dist - "a priori" standard deviation for distance observations [mm]
#    6. dest_crs - destination Coordinate Reference System - set EPSG code [default: 3857 - Web Mercator projection coordinate system]
#    7. points_object - list with names of points that represnt object of interests

# Examples

points1 <- st_read(dsn='Primer_1_ulazni_podaci_Ikea_Beograd','Tacke_mreza_objekat_1')
observations1 <- st_read(dsn='Primer_1_ulazni_podaci_Ikea_Beograd','Plan_opazanja')

u1 <- surveynet.shp(points = points1, observations = observations1, fix_x = list("A","B"), fix_y = list("A","B"), st_dir = 3, st_dist = 3, dest_crs = NA, points_object = list("T1","T2","T3","T4") )

points2 <- st_read(dsn='Primer_2_ulazni_podaci_marina_Visnjicka_banja/shapefiles','tacke')
observations2 <- st_read(dsn='Primer_2_ulazni_podaci_marina_Visnjicka_banja/shapefiles','plan_opazanja')

observations3 <- readOGR("Primer_2_ulazni_podaci_marina_Visnjicka_banja/shapefiles/plan_opazanja.shp")
observations4 <- st_as_sf(observations3)

points3 <- readOGR("Primer_2_ulazni_podaci_marina_Visnjicka_banja/shapefiles/tacke.shp")
points4 <- st_as_sf(points3)

u2 <- surveynet.shp(points = points4, observations = observations4, fix_x = list("T1"), fix_y = list("T1","T3"), st_dir = 3, st_dist = 3, dest_crs = NA)

###############
# surveynet.kml
###############

# Parameters:
#    1. points -  sf object with geometry type POINT - geodetic network [Example: Data/Input/kml]
#    2. observations - sf object with geometry type LINESTRING [Example: Data/Input/kml]
#    3. fix_x - list with names of points that define datum - X coordinate
#    3. fix_y - list with names of points that define datum - Y coordinate
#    4. st_dir - "a priori" standard deviation for direction observations ["]
#    5. st_dist - "a priori" standard deviation for distance observations [mm]
#    6. dest_crs - destination Coordinate Reference System - set EPSG code [default: 3857 - Web Mercator projection coordinate system]
#    7. points_object - list with names of points that represnt object of interests

# Examples
points_kml1 <- st_read(dsn='Data/Input/Without_observations/kml/Example_1_Ikea_Beograd/Points.kml')
observations_kml1 <- st_read(dsn='Data/Input/Without_observations/kml/Example_1_Ikea_Beograd/Observations.kml')

k1 <- surveynet.kml(points = points_kml1, observations = observations_kml1, fix_x = list("A","B"), fix_y = list("A","B"), st_dir = 3, st_dist = 3, dest_crs = 3857, points_object = list("T1","T2","T3","T4") )

points_kml2 <- st_read(dsn='Primer_2_ulazni_podaci_marina_Visnjicka_banja/KML/Tacke.kml','Tacke')
observations_kml2 <- st_read(dsn='Primer_2_ulazni_podaci_marina_Visnjicka_banja/KML/Plan_opazanja.kml','Plan_opazanja')

k2 <- surveynet.kml(points = points_kml2, observations = observations_kml2, fix_x = list("1"), fix_y = list("1","3"), st_dir = 3, st_dist = 3, dest_crs = NA)


# Examples
net_spatial_view(points = k1[[1]], observations = k1[[2]])
net_spatial_view(points = k2[[1]], observations = k2[[2]])

##################
# net_spatial_view
##################

# Function for spatial data visualisation trough package ggplot2
# Parameters:
#    1. points -  sf object with geometry type POINT and related attributes as product from surveynet.xxx function
#    2. observations - sf object with geometry type LINESTRING and related attributes as product from surveynet.xxx function

# Examples
net_spatial_view(points = u1[[1]], observations = u1[[2]])
net_spatial_view(points = u2[[1]], observations = u2[[2]])

##########################################
# net_spatial_view_web [package:: mapview]
##########################################

# Function for spatial data visualisation at web maps
# Parameters:
#    1. points -  sf object with geometry type POINT and related attributes as product from surveynet.xxx function
#    2. observations - sf object with geometry type LINESTRING and related attributes as product from surveynet.xxx function

# Examples
net_spatial_view_web(points = u1[[1]], observations = u1[[2]])
net_spatial_view_web(points = u2[[1]], observations = u2[[2]])

###########
# check_net
###########

# Function for checking input data for errors - geometrical and topological consistency, redudancy etc.
# Parameters:
#    1. points -  sf object with geometry type POINT and related attributes as product from surveynet.xxx function
#    2. observations - sf object with geometry type LINESTRING and related attributes as product from surveynet.xxx function

check_net(points = u1[[1]], observations = u1[[2]])

###################
# surveynet.mapedit
###################

# surveynet.mapedit_add
# Function for interactive adding points on web maps and storage as sf [Simple Feature]

#surveynet.mapedit_view
# Function for visualisation trough mapview package
# Parameters:
#    1. points -  sf object with geometry type POINT and related attributes as product from function surveynet.mapedit_add

# surveynet.mapedit_points
# Function for preparing points - sf attribute table check
# Parameters:
#    1. points -  sf object with geometry type POINT and related attributes as product from function surveynet.mapedit_add

# surveynet.mapedit_observations
# Function for creating observations from points - all
# Parameters:
#    1. points -  sf object with geometry type POINT and related attributes as product from function surveynet.mapedit_points

# surveynet.mapedit_observations_edit
# Function for creating observations from points - edit and interactivly with CRUD [Create, Read, Update and Delete] functionalites edit observations
# Parameters:
#    1. points -  sf object with geometry type POINT and related attributes as product from function surveynet.mapedit_points
#    2. st_dir - "a priori" standard deviation for direction observations ["]
#    3. st_dist - "a priori" standard deviation for distance observations [mm]
#

# surveynet.mapedit
# create complete sf object - points and observations
# Parameters:
#    1. points -  sf object with geometry type POINT and related attributes as product from function surveynet.mapedit_points
#    2. observations - sf object with geometry type LINESTRING and related attributes as product from function surveynet.mapedit_observations
#    3. fix_x - list with names of points that define datum - X coordinate
#    3. fix_y - list with names of points that define datum - Y coordinate
#    4. st_dir - "a priori" standard deviation for direction observations ["]
#    5. st_dist - "a priori" standard deviation for distance observations [mm]
#    6. dest_crs - destination Coordinate Reference System - set EPSG code [default: 3857 - Web Mercator projection coordinate system]
#    7. points_object - list with names of points that represnt object of interests

# Example
net_points <- surveynet.mapedit_add()

surveynet.mapedit_view(points = net_points)

points <- surveynet.mapedit_points(points = net_points)

observations <- surveynet.mapedit_observations(points = points)

# Working with at Shiny app, beacuse of some additional parameteres and reactive expressions
# me <- surveynet.mapedit(points = points, observations = observations, fix_x = list(), fix_y = list(), dest_crs = NA, points_object = list())
