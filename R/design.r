# Project: Surveyer
# Description: Package of Land and Engineering Surveying utilities
# Creating design matrix
# Authors: Milutin Pejovic, Milan Kilibarda, Branislav Bajat, Aleksandar Sekulic and Petar Bursac

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
library(matlib)
library(nngeo)

source(here("R/input_functions.r"))
source(here("R/functions.r"))


#================ Milutin =============================================================================

# Examples
vb_points <- readxl::read_xlsx(path = here::here("Data/Input/Without_observations/xlsx/VB.xlsx"), sheet = "Points", col_types = c("text", "numeric", "numeric", "logical", "logical", "logical"))
vb_obs <- readxl::read_xlsx(path = here::here("Data/Input/Without_observations/xlsx/VB.xlsx"), sheet = "Observations", col_types = c("numeric", "text", "text", "logical", "logical", "numeric", "numeric"))
vb <- surveynet.xlsx(points = vb_points, observations = vb_obs, dest_crs = 3857)


ib_points <- readxl::read_xlsx(path = here::here("Data/Input/Without_observations/xlsx/IB.xlsx"), sheet = "Points", col_types = c("text", "numeric", "numeric", "logical", "logical", "logical"))
ib_obs <- readxl::read_xlsx(path = here::here("Data/Input/Without_observations/xlsx/IB.xlsx"), sheet = "Observations", col_types = c("numeric", "text", "text", "logical", "logical", "numeric", "numeric"))
ib <- surveynet.xlsx(points = ib_points, observations = ib_obs, dest_crs = 3857)

# vb
vb.results <- design.snet(survey.net =  vb, result.units = "mm", ellipse.scale = 10, all = FALSE)

plot(vb.results$ellipse.net$geometry)
plot(vb.results$observations$geometry, add = TRUE)

# ib
ib.results <- design.snet(survey.net =  ib, result.units = "mm", ellipse.scale = 1, all = FALSE)

plot(ib.results$ellipse.net$geometry)
plot(ib.results$observations$geometry, add = TRUE)
