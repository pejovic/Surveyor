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
# TODO: Proveriti da li dobro radi za razlicite jedinice. Mislim da ne, posto W ne uzima u obzir jedinice. Takodje, treba dodati opciju
vb.results <- design.snet(survey.net =  vb, result.units = "mm", ellipse.scale = 10, all = TRUE)

vb.results$design.matrices$Qx
vb.results$observations
plot(vb.results$ellipse.net$geometry)
plot(vb.results$observations$geometry, add = TRUE)

# ib
ib.results <- design.snet(survey.net =  ib, result.units = "mm", ellipse.scale = 1, all = FALSE)

plot(ib.results$ellipse.net$geometry)
plot(ib.results$observations$geometry, add = TRUE)
mapview(vb.results$ellipse.net, zcol = 'sp') + mapview(vb.results$observations, zcol = 'rii')

adj.net_spatial_view <- function(ellipses = ellipses, observations = observations){

  #points$fill_p <- "red"
  #points$fill_p[points$Point_object == TRUE] <- "DeepSkyBlue"
  #observations$fill_o <- ifelse(observations$distance == TRUE & observations$direction == FALSE,"LightGoldenRodYellow", ifelse(observations$distance == FALSE & observations$direction == TRUE, "Khaki","orange"))

  adj.net_view <- ggplot(data=observations) +
    #geom_sf(size=1,stroke=1, color = observations$rii)+
    geom_sf(data = observations)+
    geom_sf(data=ellipses,aes(fill = sp))+ #fill = sf.colors(length(ellipses$sp)),breaks = "sp") +
    geom_sf_text(data=ellipses, aes(label=Name,hjust = 2.5, vjust =2.5))+
    xlab("\nLongitude [deg]") +
    ylab("Latitude [deg]\n") +
    ggtitle("Adjusted observational plan - net quality")+
    guides(col = guide_legend())+
    theme_bw()
  return(adj.net_view)
}

adj.net_spatial_view(vb.results$ellipse.net,vb.results$observations)
