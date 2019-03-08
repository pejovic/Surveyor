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



# fix.params <- function(net.points){
#   net.points %>% st_drop_geometry() %>% t() %>% as.data.frame(stringsAsFactors = FALSE) %>%
#     rownames_to_column() %>%
#     `colnames<-`(.[1,]) %>%
#     .[-1,] %>%
#     `rownames<-`(NULL) %>%
#     filter(Name %in% c("FIX_X", "FIX_Y")) %>%
#     gather(key = Point, value = fix, -c(Name)) %>%
#     .[["fix"]] != "FALSE"
# }


dd <- design.snet(survey.net =  vb, result.units = "mm")
Qx <- dd$Qx
dd$ellipses

merge(vb[[1]], dd$ellipses)

nn = dim(vb[[1]])[1] # TODO: ovde treba isto uzeti used.points samo. Odnosno biti siguran koliko ima tacaka za koje se racuna elipsa
fix = vb[[1]] %>% st_drop_geometry() %>% dplyr::select(FIX_X, FIX_Y) == FALSE
fix <- fix*1

Qxy(Qx, n = nn, fixd = fix*1)

lapply(dd$Qxy.list, function(x) error.ellipse(x))


error.ellipse(dd$Qxy.list[[1]], prob = 0.95)


Amat(survey.net = vb, units = "mm")


st_coordinates(ib[[1]]) %>% sf::st_as_sf(coords = c("x","y"))

rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
ee <- nngeo::st_ellipse(dd$net.points[1,], ey = dd$net.points[1,]$A, ex = dd$net.points[1,]$B) #%>% st_rota
ncg = st_geometry(ee)
plot(ncg, border = 'grey')
cntrd = st_centroid(ncg)
ncg2 = (ncg - cntrd) * rot(27*pi/180) + cntrd
plot(ncg2, add = TRUE)
plot(cntrd, col = 'red', add = TRUE, cex = .5)

ee <- nngeo::st_ellipse(ib[[1]][1,], ey = 1.919176, ex = 1.808260) #%>% st_rota
plot(ee)


