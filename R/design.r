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

source(here("R/input_functions.r"))
source(here("R/functions.r"))


#================ Milutin =============================================================================

# Examples
vb_points <- readxl::read_xlsx(path = here::here("Data/Input/Without_observations/xlsx/VB.xlsx"), sheet = "Points", col_types = c("text", "numeric", "numeric", "logical", "logical", "logical"))
vb_obs <- readxl::read_xlsx(path = here::here("Data/Input/Without_observations/xlsx/VB.xlsx"), sheet = "Observations", col_types = c("numeric", "text", "text", "logical", "logical", "numeric", "numeric"))

vb <- surveynet.xlsx(points = vb_points, observations = vb_obs, dest_crs = 3857)


Amat <- function(survey.net){
  A_dir <- survey.net[[2]] %>% filter(direction) %>% st_coordinates() %>% as.data.frame() %>% mutate_at(vars(L1), funs(factor)) %>%
    split(., .$L1) %>%
    lapply(., function(x) coef_p(pt1 = x[1, 1:2], pt2 = x[2, 1:2], pts = st_coordinates(survey.net[[1]][, 1:2]))) %>%
    do.call(rbind, .)

  A_dist <- survey.net[[2]] %>% filter(distance) %>% st_coordinates() %>% as.data.frame() %>% mutate_at(vars(L1), funs(factor)) %>%
    split(., .$L1) %>%
    lapply(., function(x) coef_d(pt1 = x[1, 1:2], pt2 = x[2, 1:2], pts = st_coordinates(survey.net[[1]][, 1:2]))) %>%
    do.call(rbind, .)

  Z_mat <- survey.net[[2]] %>% filter(direction) %>%
    spread(key = from, value = direction, fill = FALSE) %>%
    select(survey.net[[1]]$Name) %>%
    st_drop_geometry() %>%
    as.matrix()*1

  rest_mat <- matrix(0, nrow = dim(A_dist)[1], ncol = dim(Z_mat)[2])

  A <- rbind(cbind(A_dir, Z_mat), cbind(A_dist, rest_mat))

  colnames(A) <- c(paste(rep(survey.net[[1]]$Name, each = 2), rep(c("x", "y"), length(survey.net[[1]]$Name)), sep = "_"), paste(colnames(Z_mat), "z", sep = "_"))
  return(A)
}

Amat(survey.net = vb)


vb[[1]] %>% st_drop_geometry() %>% select(FIX_X, FIX_Y) %>% as.matrix()*1 %>% c()
