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



fix.params <- function(survey.net){
  survey.net[[1]] %>% st_drop_geometry() %>% t() %>% as.data.frame(stringsAsFactors = FALSE) %>%
    rownames_to_column() %>%
    `colnames<-`(.[1,]) %>%
    .[-1,] %>%
    `rownames<-`(NULL) %>%
    filter(Name %in% c("FIX_X", "FIX_Y")) %>%
    gather(key = Point, value = fix, -c(Name)) %>%
    .[["fix"]] != "FALSE"
}


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
    select(survey.net[[1]]$Name[!survey.net[[1]]$Point_object]) %>%
    st_drop_geometry() %>%
    as.matrix()*1

  fix <- fix.params(survey.net)

  rest_mat <- matrix(0, nrow = dim(A_dist)[1], ncol = dim(Z_mat)[2])

  A <- cbind(rbind(A_dir, A_dist)[, !fix], rbind(Z_mat, rest_mat))

  colnames(A) <- c(paste(rep(survey.net[[1]]$Name, each = 2), rep(c("x", "y"), length(survey.net[[1]]$Name)), sep = "_")[!fix], paste(colnames(Z_mat), "z", sep = "_"))
  return(A)
}

Amat(survey.net = vb)


ib_points <- readxl::read_xlsx(path = here::here("Data/Input/Without_observations/xlsx/IB.xlsx"), sheet = "Points", col_types = c("text", "numeric", "numeric", "logical", "logical", "logical"))
ib_obs <- readxl::read_xlsx(path = here::here("Data/Input/Without_observations/xlsx/IB.xlsx"), sheet = "Observations", col_types = c("numeric", "text", "text", "logical", "logical", "numeric", "numeric"))
ib <- surveynet.xlsx(points = ib_points, observations = ib_obs, dest_crs = 3857)

Amat(survey.net = ib)

# Weights matrix
Wmat <- function(survey.net, apriori = 1){
  #TODO: Omoguciti zadavanje i drugih kovariacionih formi izmedju merenja.
  obs.data <- survey.net[[2]] %>% st_drop_geometry() %>%
    gather(key = type, value = standard, -c(id, from, to, distance, direction)) %>%
    select(from, to, standard)
  return(diag(apriori^2/obs.data$standard^2))
}


design.snet <- function(survey.net){
  A <- Amat(survey.net)
  W <- Wmat(survey.net)
  N <- crossprod(A, W) %*% A
  Qx <- tryCatch(
    {
      x = Qx = solve(N)
    },
    error = function(e) {
      x = Qx = matlib::Ginv(N)
    })
  colnames(Qx) <- colnames(N)
  rownames(Qx) <- rownames(N)
  Kl <- A %*% tcrossprod(Qx, A)
  Qv <- solve(W) - Kl

  design <- list(A = A, W = W, Qx = Qx, Kl = Kl, Qv = Qv)

  return(design)
}

design.snet(survey.net =  ib)


