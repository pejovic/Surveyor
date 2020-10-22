#' ---
#' title: "Pollutant inventory spatialization"
#' author:
#'    - "Milan Kilibarda"
#'    - "Dragutin Protic"
#' date: "`r format(Sys.time(), '%d %B %Y')`"
#' output:
#'    html_document:
#'      keep_md: true
#'      theme: "simplex"
#'      highlight: tango
#'      toc: true
#'      toc_depth: 5
#'      toc_float: true
#'      fig_caption: yes
#' ---
#'
#+ include = FALSE
# <img src="Logo-Trans.png" align="center" alt="logo" width="2000" height = "3000" style = "border: none; float: right;">
#' This document provides the maps regarding the spatialization of pollutation inventory.
#'
#'
#+ include = TRUE, echo = FALSE, results = 'hide', warning = FALSE, message = FALSE
library(tidyverse)
library(sf)
library(readxl)
library(ggpubr)
library(ggfortify)
library(here)
library(knitr)
library(kableExtra)
library(DT)
library(mapview)
library(rgdal)
library(SerbianCyrLat)
library(stringr)
library(classInt)
library(viridis)
library(gridExtra)
library(magrittr)
library(ggmap)
library(leaflet)
library(xlsx)
library(data.table)
library(mapview)
library(mapedit)
library(matlib)
library(nngeo)
library(writexl)
library(here)
library(MASS)
#'
#'
#'
#'
#'
#+ include = FALSE
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Read surveyer functions
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::
source("./R/simulation_funs.r")
source("./R/functions.r")
#'
#'
#'
#+ include = FALSE
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Boskovic/Milovanovic
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::

mreza_sim <- read_surveynet(file = paste("C:/Users/geoinz/Documents/IG/Projektovanje_2020/Boskovic_Milovanovic/Mreza_Boskovic_Milovanovic.xlsx"))

mreza_adj <- adjust.snet(adjust = FALSE, survey.net = mreza_sim, dim_type = "2D", sd.apriori = 1,  all = FALSE, prob = 0.95, maxiter = 1, ellipse.scale = 3)

ggplot() +
  geom_sf(data = mreza_adj[[1]]$net.points, aes(fill = sp)) +
  geom_sf(data = mreza_adj[[1]]$ellipse.net) +
  ggtitle("Mean area by group") +
  theme_bw()

#'
#'
#'
#+ include = FALSE
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Koncarevic/Radovic
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::

mreza_sim <- read_surveynet(file = paste("C:/Users/geoinz/Documents/IG/Projektovanje_2020/Koncarevic_Radovic/Mreza.xlsx"))

mreza_adj <- adjust.snet(adjust = FALSE, survey.net = mreza_sim, dim_type = "2D", sd.apriori = 1,  all = FALSE, prob = 0.95, maxiter = 1, ellipse.scale = 3)

ggplot() +
  geom_sf(data = mreza_adj[[1]]$net.points, aes(fill = sp)) +
  geom_sf(data = mreza_adj[[1]]$ellipse.net) +
  ggtitle("Mean area by group") +
  theme_bw()












