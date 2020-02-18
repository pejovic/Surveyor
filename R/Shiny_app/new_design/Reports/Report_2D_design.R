#' ---
#' title: "Report - 2D geodetic network design"
#' author:
#'    - "surveyor - R package"
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
#'
#' params:
#'   ellipses: NA,
#'   observations: NA,
#'   points: NA,
#'   sp_bound: NA,
#'   rii_bound: NA
#' ---
#'
#'<img src="Grb_Gradjevinski.png" align="center" alt="logo" width="200" height = "200" style = "border: none; fixed: right;">
#'
#'
#+ include = TRUE, echo = FALSE, results = 'hide', warning = FALSE, message = FALSE
source(here::here("R/deprecated/input_functions.r"))
source(here::here("R/deprecated/inputFunction_withObservations.R"))
source(here::here("R/functions.r"))

#+ include = TRUE, echo = FALSE, results = 'hide', warning = FALSE, message = FALSE
library(shiny)
library(shinythemes)
library(leaflet)
library(tidyverse)
library(magrittr)
library(ggplot2)
#library(geomnet)
#library(ggnetwork)
library(sf)
library(ggmap)
library(sp)
library(rgdal)
library(leaflet)
#library(xlsx)
library(readxl)
library(data.table)
library(plotly)
library(mapview)
library(shinycssloaders)
library(here)
library(matlib)
library(nngeo)
library(dplyr)
library(mapedit)
library(DT)
library(leaflet.extras)
library(rhandsontable)
library(shinyBS)
library(shinyWidgets)
library(knitr)
library(rmarkdown)
library(knitr)
library(kableExtra)

#'
#'
#+ include = FALSE
# Plot settings
my_theme <- function(base_size = 10, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
      axis.title = element_text(size = 12),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#fffcfc"),
      strip.background = element_rect(fill = "#820000", color = "#820000", size =0.5),
      strip.text = element_text(face = "bold", size = 10, color = "white"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.background = element_blank(),
      panel.border = element_rect(color = "grey30", fill = NA, size = 0.5)
    )
}

theme_set(my_theme())
mycolors=c("#f32440","#2185ef","#d421ef")


#+ echo = FALSE, message = FALSE, warning = FALSE
adj.net_map <- adj.net_spatial_view_web(ellipses = params$ellipses, observations = params$observations, points = params$points, sp_bound = params$sp_bound, rii_bound = params$rii_bound)

#'
#' # Result - web map
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
adj.net_map

