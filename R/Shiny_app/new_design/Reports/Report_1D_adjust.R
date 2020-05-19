#' ---
#' title: "Report - 1D geodetic network design"
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
#'   sx_bound: NA
#'   sy_bound: NA
#'   ellipse_scale: NA
#'   result_units: NA
#'   adjusted_net_adj: NA
#' ---
#'
#'<img src="Grb_Gradjevinski.png" align="center" alt="logo" width="180" height = "200" style = "border: none; fixed: right;">
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
#' This report provides the main results regarding the adjustment of 2D geodetic network.
#'
#+ echo = FALSE, message = FALSE, warning = FALSE
adj.net_map <- plot_surveynet(snet.adj = params$adjusted_net_adj, webmap = TRUE, net.1D = FALSE, net.2D = TRUE, sp_bound = params$sp_bound, rii_bound = params$rii_bound, ellipse.scale = params$ellipse_scale, result.units = params$result_units) # DOPUNITI ZA RESULT UNITS

#'
#' # Map results
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
adj.net_map


#'
#' # Tab results
#' ## Error ellipse
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
  DT::datatable(
    params$ellipses %>%
      st_drop_geometry() %>%
      as.data.frame() %>%
      mutate(
        A = round(A, 1),
        B = round(B, 1),
        teta = round(teta, 1),
        sx = round(sx, 1),
        sy = round(sy, 1),
        sp = round(sp, 1)
      ), escape = FALSE,
    extensions = list('Buttons', 'Responsive'),
    options = list(dom = 'Bfrtip', pageLength = 100, lengthMenu = c(5, 10, 15, 20))) %>%
    formatStyle(
      'sx',
      color = styleInterval(c(params$sx_bound), c('black', 'red'))#,
      #backgroundColor = styleInterval(input$sx_map, c('lightGray', 'tomato'))
    ) %>%
    formatStyle(
      'sy',
      color = styleInterval(c(params$sy_bound), c('black', 'red'))#,
      #backgroundColor = styleInterval(input$sy_map, c('lightGray', 'tomato'))
    ) %>%
    formatStyle(
      'sp',
      color = styleInterval(c(params$sp_bound), c('black', 'red'))#,
      #backgroundColor = styleInterval(input$sp_map, c('lightGray', 'tomato'))
    )

#'
#' ## Net points
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
  DT::datatable(
    params$points %>%
      st_drop_geometry() %>%
      as.data.frame() %>%
      mutate(
        A = round(A, 1),
        B = round(B, 1),
        teta = round(teta, 1),
        sx = round(sx, 1),
        sy = round(sy, 1),
        sp = round(sp, 1),
        `dx [mm]` = round(dx, 2),
        `dy [mm]` = round(dy, 2),
        X = round(x, 2),
        Y = round(y, 2)
      ) %>%
      dplyr:: select(Name, `dx [mm]`, `dy [mm]`, X, Y, sx, sy, sp),
    escape = FALSE,
    extensions = list('Buttons', 'Responsive'),
    options = list(dom = 'Bfrtip', pageLength = 100, lengthMenu = c(5, 10, 15, 20)))%>%
    formatStyle(
      'sx',
      color = styleInterval(c(params$sx_bound), c('black', 'red'))#,
      #backgroundColor = styleInterval(input$sx_map, c('lightGray', 'tomato'))
    ) %>%
    formatStyle(
      'sy',
      color = styleInterval(c(params$sy_bound), c('black', 'red'))#,
      #backgroundColor = styleInterval(input$sy_map, c('lightGray', 'tomato'))
    ) %>%
    formatStyle(
      'sp',
      color = styleInterval(c(params$sp_bound), c('black', 'red'))#,
      #backgroundColor = styleInterval(input$sp_map, c('lightGray', 'tomato'))
    )

#'
#' ## Obseravtions
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
  DT::datatable(
    params$observations %>%
      st_drop_geometry() %>%
      as.data.frame() %>%
      mutate(
        v = round(v, 2),
        Kl = round(Kl, 2),
        Kv = round(Kv, 2),
        rii = round(rii, 2)
      ) %>%
      dplyr::select(from, to, type, Kl, Kv, rii, used),
    escape = FALSE,
    extensions = list('Buttons', 'Responsive'),
    options = list(dom = 'Bfrtip', pageLength = 100, lengthMenu = c(5, 10, 15, 20, 50)))%>%
    formatStyle(
      'rii',
      color = styleInterval(c(params$rii_bound), c('red', 'black')),
      background = styleColorBar(params$adjusted_net_adj[[2]]$rii, 'steelblue'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )







