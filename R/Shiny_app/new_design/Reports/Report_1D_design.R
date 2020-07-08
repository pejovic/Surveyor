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
#'   net1d_design: NA,
#'   sd_h_bound: NA,
#'   rii_bound: NA
#' ---
#'
#'<img src="Grb_Gradjevinski.png" align="center" alt="logo" width="180" height = "200" style = "border: none; fixed: right;">
#'
#'
#+ include = TRUE, echo = FALSE, results = 'hide', warning = FALSE, message = FALSE
source(here::here("R/functions.r"))

#+ include = TRUE, echo = FALSE, results = 'hide', warning = FALSE, message = FALSE
library(shiny)
library(shinythemes)
library(leaflet)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(sf)
library(ggmap)
library(sp)
library(rgdal)
library(leaflet)
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
#' This report provides the main results regarding the design of 1D geodetic network.
#'
#+ echo = FALSE, message = FALSE, warning = FALSE
adj.1dnet_map <- plot_surveynet(snet.adj = params$net1d_design, webmap = FALSE, net.1D = TRUE, net.2D = FALSE)
#'
#' # Plot results
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
adj.1dnet_map

#'
#' # Tab results
#' ## Net points
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"
DT::datatable(
  params$net1d_design[[1]] %>%
    as.data.frame() %>%
    mutate(
      h = round(h, 2),
      sd_h = round(sd_h, 2)
    ) %>%
    dplyr:: select(Name, FIX_1D, Point_object, h, sd_h),
  escape=F,
  extensions = list('Buttons', 'Scroller'),
  options = list(dom = 'Bfrtip', buttons = I('colvis'),
                 deferRender = TRUE,
                 scrollY = 500,
                 scrollX = 300,
                 scroller = TRUE)
) %>%
  formatStyle(
    'sd_h',
    color = styleInterval(c(params$sd_h_bound), c('black', 'aqua')),
    backgroundColor = styleInterval(params$sd_h_bound, c('lightGray', '#FF6347'))
  )

#'
#' ## Observations
#'
#+ echo = FALSE, result = TRUE, eval = TRUE, out.width="100%"

DT::datatable(
  params$net1d_design[[2]] %>%
    as.data.frame() %>%
    mutate(
      Kl = round(Kl, 2),
      Kv = round(Kv, 2),
      rii = round(rii, 2)
    ) %>%
    dplyr::select(from, to, type, Kl, Kv, rii),
  escape=F,
  extensions = list('Buttons', 'Scroller'),
  options = list(dom = 'Bfrtip', buttons = I('colvis'),
                 deferRender = TRUE,
                 scrollY = 500,
                 scrollX = 300,
                 scroller = TRUE)
)%>%
  formatStyle(
    'rii',
    color = styleInterval(c(params$rii_bound), c('red', 'black')),
    background = styleColorBar(params$net1d_design[[2]]$rii, 'steelblue'),
    backgroundSize = '100% 90%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center'
  )





