
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
library(matlib)
library(nngeo)
library(writexl)
library(here)

source("./R/Simulations_functions.r")
source("./R/functions.r")


file_path <- here::here("Data/Input/With_observations/DNS_1D/DNS_1D_nulta.xlsx")
dns.snet <- read_surveynet(file = file_path)
dns.snet.adj <- adjust.snet(adjust = FALSE, survey.net = dns.snet, wdh_model = "n_dh", dim_type = "1D", sd.apriori = 0.2 ,  all = FALSE)

file_path <- here::here("Data/Input/With_observations/Brana/Brana.xlsx")
brana.snet <- read_surveynet(file = file_path)
brana.snet.adj <- adjust.snet(adjust = TRUE, survey.net = brana.snet, dim_type = "2D", sd.apriori = 1 ,  all = FALSE)

file_path <- here::here("Data/Input/With_observations/Makis/Makis_observations.xlsx")
makis.snet <- read_surveynet(file = file_path)
makis.snet.adj <- adjust.snet(adjust = FALSE, survey.net = makis.snet, dim_type = "2D", sd.apriori = 3 ,  all = FALSE)

file_path <- here::here("Data/Input/With_observations/Zadatak 1/Zadatak_1.xlsx")
zadatak1.snet <- read_surveynet(file = file_path)
zadatak1.snet.adj <- adjust.snet(adjust = FALSE, survey.net = zadatak1.snet, dim_type = "2D", sd.apriori = 3 ,  all = FALSE)

file_path <- here::here("Data/Input/Without_observations/xlsx/TETO_plan opazanja.xlsx")
teto.snet <- read_surveynet(file = file_path)
teto.snet.adj <- adjust.snet(adjust = FALSE, survey.net = teto.snet, dim_type = "2D", sd.apriori = 1 ,  all = FALSE)


