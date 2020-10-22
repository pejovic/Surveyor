
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
library(MASS)

source("./R/simulation_funs.r")
source("./R/functions.r")
source("./R/Deformation_analysis_functions.r")


snet1 = "C:/R_projects/Surveyer/Data/Input/With_observations/Brose/Brose_nulta.xlsx"
snet2 = "C:/R_projects/Surveyer/Data/Input/With_observations/Brose/Brose_prva.xlsx"
aa <- snet.stable(snet_1_path = snet1, snet_2_path = snet2, sd.apriori = 0.4, wdh_model = "n_dh", units = "mm",  dim_type = "1D", prob = 0.99)


snet1 = "C:/R_projects/Surveyer/Data/Input/With_observations/snet_1D/snet_1.xlsx"
snet2 = "C:/R_projects/Surveyer/Data/Input/With_observations/snet_1D/snet_2.xlsx"
aa <- snet.stable(snet_1_path = snet1, snet_2_path = snet2, sd.apriori = 1, units = "mm",  dim_type = "1D", prob = 0.95, wdh_model = "sd_dh")


snet1 = "C:/R_projects/Surveyer/Data/Input/With_observations/1D_case_1/epoch_1.xlsx"
snet2 = "C:/R_projects/Surveyer/Data/Input/With_observations/1D_case_1/epoch_2.xlsx"
aa <- snet.stable(snet_1_path = snet1, snet_2_path = snet2, sd.apriori = 0.5, units = "mm",  dim_type = "1D", prob = 0.95, wdh_model = "n_dh")




