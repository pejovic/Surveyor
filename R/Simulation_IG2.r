
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


# Read mreza file
mreza <- read_surveynet(file = here::here("Data/Simulation/TETO_extended.xlsx"))
# Simulation
mreza_sim <- sim_snetobs(survey.net = mreza, red = TRUE, seed = 1018)
# Create list and write file
mreza_list <- list(Points = mreza_sim[[1]] %>% st_drop_geometry(), Observations = mreza_sim[[2]] %>% st_drop_geometry() %>% mutate_at(.vars = c("e_cent_from", "e_cent_to", "e_focus", "e_air"), .funs = ~NA) %>% dplyr::select(from:e_air)) %>%
  writexl::write_xlsx(., path = here::here("Data/Simulation/TETO_extended_sim.xlsx"))
# Read simulated mreza
mreza_sim <- read_surveynet(file = here::here("Data/Simulation/TETO_extended_sim.xlsx"))
plot_surveynet(snet = mreza_sim, webmap = FALSE, net.1D = FALSE, net.2D = TRUE)
mreza_adj <- adjust.snet(adjust = TRUE, survey.net = mreza_sim, dim_type = "2D", sd.apriori = 3,  all = FALSE, prob = 0.95)
plot_surveynet(snet = mreza_adj, snet.adj = TRUE, webmap = FALSE, net.1D = FALSE, net.2D = TRUE)



students <- readxl::read_xlsx(here::here("Data/Simulation/students.xlsx"))
mreza <- read_surveynet(file = here::here("Data/Simulation/TETO_reduced.xlsx"))

for(i in 1:dim(students)[1]){
  mreza_sim <- sim_snetobs(survey.net = mreza, red = TRUE, seed = as.numeric(students[i, "br_ind"]))
  mreza_list <- list(Points = mreza_sim[[1]] %>% st_drop_geometry() %>% dplyr::mutate_at(.vars = c("x", "y"), ~round(., 3)), Observations = mreza_sim[[2]] %>% st_drop_geometry() %>% mutate_at(.vars = c("sd_Hz", "sd_dist", "e_cent_from", "e_cent_to", "e_focus", "e_air"), .funs = ~NA) %>% dplyr::select(from:e_air)) %>%
    writexl::write_xlsx(., path = here::here(paste("Data/Simulation/", paste(students[i, "Prezime"], students[i, "Ime"], students[i, "br_ind"], "xlsx", sep = "."), sep = "")))
}


i = 10

# Read mreza file
mreza <- read_surveynet(file = here::here(paste("Data/Simulation/", paste(students[i, "Prezime"], students[i, "Ime"], students[i, "br_ind"], "xlsx", sep = "."), sep = "")))
# Simulation
mreza_sim <- sim_snetobs(survey.net = mreza, red = TRUE, seed = 1006.17)
# Create list and write file
mreza_list <- list(Points = mreza_sim[[1]] %>% st_drop_geometry() %>% dplyr::mutate_at(.vars = c("x", "y"), .funs = round(., 2)), Observations = mreza_sim[[2]] %>% st_drop_geometry() %>% mutate_at(.vars = c("e_cent_from", "e_cent_to", "e_focus", "e_air"), .funs = ~NA) %>% dplyr::select(from:e_air)) %>%
  writexl::write_xlsx(., path = here::here("Data/Simulation/TETO_reduced_sim.xlsx"))
# Read simulated mreza
mreza_sim <- read_surveynet(file = here::here(paste("Data/Simulation/", paste(students[i, "Prezime"], students[i, "Ime"], students[i, "br_ind"], "xlsx", sep = "."), sep = "")))
plot_surveynet(snet = mreza, webmap = FALSE, net.1D = FALSE, net.2D = TRUE)
mreza_adj <- adjust.snet(adjust = TRUE, survey.net = mreza_sim, dim_type = "2D", sd.apriori = 5,  all = FALSE, prob = 0.99)
plot_surveynet(snet = mreza_adj, snet.adj = TRUE, webmap = FALSE, net.1D = FALSE, net.2D = TRUE)

list.files(here::here(paste("Data/Simulation/")))




