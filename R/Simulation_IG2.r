
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


dplyr::filter(students, Ime == "Saša")

i = 20

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
mreza_adj <- adjust.snet(adjust = TRUE, survey.net = mreza_sim, dim_type = "2D", sd.apriori = 5,  all = TRUE, prob = 0.99, maxiter = 1)
plot_surveynet(snet = mreza_adj, snet.adj = TRUE, webmap = FALSE, net.1D = FALSE, net.2D = TRUE)

list.files(here::here(paste("Data/Simulation/")))

dplyr::filter(mreza_sim[[2]], from == "M2")


i = 20
mreza_sim <- read_surveynet(file = paste("C:/Users/geoinz/Documents/IG/IG2_2020", paste(students[i, "Prezime"], students[i, "Ime"], students[i, "br_ind"], sep = "."),  paste(students[i, "Prezime"], students[i, "Ime"], students[i, "br_ind"], "xlsx", sep = "."), sep = "/"))



t1 <- (360 - mreza_sim[[2]]$Hz[2]) + mreza_sim[[2]]$Hz[5] + (mreza_sim[[2]]$Hz[12] - mreza_sim[[2]]$Hz[11])
t2 <- (mreza_sim[[2]]$Hz[3] - mreza_sim[[2]]$Hz[2]) + mreza_sim[[2]]$Hz[12] + (mreza_sim[[2]]$Hz[9] - mreza_sim[[2]]$Hz[8])
t3 <- (mreza_sim[[2]]$Hz[6] - mreza_sim[[2]]$Hz[5]) + (mreza_sim[[2]]$Hz[11] - mreza_sim[[2]]$Hz[10]) + (mreza_sim[[2]]$Hz[9] - mreza_sim[[2]]$Hz[7])
t4 <- (360 - mreza_sim[[2]]$Hz[3]) + mreza_sim[[2]]$Hz[8] + mreza_sim[[2]]$Hz[6]

f1 <- abs(180 - t1)*3600
f2 <- abs(180 - t2)*3600
f3 <- abs(180 - t3)*3600
f4 <- abs(180 - t4)*3600

salfa <- max(c(1/sqrt(6), f2/sqrt(6), f3/sqrt(6), f4/sqrt(6)))
salfa

sd1 <- mreza_sim[[2]][mreza_sim[[2]]$to %in% c("M1", "M2") & mreza_sim[[2]]$from %in% c("M1", "M2") , ] %>% dplyr::summarize(sd = sd(HD)) %>% .$sd
sd2 <- mreza_sim[[2]][mreza_sim[[2]]$to %in% c("M1", "M9") & mreza_sim[[2]]$from %in% c("M1", "M9") , ] %>% dplyr::summarize(sd = sd(HD)) %>% .$sd
sd3 <- mreza_sim[[2]][mreza_sim[[2]]$to %in% c("M1", "M10") & mreza_sim[[2]]$from %in% c("M1", "M10") , ] %>% dplyr::summarize(sd = sd(HD)) %>% .$sd
sd4 <- mreza_sim[[2]][mreza_sim[[2]]$to %in% c("M2", "M9") & mreza_sim[[2]]$from %in% c("M2", "M9") , ] %>% dplyr::summarize(sd = sd(HD)) %>% .$sd
sd5 <- mreza_sim[[2]][mreza_sim[[2]]$to %in% c("M2", "M10") & mreza_sim[[2]]$from %in% c("M2", "M10") , ] %>% dplyr::summarize(sd = sd(HD)) %>% .$sd
sd6 <- mreza_sim[[2]][mreza_sim[[2]]$to %in% c("M9", "M10") & mreza_sim[[2]]$from %in% c("M9", "M10") , ] %>% dplyr::summarize(sd = sd(HD)) %>% .$sd

sdist <- sd(c(sd1, sd2, sd3, sd4, sd5, sd6))*1000
sdist

sdist/sqrt(2)/sqrt(2)



mreza_sim$observations$sd_Hz <- round(salfa, 0)
mreza_sim$observations$sd_dist <- 2#round(sdist, 0)
mreza_sim$observations$distance <- TRUE
mreza_sim$observations$direction <- TRUE

mreza_sim <- read_surveynet(file = paste("C:/Users/geoinz/Documents/IG/IG2_2020/Milic.Saša.1023.17/Milic.Saša.1023.17_V3.xlsx"))
mreza_sim <- read_surveynet(file = paste("C:/Users/geoinz/Documents/IG/IG2_2020/Dimitrijevic.Jovan.1040.17/Dimitrijevic.Jovan.1040.17_V3.xlsx"))

mreza_adj <- adjust.snet(adjust = TRUE, survey.net = mreza_sim, dim_type = "2D", sd.apriori = 1,  all = TRUE, prob = 0.99, maxiter = 1)

mreza_adj$Observations$f

mreza_adj$Observations$v

