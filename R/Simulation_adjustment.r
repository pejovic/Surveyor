
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


A <- c(393.979,	419.038)
B <- c(366.358, 550.138)
C <- c(601.903, 632.171)
D <- c(705.481, 538.638)
O1 <- c(500.000, 500.000)
O2 <- c(585.023, 548.609)

points <- rbind(A, B, C, D, O1, O2) %>% as.data.frame() %>% rownames_to_column("Name") %>% rename(x = V1, y = V2)
points <- points %>% dplyr::mutate(id = row_number(), FIX_X = FALSE, FIX_Y = FALSE, Point_object = FALSE) %>% dplyr::select(id, Name, x, y, FIX_X, FIX_Y, Point_object)


obs_plan <- data.frame(station = rep(c("A", "A", "A", "B", "B", "B", "C", "C", "C", "D", "D", "D"), 2),
                       obs.point = rep(c("B", "O1", "O2", "A", "O2", "O1", "D", "O2","O1", "C", "O1", "O2"), 2),
                       type = c(rep("p", 12), rep("d", 12)), stringsAsFactors = FALSE)
obs_plan <- obs_plan[-which(obs_plan$obs.point %in% c("A", "B", "C", "D") & obs_plan$type == "d"), ]

points = A_points; obs.plan = A_sim_obs
Hz0 = NA; red = TRUE; sd_Hz = 5; sd_dist = 3; sd_cent_station = 1; sd_cent_target = 1; seed = NULL


sim.survey.net.raw <- sim.obs(points = points, obs.plan = obs_plan)

sim.survey.net <- import_surveynet2D(points = sim.survey.net.raw[[1]], observations = sim.survey.net.raw[[2]])

A <- Amat(survey.net = sim.survey.net, units = "mm")
f <- data.frame(f = fmat(survey.net = sim.survey.net))
P <- data.frame(Wmat(survey.net = sim.survey.net, apriori = 5))

design.snet(survey.net = sim.survey.net, apriori = 1, prob = NA, result.units = list("mm", "cm", "m"), ellipse.scale = 1, axes = c("Easting", "Northing"), teta.unit = list("deg", "rad"), all = FALSE)

survey.net = sim.survey.net; apriori = 1; prob = NA; result.units = list("mm", "cm", "m"); ellipse.scale = 1; axes = c("Easting", "Northing"); teta.unit = list("deg", "rad"); all = FALSE

writexl::write_xlsx(obs1_list, path = "obs11_list.xlsx")

# Zadatak


A_points <- readxl::read_xlsx(path = ("B:/_Bechelor/_Ispiti/Projektovanje/28.6.2019/A_plan.xlsx"), sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
A_obs <- readxl::read_xlsx(path = ("B:/_Bechelor/_Ispiti/Projektovanje/28.6.2019/A_plan.xlsx"), sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric", "numeric"))
A.survey.net <- import_surveynet2D(points = A_points, observations = A_obs)

B_points <- readxl::read_xlsx(path = ("B:/_Bechelor/_Ispiti/Projektovanje/28.6.2019/B_plan.xlsx"), sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
B_obs <- readxl::read_xlsx(path = ("B:/_Bechelor/_Ispiti/Projektovanje/28.6.2019/B_plan.xlsx"), sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric", "numeric"))
B.survey.net <- import_surveynet2D(points = B_points, observations = B_obs)




A.design <- design.snet(survey.net = A.survey.net, sd.apriori = 1, prob = NA, result.units = "mm", ellipse.scale = 1, teta.unit = list("deg", "rad"), all = TRUE)
B.design <- design.snet(survey.net = B.survey.net, sd.apriori = 1, prob = NA, result.units = "mm", ellipse.scale = 1, teta.unit = list("deg", "rad"), all = TRUE)

A.design$design.matrices
A.design$net.points
B.design$net.points




# Makis

Makis_points <- readxl::read_xlsx(path = ("./Data/Input/With_observations/Makis/Makis_observations.xlsx"), sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
Makis_obs <- readxl::read_xlsx(path = ("./Data/Input/With_observations/Makis/Makis_observations.xlsx"), sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric", "numeric"))
Makis.survey.net <- import_surveynet2D(points = Makis_points, observations = Makis_obs)


Makis.design <- design.snet(survey.net = Makis.survey.net, sd.apriori = 6, prob = NA, result.units = "mm", ellipse.scale = 1, axes = c("Easting", "Northing"), teta.unit = list("deg", "rad"), all = TRUE)


A <- Amat(survey.net = Makis.survey.net, units = "mm")
f <- data.frame(f = fmat(survey.net = Makis.survey.net))
P <- data.frame(Wmat(survey.net = Makis.survey.net, sd.apriori = 3))



# AVALA

avala_points <- readxl::read_xlsx(path = here::here("Data/Input/With_observations/Avala/Avala_observations.xlsx"), sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
avala_obs <- readxl::read_xlsx(path = here::here("Data/Input/With_observations/Avala/Avala_observations.xlsx"), sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
avala <- import_surveynet2D(points = avala_points, observations = avala_obs)
avala[[2]] <- mutate(avala[[2]], Hz = HzD + HzM/60 + HzS/3600, Vz = VzD + VzM/60 + VzS/3600, HD = SD*sin(Vz*pi/180)) %>%
  dplyr::filter(to %in% c("S1","S2","S3","S4","S5","S6")) %>% dplyr::filter(from != "S7")


ggplot(data = avala[[1]]) + geom_sf() + ggrepel::geom_label_repel(
  data = avala[[1]],
  aes(label = Name, geometry = geometry),
  stat = "sf_coordinates",
  min.segment.length = 0,
  colour = "magenta",
  segment.colour = "magenta"
) + theme_bw()


# Brana

brana_points <- readxl::read_xlsx(path = here::here("Data/Input/With_observations/Brana/Brana.xlsx"), sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
brana_obs <- readxl::read_xlsx(path = here::here("Data/Input/With_observations/Brana/Brana.xlsx"), sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
brana <- import_surveynet2D(points = brana_points, observations = brana_obs)


brana.adjust <- adjust.snet(survey.net = brana, result.units = "mm", adjust = FALSE)

brana.adjust$net.points

plot(brana[[1]]$geometry)


# TETO


TETO_points <- readxl::read_xlsx(path = ("D:/R_projects/Surveyer/Data/Input/Without_observations/xlsx/TETO_plan opazanja.xlsx"), sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
TETO_obs <- readxl::read_xlsx(path = ("D:/R_projects/Surveyer/Data/Input/Without_observations/xlsx/TETO_plan opazanja.xlsx"), sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric", "numeric"))
TETO.survey.net <- import_surveynet2D(points = TETO_points, observations = TETO_obs, axes = c("Easting", "Northing"))

TETO_sim_obs <- rbind(dplyr::filter(TETO.survey.net$observations, direction), dplyr::filter(TETO.survey.net$observations, direction)) %>%
  dplyr::select(from = station, to = obs.point) %>% sf::st_drop_geometry() %>%
  dplyr::mutate(type = c(rep("p", 50), rep("d", 50)))



TETO.adjust <- adjust.snet(survey.net = TETO.survey.net, adjust = FALSE, result.units = "mm")

writexl::write_xlsx(TETO.adjust$observations %>% select(-geometry), "TETO_obs.xlsx")
writexl::write_xlsx(TETO.adjust$net.points %>% select(-geometry), "TETO_points.xlsx")

#TETO Simulation

sim.survey.net.raw <- sim.obs(points = TETO_points, obs.plan = TETO_sim_obs)


# Zadatak


A_points <- readxl::read_xlsx(path = ("E:/_Bechelor/_Ispiti/Projektovanje/17.9.2019/A_plan.xlsx"), sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
A_obs <- readxl::read_xlsx(path = ("E:/_Bechelor/_Ispiti/Projektovanje/17.9.2019/A_plan.xlsx"), sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric", "numeric"))
A.survey.net <- import_surveynet2D(points = A_points, observations = A_obs)

B_points <- readxl::read_xlsx(path = ("E:/_Bechelor/_Ispiti/Projektovanje/17.9.2019/B_plan.xlsx"), sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
B_obs <- readxl::read_xlsx(path = ("E:/_Bechelor/_Ispiti/Projektovanje/17.9.2019/B_plan.xlsx"), sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric", "numeric"))
B.survey.net <- import_surveynet2D(points = B_points, observations = B_obs, axes = c("Northing", "Easting"))

plot(A.survey.net[[1]]$geometry)
plot(A.survey.net[[2]]$geometry,add = TRUE)


A.design <- adjust.snet(adjust = FALSE, survey.net = A.survey.net, sd.apriori = 3, prob = NA, result.units = "mm", ellipse.scale = 1, teta.unit = list("deg", "rad"), all = TRUE)
B.design <- adjust.snet(adjust = FALSE,survey.net = B.survey.net, sd.apriori = 3, prob = NA, result.units = "mm", ellipse.scale = 1, teta.unit = list("deg", "rad"), all = TRUE)

#A.design$design.matrices
A.design$net.points
B.design$net.points



# Zadatak 06.02.2020

# Zadatak


A_points <- readxl::read_xlsx(path = ("B:/_Bechelor/_Ispiti/Projektovanje/06.02.2020/A_plan.xlsx"), sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
A_obs <- readxl::read_xlsx(path = ("B:/_Bechelor/_Ispiti/Projektovanje/06.02.2020/A_plan.xlsx"), sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric", "numeric"))
A.survey.net <- import_surveynet2D(points = A_points, observations = A_obs)

B_points <- readxl::read_xlsx(path = ("B:/_Bechelor/_Ispiti/Projektovanje/06.02.2020/B_plan.xlsx"), sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
B_obs <- readxl::read_xlsx(path = ("B:/_Bechelor/_Ispiti/Projektovanje/06.02.2020/B_plan.xlsx"), sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric", "numeric"))
B.survey.net <- import_surveynet2D(points = B_points, observations = B_obs)




A.design <- adjust.snet(adjust = FALSE, survey.net = A.survey.net, sd.apriori = 3, prob = NA, result.units = "mm", ellipse.scale = 1, teta.unit = list("deg", "rad"), all = TRUE)
B.design <- adjust.snet(adjust = FALSE,survey.net = B.survey.net, sd.apriori = 3, prob = NA, result.units = "mm", ellipse.scale = 1, teta.unit = list("deg", "rad"), all = TRUE)

#A.design$design.matrices
A.design$net.points
B.design$net.points





# Zadatak 06.02.2020 - 2

# Zadatak


A_points <- readxl::read_xlsx(path = ("E:/_Bechelor/_Ispiti/Projektovanje/17.3.2019/A_plan.xlsx"), sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
A_obs <- readxl::read_xlsx(path = ("E:/_Bechelor/_Ispiti/Projektovanje/17.3.2019/A_plan.xlsx"), sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric", "numeric"))
A.survey.net <- import_surveynet2D(points = A_points, observations = A_obs)





A.design <- adjust.snet(adjust = FALSE, survey.net = A.survey.net, sd.apriori = 3, prob = NA, result.units = "mm", ellipse.scale = 1, teta.unit = list("deg", "rad"), all = TRUE)


#A.design$design.matrices
A.design$net.points
B.design$net.points





# Simulation

B_sim_obs <- rbind(dplyr::filter(B.survey.net[[2]], direction), dplyr::filter(B.survey.net[[2]], distance)) %>%
  dplyr::select(station = from, obs.point = to) %>% sf::st_drop_geometry() %>%
  dplyr::mutate(type = c(rep("p", dim(dplyr::filter(B.survey.net[[2]], direction))[1]), rep("d", dim(dplyr::filter(B.survey.net[[2]], distance))[1])))

coord_sim_res <- matrix(round(rnorm(n = 16, mean = 0, sd = 3)/1000, 3), ncol = 2)

B_points$x <- B_points$x + coord_sim_res[, 1]
B_points$y <- B_points$y + coord_sim_res[, 2]

sim.B.net <- sim.obs(points = B_points, obs.plan = B_sim_obs, sd_Hz = 5, sd_cent_station = 1, sd_dist = 3, sd_cent_target = 1)
sim.B.net <- import_surveynet2D(points = sim.B.net$Points, observations = sim.B.net$Observations)

writexl::write_xlsx(sim.B.net, path = "E:/_Bechelor/_Ispiti/Projektovanje/17.9.2019/simBobs.xlsx")

#Izravnanje
B_points <- readxl::read_xlsx(path = ("E:/_Bechelor/_Ispiti/Projektovanje/17.9.2019/simBobs.xlsx"), sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
B_obs <- readxl::read_xlsx(path = ("E:/_Bechelor/_Ispiti/Inzenjerska2/Redovni/17.9.2019/simBobs.xlsx"), sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric", "numeric"))
B.survey.net <- import_surveynet2D(points = B_points, observations = B_obs)

B.net.adjust <- adjust.snet(survey.net = sim.B.net, result.units = "mm", sd.apriori = 3)




# GORICA

# Makis

Gorica_points <- readxl::read_xlsx(path = ("./Data/Input/With_observations/Brana_Gorica/Brana_Gorica_nulta_serija.xlsx"), sheet = "Points")#, col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
Gorica_obs <- readxl::read_xlsx(path = ("./Data/Input/With_observations/Brana_Gorica/Brana_Gorica_nulta_serija.xlsx"), sheet = "Observations")#, col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric", "numeric"))
Gorica.survey.net <- import_surveynet2D(points = Gorica_points, observations = Gorica_obs)

Gorica.net.adjust <- adjust.snet(survey.net = Gorica.survey.net, result.units = "mm", sd.apriori = 10)



# Simulation IG2

# Zadatak 06.02.2020 - 2

# Zadatak


ig_points <- readxl::read_xlsx(path = ("C:/R_projects/Surveyer/Data/Input/Without_observations/xlsx/zadatak_IG2.xlsx"), sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
ig_obs <- readxl::read_xlsx(path = ("C:/R_projects/Surveyer/Data/Input/Without_observations/xlsx/zadatak_IG2.xlsx"), sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric", "numeric"))
ig.net <- import_surveynet2D(points = ig_points, observations = ig_obs)





A.design <- adjust.snet(adjust = FALSE, survey.net = ig.net, sd.apriori = 3, prob = NA, result.units = "mm", ellipse.scale = 1, teta.unit = list("deg", "rad"), all = TRUE)


#A.design$design.matrices
A.design$net.points
B.design$net.points


ig_sim_obs <- rbind(dplyr::filter(ig.net[[2]], direction), dplyr::filter(ig.net[[2]], distance)) %>%
  dplyr::select(station = from, obs.point = to) %>% sf::st_drop_geometry() %>%
  dplyr::mutate(type = c(rep("p", dim(dplyr::filter(ig.net[[2]], direction))[1]), rep("d", dim(dplyr::filter(ig.net[[2]], distance))[1])))

coord_sim_res <- matrix(round(rnorm(n = 12, mean = 0, sd = 3)/1000, 3), ncol = 2)

ig_points$x <- ig_points$x + coord_sim_res[, 1]
ig_points$y <- ig_points$y + coord_sim_res[, 2]

sim.ig.net <- sim.obs(points = ig_points, obs.plan = ig_sim_obs, sd_Hz = 5, sd_cent_station = 1, sd_dist = 3, sd_cent_target = 1)
sim.B.net <- import_surveynet2D(points = sim.B.net$Points, observations = sim.B.net$Observations)



########### 1D ############################################

dns_points <- readxl::read_xlsx(path = ("C:/R_projects/Surveyer/Data/Input/With_observations/DNS_1D/DNS_1D_nulta.xlsx"), sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "numeric", "logical", "logical", "logical", "logical"))
dns_obs <- readxl::read_xlsx(path = ("C:/R_projects/Surveyer/Data/Input/With_observations/DNS_1D/DNS_1D_nulta.xlsx"), sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

file_path <- "C:/R_projects/Surveyer/Data/Input/With_observations/DNS_1D/DNS_1D_nulta.xlsx"
file_path <- here::here("Data/Input/With_observations/Brana/Brana.xlsx")

read_surveynet(file = file_path, dim = c("1D", "2D"), s0, dest_crs = NA, axes = c("Easting", "Northing")){
  ponts_col_type <- c("numeric", "text", "numeric", "numeric", "numeric", "logical", "logical", "logical")
  obs_col_type <- c("text", "text", rep("numeric", 15))
  points <- readxl::read_xlsx(path = file_path, sheet = "Points", col_types = ponts_col_type) %>% mutate_at(., .vars = c("Name"), as.character)
  observations <- readxl::read_xlsx(path = file_path, sheet = "Observations", col_types = obs_col_type) %>% mutate_at(., .vars = c("from", "to"), as.character)

  fixed.points <- points[apply(points[, c("FIX_2D", "FIX_1D")], 1, any), , ]$Name %>% .[!is.na(.)]

  if(dim == "2D"){
    if(which(axes == "Easting") == 2){points <- points %>% dplyr::rename(x = y,  y = x)}

    observations$x_from <- points$x[match(observations$from, points$Name)]
    observations$y_from <- points$y[match(observations$from, points$Name)]
    observations$x_to <- points$x[match(observations$to, points$Name)]
    observations$y_to <- points$y[match(observations$to, points$Name)]

    points <- points %>% as.data.frame() %>% sf::st_as_sf(coords = c("x","y")) # %>% sf::st_set_crs(dest_crs) TODO dodaj opciju za dodeljivanje CRS-a

    observations <- observations %>% dplyr::mutate(Hz = HzD + HzM/60 + HzS/3600,
                                                   Vz = VzD + VzM/60 + VzS/3600,
                                                   distance = (!is.na(.$HD) | !is.na(.$SD)),
                                                   direction = !is.na(Hz))

    if(dplyr::select(observations, HzD, HzM, HzS) %>% is.na() %>% all()){
      observations$direction[!is.na(observations$sd_Hz)] <- TRUE
    }
    if(dplyr::select(observations, HD, SD) %>% is.na() %>% all()){
      observations$distance[!is.na(observations$sd_dist)] <- TRUE
    }
    # Eliminacija merenih duzina izmedju fiksnih tacaka duzina izmedju
    if(length(fixed.points) > 0){
      fixed.distances <- which(observations$distance & observations$from %in% fixed.points & observations$to %in% fixed.points)
      observations[fixed.distances, "distance"] <- FALSE
      observations[fixed.distances, "sd_dist"] <- NA
    }

    observations <- as.data.table(observations) %>% dplyr::mutate(id = seq.int(nrow(.))) %>% split(., f = as.factor(.$id)) %>%
      lapply(., function(row) {lmat <- matrix(unlist(row[c("x_from", "y_from", "x_to", "y_to")]), ncol = 2, byrow = TRUE)
      st_linestring(lmat)}) %>%
      sf::st_sfc() %>%
      sf::st_sf('ID' = seq.int(nrow(observations)), observations, 'geometry' = .)

    observations <- observations %>% sf::st_set_crs(dest_crs)
  }



}



import_surveynet2D <- function(points = points, observations = observations, dest_crs = NA, axes = c("Easting", "Northing")){

  observations <- mutate_at(observations, .vars = c("from", "to"), as.character)
  points <- mutate_at(points, .vars = c("Name"), as.character)

  fixed.points <- points[apply(points[, c("FIX_X", "FIX_Y")], 1, all), , drop=FALSE]$Name

  # Create geometry columns for points
  if (is.na(dest_crs)){
    dest_crs <- 3857
  } else{
    dest_crs <- dest_crs
  }

  if(which(axes == "Easting") == 2){points <- points %>% dplyr::rename(x = y,  y = x)}

  observations$x_from <- points$x[match(observations$from, points$Name)]
  observations$y_from <- points$y[match(observations$from, points$Name)]
  observations$x_to <- points$x[match(observations$to, points$Name)]
  observations$y_to <- points$y[match(observations$to, points$Name)]

  points <- points %>% as.data.frame() %>% sf::st_as_sf(coords = c("x","y")) %>% sf::st_set_crs(dest_crs)

  observations <- observations %>% dplyr::mutate(Hz = HzD + HzM/60 + HzS/3600,
                                                 Vz = VzD + VzM/60 + VzS/3600,
                                                 distance = (!is.na(.$HD) | !is.na(.$SD)),
                                                 direction = !is.na(Hz))

  if(dplyr::select(observations, HzD, HzM, HzS) %>% is.na() %>% all()){
    observations$direction[!is.na(observations$sd_Hz)] <- TRUE
  }
  if(dplyr::select(observations, HD, SD) %>% is.na() %>% all()){
    observations$distance[!is.na(observations$sd_dist)] <- TRUE
  }
  # Eliminacija merenih duzina izmedju fiksnih tacaka duzina izmedju
  if(length(fixed.points) > 0){
    fixed.distances <- which(observations$distance & observations$from %in% fixed.points & observations$to %in% fixed.points)
    observations[fixed.distances, "distance"] <- FALSE
    observations[fixed.distances, "sd_dist"] <- NA
  }


  observations <- as.data.table(observations) %>% dplyr::mutate(id = seq.int(nrow(.))) %>% split(., f = as.factor(.$id)) %>%
    lapply(., function(row) {lmat <- matrix(unlist(row[c("x_from", "y_from", "x_to", "y_to")]), ncol = 2, byrow = TRUE)
    st_linestring(lmat)}) %>%
    sf::st_sfc() %>%
    sf::st_sf('ID' = seq.int(nrow(observations)), observations, 'geometry' = .)

  observations <- observations %>% sf::st_set_crs(dest_crs)

  # Creating list
  survey_net <- list(points, observations)
  return(survey_net)
}



