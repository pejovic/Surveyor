
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

dns_points <- readxl::read_xlsx(path = ("D:/R_projects/Surveyer/Data/Input/With_observations/DNS_1D/DNS_1D_nulta.xlsx"), sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "numeric", "logical", "logical", "logical", "logical"))
dns_obs <- readxl::read_xlsx(path = ("D:/R_projects/Surveyer/Data/Input/With_observations/DNS_1D/DNS_1D_nulta.xlsx"), sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

file = file_path

read_surveynet <- function(file, dest_crs = NA, axes = c("Easting", "Northing")){
  # TODO: Set warning if there are different or not used points in two elements of survey.net list.
  # TODO: Check if any point has no sufficient measurements to be adjusted.
  # setting columns type
  ponts_col_type <- c("numeric", "text", "numeric", "numeric", "numeric", "logical", "logical", "logical")
  obs_col_type <- c("text", "text", rep("numeric", 15))
  # reading data
  points <- readxl::read_xlsx(path = file, sheet = "Points", col_types = ponts_col_type) %>% mutate_at(., .vars = c("Name"), as.character)
  observations <- readxl::read_xlsx(path = file, sheet = "Observations", col_types = obs_col_type) %>% mutate_at(., .vars = c("from", "to"), as.character)

  if(sum(rowSums(is.na(points[, c("x", "y")])) != 0) != 0){
    warning("Network has no spatial coordinates")}else{
      # Creating sf class from observations
      observations$x_from <- points$x[match(observations$from, points$Name)]
      observations$y_from <- points$y[match(observations$from, points$Name)]
      observations$x_to <- points$x[match(observations$to, points$Name)]
      observations$y_to <- points$y[match(observations$to, points$Name)]

      points <- points %>% as.data.frame() %>% sf::st_as_sf(coords = c("x","y"), remove = FALSE)
      if(which(axes == "Easting") == 2){points <- points %>% dplyr::rename(x = y,  y = x)}

      observations <- as.data.table(observations) %>% dplyr::mutate(id = seq.int(nrow(.))) %>% split(., f = as.factor(.$id)) %>%
        lapply(., function(row) {lmat <- matrix(unlist(row[c("x_from", "y_from", "x_to", "y_to")]), ncol = 2, byrow = TRUE)
        st_linestring(lmat)}) %>%
        sf::st_sfc() %>%
        sf::st_sf('ID' = seq.int(nrow(observations)), observations, 'geometry' = .)
    }


  #if(sum(rowSums(is.na(observations[, c("HzD", "HzM", "HzS")])) != 0) != 0){stop("There is uncomplete observations")}

  observations <- observations %>% dplyr::mutate(Hz = HzD + HzM/60 + HzS/3600,
                                                 Vz = VzD + VzM/60 + VzS/3600,
                                                 tdh = SD*cos(Vz*pi/180),
                                                 distance = (!is.na(HD) | !is.na(SD)) & !is.na(sd_dist),
                                                 direction = !is.na(Hz) & !is.na(sd_Hz),
                                                 diff_level = (!is.na(dh) | !is.na(tdh)))

  # In network design, observation is included if measurement standard is provided
  if(dplyr::select(observations, HzD, HzM, HzS) %>% is.na() %>% all()){
    observations$direction[!is.na(observations$sd_Hz)] <- TRUE
  }
  if(dplyr::select(observations, HD, SD) %>% is.na() %>% all()){
    observations$distance[!is.na(observations$sd_dist)] <- TRUE
  }
  if(dplyr::select(observations, dh) %>% is.na() %>% all()){
    observations$diff_level[(!is.na(.$dh) | !is.na(.$sd_dh) | !is.na(.$d_dh) | !is.na(.$n_dh))] <- TRUE
  }

  # Eliminacija merenih duzina i visinsih razlika izmedju fiksnih tacaka duzina izmedju
  # checking for fixed points
  # TODO what in case of no-fixed points
  fixed_points <- points %>% dplyr::filter(FIX_2D | FIX_1D) %>% .$Name

  if(length(fixed_points) > 1){
    observations[observations$from %in% fixed_points & observations$to %in% fixed_points, "distance"] <- FALSE
    observations[observations$from %in% fixed_points & observations$to %in% fixed_points, "diff_level"] <- FALSE
  }

  # Setting coordinate system
  if(!is.na(dest_crs)){
    observations %<>% sf::st_set_crs(dest_crs)
    points %<>% sf::st_set_crs(dest_crs)
  }

  # Creating list
  survey_net <- list(points, observations)
  names(survey_net) <- c("points", "observations")
  return(survey_net)
}

#file_path <- "D:/R_projects/Surveyer/Data/Input/With_observations/DNS_1D/DNS_1D_nulta.xlsx"
#file_path <- here::here("Data/Input/With_observations/Brana/Brana.xlsx")


#dns <- read_surveynet(file = file_path)

#surveynet <- dns

Amat1D <- function(survey.net){
  used_points <- unique(c(survey.net$observations$from, survey.net$observations$to))
  point_names <- unique(survey.net$points$Name)
  point_names <- point_names[point_names %in% used_points]
  if(!all(used_points %in% point_names)){stop("Some points are missed")}

  Amat <- data.frame(matrix(0, ncol = length(point_names), nrow = dim(survey.net$observations)[1]))
  names(Amat) <- point_names
  for(i in 1:dim(Amat)[1]){
    Amat[i, survey.net$observations$from[i]] <- -1
    Amat[i, survey.net$observations$to[i]] <- 1
  }
  fixed_points <- survey.net$points[apply(survey.net$points[, c("FIX_1D")], 1, any), , ]$Name %>% .[!is.na(.)]
  Amat <- Amat %>% select(-fixed_points)

  Amat <- as.matrix(Amat)
  return(Amat)
}

Wmat1D <- function(survey.net, wdh_model = list("sd_dh", "d_dh", "n_dh", "E"), sd0 = 1, d0 = NA, n0 = 1){
  "%!in%" <- Negate("%in%")
  if(wdh_model %!in% c("sd_dh", "d_dh", "n_dh", "E")){stop("Model of weigths is not properly specified, see help")}
  wdh_model <- wdh_model[[1]]
  if(is(survey.net[[2]], "sf")){survey.net[[2]] <- survey.net[[2]] %>% st_drop_geometry()}

  survey.net[[2]] %<>%
    dplyr::mutate(weigth = case_when(
      wdh_model == "sd_dh" ~ sd.apriori/sd_dh,
      wdh_model == "d_dh" ~ 1/d_dh,
      wdh_model == "n_dh" ~ n0/n_dh,
      wdh_model == "E" ~ 1
    )
  )
  return(diag(survey.net[[2]]$weigth))
}

fmat1D <- function(survey.net, units = units){
  unit.lookup <- c("mm" = 1000, "cm" = 100, "m" = 1)
  survey.net[[2]]$h_from <- survey.net[[1]]$h[match(survey.net[[2]]$from, survey.net[[1]]$Name)]
  survey.net[[2]]$h_to <- survey.net[[1]]$h[match(survey.net[[2]]$to, survey.net[[1]]$Name)]
  f <- ((survey.net[[2]]$h_to-survey.net[[2]]$h_from)-survey.net[[2]]$dh)*unit.lookup[units]
  return(f)
}

# Funkcija koja izdvaja elemente Qx matrice u listu za elipsu svake tacke
Qxy <- function(Qx, fix = fix.mat[, 2]){
  k = 0
  Qxxx <- as.list(rep(NA, length(fix)))
  for(i in 1:length(Qxxx)){
    k = 2*fix[i] + k
    if (fix[i]){
      Qxxx[[i]] <- cbind(c(Qx[k-1, k-1], Qx[k-1, k]), c(Qx[k, k-1], Qx[k, k]))
    } else {
      Qxxx[[i]] <- diag(rep(fix[i]*1, 2))
    }
  }
  return(Qxxx)
}

#Wmat1D(survey.net = dns, w_model = "n_dh")
#Amat1D(survey.net = dns)
#fmat1D(survey.net = dns, units = "mm")


file_path <- "D:/R_projects/Surveyer/Data/Input/With_observations/DNS_1D/DNS_1D_nulta.xlsx"
file_path <- here::here("Data/Input/With_observations/Brana/Brana.xlsx")
file_path <- here::here("Data/Input/With_observations/Makis/Makis_observations.xlsx")
file_path <- here::here("Data/Input/With_observations/Zadatak 1/Zadatak_1.xlsx")
dns <- read_surveynet(file = file_path)
survey.net = dns
dim_type = "2D"
result.units = "mm"
prob = 0.99
adjust = TRUE
sd.apriori = 1
coord_tolerance = 1e-2
maxiter = 50
teta.unit = list("deg", "rad"); units.dir = "sec"; units.dist = "mm"; use.sd.estimated = TRUE; all = FALSE; ellipse.scale = 1

adjust.snet <- function(adjust = TRUE, survey.net, dim_type = list("1D", "2D"), sd.apriori = 1, maxiter = 50, prob = 0.95, coord_tolerance = 1e-3, result.units = list("mm", "cm", "m"), ellipse.scale = 1, teta.unit = list("deg", "rad"), units.dir = "sec", units.dist = "mm", use.sd.estimated = TRUE, all = TRUE){
  dim_type <- dim_type[[1]]
  "%!in%" <- Negate("%in%")
  if(!adjust){use.sd.estimated <- FALSE}
  units <- result.units[[1]]
  res.unit.lookup <- c("mm" = 1000, "cm" = 100, "m" = 1)
  disp.unit.lookup <- c("mm" = 2, "cm" = 3, "m" = 4)

  # TODO: This has to be solved within the read.surveynet function
  used.points <- unique(c(survey.net$observations$from, survey.net$observations$to))
  if(!!any(used.points %!in% survey.net[[1]]$Name)) stop(paste("There is no coordinates for point", used.points[which(used.points %!in% survey.net[[1]]$Name)]), sep = " ")
  survey.net[[1]] <- survey.net[[1]][which(survey.net[[1]]$Name %in% used.points), ]

  fix.mat <- !cbind(survey.net[[1]]$FIX_1D, survey.net[[1]]$FIX_2D)


  # Model
  if(dim_type == "2D"){
    observations <- tidyr::gather(survey.net[[2]] %>% purrr::when(is(., "sf") ~ st_drop_geometry(.), ~.) %>% dplyr::select(from, to, direction, distance), key = type, value = used, -c(from, to)) %>%
      dplyr::filter(used == TRUE) %>%
      dplyr::mutate(from_to = str_c(.$from, .$to, sep = "_"))

    fix.mat2D <- !rep(survey.net[[1]]$FIX_2D, each = 2)
    if(length(fix.mat2D) != sum(fix.mat2D)){
      df <- (dim(observations)[1] - sum(fix.mat2D)) #abs(diff(dim(A.mat)))
    }else{
      df <- (dim(observations)[1] - sum(fix.mat2D)) + 3
    }
    if(adjust){
      e <- 1
      iter <- 0
      coords.iter_0 <- as.vector(t(cbind(survey.net[[1]]$x, survey.net[[1]]$y)))[fix.mat2D]
      while (e > coord_tolerance && iter < maxiter) {
        iter <- iter + 1
        coords.iter.inc <- as.vector(t(cbind(survey.net[[1]]$x, survey.net[[1]]$y)))[fix.mat2D]
        A.mat <- Amat(survey.net, units = units) # Nije dobro u drugoj iteraciji
        W.mat <- Wmat(survey.net, sd.apriori = sd.apriori)
        rownames(A.mat) <- observations$from_to
        colnames(W.mat) <- observations$from_to
        rownames(W.mat) <- observations$from_to
        # MNK solution
        N.mat <- crossprod(A.mat, W.mat) %*% A.mat
        Qx.mat <- tryCatch(
          {
            x = Qx.mat = solve(N.mat)
          },
          error = function(e) {
            x = Qx.mat = MASS::ginv(N.mat)
          })
        colnames(Qx.mat) <- colnames(N.mat)
        rownames(Qx.mat) <- rownames(N.mat)
        f.mat <- fmat(survey.net = survey.net, units.dir = units.dir, units.dist = units)
        n.mat <- crossprod(A.mat, W.mat) %*% f.mat
        x.mat <- -Qx.mat %*% n.mat
        v.mat <- A.mat%*%x.mat + f.mat
        Ql.mat <- A.mat %*% tcrossprod(Qx.mat, A.mat)
        Qv.mat <- solve(W.mat) - Ql.mat
        r <- Qv.mat%*%W.mat
        coords.est <- coords.iter.inc + x.mat[1:sum(fix.mat2D)]/res.unit.lookup[units]
        survey.net[[1]][!survey.net[[1]]$FIX_2D, c("x", "y")] <- matrix(coords.est, ncol = 2, byrow = TRUE)
        survey.net[[1]] <- survey.net[[1]] %>% st_drop_geometry() %>% sf::st_as_sf(coords = c("x","y"), remove = FALSE)
        survey.net[[2]]$x_from <- survey.net[[1]]$x[match(survey.net[[2]]$from, survey.net[[1]]$Name)]
        survey.net[[2]]$y_from <- survey.net[[1]]$y[match(survey.net[[2]]$from, survey.net[[1]]$Name)]
        survey.net[[2]]$x_to <- survey.net[[1]]$x[match(survey.net[[2]]$to, survey.net[[1]]$Name)]
        survey.net[[2]]$y_to <- survey.net[[1]]$y[match(survey.net[[2]]$to, survey.net[[1]]$Name)]
        survey.net[[2]] <- as.data.table(survey.net[[2]]) %>% dplyr::mutate(id = seq.int(nrow(.))) %>% split(., f = as.factor(.$id)) %>%
          lapply(., function(row) {lmat <- matrix(unlist(row[c("x_from", "y_from", "x_to", "y_to")]), ncol = 2, byrow = TRUE)
          st_linestring(lmat)}) %>%
          sf::st_sfc() %>%
          sf::st_sf(survey.net[[2]], 'geometry' = .)
        e <- max(coords.est-coords.iter.inc)
      }
      x.mat <- x.mat[1:sum(fix.mat2D)]  #(coords.est-coords.iter_0)*res.unit.lookup[units]
      sd.estimated <- sqrt((crossprod(v.mat, W.mat) %*% v.mat)/(df))
      model_adequacy <- model_adequacy_test(sd.apriori, sd.estimated, df, prob = prob)
      if(!model_adequacy){
        tds <- data.frame(Observation = rownames(A.mat), statistics = as.numeric(abs(v.mat)/(sd.apriori*sqrt(diag(Qv.mat))))) %>% dplyr::arrange(., desc(statistics))
        print("Check the statistics for individual observations. Suggestion: Remove the observation with the highest value of the statistics:")
        print(tds)
        stop()
      }
      if(use.sd.estimated){sd.apriori <- sd.estimated}
      # Results
      coords.inc <- data.frame(parameter = colnames(A.mat)[1:sum(fix.mat2D)], coords.inc = as.numeric(x.mat))
      coords.inc <- coords.inc %>%
        # `colnames<-`(c("parameter", "coords.inc")) %>%
        # dplyr::rename(coords.inc = .) %>%
        tidyr::separate(.,col = parameter, into = c("Name", "inc.name"), sep = "_") %>%
        tidyr::pivot_wider(., names_from = c(inc.name), values_from = c(coords.inc)) %>%
        dplyr::mutate_all(., ~replace(., is.na(.), 0))

      point.adj.results <- dplyr::left_join(survey.net[[1]], coords.inc, by = "Name") %>%
        dplyr::mutate_at(., .vars = c("dx", "dy"), ~replace(., is.na(.), 0)) %>%
        #cbind(., st_coordinates(.)) %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(x0 = x - dx/res.unit.lookup[units], y0 = y - dy/res.unit.lookup[units]) %>%
        dplyr::mutate_at(., .vars = c("dx", "dy"), round, disp.unit.lookup[units]) %>%
        sf::st_as_sf(coords = c("x","y"), remove = FALSE) %>%
        dplyr::select(id, Name, x0, y0, dx, dy, x, y, h, FIX_2D, Point_object, geometry)
      # TODO: Gubi se projekcija!!!!
      # TODO: Srediti oko velikog i malog X i Y.
    }else{
      A.mat <- Amat(survey.net, units = units)
      W.mat <- Wmat(survey.net, sd.apriori = sd.apriori)
      rownames(A.mat) <- observations$from_to
      colnames(W.mat) <- observations$from_to
      rownames(W.mat) <- observations$from_to
      # MNK solution
      N.mat <- crossprod(A.mat, W.mat) %*% A.mat
      Qx.mat <- tryCatch(
        {
          x = Qx.mat = solve(N.mat)
        },
        error = function(e) {
          x = Qx.mat = MASS::ginv(N.mat)
        })
      colnames(Qx.mat) <- colnames(N.mat)
      rownames(Qx.mat) <- rownames(N.mat)
      Ql.mat <- A.mat %*% tcrossprod(Qx.mat, A.mat)
      Qv.mat <- solve(W.mat) - Ql.mat
      r <- Qv.mat%*%W.mat
    }
    # Computing error ellipses
    Qxy.list <- Qxy(Qx.mat, fix = fix.mat[, 2])
    ellipses <- lapply(Qxy.list, function(x) error.ellipse(x, prob = prob, sd.apriori = sd.apriori, teta.unit = teta.unit[[1]]))
    ellipses <- do.call(rbind, ellipses) %>%
      as.data.frame() %>%
      dplyr::select(A = V1, B = V2, teta = V3) %>%
      mutate(Name = used.points) %>%
      dplyr::mutate_if(is.numeric, round, disp.unit.lookup[units])
    # Computing parameters sigmas
    sigmas <- lapply(Qxy.list, function(x) sigma.xy(x, sd.apriori = sd.apriori)) %>%
      do.call(rbind,.) %>%
      as.data.frame() %>%
      dplyr::select(sx = V1, sy = V2) %>% #TODO: proveriti da li ovde treba voditi racuna o redosledu sx i sy.
      dplyr::mutate(sp = sqrt(sx^2 + sy^2), Name = used.points) %>%
      dplyr::mutate_if(is.numeric, round, disp.unit.lookup[units])

    if(adjust){
      survey.net[[1]] <- merge(point.adj.results, ellipses, by = "Name") %>% merge(., sigmas)
    }else{
      survey.net[[1]] <- merge(survey.net[[1]], ellipses, by = "Name") %>% merge(., sigmas)
    }

      # Preparing ellipses as separate sf outcome
      # TODO Proveriti da li elipse uzimaju definitivne koordinate ili priblizne!
      ellipse.net <- do.call(rbind, lapply(split(survey.net[[1]], survey.net[[1]]$Name), function(x) sf.ellipse(x, scale = ellipse.scale)))
      ellipse.net <- merge(ellipse.net, sigmas)
      ellipse.net <- sf::st_set_crs(ellipse.net, value = st_crs(survey.net[[2]]))

  }else{
    observations <- tidyr::gather(survey.net[[2]] %>% purrr::when(is(., "sf") ~ st_drop_geometry(.), ~.) %>% dplyr::select(from, to, diff_level), key = type, value = used, -c(from, to)) %>%
      dplyr::filter(used == TRUE) %>%
      dplyr::mutate(from_to = str_c(.$from, .$to, sep = "_"))
    fix.mat1D <- !(survey.net[[1]]$FIX_1D)

    A.mat <- Amat1D(survey.net)
    W.mat <- Wmat1D(survey.net = survey.net, wdh_model = wdh_model, sd0 = 1, d0 = NA, n0 = 1)
    rownames(A.mat) <- observations$from_to
    colnames(W.mat) <- observations$from_to
    rownames(W.mat) <- observations$from_to
    if(length(fix.mat1D) != sum(fix.mat1D)){
      df <- (dim(observations)[1] - sum(fix.mat1D)) #abs(diff(dim(A.mat)))
    }else{
      df <- (dim(observations)[1] - sum(fix.mat1D)) + 1
    }
    # MNK solution
    N.mat <- crossprod(A.mat, W.mat) %*% A.mat
    Qx.mat <- tryCatch(
      {
        x = Qx.mat = solve(N.mat)
      },
      error = function(e) {
        x = Qx.mat = MASS::ginv(N.mat)
      })
    colnames(Qx.mat) <- colnames(N.mat)
    rownames(Qx.mat) <- rownames(N.mat)
    Ql.mat <- A.mat %*% tcrossprod(Qx.mat, A.mat)
    Qv.mat <- solve(W.mat) - Ql.mat
    r <- Qv.mat%*%W.mat
    if(adjust){
      e <- 1
      iter <- 0
      coords.iter_0 <- as.vector(t(cbind(survey.net[[1]]$h, survey.net[[1]]$y)))[fix.mat1D]
      while (e > coord_tolerance && iter < maxiter) {
        iter <- iter + 1
        coords.iter.inc <- survey.net[[1]]$h[fix.mat1D]
        f.mat <- fmat1D(survey.net = survey.net, units = units)
        n.mat <- crossprod(A.mat, W.mat) %*% f.mat
        x.mat <- -Qx.mat %*% n.mat
        v.mat <- A.mat%*%x.mat + f.mat
        survey.net[[1]]$h[fix.mat1D] <- survey.net[[1]]$h[fix.mat1D] + x.mat/res.unit.lookup[units]
        e <- max(abs(survey.net[[1]]$h[fix.mat1D]-coords.iter.inc))
      }
      x.mat <- x.mat[1:sum(fix.mat1D)]  #(coords.est-coords.iter_0)*res.unit.lookup[units]
      sd.estimated <- sqrt((crossprod(v.mat, W.mat) %*% v.mat)/(df))
      model_adequacy <- model_adequacy_test(sd.apriori, sd.estimated, df, prob = prob)
      if(!model_adequacy){
        tds <- data.frame(Observation = rownames(A.mat), statistics = as.numeric(abs(v.mat)/(sd.apriori*sqrt(diag(Qv.mat))))) %>% dplyr::arrange(., desc(statistics))
        print("Check the statistics for individual observations. Suggestion: Remove the observation with the highest value of statistics:")
        print(tds)
        stop()
      }
    }
  }

  if(adjust){
    observations <- observations %>% dplyr::mutate(v = v.mat, Ql = diag(Ql.mat), Qv = diag(Qv.mat), rii = diag(r)) %>%
      dplyr::mutate_if(is.numeric, round, disp.unit.lookup[units])
  }else{
    observations <- observations %>% dplyr::mutate(Ql = diag(Ql.mat), Qv = diag(Qv.mat), rii = diag(r)) %>%
      dplyr::mutate_if(is.numeric, round, disp.unit.lookup[units])
  }

  if(sum(rowSums(is.na(survey.net[[1]][, c("x", "y")])) != 0) == 0){
    observations <- observations %>%
      dplyr::mutate(x_from = point.adj.results$x[match(observations$from, point.adj.results$Name)],
                    y_from = point.adj.results$y[match(observations$from, point.adj.results$Name)],
                    x_to = point.adj.results$x[match(observations$to, point.adj.results$Name)],
                    y_to = point.adj.results$y[match(observations$to, point.adj.results$Name)])

    observations <- observations %>%
      as.data.table(.) %>%
      dplyr::mutate(id = seq.int(nrow(.))) %>%
      split(., f = as.factor(.$id)) %>%
      lapply(., function(row) {lmat <- matrix(unlist(row[c("x_from", "y_from", "x_to", "y_to")]), ncol = 2, byrow = TRUE)
      st_linestring(lmat)}) %>%
      sf::st_sfc() %>%
      sf::st_sf('ID' = seq.int(nrow(observations)), observations, 'geometry' = .) %>%
      dplyr::select(-c(x_from, y_from, x_to, y_to))
  }

  observations %<>% sf::st_set_crs(value = st_crs(survey.net[[2]]))

  design <- list(design.matrices = list(A = A.mat, W = W.mat, Qx = Qx.mat, Ql = Ql.mat, Qv = Qv.mat), ellipse.net = ellipse.net, net.points = survey.net[[1]], observations = observations)
  design$net.points <- sf::st_set_crs(design$net.points, value = st_crs(survey.net[[2]]))

  if(all){
    return(design)
  }else{
    return(design[-1])
  }

}

file_path <- "D:/R_projects/Surveyer/Data/Input/With_observations/DNS_1D/DNS_1D_nulta.xlsx"
file_path <- here::here("Data/Input/With_observations/Brana/Brana.xlsx")
file_path <- here::here("Data/Input/With_observations/Makis/Makis_observations.xlsx")
file_path <- here::here("Data/Input/With_observations/Zadatak 1/Zadatak_1.xlsx")


adjust.snet(adjust = FALSE, survey.net = file_path, dim_type = "1D")





