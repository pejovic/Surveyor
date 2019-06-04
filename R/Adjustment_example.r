
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
library(leaflet.extras)
library(here)
library(matlib)
library(nngeo)
library(writexl)

source(here("/R/functions.r"))

avala_points <- readxl::read_xlsx(path = here::here("Data/Input/With_observations/Avala/Avala_observations.xlsx"), sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
avala_obs <- readxl::read_xlsx(path = here::here("Data/Input/With_observations/Avala/Avala_observations.xlsx"), sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
avala <- surveynet2DAdjustment_Import_fun.xlsx(points = avala_points, observations = avala_obs, dest_crs = 3857)

# Creating Hz, Vz in decimal degrees and computing horizontal distances and filtering observations to "S" points.
avala[[2]] <- mutate(avala[[2]], Hz = HzD + HzM/60 + HzS/3600, Vz = VzD + VzM/60 + VzS/3600, HD = SD*sin(Vz*pi/180)) %>%
  dplyr::filter(To %in% c("S1","S2","S3","S4","S5","S6","S7"))


avala[[2]]


# Zadatak 1

ni <- function(pt1_coords, pt2_coords, type = list("dec", "dms", "rad"), axes = c("Easting", "Northing")){
  ## check if the axis were set:
  if(length(axes) < 2) stop("axes must be defined with East and North")
  if(!any(axes %in% list("Northing", "Easting"))){ stop(paste(type, "axes must be North and East.")) }

  north_ind <- which(axes == "Northing")
  east_ind <- which(axes == "Easting")

  ## check if the type exists:
  if(length(type) > 1){ type <- type[[1]] }
  if(!any(type %in% list("dms", "dec", "rad"))){ stop(paste(type, "method not available.")) }

  ## body
  east1 <- pt1_coords[east_ind]
  north1 <- pt1_coords[north_ind]
  east2 <- pt2_coords[east_ind]
  north2 <- pt2_coords[north_ind]
  deast <- as.numeric(east2 - east1)
  dnorth <- as.numeric(north2 - north1)
  atg <- ifelse(deast < 0, atan(dnorth/deast)*180/pi + 180, atan(dnorth/deast)*180/pi)
  ang <- ifelse(atg < 0, atg + 360, atg)

  deg <- floor(ang); minut <- floor((ang-deg)*60); sec <- ((ang-deg)*60-minut)*60

  if(type == "dms"){
    ang <- c(deg, minut, sec)
    names(ang) <- c("deg","min","sec")
  }
  if(type == "rad"){
    ang <- ang*pi/180
  }

  return(ang)
}


points = avala_points; observations = avala_obs


import_surveynet2D <- function(points = points, observations = observations, dest_crs = NA){

  # Create geometry columns for points
  if (is.na(dest_crs)){
    dest_crs <- 3857
  }

  observations$x_station <- points$x[match(observations$from, points$Name)]
  observations$y_station <- points$y[match(observations$from, points$Name)]
  observations$x_obs.point <- points$x[match(observations$to, points$Name)]
  observations$y_obs.point <- points$y[match(observations$to, points$Name)]

  points <- points %>% as.data.frame() %>% sf::st_as_sf(coords = c("x","y")) %>% sf::st_set_crs(dest_crs)

  observations <- observations %>% dplyr::mutate(Hz = HzD + HzM/60 + HzS/3600,
                                                 Vz = VzD + VzM/60 + VzS/3600,
                                                 distance = (!is.na(.$HD) | !is.na(.$SD)),
                                                 direction = !is.na(Hz))

  observations <- as.data.table(observations) %>% dplyr::mutate(id = seq.int(nrow(.))) %>% split(., f = as.factor(.$id)) %>%
        lapply(., function(row) {lmat <- matrix(unlist(row[14:17]), ncol = 2, byrow = TRUE)
                                         st_linestring(lmat)}) %>%
        sf::st_sfc() %>%
        sf::st_sf('ID' = seq.int(nrow(observations)), observations, 'geometry' = .)

  # Creating list
  survey_net <- list(points, observations = observations)
  return(survey_net)
}


survey.net <- avala
st.survey.net <- survey.net[[2]] %>% dplyr::filter(direction) %>% sf::st_drop_geometry() %>% dplyr::filter(from == "S4")


fdir_st <- function(st.survey.net, units.dir = "sec", axes = c("Easting", "Northing")){
  units.table <- c("sec" = 3600, "min" = 60, "deg" = 1)
  st.survey.net <- st.survey.net %>% split(., f = as.factor(.$to)) %>%
    lapply(., function(x) {x$ni = ni(pt1_coords = as.numeric(x[, c("y_station", "x_station")]), pt2_coords = as.numeric(x[, c("y_obs.point", "x_obs.point")]), type = "dec", axes = c("Easting", "Northing")); return(x)}) %>%
    do.call(rbind,.) %>%
    dplyr::mutate(z = Hz-ni) %>%
    dplyr::arrange(ID)
  st.survey.net$z <- ifelse(st.survey.net$z < 0, st.survey.net$z + 360, st.survey.net$z)
  z0_mean <- mean(st.survey.net$z)
  st.survey.net$Hz0 <- z0_mean + st.survey.net$ni
  st.survey.net$Hz0 <- ifelse(st.survey.net$z > 360, st.survey.net$Hz0 - 360, st.survey.net$Hz0)
  f <- (st.survey.net$Hz0 - st.survey.net$Hz)
  f <- ifelse(f >= 360 | f >= 355, f - 360, f)*units.table[units.dir]
  return(f)
}



fdir_st(st.survey.net = st.survey.net, units = "sec")

fmat <- function(survey.net, units.dir = "sec", units.dist = "mm", axes = c("Easting", "Northing")){
  f_dir <- survey.net[[2]] %>% dplyr::filter(direction) %>% st_drop_geometry() %>% split(.,.$from) %>%
           lapply(., function(x) fdir_st(x, units.dir = units.dir)) %>%
           do.call(c, .) %>% as.numeric()
  dist.units.table <- c("mm" = 1000, "cm" = 100, "m" = 1)
  survey.net[[2]] <- survey.net[[2]] %>% dplyr::filter(distance) %>% st_drop_geometry() %>%
    dplyr::mutate(dist0 = sqrt((x_station-x_obs.point)^2+(y_station-y_obs.point)^2))
  f_dist <- (survey.net[[2]]$dist0 - survey.net[[2]]$HD)*dist.units.table[units.dist]
  return(c(f_dir, f_dist))
}

# Wmat je ista, samo je promenjen naziv standarda. Stavljeni su "sd_Hz" i "sd_dist".
Wmat <- function(survey.net, apriori = 1){
  #TODO: Omoguciti zadavanje i drugih kovariacionih formi izmedju merenja.
  obs.data <- rbind(survey.net[[2]] %>% st_drop_geometry() %>%
                      dplyr::filter(direction) %>%
                      dplyr::select(from, to, standard = sd_Hz) %>%
                      dplyr::mutate(type = "direction"),
                    survey.net[[2]] %>% st_drop_geometry() %>%
                      dplyr::filter(distance) %>%
                      dplyr::select(from, to, standard = sd_dist) %>%
                      dplyr::mutate(type = "distance")
  )
  return(diag(apriori^2/obs.data$standard^2))
}

Amat <- function(survey.net, units, axes = c("Easting", "Northing")){

  A_dir <- survey.net[[2]] %>% dplyr::filter(direction) %>% st_coordinates() %>% as.data.frame() %>% mutate_at(vars(L1), list(name = ~factor)) %>%
    split(., .$L1) %>%
    lapply(., function(x) coef_p(pt1 = x[1, 1:2], pt2 = x[2, 1:2], pts = st_coordinates(survey.net[[1]][, 1:2]), units = units, axes = axes)) %>%
    do.call(rbind, .)

  A_dist <- survey.net[[2]] %>% dplyr::filter(distance) %>% st_coordinates() %>% as.data.frame() %>% mutate_at(vars(L1), list(name = ~factor)) %>%
    split(., .$L1) %>%
    lapply(., function(x) coef_d(pt1 = x[1, 1:2], pt2 = x[2, 1:2], pts = st_coordinates(survey.net[[1]][, 1:2]), units = units, axes = axes)) %>%
    do.call(rbind, .)

  Z_mat <- survey.net[[2]] %>% dplyr::filter(direction) %>%
    tidyr::spread(key = from, value = direction, fill = FALSE) %>%
    dplyr::select(survey.net[[1]]$Name[!survey.net[[1]]$Point_object]) %>%
    st_drop_geometry() %>%
    as.matrix()*1

  fix <- fix.params(net.points = survey.net[[1]], axes = axes)

  rest_mat <- matrix(0, nrow = dim(A_dist)[1], ncol = dim(Z_mat)[2])

  A <- cbind(rbind(A_dir, A_dist)[, !fix], rbind(Z_mat, rest_mat))

  if(("Easting" == axes)[1]) {sufix <- c("dE", "dN")} else {sufix <- c("dN", "dE")}
  colnames(A) <- c(paste(rep(survey.net[[1]]$Name, each = 2), rep(sufix, length(survey.net[[1]]$Name)), sep = "_")[!fix], paste(colnames(Z_mat), "z", sep = "_"))
  return(A)
}



z_points <- readxl::read_xlsx(path = here::here("Data/Input/With_observations/Zadatak 1/Zadatak_1.xlsx"), sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
z_obs <- readxl::read_xlsx(path = here::here("Data/Input/With_observations/Zadatak 1/Zadatak_1.xlsx"), sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric", "numeric","numeric", "numeric", "numeric"))
zadatak1 <- import_surveynet2D(points = z_points, observations = z_obs, dest_crs = NA)


A <- Amat(survey.net = zadatak1, units = "mm")
f <- data.frame(f = fmat(survey.net = zadatak1))
P <- data.frame(Wmat(survey.net = zadatak1, apriori = 5))

zadatak1.list <- list("A" = A, "f" = f, "P" = P)

write_xlsx(zadatak1.list, path = "zadatak1_design.xlsx")

# Avala


avala_points <- readxl::read_xlsx(path = here::here("Data/Input/With_observations/Avala/Avala_observations.xlsx"), sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
avala_obs <- readxl::read_xlsx(path = here::here("Data/Input/With_observations/Avala/Avala_observations.xlsx"), sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric", "numeric","numeric", "numeric", "numeric")) %>%
  dplyr::filter(., to %in% c("S1","S2","S3","S4","S5","S6","S7"))


avala <- import_surveynet2D(points = avala_points, observations = avala_obs, dest_crs = 3857)
A <- Amat(survey.net = avala, units = "mm")
f <- data.frame(f = fmat(survey.net = avala))
P <- data.frame(Wmat(survey.net = zadatak1, apriori = 5))



# Makis


Makis_points <- readxl::read_xlsx(path = here::here("Data/Input/With_observations/Makis/Makis_observations.xlsx"), sheet = "Points", col_types = c("numeric", "text", "numeric", "numeric", "logical", "logical", "logical"))
Makis_obs <- readxl::read_xlsx(path = here::here("Data/Input/With_observations/Makis/Makis_observations.xlsx"), sheet = "Observations", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric","numeric","numeric", "numeric", "numeric","numeric", "numeric", "numeric"))


Makis <- import_surveynet2D(points = Makis_points, observations = Makis_obs, dest_crs = 3857)
A <- Amat(survey.net = Makis, units = "mm")
f <- data.frame(f = fmat(survey.net = Makis))
P <- data.frame(Wmat(survey.net = Makis, apriori = 5))


