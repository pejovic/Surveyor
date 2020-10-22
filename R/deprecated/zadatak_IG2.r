
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

reclass <- function (df, vec_types) {
  for (i in 1:ncol(df)) {
    type <- vec_types[i]
    class(df[ , i]) <- type
  }
  return(df)
}

create_surveynet <- function(points, observations, dest_crs = NA, axes = c("Easting", "Northing")){
  points_col_type <- c("numeric", "text", "numeric", "numeric", "numeric", "logical", "logical", "logical")
  obs_col_type <- c("numeric", "text", "text", rep("numeric", 19))
  # reading data
  points <- reclass(df = as.data.frame(points), vec_types = points_col_type) %>% mutate_at(., .vars = c("Name"), as.character)
  observations <- reclass(df = as.data.frame(observations), vec_types = obs_col_type) %>% mutate_at(., .vars = c("from", "to"), as.character)

  if(sum(rowSums(is.na(points[, c("x", "y")])) != 0) != 0){
    warning("Network has no spatial coordinates")}else{
      # Creating sf class from observations
      observations$x_from <- points$x[match(observations$from, points$Name)]
      observations$y_from <- points$y[match(observations$from, points$Name)]
      observations$x_to <- points$x[match(observations$to, points$Name)]
      observations$y_to <- points$y[match(observations$to, points$Name)]

      points <- points %>% as.data.frame() %>% sf::st_as_sf(coords = c("x","y"), remove = FALSE)
      if(which(axes == "Easting") == 2){points <- points %>% dplyr::rename(x = y,  y = x)}

      observations <- observations %>% dplyr::mutate(id = seq.int(nrow(.))) %>% split(., f = as.factor(.$id)) %>%
        lapply(., function(row) {lmat <- matrix(unlist(row[c("x_from", "y_from", "x_to", "y_to")]), ncol = 2, byrow = TRUE)
        st_linestring(lmat)}) %>%
        sf::st_sfc() %>%
        sf::st_sf(observations, 'geometry' = .) %>%
        dplyr::mutate(ID = 1:n())
    }



  #if(sum(rowSums(is.na(observations[, c("HzD", "HzM", "HzS")])) != 0) != 0){stop("There is uncomplete observations")}


  observations <- observations %>% dplyr::mutate(Hz = HzD + HzM/60 + HzS/3600,
                                                 Vz = VzD + VzM/60 + VzS/3600,
                                                 tdh = SD*cos(Vz*pi/180),
                                                 distance = (!is.na(HD) | !is.na(SD)) & !is.na(sd_dist),
                                                 direction = !is.na(Hz) & !is.na(sd_Hz),
                                                 diff_level = (!is.na(dh) | !is.na(tdh)))

  # In network design, observation is included if measurement standard is provided
  if(observations %>% purrr::when(is(., "sf") ~ st_drop_geometry(.), ~.) %>% dplyr::select(HzD, HzM, HzS) %>% is.na() %>% all()){
    observations$direction[!is.na(observations$sd_Hz)] <- TRUE
  }
  if(observations %>% purrr::when(is(., "sf") ~ st_drop_geometry(.), ~.) %>% dplyr::select(HD, SD) %>% is.na() %>% all()){
    observations$distance[!is.na(observations$sd_dist)] <- TRUE
  }
  if(observations %>% purrr::when(is(., "sf") ~ st_drop_geometry(.), ~.) %>% dplyr::select(dh) %>% is.na() %>% all()){
    observations$diff_level[(!is.na(observations$dh) | !is.na(observations$sd_dh) | !is.na(observations$d_dh) | !is.na(observations$n_dh))] <- TRUE
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



# Read mreza file
mreza <- read_surveynet(file = here::here("Data/Simulation/TETO_extended.xlsx"))
# Simulation
mreza_sim <- sim_snetobs(survey.net = mreza, red = TRUE, seed = 1018)
# Create list and write file
mreza_list <- list(Points = mreza_sim[[1]] %>% st_drop_geometry(), Observations = mreza_sim[[2]] %>% st_drop_geometry() %>% mutate_at(.vars = c("e_cent_from", "e_cent_to", "e_focus", "e_air"), .funs = ~NA) %>% dplyr::select(from:e_air)) %>%
  writexl::write_xlsx(., path = here::here("Data/Simulation/TETO_extended_sim.xlsx"))
# Read simulated mreza
mreza_sim <- read_surveynet(file = here::here("Data/Simulation/TETO_extended_sim.xlsx"))
mreza_sim$observations$sd_Hz <- 10
mreza_sim$observations$sd_dist <- 3
plot_surveynet(snet = mreza_sim, webmap = FALSE, net.1D = FALSE, net.2D = TRUE)
mreza_adj <- adjust.snet(adjust = TRUE, survey.net = mreza_sim, dim_type = "2D", sd.apriori = 3,  all = TRUE, prob = 0.95)



# Objekat
j4 <- readxl::read_xlsx(here::here("Data/Simulation/j4_cross.xlsx"))

j4 <- j4 %>% tidyr::separate(., col = "Tacka", into = c("h", "v"), sep = "/")
vv <- sample(paste(c(3:7)), size = 2)
hh <- sample(c("TA", "TB", "TC", "T"), size = 2)

object_points <- j4 %>% dplyr::filter(h %in% hh & v %in% vv) %>% dplyr::mutate(id = 1:n(), Name = paste(h, v, sep = "_"), h = NA, FIX_2D = FALSE, FIX_1D = NA, Point_object = TRUE) %>% dplyr::select(id, Name, x = Y, y = X, h, FIX_2D, FIX_1D, Point_object)# %>% sf::st_as_sf(coords = c("X","Y"), remove = FALSE)

stations <- sample(mreza_adj[[2]][[1]]$Name, 3)

station_points <- mreza_adj[[2]][[1]] %>% st_drop_geometry() %>% dplyr::filter(Name %in% stations) %>% dplyr::select(id, Name, x = x, y = y, h, FIX_2D, Point_object) %>% dplyr::mutate(FIX_2D = TRUE, FIX_1D = NA)

points <- rbind(station_points, object_points)

target_obs <- data.frame(from = as.character(rep(station_points$Name, each = dim(object_points)[1])), to = as.character(rep(object_points$Name, dim(station_points)[1])), stringsAsFactors = FALSE) %>% mutate(ID = 1:n(), HzD = NA, HzM = NA, HzS = NA, HD = NA, SD = NA, VzD = NA, VzM = NA, VzS = NA, dh = NA, sd_Hz = 7, sd_dist = 3, sd_Vz = NA, sd_dh = NA, d_dh = NA, n_dh = NA, e_cent_from = 0.8, e_cent_to = 1.2, e_focus = 0, e_air = 0.6) %>% dplyr::select(ID, everything())

net_obs <- mreza[[2]] %>% dplyr::filter(from %in% stations & to %in% stations) %>% st_drop_geometry() %>% dplyr::select(from, to) %>%
    mutate(ID = 1:n(), HzD = NA, HzM = NA, HzS = NA, HD = NA, SD = NA, VzD = NA, VzM = NA, VzS = NA, dh = NA, sd_Hz = 3, sd_dist = 3, sd_Vz = NA, sd_dh = NA, d_dh = NA, n_dh = NA, e_cent_from = 0.8, e_cent_to = 0.8, e_focus = 0, e_air = 0.6) %>% dplyr::select(ID,from, to, HzD:e_air)

l1 <- split(target_obs, f = factor(target_obs$from, levels = unique(target_obs$from)))
l2 <- split(net_obs, f = factor(net_obs$from, levels = unique(target_obs$from)))

observations <- purrr::map2(.x = l2, .y = l1, .f = rbind) %>% do.call(rbind, .) %>% dplyr::mutate(ID = 1:n())

mreza_ispit <- create_surveynet(points = points, observations = observations)

mreza_ispit_sim <- sim_snetobs(survey.net = mreza_ispit, red = TRUE, seed = 2020)


mreza_list <- list(Points = mreza_ispit_sim[[1]] %>% st_drop_geometry() %>% dplyr::mutate_at(.vars = c("x", "y"), ~round(., 3)), Observations = mreza_ispit_sim[[2]] %>% st_drop_geometry() %>% mutate_at(.vars = c("sd_Hz", "sd_dist", "e_cent_from", "e_cent_to", "e_focus", "e_air"), .funs = ~NA) %>% dplyr::select(from:e_air)%>% split(., f = factor(.$from, levels = unique(.$from))) %>% lapply(., function(x) dplyr::arrange(x, HzD)) %>% do.call(rbind, .))



mreza_list %>% writexl::write_xlsx(., path = here::here("Data", "ispit_10_6_2020.xlsx"))
ispit <- read_surveynet(file = here::here("Data", "ispit_10_6_2020.xlsx"))

mreza_sim_ispit <- create_surveynet(points = ispit[[1]] %>% dplyr::select(id,Name,x,y,h,FIX_2D,FIX_1D,Point_object) %>% st_drop_geometry(), observations = ispit[[2]] %>% mutate(ID = 1:n()) %>% dplyr::select(ID,from:e_air) %>% st_drop_geometry())


mreza_sim_ispit[[2]] <- mreza_sim_ispit[[2]] %>% dplyr::mutate(Hz = HzD + HzM/60 + HzS/3600,
                                                               Vz = VzD + VzM/60 + VzS/3600,
                                                               tdh = SD*cos(Vz*pi/180),
                                                               distance = (!is.na(HD) | !is.na(SD)) | !is.na(sd_dist),
                                                               direction = !is.na(Hz) | !is.na(sd_Hz),
                                                               diff_level = (!is.na(dh) | !is.na(tdh)))

mreza_sim_ispit$observations$sd_Hz <- 5
mreza_sim_ispit$observations$sd_dist <- 3

mreza_adj_ispit <- adjust.snet(adjust = TRUE, survey.net = mreza_sim_ispit, dim_type = "2D", sd.apriori = 5,  all = TRUE, prob = 0.95, maxiter = 1)

plot_surveynet(snet = mreza_sim_ispit, webmap = FALSE, net.1D = FALSE, net.2D = TRUE)


save(mreza_adj_ispit, file = "resenje_10062020.Rdata")



