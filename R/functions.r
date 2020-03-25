# Project: Surveyer
# Description: Land and Engineering Surveying utilities
# Authors: Milutin Pejovic, Petar Bursac, Milan Kilibarda, Branislav Bajat, Aleksandar Sekulic
# Date:

# Functions:

dist <- function(pt1_coords, pt2_coords){
  dEasting <- as.numeric(pt2_coords[1] - pt1_coords[1])
  dNorthing <- as.numeric(pt2_coords[2] - pt1_coords[2])
  distance <- sqrt(dEasting^2 + dNorthing^2)
  return(distance)
}



ni <- function(pt1_coords, pt2_coords, type = list("dec", "dms", "rad")){
  ## check if the type exists:
  if(length(type) > 1){ type <- type[[1]]}
  if(!any(type %in% list("dms", "dec", "rad"))){stop(paste(type, "method not available."))}

  ## body
  dEasting <- as.numeric(pt2_coords[1] - pt1_coords[1])
  dNorthing <- as.numeric(pt2_coords[2] - pt1_coords[2])
  atg <- ifelse(dNorthing < 0, atan(dEasting/dNorthing)*180/pi + 180, atan(dEasting/dNorthing)*180/pi)
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

### coeficients for distances #####################################################
coef_d <- function (pt1, pt2, pts, units) {
  units.table <- c("mm" = 1000, "cm" = 100, "m" = 1)
  pt1 <- as.numeric(pt1)
  pt2 <- as.numeric(pt2)
  coords <- pts
  vec_d <- c(rep(0, dim(pts)[1]*2))#c(rep(0, length(coords)))

  y_coords <- coords[, 2]
  x_coords <- coords[, 1]
  y1 <- pt1[2]
  x1 <- pt1[1]
  y2 <- pt2[2]
  x2 <- pt2[1]
  dy1 <- (y_coords-y1)
  dx1 <- (x_coords-x1)
  dy2 <- (y_coords-y2)
  dx2 <- (x_coords-x2)
  i <- which(dy1 == dx1 & dy1 == 0 & dx1 == 0)
  j <- which(dy2 == dx2 & dy2 == 0 & dx2 == 0)

  dy <- (y2-y1)*units.table[units]
  dx <- (x2-x1)*units.table[units]
  d <- sqrt(dy^2+dx^2)

  A <- (-dy/d)
  B <- (-dx/d)
  A1 <- -A
  B1 <- -B

  vec_d[2*i-1] <- B
  vec_d[2*j-1] <- B1
  vec_d[2*i] <- A
  vec_d[2*j] <- A1
  return(vec_d)
}

### coeficients for directions (pravac) #####################################################

coef_p <- function (pt1, pt2, pts, units) {
  units.table <- c("mm" = 1000, "cm" = 100, "m" = 1)
  pt1 <- as.numeric(pt1)
  pt2 <- as.numeric(pt2)
  coords <- pts
  vec_p <- c(rep(0, dim(pts)[1]*2))#c(rep(0, length(coords)))
  ro <- 180/pi*3600

  y_coords <- coords[, 2]
  x_coords <- coords[, 1]

  y1 <- pt1[2]
  x1 <- pt1[1]
  y2 <- pt2[2]
  x2 <- pt2[1]

  dy1 <- (y_coords-y1)
  dx1 <- (x_coords-x1)
  dy2 <- (y_coords-y2)
  dx2 <- (x_coords-x2)

  i <- which(dy1 == dx1 & dy1 == 0 & dx1 == 0)
  j <- which(dy2 == dx2 & dy2 == 0 & dx2 == 0)

  dy <- (y2-y1)*units.table[units]
  dx <- (x2-x1)*units.table[units]
  d <- sqrt(dy^2 + dx^2)

  A <- (ro*dx/d^2)
  B <- (-ro*dy/d^2)
  A1 <- -(ro*dx/d^2)
  B1 <- -(-ro*dy/d^2)

  vec_p[2*i-1] <- B
  vec_p[2*j-1] <- B1
  vec_p[2*i] <- A
  vec_p[2*j] <- A1
  return(vec_p)
}

# net.points <- survey.net[[1]]
fix_params2D <- function(net.points){
  rep(as.logical(c(apply(cbind(net.points$FIX_2D), 1, as.numeric))), each = 2)
}

# survey.net <- brana
Amat <- function(survey.net, units){

  if(any(survey.net[[2]]$direction)){
    A_dir <- survey.net[[2]] %>% dplyr::filter(direction) %>% st_coordinates() %>% as.data.frame() %>% mutate_at(vars(L1), as.factor) %>%
      split(., .$L1) %>%
      lapply(., function(x) coef_p(pt1 = x[1, 1:2], pt2 = x[2, 1:2], pts = st_coordinates(survey.net[[1]][, 1:2]), units = units)) %>%
      do.call(rbind, .)
  }else{
    A_dir <- NULL
  }

  if(any(survey.net[[2]]$distance)){
    A_dist <- survey.net[[2]] %>% dplyr::filter(distance) %>% st_coordinates() %>% as.data.frame() %>% mutate_at(vars(L1), as.factor) %>%
      split(., .$L1) %>%
      lapply(., function(x) coef_d(pt1 = x[1, 1:2], pt2 = x[2, 1:2], pts = st_coordinates(survey.net[[1]][, 1:2]), units = units)) %>%
      do.call(rbind, .)
  }else{
    A_dist <- NULL
  }

  station.names <- survey.net[[2]] %>% dplyr::filter(direction) %>% dplyr::select(from) %>% st_drop_geometry() %>% unique() %>% unlist(use.names = FALSE)

  if(!all(is.na(survey.net[[2]]$sd_Hz))){
    Z_mat <- survey.net[[2]] %>% dplyr::filter(direction) %>%
      tidyr::spread(key = from, value = direction, fill = FALSE) %>%
      dplyr::select(as.character(station.names)) %>%
      st_drop_geometry() %>%
      as.matrix()*1
  }else{
    Z_mat <- NULL
  }

  fix <- fix_params2D(net.points = survey.net[[1]])
  if(!is.null(A_dir) & !is.null(A_dist)){
    rest_mat <- matrix(0, nrow = dim(A_dist)[1], ncol = dim(Z_mat)[2])
  }else{
    rest_mat <- NULL
  }

  A <- cbind(rbind(A_dir, A_dist)[, !fix], rbind(Z_mat, rest_mat))

  # Removing zero rows (measured distances between the fixed points)
  # A <- A[apply(A !=0, 1, any), , drop=FALSE]

  sufix <- c("dx", "dy")
  colnames(A) <- c(paste(rep(survey.net[[1]]$Name, each = 2), rep(sufix, length(survey.net[[1]]$Name)), sep = "_")[!fix], paste(colnames(Z_mat), "z", sep = "_"))
  return(A)
}

# Weights matrix
# Wmat je ista, samo je promenjen naziv standarda. Stavljeni su "sd_Hz" i "sd_dist".
Wmat <- function(survey.net, sd.apriori = 1){
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
  return(diag(sd.apriori^2/obs.data$standard^2))
}



# # Funkcija koja izdvaja elemente Qx matrice u listu za elipsu svake tacke
# Qxy <- function(Qx, n, fixd = fix){
#   k = 0
#   fixd <- cbind(fixd, fixd[,1] + fixd[, 2])
#   Qxxx <- as.list(rep(NA, dim(fixd)[1]))
#   for(i in 1:length(Qxxx)){
#     k = fixd[i, 1] + fixd[i, 2] + k
#     if(fixd[i, 3] == 1){
#       Qxxx[[i]] <- diag(fixd[i, c(1, 2)])*Qx[k, k]
#     }
#     else if (fixd[i, 3] == 2){
#       Qxxx[[i]] <- cbind(c(Qx[k-1, k-1], Qx[k-1, k]), c(Qx[k, k-1], Qx[k, k]))
#     } else {
#       Qxxx[[i]] <- diag(fixd[i, c(1, 2)])
#     }
#   }
#   return(Qxxx)
# }
# #

error.ellipse <- function(Qxy, prob = NA, sd.apriori = 1, teta.unit = list("deg", "rad")) {
  Qee <- Qxy[1, 1]
  Qnn <- Qxy[2, 2]
  Qen <- Qxy[1, 2]
  if(any(c(Qee, Qnn) == 0)){
    A <- 0
    B <- 0
    teta <- 0
    ellipse <- c(A, B, teta)
  }else{
    k <- sqrt((Qnn - Qee)^2 + 4*Qen^2)
    lambda1 <- 0.5*(Qee + Qnn + k)
    lambda2 <- 0.5*(Qee + Qnn - k)
    if(is.na(prob)){
      A <- sd.apriori*sqrt(lambda1)
      B <- sd.apriori*sqrt(lambda2)
    }else{
      A <- sd.apriori*sqrt(lambda1*qchisq(prob, df = 2))
      B <- sd.apriori*sqrt(lambda2*qchisq(prob, df = 2))
    }
    teta <- ifelse((Qnn - Qee) == 0, 0, 0.5*atan(2*Qen/(Qnn - Qee)))
    teta <- ifelse(teta >= 0, teta, teta + 2*pi)
    teta <- ifelse(teta >= pi, teta - pi, teta)
    if(teta.unit[[1]] == "deg"){
      ellipse <- c(A, B, teta*180/pi)
    }else{
      ellipse <- c(A, B, teta)
    }
  }
  return(ellipse)
}
#

rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

sf.ellipse <- function(ellipse.param, scale = 10){
  ellipse <- nngeo::st_ellipse(ellipse.param, ey = ellipse.param$A*scale, ex = ellipse.param$B*scale)
  geom.ellipse = st_geometry(ellipse)
  ellipse.cntrd = st_centroid(geom.ellipse)
  ellipse.rot <- (geom.ellipse - ellipse.cntrd) * rot(ellipse.param$teta*pi/180) + ellipse.cntrd
  ellipse.sf <- st_sf(Name = ellipse.param$Name, A = ellipse.param$A, B = ellipse.param$B, teta = ellipse.param$teta, Geometry = ellipse.rot)
  return(ellipse.sf)
}

sigma.xy <- function(Qxy.mat, sd.apriori){
  sigma <- diag(diag(as.numeric(sd.apriori), 2, 2)%*%diag(sqrt(diag(Qxy.mat)), 2, 2))
}

# TODO Treba resiti da u points idu samo tacke koje ucestvuju u merenjima.
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

#st.survey.net <- Makis.survey.net[[2]] %>% dplyr::filter(from == "OM1")
#st.survey.net <- brana[[2]] %>% dplyr::filter(from == "T1")
#st.survey.net <- avala[[2]] %>% dplyr::filter(from == "S2")
# st.survey.net <- A.survey.net[[2]] %>% dplyr::filter(from == "C")
# st.survey.net <- Gorica.survey.net[[2]] %>% dplyr::filter(from == "1")

fdir_st <- function(st.survey.net, units.dir = "sec"){
  units.table <- c("sec" = 3600, "min" = 60, "deg" = 1)
  st.survey.net <- st.survey.net %>% split(., f = as.factor(.$to)) %>%
    lapply(., function(x) {x$ni = ni(pt1_coords = as.numeric(x[, c("x_from", "y_from")]), pt2_coords = as.numeric(x[, c("x_to", "y_to")]), type = "dec"); return(x)}) %>%
    do.call(rbind,.) %>%
    dplyr::mutate(z = Hz-ni) %>%
    dplyr::arrange(ID)
  st.survey.net$z <- ifelse(st.survey.net$z < 1, st.survey.net$z + 360, st.survey.net$z)
  z0_mean <- mean(st.survey.net$z)
  st.survey.net$Hz0 <- z0_mean + st.survey.net$ni
  st.survey.net$Hz0 <- ifelse(st.survey.net$Hz0 > 359, st.survey.net$Hz0 - 360, st.survey.net$Hz0)
  st.survey.net$Hz <- ifelse(st.survey.net$Hz < 1, st.survey.net$Hz + 360, st.survey.net$Hz)
  st.survey.net$Hz <- ifelse(st.survey.net$Hz >= 360, st.survey.net$Hz - 360, st.survey.net$Hz)

  f <- (st.survey.net$Hz0 - st.survey.net$Hz)*units.table[units.dir]
  return(f)
}

fmat <- function(survey.net, units.dir = "sec", units.dist = "mm"){
  f_dir <- survey.net[[2]] %>% dplyr::filter(direction == TRUE) %>% st_drop_geometry() %>% split(.,.$from) %>%
    lapply(., function(x) fdir_st(x, units.dir = units.dir)) %>%
    do.call("c",.) %>% as.numeric() %>% as.vector()
  dist.units.table <- c("mm" = 1000, "cm" = 100, "m" = 1)
  survey.net[[2]] <- survey.net[[2]] %>% dplyr::filter(distance) %>% st_drop_geometry() %>%
    dplyr::mutate(dist0 = sqrt((x_from-x_to)^2+(y_from-y_to)^2))
  f_dist <- (survey.net[[2]]$dist0 - survey.net[[2]]$HD)*dist.units.table[units.dist]
  return(c(f_dir, f_dist))
}

model_adequacy_test <- function(sd.apriori, sd.estimated, df, prob){
  if(sd.estimated > sd.apriori){
    F.estimated <- sd.estimated^2/sd.apriori^2
    F.quantile <- qf(p = prob, df1 = df, df2 = 10^1000)
  }else{
    F.estimated <- sd.apriori^2/sd.estimated^2
    F.quantile <- qf(p = prob, df1 = 10^1000, df2 = df)
  }
  if(F.estimated < F.quantile){print(paste("sd.estimated =", round(sd.estimated, 2), "/ sd.apriori =", round(sd.apriori, 2), "/ Model is correct", sep = " "))} else{
    print(paste("sd.estimated =", round(sd.estimated, 2), "/ sd.apriori =", round(sd.apriori, 2), "/ Model is not correct", sep = " "))
  }
  return(F.estimated < F.quantile)
    # print(paste(round(F.estimated, 2), ">", round(F.quantile, 2), "Model is not correct", sep = " "))
    # Data snooping and others have to be put in the list
    # print(paste(round(F.estimated, 2), "<", round(F.quantile, 2), "Model is correct", sep = " "))
}

read_surveynet <- function(file, dest_crs = NA, axes = c("Easting", "Northing")){
  # TODO: Ako se definise neki model tezina za koji ne postoje podaci stavi warning. Npr. za model "sd_dh" mora da postoji i sd.apriori koji se pretvara u sd0.

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
      wdh_model == "sd_dh" ~ sd0/sd_dh,
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
Qxy <- function(Qx, fix){
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



adjust.snet <- function(adjust = TRUE, survey.net, dim_type = list("1D", "2D"), sd.apriori = 1, wdh_model = list("sd_dh", "d_dh", "n_dh", "E"), n0 = 1, maxiter = 50, prob = 0.95, coord_tolerance = 1e-3, result.units = list("mm", "cm", "m"), ellipse.scale = 1, teta.unit = list("deg", "rad"), units.dir = "sec", units.dist = "mm", use.sd.estimated = TRUE, all = TRUE){
  dim_type <- dim_type[[1]]
  "%!in%" <- Negate("%in%")
  if(!adjust){use.sd.estimated <- FALSE}
  units <- result.units[[1]]
  res.unit.lookup <- c("mm" = 1000, "cm" = 100, "m" = 1)
  disp.unit.lookup <- c("mm" = 2, "cm" = 3, "m" = 4)

  # TODO: This has to be solved within the read.surveynet function
  used.points <- survey.net[[1]]$Name[survey.net[[1]]$Name %in% unique(c(survey.net[[2]]$from, survey.net[[2]]$to))]
  if(!!any(used.points %!in% survey.net[[1]]$Name)) stop(paste("There is no coordinates for point", used.points[which(used.points %!in% survey.net[[1]]$Name)]), sep = " ")
  survey.net[[1]] <- survey.net[[1]][which(survey.net[[1]]$Name %in% used.points), ]

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
        tidyr::separate(.,col = parameter, into = c("Name", "inc.name"), sep = "_") %>%
        tidyr::pivot_wider(., names_from = c(inc.name), values_from = c(coords.inc)) %>%
        dplyr::mutate_all(., ~replace(., is.na(.), 0))

      point.adj.results <- dplyr::left_join(survey.net[[1]], coords.inc, by = "Name") %>%
        dplyr::mutate_at(., .vars = c("dx", "dy"), ~replace(., is.na(.), 0)) %>%
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
    Qxy.list <- Qxy(Qx.mat, fix = !survey.net[[1]]$FIX_2D)
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
      survey.net[[1]] <- merge(point.adj.results, ellipses, by = "Name") %>% merge(., sigmas) %>% dplyr::arrange(id)
    }else{
      survey.net[[1]] <- merge(survey.net[[1]], ellipses, by = "Name") %>% merge(., sigmas) %>% dplyr::arrange(id)
    }

    # Preparing ellipses as separate sf outcome
    # TODO Proveriti da li elipse uzimaju definitivne koordinate ili priblizne!
    ellipse.net <- do.call(rbind, lapply(split(survey.net[[1]], factor(survey.net[[1]]$Name, levels = survey.net[[1]]$Name)), function(x) sf.ellipse(x, scale = ellipse.scale)))
    ellipse.net <- merge(ellipse.net, sigmas)
    ellipse.net <- sf::st_set_crs(ellipse.net, value = st_crs(survey.net[[2]]))

    points <- list(net.points = survey.net[[1]], ellipse.net = ellipse.net)

  }else{
    observations <- tidyr::gather(survey.net[[2]] %>% purrr::when(is(., "sf") ~ st_drop_geometry(.), ~.) %>% dplyr::select(from, to, diff_level), key = type, value = used, -c(from, to)) %>%
      dplyr::filter(used == TRUE) %>%
      dplyr::mutate(from_to = str_c(.$from, .$to, sep = "_"))
    fix.mat1D <- !(survey.net[[1]]$FIX_1D)

    A.mat <- Amat1D(survey.net)
    W.mat <- Wmat1D(survey.net = survey.net, wdh_model = wdh_model, n0 = 1)
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
      coords.iter_0 <- as.vector(survey.net[[1]]$h)[fix.mat1D]
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
      x.mat <- x.mat[1:sum(fix.mat1D)]  #
      h.inc <- (survey.net[[1]]$h[fix.mat1D]-coords.iter_0)*res.unit.lookup[units]
      sd.estimated <- sqrt((crossprod(v.mat, W.mat) %*% v.mat)/(df))
      model_adequacy <- model_adequacy_test(sd.apriori, sd.estimated, df, prob = prob)
      if(!model_adequacy){
        tds <- data.frame(Observation = rownames(A.mat), statistics = as.numeric(abs(v.mat)/(sd.apriori*sqrt(diag(Qv.mat))))) %>% dplyr::arrange(., desc(statistics))
        print("Check the statistics for individual observations. Suggestion: Remove the observation with the highest value of statistics:")
        print(tds)
        stop()
      }
      if(use.sd.estimated){sd.apriori <- sd.estimated}
    }
      # Results
      if(adjust){
        h.inc <- data.frame(Name = as.character(colnames(A.mat)), dh = as.numeric(h.inc), sd_h = c(sd.apriori)*sqrt(diag(Qx.mat)), stringsAsFactors  = FALSE)
        survey.net[[1]] <- dplyr::left_join(survey.net[[1]], h.inc, by = "Name") %>%
          dplyr::mutate_at(., .vars = c("dh", "sd_h"), ~replace(., is.na(.), 0)) %>%
          dplyr::mutate(h0 = h - dh/res.unit.lookup[units]) %>%
          dplyr::mutate_at(., .vars = "dh", round, disp.unit.lookup[units]) %>%
          dplyr::select(id, Name, x, y, h0, dh, h, sd_h, FIX_1D, Point_object)
      }else{
        h.inc <- data.frame(Name = as.character(colnames(A.mat)), sd_h = c(sd.apriori)*sqrt(diag(Qx.mat)), stringsAsFactors  = FALSE)
        survey.net[[1]] <- dplyr::left_join(survey.net[[1]], h.inc, by = "Name") %>%
          dplyr::mutate_at(., .vars = "sd_h", ~replace(., is.na(.), 0)) %>%
          dplyr::mutate_at(., .vars = "sd_h", round, disp.unit.lookup[units]) %>%
          dplyr::select(id, Name, x, y, h, sd_h, FIX_1D, Point_object)
      }
    points <- survey.net[[1]]
  }

  if(adjust){
    observations <- observations %>% dplyr::mutate(v = v.mat, Ql = diag(Ql.mat), Qv = diag(Qv.mat), rii = diag(r)) %>%
      dplyr::mutate_if(is.numeric, round, disp.unit.lookup[units]) #%>%
    #dplyr::rename_at(., .vars = "v", .funs = paste("v", paste("[", units, "]", sep = ""), sep = " "))
  }else{
    observations <- observations %>% dplyr::mutate(Ql = diag(Ql.mat), Qv = diag(Qv.mat), rii = diag(r)) %>%
      dplyr::mutate_if(is.numeric, round, disp.unit.lookup[units])
  }

  if(sum(rowSums(is.na(survey.net[[1]][, c("x", "y")])) != 0) == 0){
    observations <- observations %>%
      dplyr::mutate(x_from = survey.net[[1]]$x[match(observations$from, survey.net[[1]]$Name)],
                    y_from = survey.net[[1]]$y[match(observations$from, survey.net[[1]]$Name)],
                    x_to = survey.net[[1]]$x[match(observations$to, survey.net[[1]]$Name)],
                    y_to = survey.net[[1]]$y[match(observations$to, survey.net[[1]]$Name)])

    observations <- observations %>%
      as.data.table(.) %>%
      dplyr::mutate(id = seq.int(nrow(.))) %>%
      split(., f = as.factor(.$id)) %>%
      lapply(., function(row) {lmat <- matrix(unlist(row[c("x_from", "y_from", "x_to", "y_to")]), ncol = 2, byrow = TRUE)
      st_linestring(lmat)}) %>%
      sf::st_sfc() %>%
      sf::st_sf('ID' = seq.int(nrow(observations)), observations, 'geometry' = .) %>%
      dplyr::select(-c(x_from, y_from, x_to, y_to))
    observations %<>% sf::st_set_crs(value = st_crs(survey.net[[2]]))

    results <- list(design.matrices = list(A = A.mat, W = W.mat, Qx = Qx.mat, Ql = Ql.mat, Qv = Qv.mat), points, observations = observations)
    # results$net.points <- sf::st_set_crs(results$net.points, value = st_crs(survey.net[[2]])) TODO: gubi se projekcija pa ovo ne moze da se uradi.
  }

  results <- list(design.matrices = list(A = A.mat, W = W.mat, Qx = Qx.mat, Ql = Ql.mat, Qv = Qv.mat), Points = points, Observations = observations)


  if(all){
    return(results)
  }else{
    return(results[-1])
  }

}



##################
# plot_surveynet
##################

# Function for data geovisualisation trough package ggplot2 and mapview
# Parameters:
#    1. snet -  object from function read_surveynet
#    2. webmap - plot 2d net using mapview package
#    3. net.1D - 2d net indicator
#    4. net.2D - 1d net indicator

# snet = dns.snet

plot_surveynet <- function(snet = snet, webmap = FALSE, net.1D = FALSE, net.2D = FALSE){
  points <- snet$points
  observations <- snet$observations

  if(net.2D == TRUE) {
    points %<>% dplyr::mutate(Point_type = dplyr::case_when(Point_object == FALSE ~ "Geodetic network",
                                                            Point_object == TRUE ~ "Points at object"))
    observations %<>% dplyr::mutate(Observation_type = dplyr::case_when(distance == TRUE & direction == FALSE ~ "Distance",
                                                                        distance == FALSE & direction == TRUE ~ "Direction",
                                                                        distance == TRUE & direction == TRUE ~ "Both",
                                                                        distance == FALSE & direction == FALSE ~ "None"))

    if(webmap == TRUE){

      if(is.na(sf::st_crs(points)) == TRUE) {
        points %<>% sf::st_set_crs(., 3857)
      }

      if(is.na(sf::st_crs(observations)) == TRUE) {
        observations %<>% sf::st_set_crs(., 3857)
      }

      points <- st_transform(points, 3857)
      observations <- st_transform(observations, 3857)

      webmap.net <- mapview(points, zcol = "Point_type", col.regions = c("red","grey")) + mapview(observations, zcol = "Observation_type")
      return(webmap.net)

    } else {
      plot.net <- ggplot() +
        geom_sf(data=observations, aes(color = Observation_type),size=0.5,stroke=0.5)+
        geom_sf(data=points, aes(fill = Point_type), shape = 24,  size=2, stroke=0.5) +
        geom_sf_text(data=points, aes(label=Name,hjust = 1.5, vjust =1.5))+
        xlab("\nEasting [m]") +
        ylab("Northing [m]\n") +
        ggtitle("GEODETIC 2D NETWORK")+
        labs(subtitle = "Points and Observational plan")+
        guides(col = guide_legend())+
        theme_bw()+
        theme(legend.position = 'bottom')

      return(plot.net)
    }
  }

  if(net.1D == TRUE){
    observations %<>% dplyr::mutate(from_to = paste(from, to, sep = "-"))

    p.plot <- ggplotly(ggplot()+
               geom_point(data = points,
                          aes(x = Name,
                              y = h,
                              colour = h))+
               scale_colour_gradient(low="blue",
                                     high="red")+
               xlab("Name") +
               ylab("h [m]") +
               ggtitle("GEODETIC 1D NETWORK")+
               labs(colour = "h [m]")+
               theme_bw()+
               ylim(min(points$h)-50,
                    max(points$h)+50), showlegend = T
             )

    o.plot <- ggplotly(
      ggplot()+
        geom_point(data = observations,
                   aes(x = from_to,
                       y = dh,
                       colour = dh))+
        scale_colour_gradient(low="orange",
                              high="red")+
        xlab("Name") +
        ylab("dh [m]") +
        ggtitle("GEODETIC 1D NETWORK")+
        labs(colour = "dh [m]")+
        theme_bw()+
        ylim(min(observations$dh)-1.5,
             max(observations$dh)+1.5), showlegend = T
    )

    plot.1d.net <- plotly::subplot(style(p.plot, showlegend = FALSE),
                                   style(o.plot, showlegend = TRUE),
                                   nrows = 2,
                                   shareX = FALSE,
                                   shareY = FALSE,
                                   titleX = TRUE,
                                   titleY = TRUE)
    return(plot.1d.net)
  }
}
