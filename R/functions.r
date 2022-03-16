# Project: Surveyer
# Description: Land and Engineering Surveying utilities
# Authors: Milutin Pejovic, Petar Bursac, Milan Kilibarda, Branislav Bajat, Aleksandar Sekulic
# Date:

# Functions:

#' @title Read surveynet data
#' @description Read raw data stored in pre-defined excel file. 
#' @param file PARAM_DESCRIPTION
#' @param dest_crs Destination coordinate system (EPSG), Default: NA
#' @param axes order of coordinate axes, Default: c("Easting", "Northing")
#' @return surveynet object
#' @details 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[readxl]{read_excel}}
#'  \code{\link[sf]{st_as_sf}},\code{\link[sf]{sfc}},\code{\link[sf]{sf}},\code{\link[sf]{st_crs}}
#'  \code{\link[dplyr]{rename}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{select}},\code{\link[dplyr]{filter}}
#'  \code{\link[purrr]{when}}
#' @rdname read_surveynet
#' @export 
#' @importFrom readxl read_xlsx
#' @importFrom magrittr %>% %<>%
#' @importFrom sf st_as_sf st_sfc st_sf st_set_crs st_linestring
#' @importFrom dplyr rename mutate select filter across
#' @importFrom purrr when
read_surveynet <- function(file, dest_crs = NA, axes = c("Easting", "Northing")){
  # TODO: Ako se definise neki model tezina za koji ne postoje podaci stavi warning. Npr. za model "sd_dh" mora da postoji i sd.apriori koji se pretvara u sd0.
  # TODO: Set warning if there are different or not used points in two elements of survey.net list.
  # TODO: Check if any point has no sufficient measurements to be adjusted.
  # setting columns type
  points_col_type <- c("numeric", "text", "numeric", "numeric", "numeric", "logical", "logical", "logical")
  obs_col_type <- c("text", "text", rep("numeric", 19))
  # reading data
  points <- readxl::read_xlsx(path = file, sheet = "Points", col_types = points_col_type) %>% dplyr::mutate(across(.cols = "Name", .fns = as.character)) #mutate_at(., .vars = c("Name"), as.character)
  observations <- readxl::read_xlsx(path = file, sheet = "Observations", col_types = obs_col_type) %>% dplyr::mutate(across(.cols = c("from", "to"), .fns = as.character))  #mutate_at(., .vars = c("from", "to"), as.character)
  if(any(duplicated(observations[, c(1:2)]))){warning("There are duplicated measurements!")}
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
        sf::st_sf('ID' = seq.int(nrow(observations)), observations, 'geometry' = .)
    }



  #if(sum(rowSums(is.na(observations[, c("HzD", "HzM", "HzS")])) != 0) != 0){stop("There is uncomplete observations")}


  observations <- observations %>% dplyr::mutate(Hz = HzD + HzM/60 + HzS/3600,
                                                 Vz = VzD + VzM/60 + VzS/3600,
                                                 tdh = SD*cos(Vz*pi/180), # ne znam cemu ovo sluzi
                                                 distance = (!is.na(HD) | !is.na(SD)) & !is.na(sd_dist),
                                                 direction = !is.na(Hz) & !is.na(sd_Hz),
                                                 diff_level = !is.na(dh) & (!is.na(d_dh) | !is.na(sd_dh) | !is.na(n_dh)))

  # In network design, observation is included if measurement standard is provided
  if(observations %>% purrr::when(is(., "sf") ~ st_drop_geometry(.), ~.) %>% dplyr::select(HzD, HzM, HzS) %>% is.na() %>% all()){
    observations$direction[!is.na(observations$sd_Hz)] <- TRUE
  }
  if(observations %>% purrr::when(is(., "sf") ~ st_drop_geometry(.), ~.) %>% dplyr::select(HD, SD) %>% is.na() %>% all()){
    observations$distance[!is.na(observations$sd_dist)] <- TRUE
  }
  if(observations %>% purrr::when(is(., "sf") ~ st_drop_geometry(.), ~.) %>% dplyr::select(dh) %>% is.na() %>% all()){
    observations$diff_level[(!is.na(observations$sd_dh) | !is.na(observations$d_dh) | !is.na(observations$n_dh))] <- TRUE
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


#' @title dec2dms
#' @description FUNCTION_DESCRIPTION
#' @param ang PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname dec2dms
dec2dms <- function(ang){
  deg <- floor(ang); minut <- floor((ang-deg)*60); sec <- ((ang-deg)*60-minut)*60
  return(paste(deg, minut, round(sec, 0), sep = " "))
}


#' @title dist
#' @description Calculate distance
#' @param pt1_coords PARAM_DESCRIPTION
#' @param pt2_coords PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname dist
dist <- function(pt1_coords, pt2_coords){
  dEasting <- as.numeric(pt2_coords[1] - pt1_coords[1])
  dNorthing <- as.numeric(pt2_coords[2] - pt1_coords[2])
  distance <- sqrt(dEasting^2 + dNorthing^2)
  return(distance)
}



#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param pt1_coords PARAM_DESCRIPTION
#' @param pt2_coords PARAM_DESCRIPTION
#' @param type PARAM_DESCRIPTION, Default: list("dec", "dms", "rad")
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ni
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
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param pt1 PARAM_DESCRIPTION
#' @param pt2 PARAM_DESCRIPTION
#' @param pts PARAM_DESCRIPTION
#' @param units PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname coef_d
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

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param pt1 PARAM_DESCRIPTION
#' @param pt2 PARAM_DESCRIPTION
#' @param pts PARAM_DESCRIPTION
#' @param units PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname coef_p
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
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param net.points PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname fix_params2D
fix_params2D <- function(net.points){
  rep(as.logical(c(apply(cbind(net.points$FIX_2D), 1, as.numeric))), each = 2)
}

# survey.net <- brana
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param survey.net PARAM_DESCRIPTION
#' @param units PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{select}}
#'  \code{\link[tidyr]{spread}}
#' @rdname Amat
#' @importFrom dplyr filter select mutate_at
#' @importFrom tidyr spread
#' @importFrom sf st_coordinates st_drop_geometry
Amat <- function(survey.net, units){

  if(any(survey.net[[2]]$direction)){
    A_dir <- survey.net[[2]] %>% dplyr::filter(direction) %>% sf::st_coordinates() %>% as.data.frame() %>% dplyr::mutate(across(.cols = "L1", as.factor)) %>%
      split(., .$L1) %>%
      lapply(., function(x) coef_p(pt1 = x[1, 1:2], pt2 = x[2, 1:2], pts = sf::st_coordinates(survey.net[[1]][, 1:2]), units = units)) %>%
      do.call(rbind, .)
  }else{
    A_dir <- NULL
  }
  
  if(any(survey.net[[2]]$distance)){
    A_dist <- survey.net[[2]] %>% dplyr::filter(distance) %>% sf::st_coordinates() %>% as.data.frame() %>% dplyr::mutate(across(.cols = "L1", as.factor)) %>%
      split(., .$L1) %>%
      lapply(., function(x) coef_d(pt1 = x[1, 1:2], pt2 = x[2, 1:2], pts = sf::st_coordinates(survey.net[[1]][, 1:2]), units = units)) %>%
      do.call(rbind, .)
  }else{
    A_dist <- NULL
  }

  station.names <- survey.net[[2]] %>% dplyr::filter(direction) %>% dplyr::select(from) %>% sf::st_drop_geometry() %>% unique() %>% unlist(use.names = FALSE)

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
  
  # OVde je nesto bilo pa smo obrisali

  A <- cbind(rbind(A_dir, A_dist)[, !fix], rbind(Z_mat, rest_mat))

  sufix <- c("dx", "dy")
  colnames(A) <- c(paste(rep(survey.net[[1]]$Name, each = 2), rep(sufix, length(survey.net[[1]]$Name)), sep = "_")[!fix], paste(colnames(Z_mat), "z", sep = "_"))
  return(A)
}

# Weights matrix
# Wmat je ista, samo je promenjen naziv standarda. Stavljeni su "sd_Hz" i "sd_dist".
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param survey.net PARAM_DESCRIPTION
#' @param sd.apriori PARAM_DESCRIPTION, Default: 1
#' @param res.units PARAM_DESCRIPTION, Default: 'mm'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{select}},\code{\link[dplyr]{mutate}}
#' @rdname Wmat
#' @importFrom dplyr filter select mutate
Wmat <- function(survey.net, sd.apriori = 1, res.units = "mm"){
  res.unit.lookup <- c("mm" = 1, "cm" = 10, "m" = 1000)
  #TODO: Omoguciti zadavanje i drugih kovariacionih formi izmedju merenja.
  obs.data <- rbind(survey.net[[2]] %>% st_drop_geometry() %>%
                      dplyr::filter(direction) %>%
                      dplyr::select(from, to, standard = sd_Hz) %>%
                      dplyr::mutate(type = "direction"),
                    survey.net[[2]] %>% st_drop_geometry() %>%
                      dplyr::filter(distance) %>%
                      dplyr::select(from, to, standard = sd_dist) %>%
                      dplyr::mutate(type = "distance", standard = standard/res.unit.lookup[res.units])
  )
  return(diag(sd.apriori^2/obs.data$standard^2))
}

# Funkcija koja izdvaja elemente Qx matrice u listu za elipsu svake tacke
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param Qx PARAM_DESCRIPTION
#' @param fix PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname Qxy
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

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param Qxy PARAM_DESCRIPTION
#' @param prob PARAM_DESCRIPTION, Default: NA
#' @param sd.apriori PARAM_DESCRIPTION, Default: 1
#' @param teta.unit PARAM_DESCRIPTION, Default: list("deg", "rad")
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname error.ellipse
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

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param a PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname rot
rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param ellipse.param PARAM_DESCRIPTION
#' @param scale PARAM_DESCRIPTION, Default: 10
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[nngeo]{st_ellipse}}
#' @rdname sf.ellipse
#' @importFrom nngeo st_ellipse
sf.ellipse <- function(ellipse.param, scale = 10){
  ellipse <- nngeo::st_ellipse(ellipse.param, ey = ellipse.param$A*scale, ex = ellipse.param$B*scale)
  geom.ellipse = st_geometry(ellipse)
  ellipse.cntrd = st_centroid(geom.ellipse)
  ellipse.rot <- (geom.ellipse - ellipse.cntrd) * rot(ellipse.param$teta*pi/180) + ellipse.cntrd
  ellipse.sf <- st_sf(Name = ellipse.param$Name, A = ellipse.param$A, B = ellipse.param$B, teta = ellipse.param$teta, Geometry = ellipse.rot)
  return(ellipse.sf)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param Qxy.mat PARAM_DESCRIPTION
#' @param sd.apriori PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname sigma.xy
sigma.xy <- function(Qxy.mat, sd.apriori){
  sigma <- diag(diag(as.numeric(sd.apriori), 2, 2)%*%diag(sqrt(diag(Qxy.mat)), 2, 2))
}

# st.survey.net <- makis.snet[[2]] %>% dplyr::filter(from == "OM20")
# st.survey.net <- brana.snet[[2]] %>% dplyr::filter(from == "T1")
# st.survey.net <- avala[[2]] %>% dplyr::filter(from == "S2")
# st.survey.net <- A.survey.net[[2]] %>% dplyr::filter(from == "C")
# st.survey.net <- Gorica.survey.net[[2]] %>% dplyr::filter(from == "1")
# st.survey.net <- ab[[2]] %>% dplyr::filter(from == "P2")
#  st.survey.net <- mreza_sim[[2]] %>% dplyr::filter(from == "M10")
#  st.survey.net <- zadatak1.snet[[2]] %>% dplyr::filter(from == "T2")
# st.survey.net <- cut45[[2]] %>% dplyr::filter(from == "C53")

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param st.survey.net PARAM_DESCRIPTION
#' @param units.dir PARAM_DESCRIPTION, Default: 'sec'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{mutate}},\code{\link[dplyr]{arrange}}
#' @rdname fdir_st
#' @importFrom dplyr mutate arrange
fdir_st <- function(st.survey.net, units.dir = "sec"){
  units.table <- c("sec" = 3600, "min" = 60, "deg" = 1)
  st.survey.net <- st.survey.net %>% split(., f = as.factor(.$to)) %>%
    lapply(., function(x) {x$ni = ni(pt1_coords = as.numeric(x[1, c("x_from", "y_from")]), pt2_coords = as.numeric(x[1, c("x_to", "y_to")]), type = "dec"); return(x)}) %>%
    do.call(rbind,.) %>%
    dplyr::mutate(z = Hz-ni) %>%
    dplyr::arrange(ID)
  #st.survey.net$z <- ifelse(st.survey.net$z < 0 & st.survey.net$z < -0.01, st.survey.net$z + 360, st.survey.net$z)
  st.survey.net$z <- ifelse(st.survey.net$z < 0, st.survey.net$z + 360, st.survey.net$z)
  z0_mean <- mean(st.survey.net$z)
  f <-  st.survey.net$ni + z0_mean - st.survey.net$Hz
  f <- ifelse(f < -1, f + 360 , f)
  f <- ifelse(f > 359, f - 360, f)
  f <- f*units.table[units.dir]
  return(f)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param survey.net PARAM_DESCRIPTION
#' @param units.dir PARAM_DESCRIPTION, Default: 'sec'
#' @param units.dist PARAM_DESCRIPTION, Default: 'mm'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{filter}},\code{\link[dplyr]{mutate}}
#' @rdname fmat
#' @importFrom dplyr filter mutate
fmat <- function(survey.net, units.dir = "sec", units.dist = "mm"){
  f_dir <- survey.net[[2]] %>% dplyr::filter(direction == TRUE) %>% st_drop_geometry() %>% split(.,factor(.$from, levels = unique(.$from))) %>%
    lapply(., function(x) fdir_st(x, units.dir = units.dir)) %>%
    do.call("c",.) %>% as.numeric() %>% as.vector()
  dist.units.table <- c("mm" = 1000, "cm" = 100, "m" = 1)
  survey.net[[2]] <- survey.net[[2]] %>% dplyr::filter(distance) %>% st_drop_geometry() %>%
    dplyr::mutate(dist0 = sqrt((x_from-x_to)^2+(y_from-y_to)^2))
  f_dist <- (survey.net[[2]]$dist0 - survey.net[[2]]$HD)*dist.units.table[units.dist]
  return(c(f_dir, f_dist))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param sd.apriori PARAM_DESCRIPTION
#' @param sd.estimated PARAM_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param prob PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname model_adequacy_test
model_adequacy_test <- function(sd.apriori, sd.estimated, df, prob){
  if(sd.estimated > sd.apriori){
    F.estimated <- sd.estimated^2/sd.apriori^2
    F.quantile <- qf(p = prob, df1 = df, df2 = 10^1000)
  }else{
    F.estimated <- sd.apriori^2/sd.estimated^2
    F.quantile <- qf(p = prob, df1 = 10^1000, df2 = df)
  }
  if(F.estimated < F.quantile){
    note <- "Model is correct"; print(note)
  }else{
    note <- "Model is not correct. Please check Baarda test statistics for individual observations. Suggestion: Remove one observation with the highest statistics"; print(note)
  }
  return(list(F.estimated < F.quantile, "F_test" = F.estimated, "Crital value F-test" =  F.quantile, note))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param sd.apriori PARAM_DESCRIPTION
#' @param sd.estimated PARAM_DESCRIPTION
#' @param df PARAM_DESCRIPTION
#' @param prob PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname model_adequacy_test.shiny
model_adequacy_test.shiny <- function(sd.apriori, sd.estimated, df, prob){
  if(sd.estimated > sd.apriori){
    F.estimated <- sd.estimated^2/sd.apriori^2
    F.quantile <- qf(p = prob, df1 = df, df2 = 10^1000)
  }else{
    F.estimated <- sd.apriori^2/sd.estimated^2
    F.quantile <- qf(p = prob, df1 = 10^1000, df2 = df)
  }

  mlist <- list(F.estimated = F.estimated, F.quantile = F.quantile,
                model = if(F.estimated < F.quantile){
                  paste("sd.estimated =", round(sd.estimated, 2), "/ sd.apriori =", round(sd.apriori, 2), "/ Model is correct", sep = " ")} else{
                  paste("sd.estimated =", round(sd.estimated, 2), "/ sd.apriori =", round(sd.apriori, 2), "/ Model is not correct", sep = " ")
                }
                  )
  return(mlist)
}


#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param survey.net PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname Amat1D
Amat1D <- function(survey.net){
  survey.net[[2]] %<>% dplyr::filter(diff_level)
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
  Amat <- Amat[, !survey.net$points$FIX_1D] #%>% select(-fixed_points)
  Amat <- as.matrix(Amat)
  return(Amat)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param survey.net PARAM_DESCRIPTION
#' @param wdh_model PARAM_DESCRIPTION, Default: list("sd_dh", "d_dh", "n_dh", "E")
#' @param sd0 PARAM_DESCRIPTION, Default: 1
#' @param d0 PARAM_DESCRIPTION, Default: NA
#' @param n0 PARAM_DESCRIPTION, Default: 1
#' @param res.units PARAM_DESCRIPTION, Default: 'mm'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{mutate}}
#' @rdname Wmat1D
#' @importFrom dplyr mutate case_when
Wmat1D <- function(survey.net, wdh_model = list("sd_dh", "d_dh", "n_dh", "E"), sd0 = 1, d0 = NA, n0 = 1, res.units = "mm"){
  survey.net[[2]] %<>% dplyr::filter(diff_level)
  res.unit.lookup <- c("mm" = 1, "cm" = 10, "m" = 1000)
  "%!in%" <- Negate("%in%")
  if(wdh_model %!in% c("sd_dh", "d_dh", "n_dh", "E")){stop("Model of weigths is not properly specified, see help")}
  wdh_model <- wdh_model[[1]]
  if(is(survey.net[[2]], "sf")){survey.net[[2]] <- survey.net[[2]] %>% st_drop_geometry()}

  survey.net[[2]] %<>%
    dplyr::mutate(weigth = dplyr::case_when(
      wdh_model == "sd_dh" ~ (sd0/res.unit.lookup[res.units])^2/sd_dh^2,
      wdh_model == "d_dh" ~ 1/d_dh,
      wdh_model == "n_dh" ~ n0/n_dh,
      wdh_model == "E" ~ 1
    )
    )
  return(diag(survey.net[[2]]$weigth))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param survey.net PARAM_DESCRIPTION
#' @param units PARAM_DESCRIPTION, Default: units
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname fmat1D
fmat1D <- function(survey.net, units = units){
  survey.net[[2]] %<>% dplyr::filter(diff_level)
  unit.lookup <- c("mm" = 1000, "cm" = 100, "m" = 1)
  survey.net[[2]]$h_from <- survey.net[[1]]$h[match(survey.net[[2]]$from, survey.net[[1]]$Name)]
  survey.net[[2]]$h_to <- survey.net[[1]]$h[match(survey.net[[2]]$to, survey.net[[1]]$Name)]
  f <- ((survey.net[[2]]$h_to-survey.net[[2]]$h_from)-survey.net[[2]]$dh)*unit.lookup[units]
  return(f)
}

#' @title Geodetic control network adjustment and design
#' @description Adjustment and design computation of the 1D and 2D geodetic control network.
#' @param adjust logical. If TRUE adjustment will be performed. If FALSE the computation of the control network design quality will be conducted, Default: TRUE
#' @param survey.net surveynet object. Output from \code{read_surveynet} function
#' @param dim_type Type of the geodetic control network. Can be either 1D or 2D, Default: list("1D", "2D")
#' @param sd.apriori Apriori dispersion factor, Default: 1
#' @param wdh_model Weighted model for leveling measurements, Default: list("n_dh", "sd_dh", "d_dh", "E")
#' @param n0 Number of station in the reference measurements (only for 1D network), Default: 1
#' @param maxiter Maximum number of iterations in adjustment computation, Default: 50
#' @param prob Probability in statistical testing, Default: 0.95
#' @param output Type of the output. It can be either, \code{spatial} (\code{sf} classes) or \code{report} (data frame), Default: list("spatial", "report")
#' @param coord_tolerance Tolerance in coordinate difference in two iteration, Default: 0.001
#' @param result.units Units of the results, Default: list("mm", "cm", "m")
#' @param ellipse.scale Scale parameter for displaying absolute error ellipses, Default: 1
#' @param teta.unit Units for orientation angle of the error ellipses, Default: list("deg", "rad")
#' @param units.dir Units for the residuals of the angular measurements, Default: 'sec'
#' @param use.sd.estimated logical, if estimated reference standard deviation factor should be used, Default: TRUE
#' @param all logical, if specific computation matrices should be exported, Default: FALSE
#' @return if \code{outout = spatial} a list of three sf classes is exported. Otherwise \code{dataframe} object are exported.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[sf]{st_crs}},\code{\link[sf]{st_as_sf}},\code{\link[sf]{sfc}},\code{\link[sf]{sf}},\code{\link[sf]{st_geometry}}
#'  \code{\link[tidyr]{pivot_longer}},\code{\link[tidyr]{separate}},\code{\link[tidyr]{pivot_wider}}
#'  \code{\link[purrr]{when}},\code{\link[purrr]{keep}}
#'  \code{\link[dplyr]{select}},\code{\link[dplyr]{mutate}},\code{\link[dplyr]{arrange}},\code{\link[dplyr]{filter}},\code{\link[dplyr]{mutate-joins}},\code{\link[dplyr]{mutate_all}},\code{\link[dplyr]{rename}}
#'  \code{\link[stringr]{str_c}}
#'  \code{\link[MASS]{ginv}}
#' @rdname adjust.snet
#' @export 
#' @importFrom sf st_crs st_as_sf st_sfc st_sf st_drop_geometry st_set_crs
#' @importFrom tidyr pivot_longer separate pivot_wider
#' @importFrom purrr when discard
#' @importFrom dplyr select mutate arrange filter right_join mutate_all rename left_join
#' @importFrom stringr str_c
#' @importFrom MASS ginv
adjust.snet <- function(adjust = TRUE, survey.net, dim_type = list("1D", "2D"), sd.apriori = 1, wdh_model = list("n_dh", "sd_dh", "d_dh", "E"), n0 = 1, maxiter = 50, prob = 0.95, output = list("spatial", "report"), coord_tolerance = 1e-3, result.units = list("mm", "cm", "m"), ellipse.scale = 1, teta.unit = list("deg", "rad"), units.dir = "sec", use.sd.estimated = TRUE, all = TRUE){

  dim_type <- dim_type[[1]]
  output <- output[[1]]
  "%!in%" <- Negate("%in%")
  if(!adjust){use.sd.estimated <- FALSE}
  units <- result.units[[1]]
  res.unit.lookup <- c("mm" = 1000, "cm" = 100, "m" = 1)
  disp.unit.lookup <- c("mm" = 3, "cm" = 4, "m" = 5)
  dir.unit.lookup <- c("sec" = 3600, "min" = 60, "deg" = 1)
  if(is.na(sf::st_crs(survey.net$points) == TRUE)) {
    net.crs <- 3857
  }else(
    net.crs <- st_crs(survey.net$points)
  )
    
  # TODO: This has to be solved within the read.surveynet function
  used.points <- survey.net[[1]]$Name[survey.net[[1]]$Name %in% unique(c(survey.net[[2]]$from, survey.net[[2]]$to))]
  if(!!any(used.points %!in% survey.net[[1]]$Name)) stop(paste("There is no coordinates for point", used.points[which(used.points %!in% survey.net[[1]]$Name)]), sep = " ")
  survey.net[[1]] <- survey.net[[1]][which(survey.net[[1]]$Name %in% used.points), ]

  # Model
  if(dim_type == "2D"){
    observations <- tidyr::pivot_longer(survey.net[[2]] %>% purrr::when(is(., "sf") ~ st_drop_geometry(.), ~.) %>% dplyr::select(from, to, direction, distance), cols = c(direction, distance), names_to = "type", values_to = "used") %>%
      dplyr::mutate(across(.cols = "type", .fns = ~factor(., levels = c("direction", "distance")))) %>%
      dplyr::arrange(type) %>%
      dplyr::filter(used == TRUE) %>%
      dplyr::mutate(from_to = stringr::str_c(.$from, .$to, sep = "_"))

    # tidyr::gather zamenjeno sa tidyr::pivot_longer
    #tidyr::gather(survey.net[[2]] %>% purrr::when(is(., "sf") ~ st_drop_geometry(.), ~.) %>% dplyr::select(from, to, direction, distance), key = type, value = used, -c(from, to)) %>% dplyr::filter(used == TRUE) %>% dplyr::mutate(from_to = str_c(.$from, .$to, sep = "_"))

    fix.mat <- !rep(survey.net[[1]]$FIX_2D, each = 2)
    stations <- observations %>% dplyr::filter(type == "direction") %>% .$from %>% unique()
    if(length(fix.mat) != sum(fix.mat)){
      df <- (dim(observations)[1] - (sum(fix.mat) + length(stations))) #abs(diff(dim(A.mat)))
    }else{
      df <- (dim(observations)[1] - (sum(fix.mat) + length(stations))) + 3
    }
    if(adjust){
      max.coord.corr <- 1
      iter <- 0
      coords.iter_0 <- as.vector(t(cbind(survey.net[[1]]$x, survey.net[[1]]$y)))[fix.mat]
      while (max.coord.corr > coord_tolerance && iter < maxiter) {
        iter <- iter + 1
        coords.iter.inc <- as.vector(t(cbind(survey.net[[1]]$x, survey.net[[1]]$y)))[fix.mat]
        A.mat <- Amat(survey.net, units = units)
        W.mat <- Wmat(survey.net, sd.apriori = sd.apriori, res.units = units)
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
        coords.est <- coords.iter.inc + x.mat[1:sum(fix.mat)]/res.unit.lookup[units]
        survey.net[[1]][!survey.net[[1]]$FIX_2D, c("x", "y")] <- matrix(coords.est, ncol = 2, byrow = TRUE)
        survey.net[[1]] <- survey.net[[1]] %>% st_drop_geometry() %>% sf::st_as_sf(coords = c("x","y"), remove = FALSE)
        survey.net[[2]]$x_from <- survey.net[[1]]$x[match(survey.net[[2]]$from, survey.net[[1]]$Name)]
        survey.net[[2]]$y_from <- survey.net[[1]]$y[match(survey.net[[2]]$from, survey.net[[1]]$Name)]
        survey.net[[2]]$x_to <- survey.net[[1]]$x[match(survey.net[[2]]$to, survey.net[[1]]$Name)]
        survey.net[[2]]$y_to <- survey.net[[1]]$y[match(survey.net[[2]]$to, survey.net[[1]]$Name)]
        survey.net[[2]] <- survey.net[[2]] %>% dplyr::mutate(id = seq.int(nrow(.))) %>% split(., f = as.factor(.$id)) %>%
          lapply(., function(row) {lmat <- matrix(unlist(row[c("x_from", "y_from", "x_to", "y_to")]), ncol = 2, byrow = TRUE)
          st_linestring(lmat)}) %>%
          sf::st_sfc() %>%
          sf::st_sf(survey.net[[2]], 'geometry' = .)
        max.coord.corr <- max(coords.est-coords.iter.inc)
      }
      x.mat <- (coords.est-coords.iter_0)*res.unit.lookup[units] #x.mat[1:sum(fix.mat)]
      sd.estimated <- sqrt((crossprod(v.mat, W.mat) %*% v.mat)/(df))
      model_adequacy <- model_adequacy_test(sd.apriori, sd.estimated, df, prob = prob)
      
      results <- data.frame(res = v.mat, f = f.mat, Kl = c(sd.apriori^2)*diag(Ql.mat), Kv =  c(sd.apriori^2)*diag(Qv.mat), rii = diag(r), Baarda.test = as.numeric(abs(v.mat)/c(sd.apriori)*(sqrt(diag(Qv.mat)))))
      
      adj.directions <- survey.net[[2]] %>% sf::st_drop_geometry() %>% 
        dplyr::mutate(from_to = stringr::str_c(.$from, .$to, sep = "_")) %>%
        dplyr::filter(direction) %>% 
        dplyr::mutate(Observations = paste(HzD, HzM, HzS, sep = " "), type = "direction") %>%
        cbind(results[observations$type == "direction", ]) %>% 
        dplyr::mutate(Adj_meas = Hz + res/dir.unit.lookup[units.dir]) %>%
        dplyr::mutate(Adj_meas = dplyr::if_else(Adj_meas < 0, Adj_meas + 360, Adj_meas + 0)) %>%
        dplyr::mutate(across(.cols = c("res", "f", "Kl", "Kv", "rii", "Baarda.test"), ~round(.x, disp.unit.lookup[units]))) %>%
        dplyr::mutate(Adj.observations = dec2dms(Adj_meas)) %>%
        dplyr::select(from = from, to = to, type = type, Observations, Residuals = res, Adj.observations, f, Kl, Kv, rii, Baarda.test)
      
      
      adj.distances <- survey.net[[2]] %>% sf::st_drop_geometry() %>% 
        dplyr::mutate(from_to = stringr::str_c(.$from, .$to, sep = "_")) %>%
        dplyr::filter(distance) %>% 
        dplyr::mutate(Observations = paste(HD), type = "distance") %>%
        cbind(results[observations$type == "distance", ]) %>% 
        dplyr::mutate(Adj.observations = HD + res/res.unit.lookup[units]) %>%
        dplyr::mutate(across(.cols = c("res", "f", "Kl", "Kv", "rii", "Baarda.test"), ~round(.x, disp.unit.lookup[units]))) %>%
        dplyr::select(from = from, to = to, type = type, Observations, Residuals = res, Adj.observations, f, Kl, Kv, rii, Baarda.test)

      observations <- rbind(adj.directions, adj.distances)  

        
      # observations <- survey.net[[2]] %>% 
      #   sf::st_drop_geometry() %>%
      #   dplyr::select(ID:dh, "Hz", "Vz", "tdh") %>% 
      #   purrr::discard(~all(is.na(.x))) %>%
      #   dplyr::right_join(., observations[, c("from", "to", "type")], by = c("from", "to")) %>%
      #   dplyr::arrange(type) %>%
      #   dplyr::mutate(Observations = dplyr::if_else(type == "distance", paste(HD), paste(HzD, HzM, HzS, sep = " "))) %>%
      #   dplyr::mutate(res = v.mat, f = f.mat, Kl = c(sd.apriori^2)*diag(Ql.mat), Kv =  c(sd.apriori^2)*diag(Qv.mat), rii = diag(r) ) %>%
      #   dplyr::mutate(Adj_meas = dplyr::if_else(type == "direction", Hz + res/dir.unit.lookup[units.dir], HD + res/res.unit.lookup[units])) %>%
      #   dplyr::mutate(Adj_meas = dplyr::if_else(type == "direction" & Adj_meas < 0, Adj_meas + 360, Adj_meas + 0),
      #                 Baarda.test = as.numeric(abs(v.mat)/c(sd.apriori)*(sqrt(diag(Qv.mat))))) %>%
      #   dplyr::mutate(across(.cols = c("res", "f", "Kl", "Kv", "rii", "Baarda.test"), ~round(.x, disp.unit.lookup[units]))) %>%
      #   dplyr::mutate(Adj.observations = dplyr::if_else(type == "distance", paste(HD), dec2dms(Adj_meas)))
      
      
      if(model_adequacy[[1]] & use.sd.estimated){sigma_apriori <- sd.apriori; sd.apriori <- sd.estimated}

      # Results
      coords.inc <- data.frame(parameter = colnames(A.mat)[1:sum(fix.mat)], coords.inc = as.numeric(x.mat))
      coords.inc <- coords.inc %>%
        tidyr::separate(.,col = parameter, into = c("Name", "inc.name"), sep = "_d") %>%
        tidyr::pivot_wider(., names_from = c(inc.name), values_from = c(coords.inc)) %>%
        dplyr::mutate_all(., ~replace(., is.na(.), 0)) %>%
        dplyr::rename(dx = x, dy = y)


      # TODO: Ovo treba razdvojiti u slucaju da je report ili sp!!!
      point.adj.results <- dplyr::left_join(survey.net[[1]], coords.inc, by = "Name") %>%
        dplyr::mutate(across(.cols = c("dx", "dy"), ~replace(., is.na(.), 0))) %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(x0 = x - dx/res.unit.lookup[units], y0 = y - dy/res.unit.lookup[units]) %>%
        dplyr::mutate(across(.cols = c("dx", "dy"), ~round(.x, disp.unit.lookup[units]))) %>%
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

      observations <- observations %>% dplyr::mutate(Kl =  c(sd.apriori^2)*diag(Ql.mat), Kv =  c(sd.apriori^2)*diag(Qv.mat), rii = diag(r)) %>%
        dplyr::mutate(across(where(is.numeric), ~round(.x, disp.unit.lookup[units])))
    }
    # Computing error ellipses
    Qxy.list <- Qxy(Qx.mat, fix = !survey.net[[1]]$FIX_2D)
    ellipses <- lapply(Qxy.list, function(x) error.ellipse(x, prob = prob, sd.apriori = sd.apriori, teta.unit = teta.unit[[1]]))
    ellipses <- do.call(rbind, ellipses) %>%
      as.data.frame() %>%
      dplyr::select(A = V1, B = V2, teta = V3) %>%
      mutate(Name = used.points) %>%
      dplyr::mutate(across(where(is.numeric), ~round(.x, disp.unit.lookup[units])))
    # Computing parameters sigmas
    sigmas <- lapply(Qxy.list, function(x) sigma.xy(x, sd.apriori = sd.apriori)) %>%
      do.call(rbind,.) %>%
      as.data.frame() %>%
      dplyr::select(sx = V1, sy = V2) %>% #TODO: proveriti da li ovde treba voditi racuna o redosledu sx i sy.
      dplyr::mutate(sp = sqrt(sx^2 + sy^2), Name = used.points) %>%
      dplyr::mutate(across(where(is.numeric), ~round(.x, disp.unit.lookup[units])))

    if(adjust){
      if(output == "spatial"){
        points <- merge(point.adj.results, ellipses, by = "Name") %>% merge(., sigmas) %>% dplyr::arrange(id)
        points %<>% sf::st_set_crs(., net.crs)#st_crs(survey.net[[2]]))
      }else{
        points <- merge(sf::st_drop_geometry(point.adj.results), ellipses, by = "Name") %>% merge(., sigmas) %>% dplyr::arrange(id)
      }
    }else{
      if(output == "spatial"){
        points <- merge(survey.net[[1]], ellipses, by = "Name") %>% merge(., sigmas) %>% dplyr::arrange(id)
        points %<>% sf::st_set_crs(., net.crs)#st_crs(survey.net[[2]]))
      }else{
        points <- merge(sf::st_drop_geometry(survey.net[[1]]), ellipses, by = "Name") %>% merge(., sigmas) %>% dplyr::arrange(id)
      }
    }

    if(output == "spatial"){
      # Preparing ellipses as separate sf outcome
      # TODO Proveriti da li elipse uzimaju definitivne koordinate ili priblizne!
      ellipse.net <- do.call(rbind, lapply(split(points, factor(survey.net[[1]]$Name, levels = points$Name)), function(x) sf.ellipse(x, scale = ellipse.scale)))
      ellipse.net <- merge(ellipse.net, sigmas)
      ellipse.net %<>% sf::st_set_crs(., net.crs)#st_crs(survey.net[[2]]))

      points <- list(net.points = points, ellipse.net = ellipse.net)
    }


# ====================================== 1D ====================================
  }else{
    wdh = wdh_model[[1]]
    observations <- survey.net[[2]] %>%
      purrr::when(is(., "sf") ~ st_drop_geometry(.), ~.) %>%
      dplyr::filter(diff_level) %>%
      dplyr::select(from, to, dh, all_of(wdh)) %>%
      dplyr::mutate(from_to = stringr::str_c(.$from, .$to, sep = "_"))


    fix.mat <- !(survey.net[[1]]$FIX_1D)

    A.mat <- Amat1D(survey.net)
    W.mat <- Wmat1D(survey.net = survey.net, wdh_model = wdh, n0 = 1, res.units = units)
    rownames(A.mat) <- observations$from_to
    colnames(W.mat) <- observations$from_to
    rownames(W.mat) <- observations$from_to
    if(length(fix.mat) != sum(fix.mat)){
      df <- (dim(observations)[1] - sum(fix.mat)) #abs(diff(dim(A.mat)))
    }else{
      df <- (dim(observations)[1] - sum(fix.mat)) + 1
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
      max.coord.corr <- 1
      iter <- 0
      coords.iter_0 <- as.vector(survey.net[[1]]$h)[fix.mat]
      while (max.coord.corr > coord_tolerance && iter < maxiter) {
        iter <- iter + 1
        coords.iter.inc <- survey.net[[1]]$h[fix.mat]
        f.mat <- fmat1D(survey.net = survey.net, units = units)
        n.mat <- crossprod(A.mat, W.mat) %*% f.mat
        x.mat <- -Qx.mat %*% n.mat
        v.mat <- A.mat%*%x.mat + f.mat
        survey.net[[1]]$h[fix.mat] <- survey.net[[1]]$h[fix.mat] + x.mat/res.unit.lookup[units]
        max.coord.corr <- max(abs(survey.net[[1]]$h[fix.mat]-coords.iter.inc))
      }
      x.mat <- x.mat[1:sum(fix.mat)]  #
      h.inc <- (survey.net[[1]]$h[fix.mat]-coords.iter_0)*res.unit.lookup[units]
      sd.estimated <- sqrt((crossprod(v.mat, W.mat) %*% v.mat)/(df))
      sd.apriori <- sd.apriori/(1000/res.unit.lookup[units])
      model_adequacy <- model_adequacy_test(sd.apriori, sd.estimated, df, prob = prob)
      if(model_adequacy[[1]] & use.sd.estimated){sigma_apriori <- sd.apriori; sd.apriori <- sd.estimated}
    }
      # Results
      if(adjust){
        h.inc <- data.frame(Name = as.character(colnames(A.mat)), dh = as.numeric(h.inc), sd_h = c(sd.apriori)*sqrt(diag(Qx.mat)), stringsAsFactors  = FALSE)
        points <- dplyr::left_join(survey.net[[1]], h.inc, by = "Name") %>%
          dplyr::mutate(across(.cols = c("dh", "sd_h"), ~replace(., is.na(.), 0))) %>%
          dplyr::mutate(h0 = h - dh/res.unit.lookup[units]) %>%
          dplyr::mutate(across(.cols = c("dh"), ~round(.x, disp.unit.lookup[units]))) %>%
          dplyr::select(id, Name, x, y, h0, dh, h, sd_h, FIX_1D, Point_object)


        observations <- observations %>%
          dplyr::mutate(residuals = as.numeric(v.mat),
                        adj.dh = dh + residuals/res.unit.lookup[units],
                        f = f.mat, Kl = c(sd.apriori^2)*diag(Ql.mat),
                        Kv =  c(sd.apriori^2)*diag(Qv.mat), rii = diag(r),
                        Baarda.test = as.numeric(abs(v.mat)/c(sd.apriori)*(sqrt(diag(Qv.mat))))) %>%
          dplyr::mutate(across(.cols = c("residuals", "f", "Kl", "Kv", "rii", "Baarda.test"), ~round(.x, disp.unit.lookup[units])))


      }else{
        h.inc <- data.frame(Name = as.character(colnames(A.mat)), sd_h = c(sd.apriori)*sqrt(diag(Qx.mat)), dp = sqrt((7.459*sd.apriori^2)/(1/diag(2*Qx.mat))), stringsAsFactors  = FALSE)
        points <- dplyr::left_join(survey.net[[1]], h.inc, by = "Name") %>%
          dplyr::mutate(across(.cols = c("sd_h", "dp"), ~replace(., is.na(.), 0))) %>%
          dplyr::mutate(across(.cols = c("sd_h", "dp"), ~round(.x, disp.unit.lookup[units]))) %>%
          dplyr::select(id, Name, x, y, h, sd_h, dp, FIX_1D, Point_object)

        observations <- observations %>% dplyr::select(-from_to) %>%
          dplyr::mutate(Kl =  c(sd.apriori^2)*diag(Ql.mat),
                        Kv =  c(sd.apriori^2)*diag(Qv.mat),
                        rii = diag(r)) %>%
          dplyr::mutate(across(where(is.numeric), ~round(.x, disp.unit.lookup[units])))
      }
  }
  
  #====== End of adjustment ====================================================
  
  if(adjust){
    matrices = list(A = A.mat, W = W.mat, Qx = Qx.mat, Ql = Ql.mat, Qv = Qv.mat, f = f.mat)
  }else{
    matrices = list(A = A.mat, W = W.mat, Qx = Qx.mat, Ql = Ql.mat, Qv = Qv.mat)
  }

  if(output == "spatial"){
    if(sum(rowSums(is.na(survey.net[[1]][, c("x", "y")])) != 0) == 0){
      observations <- observations %>%
        dplyr::mutate(x_from = survey.net[[1]]$x[match(observations$from, survey.net[[1]]$Name)],
                      y_from = survey.net[[1]]$y[match(observations$from, survey.net[[1]]$Name)],
                      x_to = survey.net[[1]]$x[match(observations$to, survey.net[[1]]$Name)],
                      y_to = survey.net[[1]]$y[match(observations$to, survey.net[[1]]$Name)])

      observations <- observations %>%
        dplyr::mutate(id = seq.int(nrow(.))) %>%
        split(., f = as.factor(.$id)) %>%
        lapply(., function(row) {lmat <- matrix(unlist(row[c("x_from", "y_from", "x_to", "y_to")]), ncol = 2, byrow = TRUE)
        st_linestring(lmat)}) %>%
        sf::st_sfc() %>%
        sf::st_sf('ID' = seq.int(nrow(observations)), observations, 'geometry' = .) %>%
        dplyr::select(-c(x_from, y_from, x_to, y_to))
      observations %<>% sf::st_set_crs(.,net.crs)#st_crs(survey.net[[2]]))

    }
  }

  if(dim_type == "2D"){
    if(adjust){
      Adjustment_summary = list(Type = if(sum(!fix.mat) == 0){"inner constrained"}else{"constrained"},
                                Dimensions = dim_type,
                                "Fixed points" = if(sum(survey.net[[1]]$FIX_2D) != 0){survey.net[[1]]$Name[survey.net[[1]]$FIX_2D]}else{"None"},
                                "Number of stations" = length(stations),
                                "Number of Directions" = sum(observations$type == "direction"),
                                "Number of Distances" = sum(observations$type == "distance"),
                                "Unknown coordinates" = sum(fix.mat),
                                "Unknown orientations" = observations %>% dplyr::filter(type == "direction") %>% .$from %>% unique(.) %>% length(),
                                "Degrees of freedom" = df,
                                "Number of iterations" = iter,
                                "Max.coordinate correction in last iteration:" = max.coord.corr,
                                "sigma apriori" = if(model_adequacy[[1]] & use.sd.estimated){sigma_apriori}else{sd.apriori},
                                "sigma aposteriori" = sd.estimated,
                                "Testing Probability" = prob,
                                "F-test" = model_adequacy[[2]],
                                "Crital value F-test" = model_adequacy[[3]],
                                "Test decision" = model_adequacy[[4]])
      results <- list(Summary = Adjustment_summary, Points = points, Observations = observations %>% dplyr::select(ID, from, to, type, Observations, Residuals = Residuals, Adj.observations, Kl, Kv, rii, Baarda.test), Matrices = matrices)

    }else{
      Adjustment_summary = list(Type = if(sum(!fix.mat) == 0){"inner constrained"}else{"constrained"},
                                Dimensions = dim_type,
                                "Fixed points" = if(sum(survey.net[[1]]$FIX_2D) != 0){survey.net[[1]]$Name[survey.net[[1]]$FIX_2D]}else{"None"},
                                "Number of stations" = length(stations),
                                Directions = sum(observations$type == "direction"),
                                Distances = sum(observations$type == "distance"),
                                "Unknown coordinates" = sum(fix.mat),
                                "Unknown orientations" = observations %>% dplyr::filter(type == "direction") %>% .$from %>% unique(.) %>% length(),
                                "Degrees of freedom" = df,
                                "sigma apriori" = sd.apriori)
      results <- list(Summary = Adjustment_summary, Points = points, Observations = observations, Matrices = matrices)

    }
  }else{
    if(adjust){
      Adjustment_summary = list(Type = if(sum(!fix.mat) == 0){"inner constrained"}else{"constrained"},
                                Dimensions = dim_type,
                                "Fixed points" = if(sum(survey.net[[1]]$FIX_1D) != 0){survey.net[[1]]$Name[survey.net[[1]]$FIX_1D]}else{"None"},
                                "Weightening model" = wdh_model[[1]],
                                "Number of measured height differences" = dim(observations)[1],
                                "Unknown heights" = sum(fix.mat),
                                "Degrees of freedom" = df,
                                "Number of iterations" = iter,
                                "Max.coordinate correction in last iteration:" = max.coord.corr,
                                "sigma apriori" = if(model_adequacy[[1]] & use.sd.estimated){sigma_apriori}else{sd.apriori},
                                "sigma aposteriori" = sd.estimated,
                                "Testing Probability" = prob,
                                "F-test" = model_adequacy[[2]],
                                "Crital value F-test" = model_adequacy[[3]],
                                "Test decision" = model_adequacy[[4]])
      results <- list(Summary = Adjustment_summary, Points = points, Observations = observations, Matrices = matrices)

    }else{
      Adjustment_summary = list(Type = if(sum(!fix.mat) == 0){"inner constrained"}else{"constrained"},
                                Dimensions = dim_type,
                                "Fixed points" = if(sum(survey.net[[1]]$FIX_1D) != 0){survey.net[[1]]$Name[survey.net[[1]]$FIX_1D]}else{"None"},
                                "Weightening model" = wdh_model,
                                "Number of measured height differences" = dim(observations)[1],
                                "Unknown heights" = sum(fix.mat),
                                "Degrees of freedom" = df)
                                #"Number of iterations" = iter,
                                #"Max.height correction in last iteration:" = max.coord.corr,
                                #"sigma apriori" = sigma_apriori,
                                #"sigma aposteriori" = sd.estimated,
                                #"Testing Probability" = prob,
                                #"F-test" = model_adequacy[[2]],
                                #"Crital value F-test" = model_adequacy[[3]],
                                #"Test decision" = model_adequacy[[4]])
      results <- list(Summary = Adjustment_summary, Points = points, Observations = observations, Matrices = matrices)

    }
  }

    if(!all){
      results <- results[-4]
    }else{
      results <- results
    }

  return(results)
}



