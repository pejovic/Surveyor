# Project: Surveyer
# Description: Land and Engineering Surveying utilities
# Authors: Milutin Pejovic, Petar Bursac, Milan Kilibarda, Branislav Bajat, Aleksandar Sekulic
# Date:

# Functions:


### coeficients for distances #####################################################

coef_d <- function (pt1, pt2, pts, units, axes = c("Easting", "Northing")) {
  yind <- which("Easting" == axes)
  xind <- which("Northing" == axes)
  units.table <- c("mm" = 1000, "cm" = 100, "m" = 1)
  pt1 <- as.numeric(pt1)
  pt2 <- as.numeric(pt2)
  coords <- pts
  vec_d <- c(rep(0, dim(pts)[1]*2))#c(rep(0, length(coords)))

  x_coords <- coords[, xind]
  y_coords <- coords[, yind]
  x1 <- pt1[xind]
  y1 <- pt1[yind]
  x2 <- pt2[xind]
  y2 <- pt2[yind]
  dx1 <- (x_coords-x1)
  dy1 <- (y_coords-y1)
  dx2 <- (x_coords-x2)
  dy2 <- (y_coords-y2)
  i <- which(dx1 == dy1 & dx1 == 0 & dy1 == 0)
  j <- which(dx2 == dy2 & dx2 == 0 & dy2 == 0)

  dx <- (x2-x1)*units.table[units]
  dy <- (y2-y1)*units.table[units]
  d <- sqrt(dx^2+dy^2)
  A <- (-dx/d)
  B <- (-dy/d)
  A1 <- -A
  B1 <- -B

  if(xind == 1){
    vec_d[2*i-1] <- A
    vec_d[2*j-1] <- A1
    vec_d[2*i] <- B
    vec_d[2*j] <- B1
  }else{
    vec_d[2*i-1] <- B
    vec_d[2*j-1] <- B1
    vec_d[2*i] <- A
    vec_d[2*j] <- A1
  }

  return(vec_d)
}

### coeficients for directions (pravac) #####################################################

coef_p <- function (pt1, pt2, pts, units, axes = c("Easting", "Northing")) {
  yind <- which("Easting" == axes)
  xind <- which("Northing" == axes)
  units.table <- c("mm" = 1000, "cm" = 100, "m" = 1)
  pt1 <- as.numeric(pt1)
  pt2 <- as.numeric(pt2)
  coords <- pts
  vec_p <- c(rep(0, dim(pts)[1]*2))#c(rep(0, length(coords)))
  ro <- 180/pi*3600

  x_coords <- coords[, xind]
  y_coords <- coords[, yind]
  x1 <- pt1[xind]
  y1 <- pt1[yind]
  x2 <- pt2[xind]
  y2 <- pt2[yind]
  dx1 <- (x_coords-x1)
  dy1 <- (y_coords-y1)
  dx2 <- (x_coords-x2)
  dy2 <- (y_coords-y2)
  i <- which(dx1 == dy1 & dx1 == 0 & dy1 == 0)
  j <- which(dx2 == dy2 & dx2 == 0 & dy2 == 0)

  dx <- (x2-x1)*units.table[units]
  dy <- (y2-y1)*units.table[units]
  d <- sqrt(dx^2 + dy^2)
  A <- (ro*dy/d^2)
  B <- (-ro*dx/d^2)
  A1 <- -(ro*dy/d^2)
  B1 <- -(-ro*dx/d^2)

  if(xind == 1){
    vec_p[2*i-1] <- A
    vec_p[2*j-1] <- A1
    vec_p[2*i] <- B
    vec_p[2*j] <- B1
  }else{
    vec_p[2*i-1] <- B
    vec_p[2*j-1] <- B1
    vec_p[2*i] <- A
    vec_p[2*j] <- A1
  }

  return(vec_p)
}


fix.params <- function(net.points, axes = c("Easting", "Northing")){
  if(("Easting" == axes)[1]) {
    as.logical(c(apply(cbind(net.points$FIX_X, net.points$FIX_Y), 1, as.numeric)))
  }else{
    as.logical(c(apply(cbind(net.points$FIX_Y, net.points$FIX_X), 1, as.numeric)))
  }
}


Amat <- function(survey.net, units, axes = c("Easting", "Northing")){

  A_dir <- survey.net[[2]] %>% dplyr::filter(direction) %>% st_coordinates() %>% as.data.frame() %>% mutate_at(vars(L1), as.factor) %>%
    split(., .$L1) %>%
    lapply(., function(x) coef_p(pt1 = x[1, 1:2], pt2 = x[2, 1:2], pts = st_coordinates(survey.net[[1]][, 1:2]), units = units, axes = axes)) %>%
    do.call(rbind, .)

  A_dist <- survey.net[[2]] %>% dplyr::filter(distance) %>% st_coordinates() %>% as.data.frame() %>% mutate_at(vars(L1), as.factor) %>%
    split(., .$L1) %>%
    lapply(., function(x) coef_d(pt1 = x[1, 1:2], pt2 = x[2, 1:2], pts = st_coordinates(survey.net[[1]][, 1:2]), units = units, axes = axes)) %>%
    do.call(rbind, .)

  station.names <- survey.net[[2]] %>% dplyr::filter(direction) %>% dplyr::select(from) %>% st_drop_geometry() %>% unique() %>% unlist(use.names = FALSE)

  Z_mat <- survey.net[[2]] %>% dplyr::filter(direction) %>%
    tidyr::spread(key = from, value = direction, fill = FALSE) %>%
    dplyr::select(station.names) %>%
    st_drop_geometry() %>%
    as.matrix()*1

  fix <- fix.params(net.points = survey.net[[1]], axes = axes)

  rest_mat <- matrix(0, nrow = dim(A_dist)[1], ncol = dim(Z_mat)[2])

  A <- cbind(rbind(A_dir, A_dist)[, !fix], rbind(Z_mat, rest_mat))

  if(("Easting" == axes)[1]) {sufix <- c("dE", "dN")} else {sufix <- c("dN", "dE")}
  colnames(A) <- c(paste(rep(survey.net[[1]]$Name, each = 2), rep(sufix, length(survey.net[[1]]$Name)), sep = "_")[!fix], paste(colnames(Z_mat), "z", sep = "_"))
  return(A)
}


# Weights matrix
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



# Funkcija koja izdvaja elemente Qx matrice u listu za elipsu svake tacke
Qxy <- function(Qx, n, fixd = fix){
  k = 0
  fixd <- cbind(fixd, fixd[,1] + fixd[, 2])
  Qxxx <- as.list(rep(NA, dim(fixd)[1]))
  for(i in 1:length(Qxxx)){
    k = fixd[i, 1] + fixd[i, 2] + k
    if(fixd[i, 3] == 1){
      Qxxx[[i]] <- diag(fixd[i, c(1, 2)])*Qx[k, k]
      }
    else if (fixd[i, 3] == 2){
      Qxxx[[i]] <- cbind(c(Qx[k-1, k-1], Qx[k-1, k]), c(Qx[k, k-1], Qx[k, k]))
    } else {
      Qxxx[[i]] <- diag(fixd[i, c(1, 2)])
    }
  }
  return(Qxxx)
}
#

error.ellipse <- function(Qxy, prob = NA, apriori = 1, axes = c("Easting", "Northing"), teta.unit = list("deg", "rad")) {
  if(("Easting" == axes)[1]){
    Qee <- Qxy[1, 1]
    Qnn <- Qxy[2, 2]
    Qen <- Qxy[1, 2]
  }else{
    Qee <- Qxy[2, 2]
    Qnn <- Qxy[1, 1]
    Qen <- Qxy[1, 2]
  }
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
      A <- apriori*sqrt(lambda1)
      B <- apriori*sqrt(lambda2)
    }else{
      A <- apriori*sqrt(lambda1*qchisq(prob, df = 2))
      B <- apriori*sqrt(lambda2*qchisq(prob, df = 2))
    }
    teta <- ifelse((Qnn - Qee) == 0, 0, 0.5*atan(2*Qen/(Qnn - Qee)))
    teta <- ifelse(teta >= 0, teta, teta + 2*pi)
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

sigma.xy <- function(Qxy.mat, apriori){
  sigma <- diag(diag(apriori, 2, 2)%*%diag(sqrt(diag(Qxy.mat)), 2, 2))
}

design.snet <- function(survey.net, apriori = 1, prob = NA, result.units = list("mm", "cm", "m"), ellipse.scale = 1, axes = c("Easting", "Northing"), teta.unit = list("deg", "rad"), all = FALSE){
  # TODO: Set warning if there are different or not used points in two elements of survey.net list.
  # Check which points are used for measurements, if not
  used.points <- unique(do.call(c, survey.net[[2]][, c("from", "to")] %>% st_drop_geometry()))
  used.points.ind <- which(survey.net[[1]]$Name %in% used.points)
  used.points <- survey.net[[1]]$Name[used.points.ind]
  survey.net[[1]] <- survey.net[[1]][used.points.ind, ]
  observations <- tidyr::gather(survey.net[[2]] %>%
    dplyr::select(from, to, direction, distance, geometry), key = type, value = used, -c(from, to, geometry)) %>%
    dplyr::filter(used == TRUE) %>%
    dplyr::mutate(from_to = str_c(.$from, .$to, sep = "_"))
  # =======
  units <- result.units[[1]]
  A <- Amat(survey.net, units = units, axes = axes)
  rownames(A) <- observations$from_to
  W <- Wmat(survey.net)
  colnames(W) <- observations$from_to
  rownames(W) <- observations$from_to
  N <- crossprod(A, W) %*% A
  Qx <- tryCatch(
    {
      x = Qx = solve(N)
    },
    error = function(e) {
      x = Qx = MASS::ginv(N)
    })
  colnames(Qx) <- colnames(N)
  rownames(Qx) <- rownames(N)
  Ql <- A %*% tcrossprod(Qx, A)
  Qv <- solve(W) - Ql
  r <- Qv%*%W
  fix <- if(("Easting" == axes)[1]){
    survey.net[[1]] %>% st_drop_geometry() %>%  dplyr::select(FIX_X, FIX_Y) == FALSE
  }else{
    survey.net[[1]] %>% st_drop_geometry() %>%  dplyr::select(FIX_Y, FIX_X) == FALSE
  }
  Qxy.list <- Qxy(Qx, n = lenght(used.points), fixd = fix*1)
  ellipses <- lapply(Qxy.list, function(x) error.ellipse(x, prob = 0.95, apriori = apriori, axes = axes, teta.unit = teta.unit[[1]]))
  ellipses <- do.call(rbind, ellipses) %>%
    as.data.frame() %>%
    dplyr::select(A = V1, B = V2, teta = V3) %>%
    mutate(Name = used.points)
  sigmas <- lapply(Qxy.list, function(x) sigma.xy(x, apriori = apriori)) %>%
    do.call(rbind,.) %>%
    as.data.frame() %>%
    dplyr::select(sx = V1, sy = V2) %>% #TODO: proveriti da li ovde treba voditi racuna o redosledu sx i sy.
    dplyr::mutate(sp = sqrt(sx^2 + sy^2), Name = used.points)
  survey.net[[1]] <- merge(survey.net[[1]], ellipses, by = "Name") %>% merge(., sigmas)
  observations <- observations %>% dplyr::mutate(Ql = diag(Ql), Qv = diag(Qv), rii = diag(r))
  ellipse.net <- do.call(rbind, lapply(split(survey.net[[1]], survey.net[[1]]$Name), function(x) sf.ellipse(x, scale = ellipse.scale)))
  ellipse.net <- merge(ellipse.net, sigmas)
  ellipse.net %<>% sf::st_set_crs(st_crs(survey.net[[1]]))
  design <- list(design.matrices = list(A = A, W = W, Qx = Qx, Ql = Ql, Qv = Qv), ellipse.net = ellipse.net, net.points = survey.net[[1]], observations = observations)
  if(all){
    return(design)
  }else{
    return(design[-1])
  }
}



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

  observations$distance[!is.na(observations$sd_dist)] <- TRUE
  observations$direction[!is.na(observations$sd_Hz)] <- TRUE

  observations <- as.data.table(observations) %>% dplyr::mutate(id = seq.int(nrow(.))) %>% split(., f = as.factor(.$id)) %>%
    lapply(., function(row) {lmat <- matrix(unlist(row[14:17]), ncol = 2, byrow = TRUE)
    st_linestring(lmat)}) %>%
    sf::st_sfc() %>%
    sf::st_sf('ID' = seq.int(nrow(observations)), observations, 'geometry' = .)

  # Creating list
  survey_net <- list(points, observations = observations)
  return(survey_net)
}



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





