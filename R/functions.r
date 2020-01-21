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


fix.params <- function(net.points){
  as.logical(c(apply(cbind(net.points$FIX_X, net.points$FIX_Y), 1, as.numeric)))
}

# survey.net <- brana
Amat <- function(survey.net, units){

  if(!all(is.na(survey.net[[2]]$sd_Hz))){
    A_dir <- survey.net[[2]] %>% dplyr::filter(direction) %>% st_coordinates() %>% as.data.frame() %>% mutate_at(vars(L1), as.factor) %>%
      split(., .$L1) %>%
      lapply(., function(x) coef_p(pt1 = x[1, 1:2], pt2 = x[2, 1:2], pts = st_coordinates(survey.net[[1]][, 1:2]), units = units)) %>%
      do.call(rbind, .)
  }else{
    A_dir <- NULL
  }

  if(!all(is.na(survey.net[[2]]$sd_dist))){
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

  fix <- fix.params(net.points = survey.net[[1]])
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


# adjust = TRUE; survey.net = brana; sd.apriori = 3; prob = 0.95; result.units = list("mm", "cm", "m"); ellipse.scale = 1; teta.unit = list("deg", "rad"); all = FALSE; units.dir = "sec"; units.dist = "mm"
adjust.snet <- function(adjust = TRUE, survey.net, sd.apriori = 1, prob = 0.95, result.units = list("mm", "cm", "m"), ellipse.scale = 1, teta.unit = list("deg", "rad"), units.dir = "sec", units.dist = "mm", use.sd.estimated = TRUE, all = FALSE){
  # TODO: Set warning if there are different or not used points in two elements of survey.net list.
  # TODO: Check if any point has no sufficient measurements to be adjusted.

  "%!in%" <- Negate("%in%")
  units <- result.units[[1]]
  res.unit.lookup <- c("mm" = 1000, "cm" = 100, "m" = 1)
  disp.unit.lookup <- c("mm" = 1, "cm" = 2, "m" = 3)
  used.points <- unique(do.call(c, survey.net[[2]][, c("from", "to")] %>% st_drop_geometry()))
  if(!!any(used.points %!in% survey.net[[1]]$Name)) stop(paste("There is no coordinates for point", used.points[which(used.points %!in% survey.net[[1]]$Name)]), sep = " ")
  used.points.ind <- which(survey.net[[1]]$Name %in% used.points)
  used.points <- survey.net[[1]]$Name[used.points.ind]
  survey.net[[1]] <- survey.net[[1]][used.points.ind, ]
  fix.mat <- survey.net[[1]] %>% st_drop_geometry() %>%  dplyr::select(FIX_X, FIX_Y) == FALSE
  observations <- tidyr::gather(survey.net[[2]] %>%
                                  dplyr::select(from, to, direction, distance, geometry), key = type, value = used, -c(from, to, geometry)) %>%
    dplyr::filter(used == TRUE) %>%
    dplyr::mutate(from_to = str_c(.$from, .$to, sep = "_"))
  # =
  A.mat <- Amat(survey.net, units = units)
  rownames(A.mat) <- observations$from_to # Javlja se problem kada su merene duzine izmedju fiksnih tacaka, a A.mat ih iskljucujemo.
  W.mat <- Wmat(survey.net, sd.apriori = sd.apriori) #TODO: Check if each observations has its own standard.
  colnames(W.mat) <- observations$from_to
  rownames(W.mat) <- observations$from_to # Javlja se problem kada su merene duzine izmedju fiksnih tacaka, a A.mat ih iskljucujemo.
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
  if(length(fix.mat) != sum(fix.mat)){
    df <- abs(diff(dim(A.mat)))
  }else{
    df <- abs(diff(dim(A.mat))) + 3
  }
  if(adjust){
    f.mat <- fmat(survey.net = survey.net, units.dir = units.dir, units.dist = units)
    n.mat <- crossprod(A.mat, W.mat) %*% f.mat
    x.mat <- -Qx.mat %*% n.mat
    v.mat <- A.mat%*%x.mat + f.mat
    sd.estimated <- sqrt((crossprod(v.mat, W.mat) %*% v.mat)/(df))
    if(sd.estimated > sd.apriori){
      F.estimated <- sd.estimated^2/sd.apriori^2
      F.quantile <- qf(p = prob, df1 = df, df2 = 10^1000)
    }else{
      F.estimated <- sd.apriori^2/sd.estimated^2
      F.quantile <- qf(p = prob, df1 = 10^1000, df2 = df)
    }
    if(F.estimated > F.quantile){
      F.test.conclusion <- paste("Model nije adekvatan")
      # Data snooping and others have to be put in the list
    }else{
      F.test.conclusion <- paste("Model je adekvatan")
    }
  }
  if(use.sd.estimated){sd.apriori <- sd.estimated}
  # Results
  #TODO Resiti problem dodeljivanja prirastaja i ocenjenih koordinata u Survey.net[[1]].
  # Problem nastaje kada je samo jedna koordinata neke tacke fiksna.
  # Resiti sa pivot i left_join.
  coords.inc <- x.mat %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "parameter")
  coords.inc <- coords.inc[1:sum(fix.mat),] %>%
    dplyr::rename(coords.inc = V1) %>%
    tidyr::separate(.,col = parameter, into = c("Name", "inc.name"), sep = "_") %>%
    tidyr::pivot_wider(., names_from = c(inc.name), values_from = c(coords.inc)) %>%
    dplyr::mutate_all(., ~replace(., is.na(.), 0))

  point.adj.results <- dplyr::left_join(survey.net[[1]], coords.inc, by = "Name") %>%
    dplyr::mutate_at(., .vars = c("dx", "dy"), ~replace(., is.na(.), 0)) %>%
    cbind(., st_coordinates(.)) %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(X = X + dx/res.unit.lookup[units], Y = Y + dy/res.unit.lookup[units]) %>%
    dplyr::mutate_at(., .vars = c("dx", "dy"), round, disp.unit.lookup[units]) %>%
    sf::st_as_sf(coords = c("X","Y"), remove = FALSE)
    # TODO: Gubi se projekcija!!!!

  # Computing error ellipses
  Qxy.list <- Qxy(Qx.mat, n = lenght(used.points), fixd = fix.mat*1)
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

  observations <- observations %>% dplyr::mutate(v = v.mat, Ql.mat = diag(Ql.mat), Qv.mat = diag(Qv.mat), rii = diag(r)) %>%
    dplyr::mutate_if(is.numeric, round, disp.unit.lookup[units]*2)

  if(adjust){
    observations <- observations %>%
      dplyr::mutate(x_from = point.adj.results$X[match(observations$from, point.adj.results$Name)],
                    y_from = point.adj.results$Y[match(observations$from, point.adj.results$Name)],
                    x_to = point.adj.results$X[match(observations$to, point.adj.results$Name)],
                    y_to = point.adj.results$Y[match(observations$to, point.adj.results$Name)]) %>%
      sf::st_drop_geometry()

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


  # Preparing ellipses as separate sf outcome
  ellipse.net <- do.call(rbind, lapply(split(survey.net[[1]], survey.net[[1]]$Name), function(x) sf.ellipse(x, scale = ellipse.scale)))
  ellipse.net <- merge(ellipse.net, sigmas)
  ellipse.net <- sf::st_set_crs(ellipse.net, value = st_crs(survey.net[[2]]))

  design <- list(design.matrices = list(A = A.mat, W = W.mat, Qx = Qx.mat, Ql = Ql.mat, Qv = Qv.mat), ellipse.net = ellipse.net, net.points = survey.net[[1]], observations = observations)
  design$net.points <- sf::st_set_crs(design$net.points, value = st_crs(survey.net[[2]]))

  if(all){
    return(design)
  }else{
    return(design[-1])
  }

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
  f_dir <- survey.net[[2]] %>% dplyr::filter(direction) %>% st_drop_geometry() %>% split(.,.$from) %>%
    lapply(., function(x) fdir_st(x, units.dir = units.dir)) %>%
    do.call(c, .) %>% as.numeric()
  dist.units.table <- c("mm" = 1000, "cm" = 100, "m" = 1)
  survey.net[[2]] <- survey.net[[2]] %>% dplyr::filter(distance) %>% st_drop_geometry() %>%
    dplyr::mutate(dist0 = sqrt((x_from-x_to)^2+(y_from-y_to)^2))
  f_dist <- (survey.net[[2]]$dist0 - survey.net[[2]]$HD)*dist.units.table[units.dist]
  return(c(f_dir, f_dist))
}





