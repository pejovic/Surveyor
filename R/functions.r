# Project: Surveyer
# Description: Package of Land and Engineering Surveying utilities
# Authors: Milutin Pejovic, Milan Kilibarda, Branislav Bajat, Aleksandar Sekulic and Petar Bursac
# Date:


# Functions:

#

### coeficients for distances #####################################################

coef_d <- function (pt1, pt2, pts, units, axes = c("Easting", "Northing")) {
  yind <- which("Easting" == axes)
  xind <- which("Northing" == axes)
  units.table <- c("mm" = 1000, "cm" = 100, "m" = 1)
  pt1 <- as.numeric(pt1)
  pt2 <- as.numeric(pt2)
  coords <- pts
  vec_d <- c(rep(0, length(coords)))

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
  vec_p <- c(rep(0, length(coords)))
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

  A_dir <- survey.net[[2]] %>% filter(direction) %>% st_coordinates() %>% as.data.frame() %>% mutate_at(vars(L1), funs(factor)) %>%
    split(., .$L1) %>%
    lapply(., function(x) coef_p(pt1 = x[1, 1:2], pt2 = x[2, 1:2], pts = st_coordinates(survey.net[[1]][, 1:2]), units = units, axes = axes)) %>%
    do.call(rbind, .)

  A_dist <- survey.net[[2]] %>% filter(distance) %>% st_coordinates() %>% as.data.frame() %>% mutate_at(vars(L1), funs(factor)) %>%
    split(., .$L1) %>%
    lapply(., function(x) coef_d(pt1 = x[1, 1:2], pt2 = x[2, 1:2], pts = st_coordinates(survey.net[[1]][, 1:2]), units = units, axes = axes)) %>%
    do.call(rbind, .)

  Z_mat <- survey.net[[2]] %>% filter(direction) %>%
    spread(key = from, value = direction, fill = FALSE) %>%
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


# Weights matrix
Wmat <- function(survey.net, apriori = 1){
  #TODO: Omoguciti zadavanje i drugih kovariacionih formi izmedju merenja.
  obs.data <- survey.net[[2]] %>% st_drop_geometry() %>%
    gather(key = type, value = standard, -c(id, from, to, distance, direction)) %>%
    dplyr::select(from, to, standard)
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

error.ellipse <- function(Qxy, prob = NA, apriori = 1, axes = c("Easting", "Northing"), unit = list("deg", "rad")) {
  if(("Easting" == axes)[1]){
    Qee <- Qxy[1, 1]
    Qnn <- Qxy[2, 2]
    Qen <- Qxy[1, 2]
  }else{
    Qee <- Qxy[2, 2]
    Qnn <- Qxy[1, 1]
    Qen <- Qxy[1, 2]
  }
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
  if(unit[[1]] == "deg"){
    teta <- ifelse(teta >= 0, teta, teta + 2*pi)
  }
  ellipse <- c(A, B, teta*180/pi)
  return(ellipse)
}
#

design.snet <- function(survey.net, apriori = 1, result.units = list("mm", "cm", "m"), axes = c("Easting", "Northing")){
  # TODO: Set warning if there are different or not used points in two elements of survey.net list.
  # Check which points are used for measurements, if not
  used.points <- unique(do.call(c, survey.net[[2]][, c("from", "to")] %>% st_drop_geometry()))
  used.points.ind <- which(survey.net[[1]]$Name %in% used.points)
  survey.net[[1]] <- survey.net[[1]][used.points.ind, ]
  # =======
  units <- result.units[[1]]
  A <- Amat(survey.net, units = units, axes = axes)
  W <- Wmat(survey.net)
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
  Kl <- A %*% tcrossprod(Qx, A)
  Qv <- solve(W) - Kl
  fix <- if(("Easting" == axes)[1]){
    survey.net[[1]] %>% st_drop_geometry() %>%  dplyr::select(FIX_X, FIX_Y) == FALSE
  }else{
    survey.net[[1]] %>% st_drop_geometry() %>%  dplyr::select(FIX_Y, FIX_X) == FALSE
  }
  Qxy.list <- Qxy(Qx, n = lenght(used.points), fixd = fix*1)
  ellipses <- lapply(Qxy.list, function(x) error.ellipse(x))
  ellipses <- do.call(rbind, ellipses) %>% as.data.frame() %>% dplyr::select(A = V1, B = V2, teta = V3) %>% mutate(Name = used.points)
  survey.net[[1]] <- merge(survey.net[[1]], ellipses, by = "Name")
  design <- list(A = A, W = W, Qx = Qx, Kl = Kl, Qv = Qv, net.points = survey.net[[1]])

  return(design)
}







