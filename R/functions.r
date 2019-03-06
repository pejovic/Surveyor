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


# Funkcija koja izdvaja elemente Qx matrice u listu za elipsu svake tacke
Qxy <- function(Qx, n, fixd = fix) {
  k = 0
  fixd <- cbind(fixd, fixd[,1] + fixd[, 2])
  Qxxx <- as.list(rep(NA, dim(fixd)[1]))
  for(i in 1:dim(fixd)[1]) {
    k = fixd[i,1] + fixd[i, 2] + k
    if(fixd[i,3] == 1){
      Qxxx[[i]] <- diag(fixd[i, c(1,2)])*Qx[k, k] }
    else if (fixd[i,3] == 2) {
      Qxxx[[i]] <- cbind(c(Qx[k-1, k-1], Qx[k-1, k]), c(Qx[k, k-1], Qx[k, k]))
    } else {
      Qxxx[[i]] <- diag(fixd[i, c(1, 2)])
    }
  }
  return(Qxxx)
}
#######################################################################

elipsa<-function(Qxyt) {
  k<-sqrt((Qxyt[1,1]-Qxyt[2,2])^2+4*Qxyt[1,2]^2)
  lambda1<-0.5*(Qxyt[1,1]+Qxyt[2,2]+k)
  lambda2<-0.5*(Qxyt[1,1]+Qxyt[2,2]-k)
  A<-sqrt(lambda1)
  B<-sqrt(lambda2)
  teta<-ifelse((Qxyt[1,1]-Qxyt[2,2])==0,0,0.5*atan(2*Qxyt[1,2]/(Qxyt[1,1]-Qxyt[2,2])))
  teta<-ifelse(teta>=0,teta,teta+2*pi)
  elip<-c(A,B,teta*180/pi)
  return(elip)
}
#########################################################



