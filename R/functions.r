# Project: Surveyer
# Description: Package of Land and Engineering Surveying utilities
# Authors: Milutin Pejovic, Milan Kilibarda, Branislav Bajat, Aleksandar Sekulic and Petar Bursac
#


# Functions:

#

### coeficients for distances #####################################################

coef_d <- function (pt1, pt2, pts) {
  coords <- pts@coords
  vec_d <- c(rep(0, length(coords)))
  x_coords <- coords[, 1]
  y_coords <- coords[, 2]
  x1 <- pt1[1]
  y1 <- pt1[2]
  x2 <- pt2[1]
  y2 <- pt2[2]
  dx1 <- (x_coords-x1)
  dy1 <- (y_coords-y1)
  dx2 <- (x_coords-x2)
  dy2 <- (y_coords-y2)
  i <- which(dx1 == dy1 & dx1 == 0 & dy1 == 0)
  j <- which(dx2 == dy2 & dx2 == 0 & dy2 == 0)
  dx <- (x2-x1)*1000
  dy <- (y2-y1)*1000
  d <- sqrt(dx^2+dy^2)
  A <- ifelse( pts$fix_x[i] == 0, pts$fix_x[i]*(-dx/d), (-dx/d))
  B <- ifelse( pts$fix_y[i] == 0, pts$fix_y[i]*(dy/d),(dy/d))
  A1 <- ifelse( pts$fix_x[j] == 0,-pts$fix_x[j]*(-dy/d), -(-dy/d))
  B1 <- ifelse( pts$fix_y[j] == 0,-pts$fix_y[j]*(dy/d),-(dy/d))
  vec_d[2*i-1] <- A
  vec_d[2*j-1] <- (A1)
  vec_d[2*i] <- B
  vec_d[2*j] <- (B1)
  return(vec_d)
}

### coeficients for directions (pravac) #####################################################

coef_p <- function (pt1, pt2, pts) {
  coords <- pts@coords
  vec_p <- c(rep(0, length(coords)))
  ro <- 180/pi*3600
  x_coords <- coords[, 1]
  y_coords <- coords[, 2]
  x1 <- pt1[1]
  y1 <- pt1[2]
  x2 <- pt2[1]
  y2 <- pt2[2]
  dx1 <- (x_coords-x1)
  dy1 <- (y_coords-y1)
  dx2 <- (x_coords-x2)
  dy2 <- (y_coords-y2)
  i <- which(dx1 == dy1 & dx1 == 0 & dy1 == 0)
  j <- which(dx2 == dy2 & dx2 == 0 & dy2 == 0)
  dx <- (x2-x1)*1000
  dy <- (y2-y1)*1000
  d <- sqrt(dx^2 + dy^2)
  A <- ifelse( pts$fix_x[i] == 0, pts$fix_x[i]*(ro*dy/d^2), (ro*dy/d^2))
  B <- ifelse( pts$fix_y[i] == 0, pts$fix_y[i]*(-ro*dx/d^2), (-ro*dx/d^2))
  A1 <- ifelse( pts$fix_x[j] == 0,-pts$fix_x[j]*(ro*dy/d^2), -(ro*dy/d^2))
  B1 <- ifelse( pts$fix_y[j]==0,-pts$fix_y[j]*(-ro*dx/d^2), -(-ro*dx/d^2))
  vec_p[2*i-1] <- A
  vec_p[2*j-1] <- (A1)
  vec_p[2*i] <- B
  vec_p[2*j] <- (B1)
  return(vec_p)
}
