library(dplyr)
library(readxl)
library(MASS)
library(matlib)

wdir <- 'd:/R_projects/Surveyer/'
setwd(wdir)
getwd()

sistemA <- read.csv(file = 'Data/Input_Transformations/DKS .csv', sep = ",") %>% as.data.frame()
sistemB <- read.csv(file = 'Data/Input_Transformations/WGS84 .csv', sep = ",") %>% as.data.frame()

sistem_A = sistemA
sistem_B = sistemB

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Funkcija Helmert_7_parameters::
# ima za cilj odredjivanje 7 parametara Helmertova 3D transformacije slicnosti
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Ulazni parametri:
# 1. sistema_A --- data frame sa koordinatama tacaka u koordinatnom sistemu A (izvorni)
# 2. sistema_B --- data frame sa koordinatama tacaka u koordinatnom sistemu B (ciljni)

Helmert_7_parameters <- function(sistem_A = sistem_A, sistem_B = sistem_B){
  ident <- sistem_A
  ident <- ident %>% dplyr::rename(X_A = X,
                    Y_A = Y,
                    Z_A = Z)
  ident$X_B <- sistem_B$X[match(sistem_A$Point, sistem_B$Point)]
  ident$Y_B <- sistem_B$Y[match(sistem_A$Point, sistem_B$Point)]
  ident$Z_B <- sistem_B$Z[match(sistem_A$Point, sistem_B$Point)]

  # :::::::::::::::::::::::::::::::::
  # Kreiranje matrica
  # :::::::::::::::::::::::::::::::::

  brtac <- length(ident$Point)
  brkoord <- brtac*3

  Amat <- matrix(0, nrow = brkoord, ncol = 7)
  Fmat <- matrix(0, nrow = brkoord, ncol = 1)

  X_sistemA <- matrix(nrow = 3, ncol = brtac)
  X_sistemB <- matrix(nrow = 3, ncol = brtac)

  # :::::::::::::::::::::::::::::::::
  # Popunjavanje matrice A i matrice F
  # :::::::::::::::::::::::::::::::::

  for(i in 1:length(ident$Point)){

    xi <- ident$X_A[i]
    yi <- ident$Y_A[i]
    zi <- ident$Z_A[i]
    Xi <- ident$X_B[i]
    Yi <- ident$Y_B[i]
    Zi <- ident$Z_B[i]

    Amat[i*3-2,1] = 1
    Amat[i*3-1,2] = 1
    Amat[i*3,3] = 1
    Amat[i*3-2,4] = xi
    Amat[i*3-1,4] = yi
    Amat[i*3,4] = zi
    Amat[i*3-1,5] = zi
    Amat[i*3,5] = -yi
    Amat[i*3-2,6] = -zi
    Amat[i*3,6] = xi
    Amat[i*3-2,7] = yi
    Amat[i*3-1,7] = -xi

    Fmat[i*3-2,1] = Xi - xi
    Fmat[i*3-1,1] = Yi - yi
    Fmat[i*3,1] = Zi - zi

  }

  # :::::::::::::::::::::::::::::::::
  # Matricni racun
  # :::::::::::::::::::::::::::::::::

  N <- t(Amat) %*% Amat
  n <- t(Amat) %*% Fmat
  Qx <- matlib::inv(N)
  x <- -Qx %*% n
  V <- Amat %*% x + Fmat

  # :::::::::::::::::::::::::::::::::
  # Kontrola
  # :::::::::::::::::::::::::::::::::

  vtv <- t(V) %*% V
  ftf <- t(Fmat) %*% Fmat
  ntx <- t(n) %*% x
  kontrola = vtv - ftf - ntx

  sigmaO = sqrt(vtv[1,1]/(brkoord-6))

  # :::::::::::::::::::::::::::::::::
  # Ocenjeni parametri
  # :::::::::::::::::::::::::::::::::

  tx <- x[1,1]
  ty <- x[2,1]
  tz <- x[3,1]
  mi <- x[4,1]
  alfa <- x[5,1]
  beta <- x[6,1]
  gama <- x[7,1]

  lista <- c(tx = tx, ty = ty, tz = tz, mi = mi, alfa = alfa, beta = beta, gama = gama)
  return(lista)
}

proba <- Helmert_7_parameters(sistem_A = sistemA, sistem_B = sistemB)
proba[[1]]
