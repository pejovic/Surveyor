library(shiny)
library(shinythemes)
library(leaflet)
library(tidyverse)
library(magrittr)
library(ggplot2)
#library(geomnet)
#library(ggnetwork)
library(sf)
library(ggmap)
library(sp)
library(rgdal)
library(leaflet)
#library(xlsx)
library(readxl)
library(data.table)
library(plotly)
library(mapview)
library(shinycssloaders)
library(here)
library(matlib)
library(nngeo)
library(dplyr)
library(mapedit)
library(DT)
library(leaflet.extras)
library(rhandsontable)
library(shinyBS)
library(shinyWidgets)
library(knitr)
library(rmarkdown)
library(knitr)
library(kableExtra)
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


TETO_DKS <- readxl::read_xlsx('Data/Input_Transformations/TETO_DKS.xlsx', col_types = c("text", "numeric", "numeric", "numeric")) %>% as.data.frame()
TETO_WGS <- readxl::read_xlsx('Data/Input_Transformations/TETO_WGS.xlsx', col_types = c("text", "numeric", "numeric", "numeric")) %>% as.data.frame()


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Function (x,y,h) ---> (B,L,h)
# Transformation of the coordinates in projection to geodetic on the same ellipsoid
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

data.xy = TETO_DKS
a = 6377397.155
b = 6356078.96325
m0 = 0.9999
yo = 7500000
L0 = 21


xyh2BLh <- function(data.xy = data.xy, a = a, b = b, m0 = m0, yo = yo, L0 = L0){

  # 1 step: Unmodulated coordinates
  data.xy %<>% dplyr::mutate(Yun = Y / m0,
                             Xun = (X - yo)/m0)

  # 2 step: Calculation od additional variables
  e = sqrt(1-((b^2) / (a^2)))
  e0 = sqrt(((a^2) / (b^2))-1)
  e1 = (1-sqrt(1-e^2)) / (1+sqrt(1-e^2))

  data.xy %<>% dplyr::mutate(
    mi1 = Yun / (a * (1 - (e^2/4) - (3*e^4/64) - (5*e^6/256))),
    B1 = mi1 + ((3/2) * e1 - (27/32)*e1^3) * sin(2*mi1) + ((21/16)*e1^2 - (55/32)*e1^4) * sin(4*mi1) + (151/96)*e1^3 * sin(6*mi1) + (1097/512)*e1^4*sin(8*mi1),
    V1 = a / sqrt(1 - e^2*2*sin(B1)*cos(B1)),
    M1 = (a * (1-e^2)) / (1 - e^2*(sin(B1))^2)^(3/2),
    T1 = (tan(B1))^2,
    C1 = e0^2 * (cos(B1))^2,
    D = Xun / V1,
    B = B1 - (((V1 * tan(B1))/M1) * (D^2/2 - (5 + 3*T1 + 10*C1 - 4*C1^2 - 9*e0^2)*(D^4/24) + (61 + 90*T1 + 298*C1 + 45*T1^2 - 252*e0^2 - 3*C1^2)*(D^6/720))),
    L = L0 + (1/cos(B1) * (D - (1 - 2*T1 + C1)*(D^3/6) + (5 - 2*C1 + 28*T1 - 3*C1^2 + 8*e0^2 + 24*T1^2)*(D^5/120)))
  )

  return(data.xy)
}

xyh2BLh(data.xy = sistem_A, a = a, b = b, m0 = m0, yo = yo, L0 = L0)






sistem_A
# Create sf object
sistem_A.sf <- st_as_sf(sistem_A, coords = c("X", "Y", "Z"), crs = 3909)
# Transformation from (x, y, h) ---> (B, L, h)
sistem_A.sf_BL <- st_transform(sistem_A.sf, crs = "+proj=longlat +ellps=bessel +datum=hermannskogel")
# Transformation from (B, L, h) ---> (X, Y, Z)
sistem_A.sf_XYZ <- st_transform(sistem_A.sf_BL, crs = "+proj=geocent +ellps=bessel +datum=hermannskogel +units=m +no_defs")


# Create sf object
TETO.sf <- st_as_sf(TETO_DKS, coords = c("X", "Y", "Z"), crs = 3909)
# Transformation from (x, y, h) ---> (B, L, h)
TETO.sf_BL <- st_transform(TETO.sf, crs = "+proj=longlat +ellps=bessel +datum=hermannskogel")
# Transformation from (B, L, h) ---> (X, Y, Z)
TETO_DKS.sf_XYZ <- st_transform(TETO.sf_BL, crs = "+proj=geocent +ellps=bessel +datum=hermannskogel +units=m +no_defs")

TETO_DKS_XYZ <- TETO_DKS.sf_XYZ %>%
  dplyr::mutate(X = sf::st_coordinates(.)[,1],
                Y = sf::st_coordinates(.)[,2],
                Z = sf::st_coordinates(.)[,3]) %>%
  sf::st_drop_geometry()

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

proba <- Helmert_7_parameters(sistem_A = TETO_DKS_XYZ, sistem_B = TETO_WGS)
proba[[1]]
