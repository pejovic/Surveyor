
source("R/functions.r")
source("R/Simulations_functions.r")

library(tidyverse)
library(sp)
library(rgdal)
library(readxl)
library(writexl)
library(magrittr)


# Ucitavanje tacaka

tacke.wgs <- readOGR(dsn = "Data/Rovni1.kml", layer = "Rovni")

tacke.dks <- spTransform(tacke.wgs, CRS("+proj=tmerc +lat_0=0 +lon_0=21 +k=0.9999 +x_0=7500000 +y_0=0 +ellps=bessel +towgs84=574.027,170.175,401.545,4.88786,-0.66524,-13.24673,0.99999311067 +units=m"))
coords.dks <- data.frame(Name = tacke.dks$Name, tacke.dks@coords[, 1:2])
names(coords.dks) <- c("Name", "Easting", "Northing")
#write_xlsx(coords.dks, path = "Rovni_tacke.xlsx")


obs_plan <- data.frame(station = rep(c("T1", "T1", "T1", "T1", "T3", "T3", "T3", "T3", "T4", "T4", "T4", "T4", "T5", "T5", "T5", "T5", "T6", "T6", "T6", "T6"), 2), obs.point = rep(c("T3", "T4", "T5", "T6", "T1", "T4", "T5","T6", "T1", "T3", "T5", "T6", "T1", "T3", "T4", "T6", "T1", "T3", "T4", "T5"), 2), type = c(rep("p", 20), rep("d", 20)) )

tacke.sp <- SpatialPointsDataFrame(coords = coords.dks[, 2:3], data = data.frame(Name = coords.dks$Name))
coef_d(pt1 = as.numeric(tacke.dks@coords[1, 1:2]), pt2 = as.numeric(tacke.dks@coords[2, 1:2]), units = "mm", pts = coords.dks)
coef_p(pt1 = as.numeric(tacke.dks@coords[1, 1:2]), pt2 = as.numeric(tacke.dks@coords[2, 1:2]), units = "mm", pts = coords.dks)


A <- function(obs.plan, tacke, coef_funs = list(coef_p, coef_d)){
  Amat <- matrix(NA, ncol = dim(tacke)[1]*2, nrow = dim(obs.plan)[1])
  tacke.sp <- SpatialPointsDataFrame(coords = tacke[, 2:3], data = data.frame(Name = tacke$Name))
  for(i in 1:dim(obs.plan)[1]){
    st_coords <- dplyr::filter(tacke, Name == obs.plan$station[i])[, -1] %>% as.numeric()
    viz_coords <- dplyr::filter(tacke, Name == obs.plan$obs.point[i])[, -1] %>% as.numeric()
    Amat[i, ] <- if(obs.plan$type[i] == "p"){
      coef_p(pt1 = st_coords, pt2 = viz_coords, units = "mm", pts = tacke)
    }else{
      coef_d(pt1 = st_coords, pt2 = viz_coords, units = "mm", pts = tacke)
      }
  }
  return(Amat)
}

A(obs.plan = obs_plan, tacke = coords.dks)

