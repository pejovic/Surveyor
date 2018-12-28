# Project: Surveyer
# Description: Package of Land and Engineering Surveying utilities
# Authors: Milutin Pejovic, Milan Kilibarda, Branislav Bajat, Aleksandar Sekulic and Petar Bursac
#

# Packages
library(tidyverse)
library(magrittr)
library(ggplot2)
library(geomnet)
library(ggnetwork)
library(sf)
library(ggmap)
library(sp)
library(rgdal)
library(leaflet)


# Treba napraviti funkcije surveynet.shp, surveynet.kml i surveynet.xls koje ce imati ulazne parametre:
#    1. points - putanja do shp, kml fajla koji sadrzi tacke i opciono informaciju o fixaciji tacke.
#    2. observations - putanja do shp ili kml fajla koji sadrzi linije merenja koja su po defaultu i pravac i duzina i u sebi opciono sadrze informaciju
#                      tome da li je neko merenje samo pravac na primer i informaciju o standardu koja se u kml-u upisuje u description
#                      (npr "dir 3" ako je predvidjen samo pravac sa standardom 3")
#    3. fix_x i fix_y - Ako nisu definisani u okviru points, onda mogu opciono da se definisu i ovde. Ali treba voditi racuna, da ako je definisano u points
#                       ne sme se dva puta definisati, odnosno treba izbaciti error zbog dva definisanja fiksnih tacaka
#    4. st_dir i st_dist - Moze se definisati u okviru observation kml-fajla i ako je tamo definisano ovde ne treba nista i treba izbaciti error ako se dva puta definise.
#                        A ako nema u kml-u, onda se ovde moze zadati standard kao jedan broj za st_dir i jedan broj za st_dist. A ako hoces da neko merenje ima drugi standard onda ga definisi tamo ponaosob.
#    5. dest_crs - Za sada je samo proj4 zapis zeljenog koordinatnog sistema u projekciji ako se koristi kml.

#    surveynet.xls funkcija ucitava xls fajl u kome je u jednom sheet-u upisane tacke koje imaju sve ove atribute,a u drugom sheet-u merenja koja imaju sve ove atribute.

# surveynet - Function for defining surveynet based on input parameters
# param tacke - ucitan shp


surveynet.shp <- function(points, observations, fix_x = list(), fix_y = list(), st_dir, st_dist, dest_crs = NA){
  for(i in names(points)){
    if(i == "Naziv"){
      points <- points %>% rename("Name" = "Naziv")
    }
  }
  for(i in names(observations)){
    if(i == "Name"){
      observations <- observations %>% rename("id" = "Name")
    }
  }
  # TODO Check funkcija ide ovde
  # Transformacija u dest CRS

  if(is.na(observations$id[[1]])){
    observations$id <- (1:length(observations$geometry))
  }

  # Defining datum for points
  points %<>% mutate(FIX_X = FALSE, FIX_Y = FALSE)

  points$FIX_X[points$Name %in% fix_x] <- TRUE
  points$FIX_Y[points$Name %in% fix_y] <- TRUE

  # Observational plan - adding new columns
  observations %<>% mutate(distance = TRUE,
                           direction = TRUE,
                           standard_dir = st_dir,
                           standard_dist = st_dist,
                           from = NA,
                           to = NA)

  # Observational plan - defining and ading names for first and last points for observations
  # Creating data frame from sf class object observations, with goal to extract names for first and last points
  observations_1 <- data.frame(station = NA, obs.point = NA, x_station = NA,y_station = NA, x_obs.point = NA, y_obs.point = NA)

  # Line string to multipoint
  ob_plan_first_point <- st_line_sample(observations,sample = 0)
  ob_plan_last_point <- st_line_sample(observations,sample = 1)
  coord_1 <- as.data.frame(st_coordinates(ob_plan_first_point))
  coord_2 <- as.data.frame(st_coordinates(ob_plan_last_point))

  # Multipoint to point
  pnts_1 = st_cast(ob_plan_first_point, "POINT")
  pnts_2 = st_cast(ob_plan_last_point, "POINT")
  # X- East Y - North
  observations_1 <- data.frame(station = NA, obs.point = NA, x_station = st_coordinates(pnts_1)[,1], y_station = st_coordinates(pnts_1)[,2], x_obs.point = st_coordinates(pnts_2)[,1], y_obs.point = st_coordinates(pnts_2)[,2])

  # Adding columns Names for stations and observation point with values from constraint exactly match coordinates
  # TODO srediti da radi i za y
  observations_1$station <- points$Name[match(observations_1$x_station, st_coordinates(points)[,1])]
  observations_1$obs.point <- points$Name[match(observations_1$x_obs.point, st_coordinates(points)[,1])]
  observations_1$id <- coord_1$L1

  observations$from <- observations_1$station[match(observations$id, observations_1$id)]
  observations$to <- observations_1$obs.point[match(observations$id, observations_1$id)]

  # Creating list
  survey_net <- list(points,observations)

  return(survey_net)
}


# Examples
points <- st_read(dsn='Primer_1_ulazni_podaci_Ikea_Beograd','tacke_mreza_objekat_1')
observations <- st_read(dsn='Primer_1_ulazni_podaci_Ikea_Beograd','plan_opazanja')

u1 <- surveynet.shp(points = points, observations = observations, fix_x = list("A","B"), fix_y = list("A","B"), st_dir = 3, st_dist = 3, dest_crs = NA)

points2 <- st_read(dsn='Primer_2_ulazni_podaci_marina_Visnjicka_banja/shapefiles','tacke')
observations2 <- st_read(dsn='Primer_2_ulazni_podaci_marina_Visnjicka_banja/shapefiles','plan_opazanja')

u2 <- surveynet.shp(points = points2, observations = observations2, fix_x = list("1"), fix_y = list("1","3"), st_dir = 3, st_dist = 3, dest_crs = NA)


# Function for spatial data visualisation trough package ggplot2

net_spatial_view <- function(points, observations){

  net_view <- ggplot(data=observations) +
    geom_sf(size=1,stroke=1)+
    geom_sf(data=points,shape = 24, fill = "red", size=2.5,stroke=2)+
    geom_sf_text(data=points, aes(label=Name,hjust = 1.5, vjust =1.5))+
    xlab("\nLongitude [deg]") +
    ylab("Latitude [deg]\n") +
    ggtitle("Observational plan and points [geodetic network and object points]");

  return(net_view)

}

# Examples
net_spatial_view(points = u1[[1]], observations = u1[[2]])
net_spatial_view(points = u2[[1]], observations = u2[[2]])


# Function for checking input data for errors - geometrical and topological consistency, redudancy etc.

check_net <- function(points, observations){

  observations_1 <- data.frame(station = NA,obs.point = NA, x_station = NA,y_station = NA,x_obs.point = NA,y_obs.point = NA)

  # Line string to multipoint
  pl_op_pocetne_tac <- st_line_sample(observations,sample=0)
  pl_op_krajnje_tac <- st_line_sample(observations,sample=1)
  koord_1 <- as.data.frame(st_coordinates(pl_op_pocetne_tac))
  koord_2 <- as.data.frame(st_coordinates(pl_op_krajnje_tac))

  # Multipoint to point
  pnts_1 = st_cast(pl_op_pocetne_tac, "POINT")
  pnts_2 = st_cast(pl_op_krajnje_tac, "POINT")
  # X- East Y - North
  observations_1 <- data.frame(station = NA,obs.point = NA, x_station = st_coordinates(pnts_1)[,1],y_station = st_coordinates(pnts_1)[,2],x_obs.point = st_coordinates(pnts_2)[,1],y_obs.point = st_coordinates(pnts_2)[,2])


  x1 <- match(observations_1$x_station, st_coordinates(points)[,1])
  x2 <- match(observations_1$x_obs.point, st_coordinates(points)[,1])
  y1 <- match(observations_1$y_station, st_coordinates(points)[,2])
  y2 <- match(observations_1$y_obs.point, st_coordinates(points)[,2])

  if(any(x1) == FALSE){
    message("\nX koordinate tacaka i tacaka stanica u planu opazanja se ne poklapaju.")
  } else {
    message("\nX koordinate tacaka i tacaka stanica u planu opazanja se poklapaju.")
  }
  if(any(x2) == FALSE){
    message("\nX koordinate tacaka i tacaka opazanja u planu opazanja se ne poklapaju.")
  } else {
    message("\nX koordinate tacaka i tacaka opazanja u planu opazanja se poklapaju.")
  }
  if(any(y1) == FALSE){
    message("\nY koordinate tacaka i tacaka stanica u planu opazanja se ne poklapaju.")
  } else {
    message("\nY koordinate tacaka i tacaka stanica u planu opazanja se poklapaju.")
  }
  if(any(y2) == FALSE){
    message("\nY koordinate tacaka i tacaka opazanja u planu opazanja se ne poklapaju.")
  } else {
    message("\nY koordinate tacaka i tacaka opazanja u planu opazanja se poklapaju.")
  }


  un1$Name <- as.data.frame(unique(points$Name))
  un2 <- as.data.frame(unique(observations$from))
  un3 <- as.data.frame(unique(observations$to))

  un1$station <- TRUE[match(un1$Name, u2)]
  un1$obs <- TRUE[match(un1$Name, u3)]

  if(any(un1$station) == FALSE){
    message("\nKao stanica u planu opazanja nije iskoriscena tacka:")
    return(un1$Name[match(un1$station, FALSE)])
  } else {
    message("\nSve points su iskoriscene kao stanice u planu opazanja.")
  }

  if(any(un1$obs) == FALSE){
    message("\nKao opazana tacka u planu opazanja nije iskoriscena tacka:")
    return(un1$Name[match(un1$obs, FALSE)])
  } else {
    message("\nSve points su opazane u planu opazanja.")
  }



}


check_net(points = u1[[1]], observations = u1[[2]])

