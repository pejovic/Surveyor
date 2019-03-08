# Project: Surveyer
# Description: Package of Land and Engineering Surveying utilities
# Authors: Milutin Pejovic, Milan Kilibarda, Branislav Bajat, Aleksandar Sekulic and Petar Bursac

################
# surveynet.xlsx
################

# Parameters:
#    1. points -  Excel file sheet with attributes related to points - geodetic network [Example: Data/Input/xlsx]
#    2. observations - Excel file sheet with attributes related to observations [Example: Data/Input/xlsx]
#    3. dest_crs - destination Coordinate Reference System - set EPSG code [default: 3857 - Web Mercator projection coordinate system]

surveynet.xlsx <- function(points = points, observations = observations, dest_crs = NA){
  # If column "Description" is necessary, delete it;
  j=1
  for(i in names(points)){
    if (i == "NA."){
      points <- subset(points, select = -c(j))
    }
    j=j+1
  }

  j_1=1
  for(i in names(observations)){
    if (i == "NA."){
      observations <- subset(observations, select = -c(j_1))
    }
    j_1 = j_1 + 1
  }


  # TODO Check funkcija ide ovde

  # Check function for point names, that can not be just numbers -> must contain letter
  points$Name <- as.character(points$Name)
  vec <- c(1:99999)
  j = 1
  for(i in points$Name){
    if(i %in% vec){
      points$Name[j] <- paste("T",i, sep = "")
      j = j +1

    }
  }

  observations$from <- as.character(observations$from)
  vec <- c(1:99999)
  j = 1
  for(i in observations$from){
    if(i %in% vec){
      observations$from[j] <- paste("T",i, sep = "")
      j = j +1

    }
  }

  observations$to <- as.character(observations$to)
  vec <- c(1:99999)
  j = 1
  for(i in observations$to){
    if(i %in% vec){
      observations$to[j] <- paste("T",i, sep = "")
      j = j +1

    }
  }

  # Create geometry columns for points
  if (is.na(dest_crs)){
    dest_crs <- 3857
  }

  observations$x_station <- points$x[match(observations$from, points$Name)]
  observations$y_station <- points$y[match(observations$from, points$Name)]
  observations$x_obs.point <- points$x[match(observations$to, points$Name)]
  observations$y_obs.point <- points$y[match(observations$to, points$Name)]

  points <- points %>% as.data.frame %>% sf::st_as_sf(coords = c("x","y")) %>% sf::st_set_crs(dest_crs)

  dt <- as.data.table(observations)
  dt_1 <- dt[
    , {
      geometry <- sf::st_linestring(x = matrix(c(x_station, x_obs.point, y_station, y_obs.point), ncol = 2))
      geometry <- sf::st_sfc(geometry)
      geometry <- sf::st_sf(geometry = geometry)
    }
    , by = id
    ]
  dt_1 <- sf::st_as_sf(dt_1)
  dt_1 %<>% mutate(from = observations$from,
                   to = observations$to,
                   distance = observations$distance,
                   direction = observations$direction,
                   standard_dir = observations$standard_dir,
                   standard_dist = observations$standard_dist
  )

  dt_1 <- dt_1 %>% sf::st_set_crs(dest_crs)
  observations <- dt_1

  # Creating list
  survey_net <- list(points,observations)

  return(survey_net)

}


###############
# surveynet.shp
###############

# Parameters:
#    1. points -  sf object with geometry type POINT - geodetic network [Example: Data/Input/shp]
#    2. observations - sf object with geometry type LINESTRING [Example: Data/Input/shp]
#    3. fix_x - list with names of points that define datum - X coordinate
#    3. fix_y - list with names of points that define datum - Y coordinate
#    4. st_dir - "a priori" standard deviation for direction observations ["]
#    5. st_dist - "a priori" standard deviation for distance observations [mm]
#    6. dest_crs - destination Coordinate Reference System - set EPSG code [default: 3857 - Web Mercator projection coordinate system]
#    7. points_object - list with names of points that represnt object of interests

surveynet.shp <- function(points, observations, fix_x = list(), fix_y = list(), st_dir, st_dist, dest_crs = NA, points_object = list()){
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

  # Check function for point names, that can not be just numbers -> must contain letter
  points$Name <- as.character(points$Name)
  vec <- c(1:99999)
  j = 1
  for(i in points$Name){
    if(i %in% vec){
      points$Name[j] <- paste("T",i, sep = "")
      j = j +1

    }
  }

  # Transformation to the destination CRS
  if (is.na(dest_crs)){
    dest_crs <- 3857
    points <- points %>% st_transform(dest_crs)
    observations <- observations %>% st_transform(dest_crs)
  }
  if (dest_crs == 3857){
    dest_crs <- 3857
    points <- points %>% st_transform(dest_crs)
    observations <- observations %>% st_transform(dest_crs)
  }
  if (st_crs(points)$epsg == "4326"){
    points <- points %>% st_transform(dest_crs)
  }
  if (st_crs(observations)$epsg == "4326"){
    observations <- observations %>% st_transform(dest_crs)
  }


  if(is.na(observations$id[[1]])){
    observations$id <- (1:length(observations$geometry))
  }

  # Defining datum for points
  points %<>% mutate(FIX_X = FALSE, FIX_Y = FALSE)

  points$FIX_X[points$Name %in% fix_x] <- TRUE
  points$FIX_Y[points$Name %in% fix_y] <- TRUE

  points %<>% mutate(Point_object = FALSE)
  points$Point_object[points$Name %in% points_object] <- TRUE

  # Observational plan - adding new columns
  observations %<>% mutate(from = NA,
                           to = NA,
                           distance = TRUE,
                           direction = TRUE,
                           standard_dir = st_dir,
                           standard_dist = st_dist
  )

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

###############
# surveynet.kml
###############

# Parameters:
#    1. points -  sf object with geometry type POINT - geodetic network [Example: Data/Input/kml]
#    2. observations - sf object with geometry type LINESTRING [Example: Data/Input/kml]
#    3. fix_x - list with names of points that define datum - X coordinate
#    3. fix_y - list with names of points that define datum - Y coordinate
#    4. st_dir - "a priori" standard deviation for direction observations ["]
#    5. st_dist - "a priori" standard deviation for distance observations [mm]
#    6. dest_crs - destination Coordinate Reference System - set EPSG code [default: 3857 - Web Mercator projection coordinate system]
#    7. points_object - list with names of points that represnt object of interests

surveynet.kml <- function(points, observations, fix_x = list(), fix_y = list(), st_dir, st_dist, dest_crs = NA, points_object = list()){
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
  # If column "Description" is necessary, delete it;
  j=1
  for(i in names(points)){
    if (i == "Description"){
      points <- subset(points, select = -c(j))
    }
    j=j+1
  }

  j_1=1
  for(i in names(observations)){
    if (i == "Description"){
      observations <- subset(observations, select = -c(j_1))
    }
    j_1 = j_1 + 1
  }

  # TODO Check funkcija ide ovde

  # Check function for point names, that can not be just numbers -> must contain letter
  points$Name <- as.character(points$Name)
  vec <- c(1:99999)
  j = 1
  for(i in points$Name){
    if(i %in% vec){
      points$Name[j] <- paste("T",i, sep = "")
      j = j +1

    }
  }

  # Transformation to the destination CRS
  if (is.na(dest_crs)){
    dest_crs <- 3857
    points <- points %>% st_transform(dest_crs)
    observations <- observations %>% st_transform(dest_crs)
  }
  if (st_crs(points)$epsg == "4326"){
    points <- points %>% st_transform(dest_crs)
    message("\nPoints data are reprojected to the destination Coordinate Reference System!\n")
    message(st_crs(points))
  }
  if (st_crs(observations)$epsg == "4326"){
    observations <- observations %>% st_transform(dest_crs)
    message("\nObservation data are reprojected to the destination Coordinate Reference System!\n")
    message(st_crs(observations))
  }

  # Handling with some columns
  for(i in names(points)){
    if(i == "id"){
      break
    } else {
      points$id <- (1:length(points$geometry))
    }
  }

  for(i in names(observations)){
    if(i == "id"){
      observations$id <- (1:length(observations$geometry))
    }
  }
  # Defining datum for points
  points %<>% mutate(FIX_X = FALSE, FIX_Y = FALSE)

  points$FIX_X[points$Name %in% fix_x] <- TRUE
  points$FIX_Y[points$Name %in% fix_y] <- TRUE

  points %<>% mutate(Point_object = FALSE)
  points$Point_object[points$Name %in% points_object] <- TRUE

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


##################
# net_spatial_view
##################

# Function for spatial data visualisation trough package ggplot2
# Parameters:
#    1. points -  sf object with geometry type POINT and related attributes as product from surveynet.xxx function
#    2. observations - sf object with geometry type LINESTRING and related attributes as product from surveynet.xxx function

net_spatial_view <- function(points, observations){

  points$fill_p <- "red"
  points$fill_p[points$Point_object == TRUE] <- "DeepSkyBlue"

  # Example to add little different type of observations
  # observations$distance[1:3] <- FALSE
  # observations$direction[5:7] <- FALSE
  # observations$fill_o <- "LightGoldenRodYellow"

  observations$fill_o <- ifelse(observations$distance == TRUE & observations$direction == FALSE,"LightGoldenRodYellow", ifelse(observations$distance == FALSE & observations$direction == TRUE, "Khaki","orange"))

  net_view <- ggplot(data=observations) +
    geom_sf(size=1,stroke=1, color = observations$fill_o)+
    geom_sf(data=points, shape = 24, fill = points$fill_p, size=2.5, stroke=2) +
    geom_sf_text(data=points, aes(label=Name,hjust = 1.5, vjust =1.5))+
    xlab("\nLongitude [deg]") +
    ylab("Latitude [deg]\n") +
    ggtitle("Observational plan and points [geodetic network and object points]")+
    guides(col = guide_legend())+
    theme_bw()
  return(net_view)

}


##########################################
# net_spatial_view_web [package:: mapview]
##########################################

# Function for spatial data visualisation at web maps
# Parameters:
#    1. points -  sf object with geometry type POINT and related attributes as product from surveynet.xxx function
#    2. observations - sf object with geometry type LINESTRING and related attributes as product from surveynet.xxx function

net_spatial_view_web <- function(points, observations){
  Points <- st_transform(points, 4326)
  Observations <- st_transform(observations, 4326)

  Points$type <- "Geodetic network"
  Points$type[Points$Point_object == TRUE] <- "Points at object"

  Observations$type[Observations$distance == TRUE & Observations$direction == FALSE] <- "Distance"
  Observations$type[Observations$distance == FALSE & Observations$direction == TRUE] <- "Direction"
  Observations$type[Observations$distance == TRUE & Observations$direction == TRUE] <- "Both"

  web_map_1 <- mapview(Points, zcol = "type", col.regions = c("red","grey")) + mapview(Observations, zcol = "type")

  return(web_map_1)

}

###########
# check_net
###########

# Function for checking input data for errors - geometrical and topological consistency, redudancy etc.
# Parameters:
#    1. points -  sf object with geometry type POINT and related attributes as product from surveynet.xxx function
#    2. observations - sf object with geometry type LINESTRING and related attributes as product from surveynet.xxx function

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
  un1 <- data.frame(id = points$id)
  un1$Name <- as.data.frame(unique(points$Name))
  un2 <- as.data.frame(unique(observations$from))
  un3 <- as.data.frame(unique(observations$to))

  un1$station <- TRUE[match(un1$Name, un2)]
  un1$obs <- TRUE[match(un1$Name, un3)]

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

###################
# surveynet.mapedit
###################

# Function for interactive adding points on web maps and storage as sf [Simple Feature]
surveynet.mapedit_add <- function(){
  created <- mapview() %>% editMap()
  return(created$finished)
}

# Function for visualisation trough mapview package
# Parameters:
#    1. points -  sf object with geometry type POINT and related attributes as product from function surveynet.mapedit_add

surveynet.mapedit_view <- function(points = points){
  Points <- mapview(points)
  return(Points)
}

# Function for preparing points - sf attribute table check
# Parameters:
#    1. points -  sf object with geometry type POINT and related attributes as product from function surveynet.mapedit_add

surveynet.mapedit_points <- function(points = points){
  j=1
  for(i in names(points)){
    if (i == "_leaflet_id"){
      points <- subset(points, select = -c(j))
    }
    j=j+1
  }
  j=1
  for(i in names(points)){
    if (i == "feature_type"){
      points <- subset(points, select = -c(j))
    }
    j=j+1
  }

  for(i in names(points)){
    if(i == "id"){
      break
    } else {
      points$id <- (1:length(points$geometry))
    }
  }
  points <- points %>% rename("Name" = "id")

  points$Name <- as.character(points$Name)
  vec <- c(1:99999)
  j = 1
  for(i in points$Name){
    if(i %in% vec){
      points$Name[j] <- paste("T",i, sep = "")
      j = j +1

    }
  }
  return(points)
}

# Function for creating observations from points - all
# Parameters:
#    1. points -  sf object with geometry type POINT and related attributes as product from function surveynet.mapedit_points

surveynet.mapedit_observations <- function(points = points){
  res = expand.grid(to = points$Name, from = points$Name) # combine values from two columns in all posible combinations
  res <- as.data.frame(res[!(res$to == res$from), ]) # delete rows with same values in two columns
  rownames(res) <- 1:nrow(res) # reorder index number of rows
  res <- res[ ,c("from","to")] # reorder columns
  return(res)
}

# Function for creating observations from points - edit and interactivly with CRUD [Create, Read, Update and Delete] functionalites edit observations
# Parameters:
#    1. points -  sf object with geometry type POINT and related attributes as product from function surveynet.mapedit_points
#    2. st_dir - "a priori" standard deviation for direction observations ["]
#    3. st_dist - "a priori" standard deviation for distance observations [mm]
#

surveynet.mapedit_observations_edit <- function(points = points, st_dir = st_dir, st_dist = st_dist){
  res = expand.grid(to = points$Name, from = points$Name) # combine values from two columns in all posible combinations
  res <- as.data.frame(res[!(res$to == res$from), ]) # delete rows with same values in two columns
  rownames(res) <- 1:nrow(res) # reorder index number of rows
  res <- res[ ,c("from","to")] # reorder columns
  observations <- res

  points$x <- st_coordinates(points)[,1]
  points$y <- st_coordinates(points)[,2]

  observations$x_station <- points$x[match(observations$from, points$Name)]
  observations$y_station <- points$y[match(observations$from, points$Name)]
  observations$x_obs.point <- points$x[match(observations$to, points$Name)]
  observations$y_obs.point <- points$y[match(observations$to, points$Name)]

  observations$id <- 1:nrow(observations)

  observations %<>% mutate(distance = TRUE,
                           direction = TRUE,
                           standard_dir = st_dir,
                           standard_dist = st_dist
  )
  return(observations)
}

# create complete sf object - points and observations
# Parameters:
#    1. points -  sf object with geometry type POINT and related attributes as product from function surveynet.mapedit_points
#    2. observations - sf object with geometry type LINESTRING and related attributes as product from function surveynet.mapedit_observations
#    3. fix_x - list with names of points that define datum - X coordinate
#    3. fix_y - list with names of points that define datum - Y coordinate
#    4. st_dir - "a priori" standard deviation for direction observations ["]
#    5. st_dist - "a priori" standard deviation for distance observations [mm]
#    6. dest_crs - destination Coordinate Reference System - set EPSG code [default: 3857 - Web Mercator projection coordinate system]
#    7. points_object - list with names of points that represnt object of interests

surveynet.mapedit <- function(points = points, observations = observations, fix_x = list(), fix_y = list(), dest_crs = NA, points_object = list()){

  if (is.na(dest_crs)){
    dest_crs <- 3857
    points <- points %>% st_transform(dest_crs)
  }
  #if (st_crs(points)$epsg == 4326){
  #  dest_crs <- 3857
  #  points <- points %>% st_transform(dest_crs)
  #  message("\nPoints data are reprojected to the destination Coordinate Reference System!\n")
  #  message(st_crs(points))
  #}
  points <- points %>% st_transform(dest_crs)

  # Defining datum for points
  points %<>% mutate(FIX_X = FALSE, FIX_Y = FALSE)

  points$FIX_X[points$Name %in% fix_x] <- TRUE
  points$FIX_Y[points$Name %in% fix_y] <- TRUE

  points %<>% mutate(Point_object = FALSE)
  points$Point_object[points$Name %in% points_object] <- TRUE


  points$x <- st_coordinates(points)[,1]
  points$y <- st_coordinates(points)[,2]

  observations$x_station <- points$x[match(observations$from, points$Name)]
  observations$y_station <- points$y[match(observations$from, points$Name)]
  observations$x_obs.point <- points$x[match(observations$to, points$Name)]
  observations$y_obs.point <- points$y[match(observations$to, points$Name)]

  observations$id <- 1:nrow(observations)

  observations %<>% mutate(distance = distance,
                           direction = direction,
                           standard_dir = standard_dir,
                           standard_dist = standard_dist
  )

  dt <- as.data.table(observations)
  dt_1 <- dt[
    , {
      geometry <- sf::st_linestring(x = matrix(c(x_station, x_obs.point, y_station, y_obs.point), ncol = 2))
      geometry <- sf::st_sfc(geometry)
      geometry <- sf::st_sf(geometry = geometry)
    }
    , by = id
    ]

  dt_1 <- sf::st_as_sf(dt_1)

  dt_1 %<>% mutate(from = observations$from,
                   to = observations$to,
                   distance = observations$distance,
                   direction = observations$direction,
                   standard_dir = observations$standard_dir,
                   standard_dist = observations$standard_dist
  )

  dt_1 <- dt_1 %>% sf::st_set_crs(dest_crs)
  observations <- dt_1

  # Observational plan - adding new columns
  observations %<>% mutate(distance = distance,
                           direction = direction,
                           standard_dir = standard_dir,
                           standard_dist = standard_dist
  )

  points <- subset(points, select = -c(x,y))
  # Creating list
  survey_net_mapedit <- list(points,observations)

  return(survey_net_mapedit)
}

