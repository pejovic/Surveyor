coef_d <- function (pt1, pt2, pts, units, axes = c("Easting", "Northing")) {
  yind <- which("Easting" == axes)
  xind <- which("Northing" == axes)
  units.table <- c("mm" = 1000, "cm" = 100, "m" = 1)
  pt1 <- as.numeric(pt1)
  pt2 <- as.numeric(pt2)
  coords <- pts[,-1]
  vec_d <- c(rep(0, dim(pts)[1]*2)) # Ovde je nesto promenjeno

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
  coords <- pts[,-1]
  vec_p <- c(rep(0, dim(pts)[1]*2)) # Ovde je nesto promenjeno
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

dist <- function(pt1_coords, pt2_coords, axes = c("Easting", "Northing")){
  ## check if the axis were set:
  if(length(axes) < 2) stop("axes must be defined with Easting and Northing")
  if(!any(axes %in% list("Northing", "Easting"))){ stop(paste(type, "axes must be Northing and Easting.")) }

  Northing_ind <- which(axes == "Northing")
  Easting_ind <- which(axes == "Easting")

  ## body
  Easting1 <- pt1_coords[Easting_ind]
  Northing1 <- pt1_coords[Northing_ind]
  Easting2 <- pt2_coords[Easting_ind]
  Northing2 <- pt2_coords[Northing_ind]
  dEasting <- as.numeric(Easting2 - Easting1)
  dNorthing <- as.numeric(Northing2 - Northing1)
  distance <- sqrt(dEasting^2 + dNorthing^2)
  return(distance)
}



ni <- function(pt1_coords, pt2_coords, type = list("dec", "dms", "rad"), axes = c("Easting", "Northing")){
  ## check if the axis were set:
  if(length(axes) < 2) stop("axes must be defined with Eastinging and Northinging")
  if(!any(axes %in% list("Northing", "Easting"))){ stop(paste(type, "axes must be Northing and Easting.")) }

  Northing_ind <- which(axes == "Northing")
  Easting_ind <- which(axes == "Easting")

  ## check if the type exists:
  if(length(type) > 1){ type <- type[[1]] }
  if(!any(type %in% list("dms", "dec", "rad"))){ stop(paste(type, "method not available.")) }

  ## body
  Easting1 <- pt1_coords[Easting_ind]
  Northing1 <- pt1_coords[Northing_ind]
  Easting2 <- pt2_coords[Easting_ind]
  Northing2 <- pt2_coords[Northing_ind]
  dEasting <- as.numeric(Easting2 - Easting1)
  dNorthing <- as.numeric(Northing2 - Northing1)
  atg <- ifelse(dEasting < 0, atan(dNorthing/dEasting)*180/pi + 180, atan(dNorthing/dEasting)*180/pi)
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



sim_Hz <- function(st_coords, target_coords, Hz_orient = NULL, Hz0 = 0, sd_cent = 2, sd_Hz = 5, c =  10, sd_colim = 5, sd_cent_target = NULL, sd_focus = NULL, seed = NULL, faces = list("mean", "both" ,"face1", "face2"), type = list("dms", "dec", "rad"), axes = c("Northing", "Easting")){
  ## check if the axis were set:
  if(length(axes) < 2) stop("axes must be defined with Easting and Northing")
  if(!any(axes %in% list("Northing", "Easting"))){ stop(paste(type, "axes must be Northing and Easting.")) }

  Northing_ind <- which(axes == "Northing")
  Easting_ind <- which(axes == "Easting")

  ## check if the type exists:
  if(length(type) > 1){ type <- type[[1]] }
  if(!any(type %in% list("dms", "dec", "rad"))){ stop(paste(type, "method not available.")) }


  d <- dist(pt1_coords = st_coords, pt2_coords = target_coords, axes = axes)
  ni1 <- ni(pt1_coords = st_coords, pt2_coords = target_coords, type = "dec", axes = axes)
  z <- 360-Hz0
  ro <- 180/pi*3600
  twoc = 2*c

  ## check if the type exists:
  if(length(type) > 1){ type <- type[[1]] }
  if(!any(type %in% list("dms", "dec", "rad"))){ stop(paste(type, "method not available.")) }

  if(ni1 >= Hz0){
    Hz <- z+ni1-360
  }else{
    Hz <- z+ni1
  }

  # seed mora biti fiksiran za stanicu
  if(!is.null(sd_cent)){
    set.seed(seed)
    sd_cent <- abs(rnorm(1, 0, sd_cent))
    sd_cent <- sqrt((sd_cent^2*ro^2)/(2*(d*1000)^2))
    Hz <- Hz + sd_cent/3600
  }
  if(!is.null(sd_cent_target)){
    set.seed(seed)
    sd_cent_target <- abs(rnorm(1, 0, sd_cent_target))
    sd_cent_target <- sqrt((sd_cent_target^2*ro^2)/(2*(d*1000)^2))
    Hz <- Hz + sd_cent_target/3600
  }

  set.seed(Sys.time())
  Hz <- Hz + sqrt((rnorm(1, 0, sd_Hz))^2)/3600

  if(type == "dms"){
    HzD <- floor(Hz); HzM <- floor((Hz-HzD)*60); HzS <- ((Hz-HzD)*60-HzM)*60
    Hz <- c(HzD, HzM, HzS)
    names(Hz) <- c("deg","min","sec")
    return(round(Hz))
  }
  if(type == "rad"){
    Hz <- Hz*pi/180
  }
  return(Hz)
}



sim_ang <- function(st_coords, orient_coords, target_coords, seed, Hz0 = 0, sd_Hz = 5, c =  10, sd_cent = 2, sd_cent_back = 2, sd_cent_target = 0, type = list("dms", "dec", "rad"), axes = c("Easting", "Northing")){
  ## check if the axis were set:
  if(length(axes) < 2) stop("axes must be defined with Easting and Northing")
  if(!any(axes %in% list("Northing", "Easting"))){ stop(paste(type, "axes must be Northing and Easting.")) }

  ## check if the type exists:
  if(length(type) > 1){ type <- type[[1]] }
  if(!any(type %in% list("dms", "dec", "rad"))){ stop(paste(type, "method not available.")) }

  Hz1 <- sim_Hz(st_coords = st_coords, target_coords =  target_coords, Hz0 = Hz0, sd_cent = sd_cent, sd_Hz = sd_Hz, sd_cent_target = sd_cent_target, type = "dec", seed = seed, axes = c("Northing", "Easting"))
  Hz2 <- sim_Hz(st_coords = st_coords, target_coords =  orient_coords, Hz0 = Hz0, sd_cent = sd_cent, sd_Hz = sd_Hz, sd_cent_target = sd_cent_back, type = "dec", seed = seed, axes = c("Northing", "Easting"))

  # ni1 <- ni(pt1_coords = st_coords, pt2_coords = target_coords, type = "dec", axes = c("Northing", "Easting"))
  # ni2 <- ni(pt1_coords = st_coords, pt2_coords = orient_coords, type = "dec", axes = c("Northing", "Easting"))

  dang <- as.numeric(Hz1-Hz2)
  dang <- ifelse(dang < 0, 360+dang, dang)

  if(type == "dms"){
    deg <- floor(dang); minut <- floor((dang-deg)*60); sec <- ((dang-deg)*60-minut)*60
    dang <- c(deg, minut, sec)
    names(dang) <- c("deg","min","sec")
  }
  if(type == "rad"){
    dang <- dang*pi/180
  }

  return(dang)
}


sim_dist <- function(st_coords, target_coords, sd_cent_station = NULL, sd_cent_target = NULL, sd_dist = 3, seed = NULL, axes = c("Easting", "Northing")){
  ## check if the axis were set:
  if(length(axes) < 2) stop("axes must be defined with Easting and Northing")
  if(!any(axes %in% list("Northing", "Easting"))){ stop(paste(type, "axes must be Northing and Easting.")) }

  distance = dist(pt1_coords = st_coords, pt2_coords = target_coords, axes = axes)

  if(!is.null(sd_cent_station)){
    set.seed(seed)
    sd_cent_station <- abs(rnorm(1, 0, sd_cent_station))
    sd_cent_station <- sqrt(sd_cent_station^2/2)
  }
  if(!is.null(sd_cent_target)){
    set.seed(seed)
    sd_cent_target <- abs(rnorm(1, 0, sd_cent_target))
    sd_cent_target <- sqrt(sd_cent_target^2/2)
  }
  distance <- distance + sd_cent_station/1000 + sd_cent_target/1000
  set.seed(seed)
  distance <- distance + sqrt((rnorm(1, 0, sd_dist))^2)/1000
  return(distance)
}




station_Hz <- function(station, target_points, Hz0 = 0, sd_cent_station = 2, sd_Hz = 5, c =  10, sd_colim = 5, sd_cent_target = NULL, sd_focus = NULL, seed = NULL, faces = list("mean", "both" ,"face1", "face2"), type = list("dms", "dec", "rad"), axes = c("Northing", "Easting")){
  target_points %>% group_by(Name) %>% Hz(st_coords = station, target_coords =  (target_points[,c(2,3)]), Hz0 = Hz0, sd_cent = sd_cent, sd_Hz = sd_Hz, sd_cent_target = sd_cent_target, type = "dec", seed = seed, axes = c("Northing", "Easting"))

  t(apply(target_points, 1, function(x) Hz(st_coords = station, target_coords = as.numeric(x[c(2:3)]), Hz0 = 270, sd_cent = 2, sd_Hz = 5, sd_cent_target = 2, type = "dms", seed = 111, axes = c("Northing", "Easting"))))

}

sim_Hz_all <- function(obs_Hz, points, red = TRUE, Hz0 = NA, sd_cent_station = c(1, 1, 1, 1), sd_cent_target = c(1, 1, 1, 1), sd_Hz = c(5, 5, 5, 5), seed = c(111, 222, 333, 444), type = list("dms", "dec", "rad"), axes = c("Northing", "Easting")){

  if(is.na(Hz0)) {Hz0 <- sample(1:359, size = length(unique(obs_Hz$station)))}else{
    if(length(Hz0) != length(unique(obs_Hz$station))) stop("Hz0 must be of length either 1 or number of stations")
  }

  if(length(sd_cent_station) == 1) sd_cent_station <- rep(sd_cent_station, length(unique(obs_Hz$station)))
  if(length(sd_cent_station) != length(unique(obs_Hz$station))) stop("sd_cent_station must be of length either 1 or the number of stations")

  if(length(sd_cent_target) == 1) sd_cent_target <- rep(sd_cent_target, length(unique(obs_Hz$station)))
  if(length(sd_cent_target) != length(unique(obs_Hz$station))) stop("sd_cent_target must be of length either 1 or the number of stations")

  if(length(sd_Hz) == 1) sd_Hz <- rep(sd_Hz, length(unique(obs_Hz$station)))
  if(length(sd_Hz) != length(unique(obs_Hz$station))) stop("sd_Hz must be of length either 1 or the number of stations")

  if(!is.null(seed)){
    if(length(seed) == 1) seed <- rep(seed, length(unique(obs_Hz$station)))
    if(length(seed) != length(unique(obs_Hz$station))) stop("seed must be of length either 1 or the number of stations")
  }else{
    seed <- rep(NULL, length(unique(obs_Hz$station)))
  }



  obs_Hz <- mutate(obs_Hz, x_station = rep(NA, dim(obs_Hz)[1]), y_station = rep(NA, dim(obs_Hz)[1]), x_target = rep(NA, dim(obs_Hz)[1]), y_target = rep(NA, dim(obs_Hz)[1]))
  for(i in 1:dim(obs_Hz)[1]){
    obs_Hz[i , c("x_station", "y_station")] <- points[which(obs_Hz$station[i] == points$Name), c("x", "y")]
    obs_Hz[i , c("x_target", "y_target")] <- points[which(obs_Hz$obs.point[i] == points$Name), c("x", "y")]
  }

  obs.Hz.list <- split(obs_Hz, obs_Hz$station)
  for(i in 1:length(obs.Hz.list)){
    obs.Hz.list[[i]]$Hz0 <- rep(Hz0[i], dim(obs.Hz.list[[i]])[1])
    obs.Hz.list[[i]]$sd_cent_station <- rep(sd_cent_station[i], dim(obs.Hz.list[[i]])[1])
    obs.Hz.list[[i]]$sd_cent_target <- rep(sd_cent_target[i], dim(obs.Hz.list[[i]])[1])
    obs.Hz.list[[i]]$sd_Hz <- rep(sd_Hz[i], dim(obs.Hz.list[[i]])[1])
    obs.Hz.list[[i]]$seed <- rep(seed[i], dim(obs.Hz.list[[i]])[1])
  }

  meas.Hz.list <- as.list(rep(NA, length(obs.Hz.list)))
  if(!is.null(seed)){
    for(i in 1:length(obs.Hz.list)){
      meas.Hz.list[[i]] <- t(apply(obs.Hz.list[[i]], 1, function(x) sim_Hz(st_coords = as.numeric(x[c(3:4)]), target_coords = as.numeric(x[c(5:6)]), Hz0 = as.numeric(x[7]), sd_cent = as.numeric(x[8]), sd_cent_target = as.numeric(x[9]), sd_Hz = as.numeric(x[10]), type = "dms", seed = as.numeric(x[11]), axes = c("Northing", "Easting"))))
    }
  }else{
    for(i in 1:length(obs.Hz.list)){
      meas.Hz.list[[i]] <- t(apply(obs.Hz.list[[i]], 1, function(x) sim_Hz(st_coords = as.numeric(x[c(3:4)]), target_coords = as.numeric(x[c(5:6)]), Hz0 = as.numeric(x[7]), sd_cent = as.numeric(x[8]), sd_cent_target = as.numeric(x[9]), sd_Hz = as.numeric(x[10]), type = "dms", axes = c("Northing", "Easting"))))
    }
  }

  if(red){
    meas.Hz.list <-  do.call(rbind, meas.Hz.list) %>% as.data.frame() %>% mutate(dec = deg + min/60 + sec/3600) %>% select(dec)
    Hz_meas <- cbind(do.call(rbind, obs.Hz.list), meas.Hz.list) %>% group_by(station) %>% mutate(dec.red = dec - dec[1]) %>% ungroup()
    Hz_meas$dec.red[which(Hz_meas$dec.red < 0)] <- Hz_meas$dec.red[which(Hz_meas$dec.red < 0)] + 360
    Hz_meas <- Hz_meas %>% mutate(deg = floor(dec.red), minut = floor((dec.red-deg)*60), sec = ((dec.red-deg)*60-minut)*60) %>% select(station, obs.point, deg, minut, sec)

  }else{
    Hz_meas <- cbind(do.call(rbind, obs.Hz.list), do.call(rbind, meas.Hz.list)) %>% select(station, obs.point, deg, min, sec)
  }

  return(Hz_meas)
}


sim_dist_all <- function(obs_d, points, sd_cent_station = c(1, 1, 1, 1), sd_cent_target = c(1, 1, 1, 1), sd_dist = c(3, 3, 3, 3), seed = c(111, 222, 333, 444)){

  if(length(sd_cent_station) == 1) sd_cent_station <- rep(sd_cent_station, length(unique(obs_d$station)))
  if(length(sd_cent_station) != length(unique(obs_d$station))) stop("sd_cent_station must be of length either 1 or the number of stations")

  if(length(sd_cent_target) == 1) sd_cent_target <- rep(sd_cent_target, length(unique(obs_d$station)))
  if(length(sd_cent_target) != length(unique(obs_d$station))) stop("sd_cent_target must be of length either 1 or the number of stations")

  if(length(sd_dist) == 1) sd_dist <- rep(sd_dist, length(unique(obs_d$station)))
  if(length(sd_dist) != length(unique(obs_d$station))) stop("sd_dist must be of length either 1 or the number of stations")

  if(!is.null(seed)){
    if(length(seed) == 1) seed <- rep(seed, length(unique(obs_d$station)))
    if(length(seed) != length(unique(obs_d$station))) stop("seed must be of length either 1 or the number of stations")
  }else{
    seed <- sample(1:1000, size = length(unique(obs_d$station))) # rep(NULL, length(unique(obs_d$station)))
  }

  obs_d <- mutate(obs_d, x_station = rep(NA, dim(obs_d)[1]), y_station = rep(NA, dim(obs_d)[1]), x_target = rep(NA, dim(obs_d)[1]), y_target = rep(NA, dim(obs_d)[1]))
  for(i in 1:dim(obs_d)[1]){
    obs_d[i , c("x_station", "y_station")] <- points[which(obs_d$station[i] == points$Name), c("x", "y")]
    obs_d[i , c("x_target", "y_target")] <- points[which(obs_d$obs.point[i] == points$Name), c("x", "y")]
  }

  obs.d.list <- split(obs_d, obs_d$station)
  for(i in 1:length(obs.d.list)){
    obs.d.list[[i]]$sd_cent_station <- rep(sd_cent_station[i], dim(obs.d.list[[i]])[1])
    obs.d.list[[i]]$sd_cent_target <- rep(sd_cent_target[i], dim(obs.d.list[[i]])[1])
    obs.d.list[[i]]$sd_dist <- rep(sd_dist[i], dim(obs.d.list[[i]])[1])
    obs.d.list[[i]]$seed <- rep(seed[i], dim(obs.d.list[[i]])[1])
  }

  meas.d.list <- as.list(rep(NA, length(obs.d.list)))
  if(!is.null(seed)){
    for(i in 1:length(obs.d.list)){
      meas.d.list[[i]] <- data.frame(dist = (apply(obs.d.list[[i]], 1, function(x) sim_dist(st_coords = as.numeric(x[c(3:4)]), target_coords = as.numeric(x[c(5:6)]), sd_cent = as.numeric(x[7]), sd_cent_target = as.numeric(x[8]), sd_dist = as.numeric(x[9]), seed = as.numeric(x[10]), axes = c("Northing", "Easting")))))
    }
  }else{
    for(i in 1:length(obs.d.list)){
      meas.d.list[[i]] <- data.frame(dist = (apply(obs.d.list[[i]], 1, function(x) sim_dist(st_coords = as.numeric(x[c(3:4)]), target_coords = as.numeric(x[c(5:6)]), sd_cent = as.numeric(x[7]), sd_cent_target = as.numeric(x[8]), sd_dist = as.numeric(x[9]), axes = c("Northing", "Easting")))))
    }
  }

  d_meas <- cbind(do.call(rbind, obs.d.list), do.call(rbind, meas.d.list)) %>% select(station, obs.point, dist)

  return(d_meas)
}

