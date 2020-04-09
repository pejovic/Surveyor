# A <- c(393.979,	419.038)
# B <- c(366.358, 550.138)






# st_coords = A; target_coords = B; sd_Hz = 3; e_cent_from = 1; e_cent_to = 1; e_focus = 0; Hz0 = 0; ng = 1; faces = "mean"; sd_sd_Hz = 0.3; sd_r = 1; seed = NULL; type = "dms"; axes = c("Northing", "Easting")

sim_Hz <- function(st_coords, target_coords, sd_Hz, e_cent_from = 0, e_cent_to = 0, e_focus = 0, Hz0 = 0, ng = 1, faces = list("mean", "f1f2" ,"face1", "face2"), sd_sd_Hz = 0.3, sd_r = 1, seed = NULL, type = list("dms", "dec", "rad"), axes = c("Northing", "Easting")){
  ## check if the axis were set:
  if(length(axes) < 2) stop("axes must be defined with Easting and Northing")
  if(!any(axes %in% list("Northing", "Easting"))){ stop(paste(type, "axes must be Northing and Easting.")) }

  Northing_ind <- which(axes == "Northing")
  Easting_ind <- which(axes == "Easting")

  ## check if the type exists:
  if(length(type) > 1){ type <- type[[1]] }
  if(!any(type %in% list("dms", "dec", "rad"))){ stop(paste(type, "method not available.")) }

  d <- dist(pt1_coords = st_coords, pt2_coords = target_coords)
  ro <- 180/pi*3600

  if(is.null(seed)){seed = runif(1,0,100)}

  # set.seed(seed)
  # e_cent_from <- abs(runif(1, 0, e_cent_from))

    st_coords_e <- ecent_coords(pt_coords = st_coords, e_cent = e_cent_from, seed = runif(1,0,100))
    e_cent_from <- dist(pt1_coords = st_coords, pt2_coords = st_coords_e)*1000
    sd_e_cent_from <- sqrt((e_cent_from^2*ro^2)/(2*(d*1000)^2))

  # set.seed(seed)
  # e_cent_to <- abs(runif(1, 0, e_cent_to))

    target_coords_e <- ecent_coords(pt_coords = target_coords, e_cent = e_cent_to, seed = runif(1,0,100)+1)
    e_cent_to <- dist(pt1_coords = target_coords, pt2_coords = target_coords_e)*1000
    sd_e_cent_to <- sqrt((e_cent_to^2*ro^2)/(2*(d*1000)^2)) #Use units lookup table!!

  ni1 <- ni(pt1_coords = st_coords, pt2_coords = target_coords, type = "dec")
  z <- 360-Hz0

  ## check if the type exists:
  if(length(type) > 1){type <- type[[1]]}
  if(!any(type %in% list("dms", "dec", "rad"))){ stop(paste(type, "method not available.")) }

  if(ni1 >= Hz0){
    Hz <- z+ni1-360
  }else{
    Hz <- z+ni1
  }

  #set.seed(seed)
  sd_hz <- sqrt(2*(sd_r^2+e_focus^2+sd_Hz^2)/ng + sd_e_cent_from^2+sd_e_cent_to^2)
  #sd_hz <- rnorm(1, sd_hz, sd_sd_Hz)/3600

  # sd_hz_rand <- 0
  # while(abs(sd_hz_rand) < sd_hz | abs(sd_hz_rand) > 2*sd_hz){
  #   sd_hz_rand <- rnorm(1, 0, sd_Hz)
  # }
  # sd_hz <- sd_hz_rand/3600
  #sd_hz <- rnorm(1, sd_hz, sd_sd_Hz)/3600

  sd_hz <- rnorm(1, 0, sd_Hz) #rnorm(1, sd_hz, sd_sd_Hz)/3600
  #sd_hz <- mean((sd_hz[abs(sd_hz < 0+2*sd_Hz) & abs(sd_hz > 0+1*sd_Hz)]))

  Hz <- Hz + sd_hz/3600 #sample(c(-1,1), 1)*sd_hz #+ e_cent_from/3600 + e_cent_to/3600

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

rand_coords <- function(snet.points, sp){
  sp <- sp/1000
  mu <- snet.points %>% st_drop_geometry() %>% .[, c("x","y")] %>% as.matrix()
  sigma <- matrix(c(sp^2, 0, 0, sp^2),2)
  for(i in 1:dim(mu)[1]){
    mu[i, ] <- mvrnorm(1, mu[i, ], sigma)
  }
  return(mu)
}




survey.net.st <- survey.net[[2]] %>% filter(from == "PP2")

survey.net.st %>% st_drop_geometry() %>% .[1, c("x_from", "y_from")] %>% as.numeric()






#sim_face_hz(hz = a[1]+a[2]/60+a[3]/3600, sd_Hz = 15, e_colim = 10, sd_e_colim = 1, faces = "f1f2", sd_



# survey.net <- dns
# e_cent_from = 1
# e_cent_to = 1



# pt1_coords = as.numeric(observations[i, c("x_from", "y_from")]); pt2_coords = as.numeric(observations[i, c("x_to", "y_to")]); e_cent_from = as.numeric(observations[i, "e_cent_from"]); e_cent_to = as.numeric(observations[i, "e_cent_to"]); sd_dist = as.numeric(observations[i, "sd_dist"]); seed = NULL


#set.seed(seed)
#rnorm(1, 0, sd_dist)



#survey.net = mreza; red = TRUE;red = TRUE; Hz0 = NA; seed = NULL







sim_dist_all <- function(obs_d, points, e_cent_from = c(1, 1, 1, 1), e_cent_to = c(1, 1, 1, 1), sd_dist = c(3, 3, 3, 3), seed = c(111, 222, 333, 444)){

  if(length(e_cent_from) == 1) e_cent_from <- rep(e_cent_from, length(unique(obs_d$station)))
  if(length(e_cent_from) != length(unique(obs_d$station))) stop("e_cent_from must be of length either 1 or the number of stations")

  if(length(e_cent_to) == 1) e_cent_to <- rep(e_cent_to, length(unique(obs_d$station)))
  if(length(e_cent_to) != length(unique(obs_d$station))) stop("e_cent_to must be of length either 1 or the number of stations")

  if(length(sd_dist) == 1) sd_dist <- rep(sd_dist, length(unique(obs_d$station)))
  #if(length(sd_dist) != length(unique(obs_d$station))) stop("sd_dist must be of length either 1 or the number of stations")

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
    obs.d.list[[i]]$e_cent_from <- rep(e_cent_from[i], dim(obs.d.list[[i]])[1])
    obs.d.list[[i]]$e_cent_to <- rep(e_cent_to[i], dim(obs.d.list[[i]])[1])
    obs.d.list[[i]]$sd_dist <- rep(sd_dist[i], dim(obs.d.list[[i]])[1])
    obs.d.list[[i]]$seed <- rep(seed[i], dim(obs.d.list[[i]])[1])
  }

  meas.d.list <- as.list(rep(NA, length(obs.d.list)))
  if(!is.null(seed)){
    for(i in 1:length(obs.d.list)){
      meas.d.list[[i]] <- data.frame(dist = (apply(obs.d.list[[i]], 1, function(x) sim_dist(st_coords = as.numeric(x[c(3:4)]), target_coords = as.numeric(x[c(5:6)]), e_cent = as.numeric(x[7]), e_cent_to = as.numeric(x[8]), sd_dist = as.numeric(x[9]), seed = as.numeric(x[10])))))
    }
  }else{
    for(i in 1:length(obs.d.list)){
      meas.d.list[[i]] <- data.frame(dist = (apply(obs.d.list[[i]], 1, function(x) sim_dist(st_coords = as.numeric(x[c(3:4)]), target_coords = as.numeric(x[c(5:6)]), e_cent = as.numeric(x[7]), e_cent_to = as.numeric(x[8]), sd_dist = as.numeric(x[9])))))
    }
  }

  d_meas <- cbind(do.call(rbind, obs.d.list), do.call(rbind, meas.d.list)) %>% select(station, obs.point, dist)

  return(d_meas)
}

#points = B.survey.net[[1]]; obs.plan = B_sim_obs; sd_Hz = 5; e_cent_from = 1; sd_dist = 3; e_cent_to = 1; Hz0 = NA

sim.obs <- function(points, obs.plan, Hz0 = NA, red = TRUE, sd_Hz = 10, sd_dist = 3, e_cent_from = 2, e_cent_to = 3, seed = NULL){
  obs.dist <- filter(obs.plan, type == "d") %>% select(1,2) # Selekcija merenih duzina
  sim.dist <- sim_dist_all(obs_d = obs.dist, points = points, e_cent_from = e_cent_from, e_cent_to = e_cent_to, seed = seed)

  obs.Hz <- filter(obs.plan, type == "p") %>% select(1,2) # Selekcija merenih pravaca
  sim.Hz <- sim_Hz_all(obs_Hz = obs.Hz, points = points, Hz0 = Hz0, red = red, e_cent_from = e_cent_from, e_cent_to = e_cent_to, sd_Hz = sd_Hz, seed = seed)

  obs <- dplyr::full_join(sim.Hz, sim.dist) %>%
    mutate(sd_Hz = sd_Hz, sd_dist = sd_dist, SD = NA, VzD = NA, VzM = NA, VzS = NA, sd_Vz = NA) %>%
    dplyr::select(from = station, to = obs.point, HzD = deg, HzM = minut, HzS = sec, HD = dist, SD, VzD, VzM, VzS, sd_Hz, sd_dist, sd_Vz)
  obs.list <- list(Points = points, Observations = obs)
  return(obs.list)
}







sim_ang <- function(st_coords, orient_coords, target_coords, seed, Hz0 = 0, sd_Hz = 5, c =  10, e_cent = 2, e_cent_back = 2, e_cent_to = 0, type = list("dms", "dec", "rad"), axes = c("Easting", "Northing")){
  ## check if the axis were set:
  if(length(axes) < 2) stop("axes must be defined with Easting and Northing")
  if(!any(axes %in% list("Northing", "Easting"))){ stop(paste(type, "axes must be Northing and Easting.")) }

  ## check if the type exists:
  if(length(type) > 1){ type <- type[[1]] }
  if(!any(type %in% list("dms", "dec", "rad"))){ stop(paste(type, "method not available.")) }

  Hz1 <- sim_Hz(st_coords = st_coords, target_coords =  target_coords, Hz0 = Hz0, e_cent = e_cent, sd_Hz = sd_Hz, e_cent_to = e_cent_to, type = "dec", seed = seed, axes = c("Northing", "Easting"))
  Hz2 <- sim_Hz(st_coords = st_coords, target_coords =  orient_coords, Hz0 = Hz0, e_cent = e_cent, sd_Hz = sd_Hz, e_cent_to = e_cent_back, type = "dec", seed = seed, axes = c("Northing", "Easting"))

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

station_Hz <- function(station, target_points, Hz0 = 0, e_cent_from = 2, sd_Hz = 5, e_colim =  10, sd_e_colim = 5, e_cent_to = NULL, e_focus = NULL, seed = NULL, faces = list("mean", "f1f2" ,"face1", "face2"), type = list("dms", "dec", "rad"), axes = c("Northing", "Easting")){

  target_points %>% group_by(Name) %>% Hz(st_coords = station, target_coords =  (target_points[,c(2,3)]), Hz0 = Hz0, e_cent = e_cent, sd_Hz = sd_Hz, e_cent_to = e_cent_to, type = "dec", seed = seed, axes = c("Northing", "Easting"))

  t(apply(target_points, 1, function(x) Hz(st_coords = station, target_coords = as.numeric(x[c(2:3)]), Hz0 = 270, e_cent = 2, sd_Hz = 5, e_cent_to = 2, type = "dms", seed = 111, axes = c("Northing", "Easting"))))

}

sim_Hz_all <- function(survey.net, red = TRUE, Hz0 = NA, seed = NULL, type = list("dms", "dec", "rad")){

  obs_Hz <- survey.net[[2]] %>% dplyr::filter(direction) %>% dplyr::select(from, to, HzD, HzM, HzS, sd_Hz, x_from, y_from, x_to, y_to, e_cent_from,	e_cent_to,	e_focus) %>% dplyr::mutate_at(., .vars = c("e_cent_from", "e_cent_to", "e_focus"), ~replace(., is.na(.), 0))

  if(is.na(Hz0)) {Hz0 <- sample(1:359, size = length(unique(obs_Hz$from)))}else{
    if(length(Hz0) != length(unique(obs_Hz$from))) stop("Hz0 must be of length either 1 or number of stations")
  }

  if(!is.null(seed)){
    if(length(seed) == 1) seed <- rep(seed, length(unique(obs_Hz$from)))
    if(length(seed) != length(unique(obs_Hz$from))) stop("seed must be of length either 1 or the number of stations")
  }else{
    seed <- runif(length(unique(obs_Hz$from)),0,100)
  }

  # TODO: Replace NA with ZEROS for cent errors.

  obs.Hz.list <- split(obs_Hz, obs_Hz$from)
  for(i in 1:length(obs.Hz.list)){
    obs.Hz.list[[i]]$Hz0 <- rep(Hz0[i], dim(obs.Hz.list[[i]])[1])
    obs.Hz.list[[i]]$seed <- rep(seed[i], dim(obs.Hz.list[[i]])[1])
    obs.Hz.list[[i]]$Hz <- rep(NA, dim(obs.Hz.list[[i]])[1])
  }

  for(i in 1:length(obs.Hz.list)){
    obs.Hz.list[[i]]$Hz <- as.numeric(t(apply(obs.Hz.list[[i]], 1, function(x) sim_Hz(st_coords = as.numeric(x[c("x_from", "y_from")]), target_coords = as.numeric(x[c("x_to", "y_to")]), Hz0 = as.numeric(x["Hz0"]), e_cent_from = as.numeric(x["e_cent_from"]), e_cent_to = as.numeric(x["e_cent_to"]), sd_Hz = as.numeric(x["sd_Hz"]), type = "dec", seed = as.numeric(x["seed"]), axes = c("Northing", "Easting")))))
  }

  if(red){
    obs_Hz <- obs.Hz.list %>% lapply(., function(x) dplyr::mutate(x, Hz = Hz - Hz[1])) %>% lapply(., function(x) dplyr::mutate(x, Hz = case_when(Hz < 0 ~ Hz + 360, TRUE ~ Hz))) %>% do.call(rbind,.)
  }
  obs_Hz <- obs_Hz %>% mutate(HzD = floor(Hz), HzM = floor((Hz-HzD)*60), HzS = ((Hz-HzD)*60-HzM)*60) %>%
    select(from, to, HzD, HzM, HzS, sd_Hz, geometry) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(from, to, HzD, HzM, HzS) %>%
    left_join(dplyr::select(survey.net[[2]], -c("HzD", "HzM", "HzS")), .) %>%
    dplyr::select(from, to, HzD, HzM, HzS, everything())


  obs_dist <- survey.net[[2]] %>% dplyr::filter(distance) %>% dplyr::select(from, to, sd_dist, x_from, y_from, x_to, y_to, e_cent_from, e_cent_to, e_focus) %>% dplyr::mutate_at(., .vars = c("e_cent_from", "e_cent_to", "e_focus"), ~replace(., is.na(.), 0))

  obs.dist.list <- split(obs_dist, obs_dist$from)
  for(i in 1:length(obs.dist.list)){
    obs.dist.list[[i]]$seed <- rep(seed[i], dim(obs.dist.list[[i]])[1])
    obs.dist.list[[i]]$HD_sim <- rep(NA, dim(obs.dist.list[[i]])[1])
  }

  obs.dist.list <- obs.dist.list %>% lapply(., function(x) rowwise(x) %>% dplyr::mutate(., HD = dist(pt1_coords = c(x_from, y_from), pt2_coords = c(x_to, y_to))) %>% ungroup()) %>% do.call(rbind, .) %>% dplyr::select(from, to, HD, sd_dist)

  aa <- inner_join(obs.Hz.list, obs.dist.list)
  # Ovo Je super!!! TODO: Simulacija duzina nije zavrsena!
}

sim_dist <- function(st_coords, target_coords, e_cent_from, e_cent_to, sd_dist = 3, seed = NULL,  sd_e_cent_from = 0.5, sd_e_cent_to = 0.5, sd_sd_dist = 0.2, axes = c("Easting", "Northing")){
  ## check if the axis were set:
  if(length(axes) < 2) stop("axes must be defined with Easting and Northing")
  if(!any(axes %in% list("Northing", "Easting"))){ stop(paste(type, "axes must be Northing and Easting.")) }

  # if(!is.null(e_cent_from)){
  #   set.seed(seed)
  #   e_cent_from <- abs(runif(1, 0, e_cent_from))
  #   e_cent_from <- sqrt(e_cent_from^2/2)
  # }
  # if(!is.null(e_cent_to)){
  #   set.seed(seed)
  #   e_cent_to <- abs(runif(1, 0, e_cent_to))
  #   e_cent_to <- sqrt(e_cent_to^2/2
  # })

  if(is.null(seed)){seed = runif(1,0,100)}

  st_coords_e <- ecent_coords(pt_coords = st_coords, e_cent = e_cent_from, seed = seed)
  e_cent_from <- dist(pt1_coords = st_coords, pt2_coords = st_coords_e)*1000
  sd_e_cent_from <-  sqrt(e_cent_from^2/2)

  set.seed(seed)
  target_coords_e <- ecent_coords(pt_coords = target_coords, e_cent = e_cent_to, seed = seed+1)
  e_cent_to <- dist(pt1_coords = target_coords, pt2_coords = target_coords_e)*1000
  sd_e_cent_to <- sqrt(e_cent_to^2/2)

  distance = dist(pt1_coords = st_coords_e, pt2_coords = target_coords_e)

  sd_dist <- sqrt(sd_dist^2 + sd_e_cent_from^2 + sd_e_cent_from^2)

  # sd_dist_rand <- 0
  # while(abs(sd_dist_rand) < sd_dist | abs(sd_dist_rand) > 3*sd_dist){
  #   sd_dist_rand <- rnorm(1, 0, sd_dist)
  # }
  # sd_dist <- sd_dist_rand/1000


  sd_dist <-  rnorm(1, 0, sd_dist) # rnorm(1, sd_dist, sd_sd_dist)
  #set.seed(seed)
  distance <- distance + sd_dist/1000 #sample(c(-1,1),1)*sd_dist/1000
  return(distance)
}


observations <- sf::st_drop_geometry(survey.net[[2]])
for(i in 1:dim(observations)[1]){
  if(observations$direction[i]){
    observations$Hz_sim[i] <- sim_Hz(st_coords = as.numeric(observations[i, c("x_from", "y_from")]), target_coords = as.numeric(observations[i, c("x_to", "y_to")]), Hz0 = as.numeric(observations[i, "Hz0"]), e_cent_from = as.numeric(observations[i, "e_cent_from"]), e_cent_to = as.numeric(observations[i, "e_cent_to"]), sd_Hz = as.numeric(observations[i, "sd_Hz"]), type = "dec", seed = as.numeric(observations[i, "seed"]), axes = c("Northing", "Easting"))
  }
  if(observations$distance[i]){
    observations$dist_sim[i] <- sim_dist(st_coords = as.numeric(observations[i, c("x_from", "y_from")]), target_coords = as.numeric(observations[i, c("x_to", "y_to")]), e_cent_from = as.numeric(observations[i, "e_cent_from"]), e_cent_to = as.numeric(observations[i, "e_cent_to"]), sd_dist = as.numeric(observations[i, "sd_dist"]), seed = as.numeric(observations[i, "seed"]))
  }
}

if(red){
  observations <- observations %>% dplyr::group_by(from) %>% dplyr::mutate_at(., .vars = "Hz_sim", ~Hz_sim - Hz_sim[1]) %>% dplyr::mutate(Hz_sim = dplyr::case_when(Hz_sim < 0 ~ Hz_sim + 360, Hz_sim >= 0 ~ Hz_sim)) %>% dplyr::group_by(from) %>% dplyr::arrange(ID, Hz_sim) %>% dplyr::ungroup()
}

survey.net[[2]] <- observations %>%
  dplyr::mutate(HzD = floor(Hz_sim), HzM = floor((Hz_sim-HzD)*60), HzS = ((Hz_sim-HzD)*60-HzM)*60, HD = dist_sim, Hz = Hz_sim) %>%
  dplyr::select(ID:diff_level) %>%
  left_join(., dplyr::select(survey.net[[2]], from, to), by = c("from", "to")) %>%
  as.data.frame() %>%
  st_sf(., crs = prj)

