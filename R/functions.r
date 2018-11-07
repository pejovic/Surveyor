
library(tidyverse)
library(magrittr)


A <- c(393.979,	419.038)
B <- c(366.358, 550.138)
C <- c(601.903, 632.171)
D <- c(705.481, 538.638)

T1 <- c(500.000, 500.000)
T2 <- c(585.023, 548.609)
T3 <- c(500.000, 548.609)
T4 <- c(585.023, 500.000)

points <- rbind(A, B, C, D, T1, T2, T3, T4) %>% as.data.frame() %>% rownames_to_column("Name") %>% rename(x = V1, y = V2)

obs_plan <- data.frame(station = rep(c("A", "A", "A", "B", "B", "C", "C", "C", "D", "D"), 2), obs.point = rep(c("B", "T1", "T3", "A", "T1", "D", "T2","T3", "C", "T2"), 2), type = c(rep("p", 10), rep("d", 10)) )

st_coords <- A
target_coords <- T1
orient_coords <- B

dist <- function(pt1_coords, pt2_coords, axises = c("East", "North")){
  ## check if the axis were set:
  if(length(axises) < 2) stop("Axises must be defined with East and North")
  if(!any(axises %in% list("North", "East"))){ stop(paste(type, "Axises must be North and East.")) }

  north_ind <- which(axises == "North")
  east_ind <- which(axises == "East")

  ## body
  east1 <- pt1_coords[east_ind]
  north1 <- pt1_coords[north_ind]
  east2 <- pt2_coords[east_ind]
  north2 <- pt2_coords[north_ind]
  deast <- as.numeric(east2 - east1)
  dnorth <- as.numeric(north2 - north1)
  distance <- sqrt(deast^2 + dnorth^2)
  return(distance)
}



ni <- function(pt1_coords, pt2_coords, type = list("dec", "dms", "rad"), axises = c("East", "North")){
  ## check if the axis were set:
  if(length(axises) < 2) stop("Axises must be defined with East and North")
  if(!any(axises %in% list("North", "East"))){ stop(paste(type, "Axises must be North and East.")) }

  north_ind <- which(axises == "North")
  east_ind <- which(axises == "East")

  ## check if the type exists:
  if(length(type) > 1){ type <- type[[1]] }
  if(!any(type %in% list("dms", "dec", "rad"))){ stop(paste(type, "method not available.")) }

  ## body
  east1 <- pt1_coords[east_ind]
  north1 <- pt1_coords[north_ind]
  east2 <- pt2_coords[east_ind]
  north2 <- pt2_coords[north_ind]
  deast <- as.numeric(east2 - east1)
  dnorth <- as.numeric(north2 - north1)
  atg <- ifelse(deast < 0, atan(dnorth/deast)*180/pi + 180, atan(dnorth/deast)*180/pi)
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

# ni(pt1_coords = st_coords, pt2_coords = target_coords, type = "dms", axises = c("East", "North"))


# st_coords = A; target_coords = B; hz_orient = NULL; hz0 = 255; scent = 2; standard_hz = 5; seed = 111; c =  10; sc = 5; scent_or = 5; scent_target = 5; sfocus = NULL; faces = list("mean", "both" ,"face1", "face2"); type = list("dec", "dec", "rad"); axises = c("North", "East")

hz <- function(st_coords, target_coords, hz_orient = NULL, hz0 = 0, scent = 2, standard_hz = 5, c =  10, sc = 5, scent_target = NULL, sfocus = NULL, seed = NULL, faces = list("mean", "both" ,"face1", "face2"), type = list("dms", "dec", "rad"), axises = c("North", "East")){
  ## check if the axis were set:
  if(length(axises) < 2) stop("Axises must be defined with East and North")
  if(!any(axises %in% list("North", "East"))){ stop(paste(type, "Axises must be North and East.")) }

  north_ind <- which(axises == "North")
  east_ind <- which(axises == "East")

  ## check if the type exists:
  if(length(type) > 1){ type <- type[[1]] }
  if(!any(type %in% list("dms", "dec", "rad"))){ stop(paste(type, "method not available.")) }


  d <- dist(pt1_coords = st_coords, pt2_coords = target_coords, axises = c("North", "East"))
  ni1 <- ni(pt1_coords = st_coords, pt2_coords = target_coords, type = "dec", axises = axises)
  z <- 360-hz0
  ro <- 180/pi*3600
  twoc = 2*c

  ## check if the type exists:
  if(length(type) > 1){ type <- type[[1]] }
  if(!any(type %in% list("dms", "dec", "rad"))){ stop(paste(type, "method not available.")) }

  if(ni1 >= hz0){
    hzm <- z+ni1-360
  }else{
    hzm <- z+ni1
  }

  # seed mora biti fiksiran za stanicu
  if(!is.null(scent)){
    set.seed(seed)
    scent <- abs(rnorm(1, 0, scent))
    scent <- sqrt((scent^2*ro^2)/(2*(d*1000)^2))
    hzm <- hzm + scent/3600
    }
  if(!is.null(scent_target)){
    set.seed(seed)
    scent_target <- abs(rnorm(1, 0, scent_target))
    scent_target <- sqrt((scent_target^2*ro^2)/(2*(d*1000)^2))
    hzm <- hzm + scent_target/3600
    }

  set.seed(Sys.time())
  hzm <- hzm + sqrt((rnorm(1, 0, standard_hz))^2)/3600

  if(type == "dms"){
    deg <- floor(hzm); minut <- floor((hzm-deg)*60); sec <- ((hzm-deg)*60-minut)*60
    hzm <- c(deg, minut, sec)
    names(hzm) <- c("deg","min","sec")
    return(round(hzm))
  }
  if(type == "rad"){
    hzm <- hzm*pi/180
  }
return(hzm)
}


# hz(A, T1, hz0 = 255, scent = 2, scent_target = 2, standard_hz = 6, seed = 111, type = "dms", axises = c("East", "North"))



m_ang <- function(st_coords, orient_coords, target_coords, seed, hz0 = 0, standard_hz = 5, c =  10, scent = 2, scent_or = 2, scent_target = 0, type = list("dms", "dec", "rad"), axises = c("East", "North")){
  ## check if the axis were set:
  if(length(axises) < 2) stop("Axises must be defined with East and North")
  if(!any(axises %in% list("North", "East"))){ stop(paste(type, "Axises must be North and East.")) }

  ## check if the type exists:
  if(length(type) > 1){ type <- type[[1]] }
  if(!any(type %in% list("dms", "dec", "rad"))){ stop(paste(type, "method not available.")) }

  hz1 <- hz(st_coords = st_coords, target_coords =  target_coords, hz0 = hz0, scent = scent, standard_hz = standard_hz, scent_target = scent_target, type = "dec", seed = seed, axises = c("North", "East"))
  hz2 <- hz(st_coords = st_coords, target_coords =  orient_coords, hz0 = hz0, scent = scent, standard_hz = standard_hz, scent_target = scent_or, type = "dec", seed = seed, axises = c("North", "East"))

  # ni1 <- ni(pt1_coords = st_coords, pt2_coords = target_coords, type = "dec", axises = c("North", "East"))
  # ni2 <- ni(pt1_coords = st_coords, pt2_coords = orient_coords, type = "dec", axises = c("North", "East"))

  dang <- as.numeric(hz1-hz2)
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

# m_ang(st_coords = A, target_coords = T1, orient_coords = B, seed = NULL, hz0 = 288, scent = 2, scent_or =  2, scent_target = 1,  type = "dec", axises = c("East", "North"))


m_dist <- function(st_coords, target_coords, scent_station = NULL, scent_target = NULL, standard_d = 3, seed = NULL, axises = c("East", "North")){
  ## check if the axis were set:
  if(length(axises) < 2) stop("Axises must be defined with East and North")
  if(!any(axises %in% list("North", "East"))){ stop(paste(type, "Axises must be North and East.")) }

  distance = dist(pt1_coords = st_coords, pt2_coords = target_coords, axises = axises)

  if(!is.null(scent_station)){
    set.seed(seed)
    scent_station <- abs(rnorm(1, 0, scent_station))
    scent_station <- sqrt(scent_station^2/2)
  }
  if(!is.null(scent_target)){
    set.seed(seed)
    scent_target <- abs(rnorm(1, 0, scent_target))
    scent_target <- sqrt(scent_target^2/2)
  }
  distance <- distance + scent_station/1000 + scent_target/1000
  set.seed(seed)
  distance <- distance + sqrt((rnorm(1, 0, standard_d))^2)/1000
  return(distance)
}


# m_dist(st_coords = A, target_coords = B, seed = 111, scent_station = 2, scent_target =  2, standard_d = 3)

station = as.numeric(points[1, c(2,3)])
target_points = points[c(2, 5), ]

stA <- data.frame(rep = points[which(points$Name == obs_plan[1, 1]), c(2,3)], target_points)

station_hz <- function(station, target_points, hz0 = 0, scent_station = 2, standard_hz = 5, c =  10, sc = 5, scent_target = NULL, sfocus = NULL, seed = NULL, faces = list("mean", "both" ,"face1", "face2"), type = list("dms", "dec", "rad"), axises = c("North", "East")){
  target_points %>% group_by(Name) %>% hz(st_coords = station, target_coords =  (target_points[,c(2,3)]), hz0 = hz0, scent = scent, standard_hz = standard_hz, scent_target = scent_target, type = "dec", seed = seed, axises = c("North", "East"))

  t(apply(target_points, 1, function(x) hz(st_coords = station, target_coords = as.numeric(x[c(2:3)]), hz0 = 270, scent = 2, standard_hz = 5, scent_target = 2, type = "dms", seed = 111, axises = c("North", "East"))))

}

obs_hz <- filter(obs_plan, type == "p") %>% select(1,2)
hz0 = c(0, 0, 0, 0); scent_station = c(1, 1, 1, 1); scent_target = c(1, 1, 1, 1); standard_hz = c(5, 5, 5, 5); seed = NULL #c(111, 222, 333, 444)
red = FALSE


sim_hz_meas <- function(obs_hz, points, red = TRUE, hz0 = c(0, 0, 0, 0), scent_station = c(1, 1, 1, 1), scent_target = c(1, 1, 1, 1), standard_hz = c(5, 5, 5, 5), seed = c(111, 222, 333, 444), type = list("dms", "dec", "rad"), axises = c("North", "East")){

  if(length(hz0) == 1)hz0 <- rep(hz0, length(unique(obs_hz$station)))
  if(length(hz0) != length(unique(obs_hz$station))) stop("hz0 must be of length either 1 or number of stations")

  if(length(scent_station) == 1) scent_station <- rep(scent_station, length(unique(obs_hz$station)))
  if(length(scent_station) != length(unique(obs_hz$station))) stop("scent_station must be of length either 1 or the number of stations")

  if(length(scent_target) == 1) scent_target <- rep(scent_target, length(unique(obs_hz$station)))
  if(length(scent_target) != length(unique(obs_hz$station))) stop("scent_target must be of length either 1 or the number of stations")

  if(length(standard_hz) == 1) standard_hz <- rep(standard_hz, length(unique(obs_hz$station)))
  if(length(standard_hz) != length(unique(obs_hz$station))) stop("standard_hz must be of length either 1 or the number of stations")

  if(!is.null(seed)){
    if(length(seed) == 1) seed <- rep(seed, length(unique(obs_hz$station)))
    if(length(seed) != length(unique(obs_hz$station))) stop("seed must be of length either 1 or the number of stations")
  }else{
    seed <- rep(NULL, length(unique(obs_hz$station)))
  }



  obs_hz <- mutate(obs_hz, xs = rep(NA, dim(obs_hz)[1]), ys = rep(NA, dim(obs_hz)[1]), xt = rep(NA, dim(obs_hz)[1]), yt = rep(NA, dim(obs_hz)[1]))
  for(i in 1:dim(obs_hz)[1]){
    obs_hz[i , c("xs", "ys")] <- points[which(obs_hz$station[i] == points$Name), 2:3]
    obs_hz[i , c("xt", "yt")] <- points[which(obs_hz$obs.point[i] == points$Name), 2:3]
  }

  obs.hz.list <- split(obs_hz, obs_hz$station)
  for(i in 1:length(obs.hz.list)){
    obs.hz.list[[i]]$hz0 <- rep(hz0[i], dim(obs.hz.list[[i]])[1])
    obs.hz.list[[i]]$scent_station <- rep(scent_station[i], dim(obs.hz.list[[i]])[1])
    obs.hz.list[[i]]$scent_target <- rep(scent_target[i], dim(obs.hz.list[[i]])[1])
    obs.hz.list[[i]]$standard_hz <- rep(standard_hz[i], dim(obs.hz.list[[i]])[1])
    obs.hz.list[[i]]$seed <- rep(seed[i], dim(obs.hz.list[[i]])[1])
  }

  meas.hz.list <- as.list(rep(NA, length(obs.hz.list)))
  if(!is.null(seed)){
    for(i in 1:length(obs.hz.list)){
      meas.hz.list[[i]] <- t(apply(obs.hz.list[[i]], 1, function(x) hz(st_coords = as.numeric(x[c(3:4)]), target_coords = as.numeric(x[c(5:6)]), hz0 = as.numeric(x[7]), scent = as.numeric(x[8]), scent_target = as.numeric(x[9]), standard_hz = as.numeric(x[10]), type = "dms", seed = as.numeric(x[11]), axises = c("North", "East"))))
    }
  }else{
    for(i in 1:length(obs.hz.list)){
      meas.hz.list[[i]] <- t(apply(obs.hz.list[[i]], 1, function(x) hz(st_coords = as.numeric(x[c(3:4)]), target_coords = as.numeric(x[c(5:6)]), hz0 = as.numeric(x[7]), scent = as.numeric(x[8]), scent_target = as.numeric(x[9]), standard_hz = as.numeric(x[10]), type = "dms", axises = c("North", "East"))))
    }
  }

  if(red){
    meas.hz.list <-  do.call(rbind, meas.hz.list) %>% as.data.frame() %>% mutate(dec = deg + min/60 + sec/3600) %>% select(dec)
    hz_meas <- cbind(do.call(rbind, obs.hz.list), meas.hz.list) %>% group_by(station) %>% mutate(dec.red = dec - dec[1])
    hz_meas$dec.red[which(hz_meas$dec.red < 0)] <- hz_meas$dec.red[which(hz_meas$dec.red < 0)] + 360
    hz_meas <- hz_meas %>% mutate(deg = floor(dec.red), minut = floor((dec.red-deg)*60), sec = ((dec.red-deg)*60-minut)*60) %>% select(station, obs.point, deg, minut, sec)

  }else{
    hz_meas <- cbind(do.call(rbind, obs.hz.list), do.call(rbind, meas.hz.list)) %>% select(station, obs.point, deg, min, sec)
  }

  return(hz_meas)
}


obs_hz <- filter(obs_plan, type == "p") %>% select(1,2)

sim_hz_meas(obs_hz = obs_hz, points = points, hz0 = 312, red = TRUE, scent_station = 2, scent_target = 3, standard_hz = 10, seed = NULL)



obs_d <- filter(obs_plan, type == "d") %>% select(1,2)
scent_station = c(1, 1, 1, 1); scent_target = c(3, 3, 3, 3); standard_d = c(3, 3, 3, 3); seed = c(111, 222, 333, 444)

sim_d_meas <- function(obs_d, points, scent_station = c(1, 1, 1, 1), scent_target = c(1, 1, 1, 1), standard_d = c(3, 3, 3, 3), seed = c(111, 222, 333, 444)){

      if(length(scent_station) == 1) scent_station <- rep(scent_station, length(unique(obs_d$station)))
      if(length(scent_station) != length(unique(obs_d$station))) stop("scent_station must be of length either 1 or the number of stations")

      if(length(scent_target) == 1) scent_target <- rep(scent_target, length(unique(obs_d$station)))
      if(length(scent_target) != length(unique(obs_d$station))) stop("scent_target must be of length either 1 or the number of stations")

      if(length(standard_d) == 1) standard_d <- rep(standard_d, length(unique(obs_d$station)))
      if(length(standard_d) != length(unique(obs_d$station))) stop("standard_d must be of length either 1 or the number of stations")

      if(!is.null(seed)){
        if(length(seed) == 1) seed <- rep(seed, length(unique(obs_d$station)))
        if(length(seed) != length(unique(obs_d$station))) stop("seed must be of length either 1 or the number of stations")
      }else{
        seed <- rep(NULL, length(unique(obs_d$station)))
      }

      obs_d <- mutate(obs_d, xs = rep(NA, dim(obs_d)[1]), ys = rep(NA, dim(obs_d)[1]), xt = rep(NA, dim(obs_d)[1]), yt = rep(NA, dim(obs_d)[1]))
      for(i in 1:dim(obs_d)[1]){
        obs_d[i , c("xs", "ys")] <- points[which(obs_d$station[i] == points$Name), 2:3]
        obs_d[i , c("xt", "yt")] <- points[which(obs_d$obs.point[i] == points$Name), 2:3]
      }

      obs.d.list <- split(obs_d, obs_d$station)
      for(i in 1:length(obs.d.list)){
        obs.d.list[[i]]$scent_station <- rep(scent_station[i], dim(obs.d.list[[i]])[1])
        obs.d.list[[i]]$scent_target <- rep(scent_target[i], dim(obs.d.list[[i]])[1])
        obs.d.list[[i]]$standard_d <- rep(standard_d[i], dim(obs.d.list[[i]])[1])
        obs.d.list[[i]]$seed <- rep(seed[i], dim(obs.d.list[[i]])[1])
      }

      meas.d.list <- as.list(rep(NA, length(obs.d.list)))
      if(!is.null(seed)){
        for(i in 1:length(obs.d.list)){
          meas.d.list[[i]] <- data.frame(dist = (apply(obs.d.list[[i]], 1, function(x) m_dist(st_coords = as.numeric(x[c(3:4)]), target_coords = as.numeric(x[c(5:6)]), scent = as.numeric(x[7]), scent_target = as.numeric(x[8]), standard_d = as.numeric(x[9]), seed = as.numeric(x[10]), axises = c("North", "East")))))
        }
      }else{
        for(i in 1:length(obs.d.list)){
          meas.d.list[[i]] <- data.frame(dist = (apply(obs.d.list[[i]], 1, function(x) m_dist(st_coords = as.numeric(x[c(3:4)]), target_coords = as.numeric(x[c(5:6)]), scent = as.numeric(x[7]), scent_target = as.numeric(x[8]), standard_d = as.numeric(x[9]), axises = c("North", "East")))))
        }
      }

      d_meas <- cbind(do.call(rbind, obs.d.list), do.call(rbind, meas.d.list)) %>% select(station, obs.point, dist)

      return(d_meas)
  }

sim_d_meas(obs_d = obs_d, points = points, scent_station = 2, scent_target = 2, standard_d = 3, seed = NULL)


########### ZADATAK 1 ######################################################

points <- rbind(A, B, C, D, T1, T2, T3, T4) %>% as.data.frame() %>% rownames_to_column("Name") %>% rename(x = V1, y = V2)

obs_plan1 <- data.frame(station = rep(c("A", "A", "A", "B", "B", "C", "C", "C", "D", "D"), 2), obs.point = rep(c("B", "T1", "T3", "A", "T1", "D", "T2","T3", "A", "T2"), 2), type = c(rep("p", 10), rep("d", 10)) )
obs_plan2 <- data.frame(station = rep(c("A", "A", "A", "B", "B", "C", "C", "D", "D", "D"), 2), obs.point = rep(c("B", "T1", "T4", "A", "T1", "D", "T2", "A", "T2", "T4"), 2), type = c(rep("p", 10), rep("d", 10)) )


obs_d <- filter(obs_plan1, type == "d") %>% select(1,2)
sim_d_meas(obs_d = obs_d, points = points, scent_station = 2, scent_target = 2, standard_d = 3, seed = NULL)


obs_hz <- filter(obs_plan1, type == "p") %>% select(1,2)
sim_hz <- sim_hz_meas(obs_hz = obs_hz, points = points, hz0 = 312, red = TRUE, scent_station = 2, scent_target = 3, standard_hz = 10, seed = NULL)


obs_d <- filter(obs_plan2, type == "d") %>% select(1,2)
sim_d_meas(obs_d = obs_d, points = points, scent_station = 2, scent_target = 2, standard_d = 3, seed = NULL)


obs_hz <- filter(obs_plan2, type == "p") %>% select(1,2)
sim_hz <- sim_hz_meas(obs_hz = obs_hz, points = points, hz0 = 312, red = TRUE, scent_station = 2, scent_target = 3, standard_hz = 10, seed = NULL)


write.table(sim_d, "sim_d.txt", sep = " ")


getwd()


################### PROJEKTOVANJE #######################################################



