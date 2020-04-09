
ecent_coords <- function(pts){ # pts je vektor sa 4 elementa x, y, e_cent, seed.
  e_cent <- pts[3]/1000
  sigma <- matrix(c(e_cent^2, 0, 0, e_cent^2),2)
  set.seed(pts[4])
  pt_coords <- mvrnorm(1, pts[1:2], sigma)
  return(pt_coords)
}

ni3 <- function(x1, y1, x2, y2){
  ## body
  dEasting <- as.numeric(x2 - x1)
  dNorthing <- as.numeric(y2 - y1)
  atg <- ifelse(dNorthing < 0, atan(dEasting/dNorthing)*180/pi + 180, atan(dEasting/dNorthing)*180/pi)
  ang <- ifelse(atg < 0, atg + 360, atg)
  return(ang)
}


sim_st <- function(survey.net.st, red){
  prj <- st_crs(survey.net.st)
  ro <- 180/pi*3600
  st_data <- survey.net.st %>% st_drop_geometry() %>% .[1, c("x_from", "y_from", "e_cent_from", "seed_from")] %>% as.numeric()
  st_coords_e <- ecent_coords(pts = st_data)
  e_cent_from <- dist(pt1_coords = st_data[1:2], pt2_coords = st_coords_e)*1000


  target_coords_e <- survey.net.st %>% st_drop_geometry() %>% .[, c("x_to", "y_to", "e_cent_to", "seed_to")] %>%  # TODO: seed ne radi.
    apply(., 1, function(x) ecent_coords(pts = as.numeric(x))) %>% t()

  e_cent_to <- data.frame((survey.net.st %>% st_drop_geometry() %>% .[, c("x_to", "y_to")] - target_coords_e)*1000) %>%
    dplyr::rename(e_x = x_to, e_y = y_to) %>%
    dplyr::mutate(e_cent_to = sqrt(e_x^2+e_y^2)) %>%
    .$e_cent_to

  survey.net.st$e_cent_from <- e_cent_from
  survey.net.st$e_cent_to <- e_cent_to

  survey.net.st <- survey.net.st %>%
    dplyr::mutate(x_from = st_coords_e[1],
                  y_from = st_coords_e[2],
                  x_to = target_coords_e[, 1],
                  y_to = target_coords_e[, 2],
                  d = sqrt((x_from-x_to)^2+(y_from-y_to)^2),
                  sd_e_cent_from = sqrt((e_cent_from^2*ro^2)/(2*(d*1000)^2)),
                  sd_e_cent_to = sqrt((e_cent_to^2*ro^2)/(2*(d*1000)^2)),
                  sd_e_cent_dist_from = sqrt(e_cent_from^2/2),
                  sd_e_cent_dist_to = sqrt(e_cent_to^2/2),
                  ni1 = ni3(x_from, y_from, x_to, y_to),
                  z = 360-Hz0,
                  Hz_sim = ifelse(ni1 >= Hz0, z+ni1-360, z+ni1),
                  sd_hz_sq = (e_air^2 + e_focus^2 + sd_Hz^2) + sd_e_cent_from^2 + sd_e_cent_to^2,
                  sd_dist_sq = sd_e_cent_dist_from^2 + sd_e_cent_dist_to^2 + sd_dist^2)

  if(red == TRUE){
    survey.net.st <- survey.net.st %>%
      # dplyr::mutate_at(., .vars = "Hz_sim", ~Hz_sim - Hz_sim[1]) %>%
      dplyr::mutate_at(., .vars = c("sd_hz_sq", "sd_dist_sq"), ~replace(., is.na(.), 0)) %>%
      dplyr::mutate(Hz_sim = ifelse(Hz_sim < 0, Hz_sim + 360, Hz_sim),
                    Hz_sim = Hz_sim - Hz_sim[1],
                    Hz_sim = ifelse(Hz_sim < 0, Hz_sim + 360, Hz_sim), # dplyr::case_when(Hz_sim < 0 ~ Hz_sim + 360, Hz_sim >= 0 ~ Hz_sim),
                    sd_hz_sq = ifelse(is.na(sd_hz_sq), NA, sd_hz_sq + sd_hz_sq[1]),
                    sd_hz_sample = rnorm(dim(survey.net.st)[1], 0, sqrt(sd_hz_sq)),
                    Hz_sim = Hz_sim + sd_hz_sample/3600,
                    Hz = ifelse(round(Hz_sim, 0) == 0, round(Hz_sim, 0), Hz_sim),
                    Hz = pmax(Hz, 0),
                    Hz = ifelse(is.na(sd_Hz), NA, Hz),  #sa ifelse se moze i ostalo resiti da ne stavlja 0 tamo za sd_hz_sq,
                    HzD = floor(Hz), HzM = floor((Hz-HzD)*60), HzS = round(((Hz-HzD)*60-HzM)*60, 1),
                    sd_dist_sample = rnorm(dim(survey.net.st)[1], 0, sqrt(sd_dist_sq)),
                    dist_sim = d + sd_dist_sample/1000,
                    HD = round(dist_sim, 4),
                    HD = ifelse(is.na(sd_dist), NA, HD)) %>%
      as.data.frame() %>%
      dplyr::arrange(ID, Hz) %>%
      st_sf(., crs = prj)
  }else{
    survey.net.st <- survey.net.st %>%
      dplyr::mutate(Hz_sim = ifelse(Hz_sim < 0, Hz_sim + 360, Hz_sim),
                    sd_hz_sample = rnorm(dim(survey.net.st)[1], 0, sqrt(sd_hz_sq)),
                    Hz_sim = Hz_sim + sd_hz_sample/3600,
                    Hz = pmax(Hz, 0),
                    HzD = floor(Hz), HzM = floor((Hz-HzD)*60), HzS = round(((Hz-HzD)*60-HzM)*60, 1),
                    sd_dist_sample = rnorm(dim(survey.net.st)[1], 0, sqrt(sd_dist_sq)),
                    dist_sim = d + sd_dist_sample/1000,
                    HD = round(dist_sim, 4),
                    HD = ifelse(is.na(sd_dist), NA, HD)) %>%
      as.data.frame() %>%
      dplyr::arrange(ID, Hz) %>%
      st_sf(., crs = prj)
  }
  return(survey.net.st)
}



sim_snetobs <- function(survey.net, red = TRUE, Hz0 = NA, seed = NULL, type = list("dms", "dec", "rad")){

  survey.net[[2]] <- survey.net[[2]] %>% dplyr::mutate_at(., .vars = c("e_cent_from", "e_cent_to", "e_focus", "e_air"), ~replace(., is.na(.), 0))

  if(!is.na(Hz0)){
    if(length(Hz0) == 1) Hz0 <- rep(Hz0, length(unique(survey.net[[2]]$from)))
    if(length(Hz0) != length(unique(survey.net[[2]]$from))) stop("Hz0 must be of length either 1 or number of stations")
  }else{
    Hz0 <- sample(1:359, size = length(unique(survey.net[[2]]$from)))
  }


  if(!is.null(seed)){
    if(length(seed) == 1) seed <- rep(seed, length(unique(survey.net[[2]]$from)))
    if(length(seed) != length(unique(survey.net[[2]]$from))) stop("seed must be of length either 1 or the number of stations")
  }else{
    seed <- runif(length(unique(survey.net[[2]]$from)),0,100)#
  }

  nto <- survey.net[[2]] %>% st_drop_geometry() %>% dplyr::mutate(from = as_factor(from)) %>% group_by(from) %>% summarize(n = n())
  survey.net[[2]] <- survey.net[[2]] %>%
    group_by(from) %>%
    mutate(id = row_number()) %>% ungroup() %>%
    dplyr::mutate(Hz0 = rep(Hz0, nto$n),
                  Hz_sim = NA,
                  dist_sim = NA,
                  seed_from = rep(seed, nto$n),
                  seed_to = seed_from + id)

  survey.net[[2]] <- survey.net[[2]] %>% split(., f = factor(.$from)) %>% lapply(., function(x) sim_st(survey.net.st = x, red = red)) %>% do.call(rbind,.) %>% dplyr::arrange(ID)

  return(survey.net)
}

#hz = mreza_sim$observations$Hz[2]; sd_Hz = mreza_sim$observations$sd_Hz[2]; e_colim = 15; sd_e_colim = 0.5; ng = 1; faces = "f1f2"

sim_face_hz <- function(hz, sd_Hz, e_colim, sd_e_colim, ng, faces = list("mean", "f1f2"), sd_sd_Hz, seed = NULL, hz_of = TRUE){
  faces = faces[1]
  if(faces == "f1f2"){
    set.seed(seed)
    hz_f1 <- hz - (rnorm(1, e_colim, sd_e_colim))/3600 + rnorm(1, 0, sd_Hz)*sqrt(2)/3600
    if(hz_f1 < 0){hz_f1 + 360}
    if(hz_f1 >= 180){
      set.seed(seed)
      hz_f2 <- (hz_f1 - 180) + (rnorm(1, e_colim, sd_e_colim))/3600 + rnorm(1, 0, sd_Hz)*sqrt(2)/3600
    }else{
      set.seed(seed)
      hz_f2 <- (hz_f1 + 180) + (rnorm(1, e_colim, sd_e_colim))/3600 + rnorm(1, 0, sd_Hz)*sqrt(2)/3600
    }
    hz <- data.frame(hz_f1 = ifelse(hz_f1 < 0, hz_f1 + 360, hz_f1) , hz_f2 = ifelse(hz_f2 < 0, hz_f2 + 360, hz_f2))
  }
  return(hz)
}

#a <- sim_face_hz(hz = mreza_sim$observations$Hz[1], sd_Hz = mreza_sim$observations$sd_Hz[1], e_colim = 15, sd_e_colim = 0.5, ng = 1, faces = "f1f2")
#a
#(a[2]+180-a[1])*3600



