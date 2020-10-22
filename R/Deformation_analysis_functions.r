

#' @title f-test
#' @description f-test
#' @param sd1 First standard deviation
#' @param sd2 Second standard deviation
#' @param df1 First degrees of freedom
#' @param df2 Second degrees of freedom
#' @param prob Probability
#' @return Test decision
#' @rdname f_test
f_test <- function(sd1, sd2, df1, df2, prob){
  if(sd1 > sd2){
    F.estimated <- sd1^2/sd2^2
    F.quantile <- qf(p = prob, df1 = df1, df2 = df2)
  }else{
    F.estimated <- sd2^2/sd1^2
    F.quantile <- qf(p = prob, df1 = df1, df2 = df2)
  }
  if(F.estimated < F.quantile){
    note <- TRUE
  }else{
    note <- FALSE
  }
  return(list(f_estimated = F.estimated, f_quantile = F.quantile, decision = note))
}

#' @title deform.snet
#' @description Two-epochs deformation analysis based on congruency testing
#' @param snet_1_path Path to the data of the first epoch of the measurements
#' @param snet_2_path Path to the data of the second epoch of the measurements
#' @param sd.apriori Apriori standard deviation
#' @param units Units of the results, Default: 'mm'
#' @param dim_type Dimension type of the network, Default: '1D'
#' @param prob Probability for statistical testing, Default: 0.95
#' @param wdh_model Weightening model for leveling measurements, Default: list("n_dh", "sd_dh", "d_dh", "E")
#' @return dataframe with two columns: "Name" indicating the name of the point and "stable" indicating the stability status of the point (TRUE or FALSE)
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[dplyr]{arrange}},\code{\link[dplyr]{select}}
#'  \code{\link[MASS]{ginv}}
#' @rdname snet.stable
#' @export 
#' @importFrom dplyr arrange select case_when
#' @importFrom MASS ginv
deform.snet <- function(snet_1_path, snet_2_path, sd.apriori, units = "mm",  dim_type = "1D", prob = 0.95, wdh_model = list("n_dh", "sd_dh", "d_dh", "E")){
  "%!in%" <- Negate("%in%")
  wdh_model <- wdh_model[[1]]
  units.lookup.table <- c(mm = 1000, cm = 100, m = 1)
  if(length(sd.apriori) == 1){
    sd0_i <- sd.apriori[1]
    sd0_j <- sd.apriori[1]
  }else{
    sd0_i <- sd.apriori[1]
    sd0_j <- sd.apriori[2]
  }
  #=================== Reading snets ===============================
  snet_i <- suppressWarnings(read_surveynet(file = snet_1_path))
  snet_j <- suppressWarnings(read_surveynet(file = snet_2_path))

  #=================== Checking snets ==============================
  if(!identical(snet_i$observations[, c("from", "to")], snet_j$observations[, c("from", "to")])){
    print("Observation plans are not identical in two epochs.")
  }

  used_points_i <- unique(c(snet_i$observations$from, snet_i$observations$to))
  used_points_j <- unique(c(snet_j$observations$from, snet_j$observations$to))

  if(!!any(used_points_i %!in% snet_i[[1]]$Name)){
    stop(paste("There is no coordinates for point", paste(used_points_i[which(used_points_i %!in% snet_i[[1]]$Name)], "in reference epoch"), sep = " "))
  }else{used_points_i <- snet_i[[1]]$Name
  }

  if(!!any(used_points_i %!in% snet_i[[1]]$Name)){
    stop(paste("There is no coordinates for point", paste(used_points_j[which(used_points_j %!in% snet_j[[1]]$Name)]), "in target epoch",  sep = " "))
  }else{used_points_j <- snet_j[[1]]$Name
  }


  if(!identical(used_points_i, used_points_i)){
    print("Two epochs contains different points.")
    if(length(used_points_i) > length(used_points_j)){
      print(paste("Points", used_points_i[which(used_points_i %!in% used_points_j)], "do not exist in the target epoch", sep = " "))
      used_points <- used_points_j
    }else{
      if(length(used_points_i) < length(used_points_j)){
        print(paste("Points", used_points_j$Points$Name[which(used_points_j %!in% used_points_i)], "do not exist in the reference epoch", sep = " "))
        used_points <- used_points_i
      }
      print("Only the results for common points will be processed.")
    }
  }else{
    used_points <- used_points_i
  }


  if(!identical(snet_i$Points$h, snet_j$Points$h)){
    stop(print("Approximate coordinates in two epoches must be the same"))
  }

  if(!identical(snet_i$points$Name[snet_i$points$Point_object], snet_j$points$Name[snet_j$points$Point_object])){
    stop(print("Reference points must be the same"))
  }
  snet_i[[1]] <- snet_i[[1]][match(snet_i[[1]]$Name, used_points), ] %>% dplyr::arrange(Point_object, desc = TRUE)
  snet_j[[1]] <- snet_j[[1]][match(snet_j[[1]]$Name, used_points), ] %>% dplyr::arrange(Point_object, desc = TRUE)
  #===========================================================================

  #=================== Adjustment ============================================
  snet_i_adj <- suppressMessages(adjust.snet(adjust = TRUE, survey.net = snet_i, dim_type = "1D", sd.apriori = sd0_i,  all = TRUE, prob = prob, wdh_model = wdh_model))
  snet_j_adj <- suppressMessages(adjust.snet(adjust = TRUE, survey.net = snet_j, dim_type = "1D", sd.apriori = sd0_j,  all = TRUE, prob = prob, wdh_model = wdh_model))
  if(any(c(snet_i_adj$Summary$`Test decision`, snet_j_adj$Summary$`Test decision`) != "Model is correct")) {
    return(list(snet_1_adj = snet_i_adj[1:3], snet_2_adj = snet_i_adj[1:3]))
    stop("One epoch adjustment failed")
  }
  #============================================================================
  points_i <- snet_i_adj$Points
  points_j <- snet_j_adj$Points

  points_i$Stable <- NA
  points_j$Stable <- NA

  Qx_i <- snet_i_adj$Matrices$Qx
  Qx_j <- snet_j_adj$Matrices$Qx

  #=================== Calculating reference accuracy factor ==================
  s0_i <- snet_i_adj$Summary$`sigma aposteriori`
  s0_j <- snet_j_adj$Summary$`sigma aposteriori`
  f_i <- snet_i_adj$Summary$`Degrees of freedom`
  f_j <- snet_j_adj$Summary$`Degrees of freedom`

  if(f_test(sd1 = s0_i, sd2 = s0_j, df1 = f_i, df2 = f_j, prob = prob)[[3]]){
    s0 <- sqrt((f_i*s0_i^2+f_j*s0_j^2)/(f_i + f_j))
    f0 <- f_i + f_j
  }else{
    if(s0_i > s0_j){
      s0 <- s0_i
      f0 <- f_i
      print("Two epoches are not equally precised! Computation will be continued with the worse reference standard deviation.")
    }else{
      s0 <- s0_j
      f0 <- f_j
      print("Two epoches are not equally precised! Computation will be continued with the worse reference standard deviation.")
    }
  }
  #============================================================================

  #=================== Testing whole network ==================================

  d_all <- (points_j$h - points_i$h)*units.lookup.table[units]

  Qd_all <- Qx_i + Qx_j

  Qd_all_ginv <- MASS::ginv(Qd_all)

  dd_all <- (crossprod(d_all, Qd_all) %*% d_all)

  teta_all_squared <- ((crossprod(d_all, Qd_all_ginv) %*% d_all)/qr(Qd_all)$rank)

  snet_test <- f_test(sd1 = sqrt(teta_all_squared), sd2 = s0, df1 = qr(Qd_all)$rank, df2 = f0, prob = prob)

  if(sum(points_i$Point_object) == 0){
    fix <- "all"
    if(!snet_test[[3]]){
      r_points_ind <- !points_i$Point_object
      names(r_points_ind) <- points_i$Name
      iter <- 0
      m <- sum(r_points_ind)
      while(!snet_test[[3]] & m > 1) {
        iter = iter + 1
        r_ind_matrix <- diag(sum(r_points_ind)) == 1
        teta_r <- rep(NA, sum(r_points_ind))
        for(i in 1:sum(r_points_ind)){
          drb <- d_all[r_points_ind][r_ind_matrix[i, ]]
          drf <- d_all[r_points_ind][!r_ind_matrix[i, ]]

          Qdbb  <- Qd_all_ginv[r_points_ind, r_points_ind][r_ind_matrix[i, ], r_ind_matrix[i, ]]
          Qdbf  <- Qd_all_ginv[r_points_ind, r_points_ind][r_ind_matrix[i, ], !r_ind_matrix[i, ]]
          Qdfb  <- Qd_all_ginv[r_points_ind, r_points_ind][!r_ind_matrix[i, ], r_ind_matrix[i, ]]
          Qdff  <- Qd_all_ginv[r_points_ind, r_points_ind][!r_ind_matrix[i, ], !r_ind_matrix[i, ]]

          dsb <- drb + solve(Qdbb) %*% Qdbf %*% drf
          Qsff <- Qdff - Qdfb %*% solve(Qdbb) %*% Qdbf

          dd_b <- (crossprod(dsb, Qdbb) %*% dsb)
          teta_r[i] <- dd_b
        }
        names(teta_r) <- points_i$Name[r_points_ind]
        max_teta_name <- names(teta_r[which(teta_r==max(teta_r))])
        points_i[points_i$Name == max_teta_name, "Stable"] <- FALSE
        points_j[points_j$Name == max_teta_name, "Stable"] <- FALSE

        r_points_ind[which(points_i$Stable == FALSE)] <- FALSE
        names(r_points_ind) <- points_i$Name

        d_rest <- d_all[r_points_ind]

        Qd_rest_ginv <- Qd_all_ginv[r_points_ind, r_points_ind]

        dd_rest <- (crossprod(d_rest, Qd_rest_ginv) %*% d_rest)

        teta_rest_squared <- ((crossprod(d_rest, Qd_rest_ginv) %*% d_rest)/qr(Qd_rest_ginv)$rank)

        snet_test <- f_test(sd1 = sqrt(teta_rest_squared), sd2 = s0, df1 = qr(Qd_rest_ginv)$rank, df2 = f0, prob = prob)
        m <- sum(r_points_ind)
      }
    }
  }else{
    # ================== Testing for reference points ============================

    r_points_names <- snet_i_adj$Points$Name[!snet_i_adj$Points$Point_object]
    o_points_names <- snet_i_adj$Points$Name[snet_i_adj$Points$Point_object]

    r_points_ind <- !points_i$Point_object
    names(r_points_ind) <- points_i$Name

    dr <- d_all[r_points_ind]
    names(dr) <- points_i$Name[r_points_ind]
    do <- d_all[!r_points_ind]
    names(do) <- points_i$Name[!r_points_ind]

    Qdrr <- Qd_all_ginv[r_points_ind, r_points_ind]
    Qdro  <- Qd_all_ginv[r_points_ind, !r_points_ind]
    Qdor  <- Qd_all_ginv[!r_points_ind, r_points_ind]
    Qdoo  <- Qd_all_ginv[!r_points_ind, !r_points_ind]

    dso <- do + solve(Qdoo) %*% Qdor %*% dr
    Qsrr <- Qdrr - Qdro %*% solve(Qdoo) %*% Qdor

    dd_r <- (crossprod(dr, Qsrr) %*% dr) # kvadratna forma

    teta_r_squared <- ((crossprod(dr, Qsrr) %*% dr)/qr(Qsrr)$rank)

    r_test <- f_test(sd1 = sqrt(teta_r_squared), sd2 = s0, df1 = qr(Qsrr)$rank, df2 = f0, prob = prob)

    if(!r_test[[3]]){
      iter <- 0
      m <- sum(r_points_ind)
      while(!r_test[[3]] & m > 1) {
        iter = iter + 1
        r_ind_matrix <- diag(sum(r_points_ind)) == 1
        teta_r <- rep(NA, sum(r_points_ind))
        for(i in 1:sum(r_points_ind)){
          drb <- d_all[r_points_ind][r_ind_matrix[i, ]]
          drf <- d_all[r_points_ind][!r_ind_matrix[i, ]]

          Qdbb <- Qdrr[r_ind_matrix[i, ], r_ind_matrix[i, ]]
          Qdbf  <- Qdrr[r_ind_matrix[i, ], !r_ind_matrix[i, ]]
          Qdfb  <- Qdrr[!r_ind_matrix[i, ], r_ind_matrix[i, ]]
          Qdff  <- Qdrr[!r_ind_matrix[i, ], !r_ind_matrix[i, ]]

          dsb <- drb + solve(Qdbb) %*% Qdbf %*% drf
          Qsff <- Qdff - Qdfb %*% solve(Qdbb) %*% Qdbf

          dd_b <- (crossprod(dsb, Qdbb) %*% dsb)
          teta_r[i] <- dd_b
        }
        names(teta_r) <- points_i$Name[r_points_ind]
        max_teta_name <- names(teta_r[which(teta_r==max(teta_r))])
        points_i[points_i$Name == max_teta_name, "Stable"] <- FALSE
        points_j[points_j$Name == max_teta_name, "Stable"] <- FALSE

        r_points_ind <- is.na(!points_i$Point_object & points_i$Stable)
        names(r_points_ind) <- points_i$Name

        dr <- d_all[r_points_ind]
        names(dr) <- points_i$Name[r_points_ind]
        do <- d_all[!r_points_ind]
        names(do) <- points_i$Name[!r_points_ind]

        Qdrr <- Qd_all_ginv[r_points_ind, r_points_ind]
        Qdro  <- Qd_all_ginv[r_points_ind, !r_points_ind]
        Qdor  <- Qd_all_ginv[!r_points_ind, r_points_ind]
        Qdoo  <- Qd_all_ginv[!r_points_ind, !r_points_ind]

        Qsrr <- Qdrr - Qdro %*% solve(Qdoo) %*% Qdor

        teta_r_squared <- ((crossprod(dr, Qsrr) %*% dr)/qr(Qsrr)$rank)

        r_test <- f_test(sd1 = sqrt(teta_r_squared), sd2 = s0, df1 = qr(Qsrr)$rank, df2 = f0, prob = prob)
        m <- sum(r_points_ind)
      }
    }
    #================== Testing for object points ==============================

    points_i$Stable[r_points_ind] <- TRUE
    points_i$Stable[!points_i$Stable] <- NA

    points_j$Stable[r_points_ind] <- TRUE
    points_j$Stable[!points_j$Stable] <- NA

    o_points_ind <- !r_points_ind
    names(o_points_ind) <- points_i$Name

    dso <- do + solve(Qdoo) %*% Qdor %*% dr
    teta_o_squared <- ((crossprod(dso, Qdoo) %*% dso)/qr(Qdoo)$rank)
    o_test <- f_test(sd1 = sqrt(teta_o_squared), sd2 = s0, df1 = qr(Qdoo)$rank, df2 = f0, prob = prob)

    if(!o_test[[3]]){
      iter <- 0
      m <- sum(o_points_ind)
      while(!o_test[[3]] & m > 1) {
        iter = iter + 1
        o_ind_matrix <- diag(sum(is.na(points_i$Stable))) == 1
        teta_o <- rep(NA, sum(is.na(points_i$Stable)))
        dobs <- rep(NA, sum(is.na(points_i$Stable)))
        Qdoo_o <- Qd_all_ginv[o_points_ind, o_points_ind]
        for(i in 1:sum(o_points_ind)){
          drb <- (points_j$h[is.na(points_i$Stable)][o_ind_matrix[i, ]] - points_i$h[is.na(points_i$Stable)][o_ind_matrix[i, ]])*units.lookup.table[units]
          drf <- (points_j$h[is.na(points_i$Stable)][!o_ind_matrix[i, ]] - points_i$h[is.na(points_i$Stable)][!o_ind_matrix[i, ]])*units.lookup.table[units]

          Qdbb  <- Qdoo_o[o_ind_matrix[i, ], o_ind_matrix[i, ]]
          Qdbf  <- Qdoo_o[o_ind_matrix[i, ], !o_ind_matrix[i, ]]
          Qdfb  <- Qdoo_o[!o_ind_matrix[i, ], o_ind_matrix[i, ]]
          Qdff  <- Qdoo_o[!o_ind_matrix[i, ], !o_ind_matrix[i, ]]

          dsb <- drb + solve(Qdbb) %*% Qdbf %*% drf
          Qsff <- Qdff - Qdfb %*% solve(Qdbb) %*% Qdbf

          dd_b <- (crossprod(dsb, Qdbb) %*% dsb)
          teta_o[i] <- dd_b
          dobs[i] <- drb
        }
        names(teta_o) <- points_i$Name[o_points_ind]
        names(dobs) <- points_i$Name[o_points_ind]
        max_teta_name <- names(teta_o[which(teta_o==max(teta_o))])
        max_do <- dobs[which(teta_o == max(teta_o))]
        points_i[points_i$Name == max_teta_name, "Stable"] <- FALSE
        points_j[points_j$Name == max_teta_name, "Stable"] <- FALSE

        o_points_ind <- is.na(points_i$Stable)
        names(o_points_ind) <- points_i$Name

        # Qdrr <-  Qd_all_ginv[o_points_ind, o_points_ind]
        # Qdro  <- Qd_all_ginv[o_points_ind, !o_points_ind]
        # Qdor  <- Qd_all_ginv[!o_points_ind, o_points_ind]
        # Qdoo  <- Qd_all_ginv[!o_points_ind, !o_points_ind]

        Qdrr <-  Qd_all_ginv[(!r_points_ind & o_points_ind), (!r_points_ind & o_points_ind)]
        Qdro  <- Qd_all_ginv[(!r_points_ind & !o_points_ind), (!r_points_ind & o_points_ind)]
        Qdor  <- Qd_all_ginv[(!r_points_ind & o_points_ind), (!r_points_ind & !o_points_ind)]
        Qdoo  <- Qd_all_ginv[(!r_points_ind & !o_points_ind), (!r_points_ind & !o_points_ind)]

        do <- d_all[!r_points_ind & o_points_ind]

        Qsrr <- Qdrr - Qdro %*% solve(Qdoo) %*% Qdor

        teta_o_squared <- ((crossprod(do, Qsrr) %*% do)/qr(Qsrr)$rank)
        o_test <- f_test(sd1 = sqrt(teta_o_squared), sd2 = s0, df1 = qr(Qsrr)$rank, df2 = f0, prob = prob)

        m <- sum(o_points_ind)
        if(!o_test[[3]] & m == 1){
          points_i[o_points_ind, "Stable"] <- FALSE
          points_j[o_points_ind, "Stable"] <- FALSE
        }
      }
    }

  }
  points_i$Stable[is.na(points_i$Stable)] <- TRUE
  points_j$Stable[is.na(points_j$Stable)] <- TRUE

  return(points_i %>% dplyr::select(id, Name, Stable))
}
