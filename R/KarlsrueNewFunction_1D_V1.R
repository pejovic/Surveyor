#========================================================= Karlsrue ========================================================================
   
kr1D_deform_snet <- function (snet_i_path, snet_j_path, snet_ij_path, Path.export, Object, Comment, dim_type = list("1D", "2D"), sd.apriori, prob = 0.95, wdh_model = list("n_dh", "sd_dh", "d_dh", "E"),maxiter = 1, result.units = list("mm", "cm", "m")) {

sink(file = Path.export)
  cat("============================================================================")
  cat("\n", "DEFORMATION ANALYSIS USING KARLSRUHE METHOD","\n", "Object:",Object,"\n","Comment:", Comment, "\n","Surveyor Package","\n", "Vladan Boskovic, dipl. inz. geod.","\n")
  cat(" Time:",as.character(Sys.time()),"\n")
  cat("============================================================================","\n")

  # Packages:     
#=========================================================

library(dplyr)
library(magrittr)
library(tidyverse)
library(knitr)
library(sf)

snet_i <- read_surveynet(file = snet_i_path)
snet_j <- read_surveynet(file = snet_j_path)


# Funkcija: combine_input
#=========================================================

combine_input <- function(snet_i, snet_j, file_path = ""){
  snet_i <- read_surveynet(file = snet_i_path)
  snet_j <- read_surveynet(file = snet_j_path)
  
  #TODO: Prvo treba proveriti da li su svi reperi isti u obe epohe
  #TODO:
  
  snet_epoch_names <- function(snet, suffix){
    point_objects <- snet$points$Name[snet$points$Point_object]
    
    snet$points$Name[snet$points$Name %in% point_objects] <- paste(snet$points$Name[snet$points$Name %in% point_objects], suffix, sep = "")
    snet$observations$from[snet$observations$from %in% point_objects] <- paste(snet$observations$from[snet$observations$from %in% point_objects], suffix, sep = "")
    snet$observations$to[snet$observations$to %in% point_objects] <- paste(snet$observations$to[snet$observations$to %in% point_objects], suffix, sep = "")
    return(snet)
  }
  
  snet_i <- snet_epoch_names(snet_i, suffix = "_i")
  snet_j <- snet_epoch_names(snet_j, suffix = "_j")
  
  snet_obs_all <- rbind(snet_i$observations, snet_j$observations) %>% dplyr::select(from:e_air)
  
  if(any(class(snet_obs_all) == "sf")){
    snet_obs_all <- snet_obs_all %>% sf::st_drop_geometry()
  }
  
  snet_points_all <- rbind(snet_i$points, snet_j$points) %>% dplyr::distinct(Name, .keep_all = TRUE)
  
  if(any(class(snet_points_all) == "sf")){
    snet_points_all <- snet_points_all %>% sf::st_drop_geometry()
  }
  
  writexl::write_xlsx(list(Points = snet_points_all, Observations = snet_obs_all), path = file_path)
}

snet_ij <-combine_input(snet_i = snet_i_path ,snet_j = snet_j_path, file_path = snet_ij_path)
snet_ij <- read_surveynet(file = snet_ij_path)


# Provera stabilnosti uslovno stabilnih tacaka
#=========================================================
cat("\n","ADJUSTMENT GEODETIC 1D NETWORK","\n")

snet_i_adj <- adjust.snet(survey.net=snet_i ,dim_type = dim_type, sd.apriori=sd.apriori, prob = prob, wdh_model = wdh_model, maxiter = maxiter, result.units = result.units) 
cat("\n","Adjustment geodetic network - Epohe 0","\n", snet_i_adj$Summary$`Test decision`)
snet_j_adj <- adjust.snet(survey.net=snet_j ,dim_type = dim_type, sd.apriori=sd.apriori, prob = prob, wdh_model = wdh_model, maxiter = maxiter, result.units = result.units)
cat("\n","Adjustment geodetic network - Epohe i", "\n", snet_j_adj$Summary$`Test decision`)

## Provera homogenosti epoha merenja:

sig0_i <- snet_i_adj$Summary$`sigma aposteriori`
sig0_j <- snet_j_adj$Summary$`sigma aposteriori`
f_i <- snet_i_adj$Summary$`Degrees of freedom`
f_j <- snet_j_adj$Summary$`Degrees of freedom`
if (sig0_i > sig0_j) {
  test <- sig0_i^2/sig0_j^2
  f_test = qf(p = snet_i_adj$Summary$`Testing Probability`, f_i, f_j)
}else {
  test <- sig0_j^2/sig0_i^2
  f_test = qf(p = snet_i_adj$Summary$`Testing Probability`, f_j, f_i)
}
if (test < f_test) {
  sig0_ij <- sqrt((sig0_i^2*f_i+sig0_j^2*f_j)/(f_i+f_j))
  cat("\n","F test:",round(test,3),"\n","F quantile:",round(f_test,3),"\n","Epohe are same accuracy","\n","sig0_ij:",round(sig0_ij,3),"\n","\n")
}else {
  sig0_ij <-max(sig0_j,sig0_j)
  cat("\n","F test:",round(test,3),"\n","F quantile:",round(f_test,3),"\n","Epohe are not same accuracy","\n","sig0_ij:",round(sig0_ij,3),"\n","\n")
}

cat("\n","\n","DEFORMATION ANALYSIS GEODETIC 1D NETWORK","\n")

## Provera stabilnosti uslovno stabilnih tacaka mreze:
iterr <- 1
tacke_om <- snet_i$points$Name[!snet_i$points$Point_object]
tacke_ob <- snet_i$points$Name[snet_i$points$Point_object]
cat("\n","ITERATION:",iterr)
cat("\n","PointNetwork:",tacke_om,"\n")
cat(" PointObject:",tacke_ob,"\n","\n")


snet_ij_adj <- adjust.snet(survey.net=snet_ij ,dim_type = dim_type, sd.apriori=sd.apriori, prob = prob, wdh_model = wdh_model, maxiter = maxiter, result.units = result.units)
cat("\n","Combine adjustment geodetic networks","\n",snet_ij_adj$Summary$`Test decision`)

omega_b <- (snet_i_adj$Summary$`sigma aposteriori`^2*snet_i_adj$Summary$`Degrees of freedom`) + (snet_j_adj$Summary$`sigma aposteriori`^2*snet_j_adj$Summary$`Degrees of freedom`)
f_b <- snet_i_adj$Summary$`Degrees of freedom`+ snet_j_adj$Summary$`Degrees of freedom`
d <- 1 # defekt mreze
n <- 1 # dimenzija mreze
k <- 2 # broj epoha
npo <- length(tacke_om)
f_h <- (k-1)*n*npo-d
omega_z <- snet_ij_adj$Summary$`sigma aposteriori`^2*snet_ij_adj$Summary$`Degrees of freedom`

F.test <- ((omega_z-omega_b)/f_h)/(omega_b/f_b)
F.quantile = qf(p = snet_ij_adj$Summary$`Testing Probability`, f_h, f_b)
cat("\n", "F test:",round(F.test,3),"\n","F quantile:",round(F.quantile,3))

if (F.test < F.quantile & length(tacke_ob)==0) {
 
   note <- "Conditionally stable points of control network are really stable"; cat("\n",note,"\n","\n")
   snet_ij <- snet_ij
  
  SumPointNetwork <- data.frame(matrix(0, ncol = 3, nrow =length(tacke_om)))
  names_SumPointNetwork <- c("Point_Network","dh[mm]","Stable")
  names(SumPointNetwork) <- names_SumPointNetwork
  SumPointNetwork$`Point_Network` <- snet_ij$points$Name[snet_ij$points$Point_object==FALSE]
  SumPointNetwork$dh <- round(SumPointNetwork$dh,2)
  SumPointNetwork$`Stable` <- "Yes"
  print(SumPointNetwork)

} else if (F.test < F.quantile) {
  note <- "Conditionally stable points of control network are really stable"; cat("\n",note,"\n","\n")
  snet_ij <- snet_ij
  
  SumPointNetwork <- data.frame(matrix(0, ncol = 3, nrow =length(snet_ij$points$Point_object[snet_ij$points$Point_object==FALSE])))
  names_SumPointNetwork <- c("Point_Network","dh","Stable")
  names(SumPointNetwork) <- names_SumPointNetwork
  SumPointNetwork$`Point_Network` <- snet_ij$points$Name[snet_ij$points$Point_object==FALSE]
  SumPointNetwork$dh <- round(SumPointNetwork$dh,2)
  SumPointNetwork$`Stable` <- "Yes"
                             
  # Lokalizacija deformacija
  
  ## Matrica Bt:
  matBt <- data.frame(matrix(0, nrow =length(snet_ij$points$Point_object[snet_ij$points$Point_object==TRUE])/2, ncol =dim(snet_ij$points)[1]))
  names_matBt <- c(snet_ij$points$Name)
  names(matBt) <- names_matBt
   if (length(tacke_ob)==1) {
    matBt[,grep("_i",names_matBt)] <- 1
    matBt[,grep("_j",names_matBt)] <- -1
    matBt <- as.matrix(matBt)
   }else{
    diag(matBt[,grep("_i",names_matBt)]) <- 1
    diag(matBt[,grep("_j",names_matBt)]) <- -1
    matBt <- as.matrix(matBt)
   }
  matBt
  
  ## Vektor d:
  vecd <- c(1:dim(matBt)[1])
  names_vecd <- c(snet_i$points$Name[snet_i$points$Point_object==TRUE])
  vecd_i <- snet_ij_adj$Points$dh[grep("_i",snet_ij_adj$Points$Name)]
  vecd_j <- snet_ij_adj$Points$dh[grep("_j",snet_ij_adj$Points$Name)]
  vecd <- vecd_j - vecd_i
  names(vecd) <- names_vecd
  vecd
  
  ## Matrica Qd:
  matQd <- (matBt %*% snet_ij_adj$Matrices$Qx) %*% t(matBt)
  Qd_diag <- as.matrix(c(diag(matQd)))
  
  ## Provera stabilnosti tacaka objekta:
  SumPointObject <- data.frame(matrix(0, ncol = 5, nrow = length(vecd)))
  names_SumPointObject <- c("Point_Object", "T", "F", "dh", "Stable")
  names(SumPointObject) <- names_SumPointObject
  SumPointObject
  for (i1 in 1:dim(SumPointObject)[1]) {
    SumPointObject[i1,1] <- names_vecd[i1]
    SumPointObject[i1,2] <- round((vecd[i1]%*%solve(Qd_diag[i1])%*%vecd[i1])/(n*(sig0_ij^2)),3)
    SumPointObject[i1,3] <- round(qf(p = snet_ij_adj$Summary$`Testing Probability`, 2, f_b),3)
    SumPointObject[i1,4] <- round(vecd[i1],2)
     if (SumPointObject[i1,2] < SumPointObject[i1,3]) {
      SumPointObject[i1,5] <- "Yes"
    } else {
      SumPointObject[i1,5] <- "No"
    }
    
  }
  SumPointObject[,2] = formatC(SumPointObject[,2],digits = 3,format = "f")
  SumPointObject[,3] = formatC(SumPointObject[,3],digits = 3,format = "f")
  SumPointObject[,4] = formatC(SumPointObject[,4],digits = 2,format = "f")
  cat("\n","Points of geodetic network")
  print(kableExtra::kbl(SumPointNetwork,format = "rst",col.names = c("Point network","dh","Stable"),align = "c"))
  cat("\n","Points of object")
  print(kableExtra::kbl(SumPointObject,format = "rst",col.names = c("Point object", "T", "F", "dh[mm]", "Stable"),align = "c"))
  
  
}  else {
  note <- "Conditionally stable points of control network are not really stable"; cat("\n",note,"\n","\n")
  
  ## Provera stabilnosti uslovno stabilnih tacaka ...iterativni postupak
  iterr <- iterr+1
  indicator <- 1
  while (indicator==1) {
    
    snet_ij_new <- rep(list(NA),length(tacke_om))
    snet_ij_adj_new <- rep(list(NA),length(tacke_om))
    omega_i <- c()
    
    for (i in 1:length(tacke_om)) {
      cat("Iteration:",i,"  ")
      
      snet_ij_new[[i]] <- snet_ij
      snet_ij_new[[i]]$points <- dplyr::filter(snet_ij_new[[i]]$points, Name!=(tacke_om[i]))
      snet_ij_new[[i]]$observations <- dplyr::filter(snet_ij_new[[i]]$observations, from!= tacke_om[i] & to!=tacke_om[i])
      
      
      snet_ij_adj_new[[i]] <- adjust.snet(survey.net = snet_ij_new[[i]],dim_type = dim_type, sd.apriori=sd.apriori, prob = prob, wdh_model = wdh_model, maxiter = maxiter, result.units = result.units)
      snet_ij_adj_new[[i]]$Tacka <- tacke_om[i]
      snet_ij_adj_new[[i]]$Omega <- snet_ij_adj_new[[i]]$Summary$`sigma aposteriori`^2*snet_ij_adj_new[[i]]$Summary$`Degrees of freedom`
      
      omega_i[i] <- snet_ij_adj_new[[i]]$Omega
      omega_i_min <- min(omega_i)
      poz <- match(omega_i,omega_i_min)
      tacka_min <- tacke_om[poz!="NA"][complete.cases(tacke_om[poz!="NA"])]
      cat(" Omega:",formatC(snet_ij_adj_new[[i]]$Omega,digits = 4,format = "f"),"  ", " Point:",snet_ij_adj_new[[i]]$Tacka,"\n")
    }
    
    cat("\n"," Unstable point:",tacka_min,"\n","\n")
    
    snet_i$points$Point_object[snet_i$points$Name==tacka_min] <- TRUE
    snet_i$observations <- snet_i$observations %>% dplyr::select(from:e_air)
    writexl::write_xlsx(list(Points = snet_i$points, Observations = snet_i$observations), path = snet_i_path)
    
    snet_j$points$Point_object[snet_j$points$Name==tacka_min] <- TRUE
    snet_j$observations <- snet_j$observations %>% dplyr::select(from:e_air)
    writexl::write_xlsx(list(Points = snet_j$points, Observations = snet_j$observations), path = snet_j_path)
    
    tacke_om <- snet_i$points$Name[!snet_i$points$Point_object]
    tacke_ob <- snet_i$points$Name[snet_i$points$Point_object]
    cat("\n","ITERATION:",iterr)
    cat("\n","PointNetwork:",tacke_om,"\n")
    cat(" PointObject:",tacke_ob,"\n","\n")
    
    snet_ij <- combine_input(snet_i=snet_i_path, snet_j=snet_j_path, file_path= snet_ij_path)
    snet_ij <- read_surveynet(file = snet_ij_path)
    snet_ij_adj <- adjust.snet(survey.net=snet_ij ,dim_type = dim_type, sd.apriori=sd.apriori, prob = prob, wdh_model = wdh_model, maxiter = maxiter, result.units = result.units)
    cat("\n","Combine adjustment geodetic networks","\n", snet_ij_adj$Summary$`Test decision`)
    
    omega_z <- snet_ij_adj$Summary$`sigma aposteriori`^2*snet_ij_adj$Summary$`Degrees of freedom`
    npo <- length(tacke_om)
    f_h <- (k-1)*n*npo-d
    
    F.test <- ((omega_z-omega_b)/f_h)/(omega_b/f_b)
    F.quantile = qf(p = snet_ij_adj$Summary$`Testing Probability`, f_h, f_b)
    cat("\n", "F test:",round(F.test,3),"\n","F quantile:",round(F.quantile,3))
    
    
    if (F.test < F.quantile) {
      note <- "Conditionally stable points of control network are really stable"; cat("\n",note,"\n","\n")
      indicator = 2
      snet_ij <- snet_ij
      
      SumPointNetwork <- data.frame(matrix(0, ncol = 3, nrow =length(snet_ij$points$Point_object[snet_ij$points$Point_object==FALSE])))
      names_SumPointNetwork <- c("Point_Network","dh","Stable")
      names(SumPointNetwork) <- names_SumPointNetwork
      SumPointNetwork$`Point_Network` <- snet_ij$points$Name[snet_ij$points$Point_object==FALSE]
      SumPointNetwork$dh <- round(SumPointNetwork$dh,2)
      SumPointNetwork$`Stable` <- "Yes"
      
      # Lokalizacija deformacija
      
      ## Matrica Bt:
      matBt <- data.frame(matrix(0, nrow =length(snet_ij$points$Point_object[snet_ij$points$Point_object==TRUE])/2, ncol =dim(snet_ij$points)[1]))
      names_matBt <- c(snet_ij$points$Name)
      names(matBt) <- names_matBt
      if (length(tacke_ob)==1) {
        matBt[,grep("_i",names_matBt)] <- 1
        matBt[,grep("_j",names_matBt)] <- -1
        matBt <- as.matrix(matBt)
      }else{
        diag(matBt[,grep("_i",names_matBt)]) <- 1
        diag(matBt[,grep("_j",names_matBt)]) <- -1
        matBt <- as.matrix(matBt)
      }
      matBt
     
      
      ## Vektor d:
      vecd <- c(1:dim(matBt)[1])
      names_vecd <- c(snet_i$points$Name[snet_i$points$Point_object==TRUE])
      vecd_i <- snet_ij_adj$Points$dh[grep("_i",snet_ij_adj$Points$Name)]
      vecd_j <- snet_ij_adj$Points$dh[grep("_j",snet_ij_adj$Points$Name)]
      vecd <- vecd_j - vecd_i
      names(vecd) <- names_vecd
      vecd
      
      ## Matrica Qd:
      matQd <- (matBt %*% snet_ij_adj$Matrices$Qx) %*% t(matBt)
      Qd_diag <- as.matrix(c(diag(matQd)))
      
      ## Provera stabilnosti tacaka objekta:
      SumPointObject <- data.frame(matrix(0, ncol = 5, nrow = length(vecd)))
      names_SumPointObject <- c("Point_Object", "T", "F", "dh", "Stable")
      names(SumPointObject) <- names_SumPointObject
      SumPointObject
      for (i1 in 1:dim(SumPointObject)[1]) {
        SumPointObject[i1,1] <- names_vecd[i1]
        SumPointObject[i1,2] <- round((vecd[i1]%*%solve(Qd_diag[i1])%*%vecd[i1])/(n*(sig0_ij^2)),3)
        SumPointObject[i1,3] <- round(qf(p = snet_ij_adj$Summary$`Testing Probability`, 2, f_b),3)
        SumPointObject[i1,4] <- round(vecd[i1],2)
        if (SumPointObject[i1,2] < SumPointObject[i1,3]) {
          SumPointObject[i1,5] <- "Yes"
        } else {
          SumPointObject[i1,5] <- "No"
        }
        
      }
      SumPointObject[,2] = formatC(SumPointObject[,2],digits = 3,format = "f")
      SumPointObject[,3] = formatC(SumPointObject[,3],digits = 3,format = "f")
      SumPointObject[,4] = formatC(SumPointObject[,4],digits = 2,format = "f")
      cat("\n","Points of geodetic network")
      print(kableExtra::kbl(SumPointNetwork,format = "rst",col.names = c("Point network","dh[mm]","Stable"),align = "c"))
      cat("\n","Points of object")
      print(kableExtra::kbl(SumPointObject,format = "rst",col.names = c("Point object", "T", "F", "dh[mm]", "Stable"),align = "c"))
      
      
    }else{
      note <- "Conditionally stable points of control network are not really stable"; cat("\n",note,"\n","\n")
      tacke_om <- snet_i$points$Name[!snet_i$points$Point_object]
      tacke_ob <- snet_i$points$Name[snet_i$points$Point_object]
      iterr=iterr+1
    }
   }
 }
sink(NULL)
}

  
kr1D_deform_snet(snet_i_path = "D:/R PROGRAMIRANJE/Vladan_programi_R/Deformation analysis geodetic network/Karlsrue/Zbirka/zbirka_0.xlsx", 
                 snet_j_path = "D:/R PROGRAMIRANJE/Vladan_programi_R/Deformation analysis geodetic network/Karlsrue/Zbirka/zbirka_1.xlsx",
                 snet_ij_path = "D:/R PROGRAMIRANJE/Vladan_programi_R/Deformation analysis geodetic network/Karlsrue/Zbirka/zbirka_01.xlsx",
                 Path.export = "D:/R PROGRAMIRANJE/Vladan_programi_R/Deformation analysis geodetic network/Karlsrue/Zbirka/zbirka_01.txt",
                 Object = "Zbirka",
                 Comment = "Deformaciona analiza",
                 dim_type="1D",sd.apriori = 1, wdh_model = "n_dh",result.units = "mm",prob = 0.95)

  




  






































































