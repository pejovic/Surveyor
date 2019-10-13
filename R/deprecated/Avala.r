
setwd("C:/Users/User/Dropbox/Merenja/Avala")
library(tidyverse)
library(readr)
library(readxl)
library(purrr)
library(tidyr)
library(writexl)

dms2dec <- function(x){
  x %>% str_replace_all(., "([°'])", "_") %>% 
    str_replace_all(., '(["])', "") %>% 
    as_tibble() %>% 
    separate(value, "_", into = c("D", "M", "S")) %>%
    mutate_all(as.numeric) %>%
    transmute(value = D + M/60 + S/3600) %>%
    .$value
}

dirmean <- function(x){
  temp <- x %>% group_by(Face) %>% mutate(ID = row_number()) %>% 
    ungroup() %>% 
    dplyr::select(ID, From, To, Hz, Vz, SD, Face) %>% 
    split(., f = as.factor(.$Face))
  if(length(temp) == 1){
    temp <- temp[[1]] %>% dplyr::select(From, To, Hz, Vz, SD)
    return(temp)
  }else{
    if(dim(temp[[1]])[1] != dim(temp[[2]])[1]) stop("There is unequal measurements in different faces of instrument")
    temp <- left_join(dplyr::select(temp[[1]], ID, From, To,  Hz1 = Hz, Vz1 = Vz, SD1 = SD), select(temp[[2]], ID, Hz2 = Hz, Vz2 = Vz, SD2 = SD), by = "ID") %>%
      dplyr::mutate(doubleC = ifelse(Hz2 >= 180, (Hz2-180-Hz1), (Hz2+180-Hz1)), Hz = Hz1 + doubleC/2, Vz = (Vz1 - Vz2 + 360)/2, SD = (SD1 + SD2)/2) %>%
      dplyr::select(From, To, Hz, Vz, SD) %>% dplyr::summarize(From = From[1], To = To[1], Hz = mean(Hz), Vz = mean(Vz), SD = mean(SD, na.rm = TRUE))  
    return(temp)
  }
}


merenja <- readxl::read_xlsx("merenja.xlsx")
merenja <- map_at(merenja, .at = c("Hz", "Vz", "H. Angle", "V. Angle"), dms2dec) %>% as_tibble() %>% select(From, To, Hz, Vz, SD, Face, `Instrument Height`, `Target Height`, `Prism Constant`)
stations <- merenja %>% split(., f = as.factor(.$From)) %>% map(~split(., f = as.factor(.$To))) 

for(i in 1:length(stations)){
  stations[[i]] <- stations[[i]] %>% map(., dirmean) %>% do.call(rbind, .) %>% arrange(Hz)
}

stations <- do.call(rbind, stations)

stations <- stations %>% mutate(HzD = floor(Hz), HzM = floor((Hz-HzD)*60), HzS = ((Hz-HzD)*60-HzM)*60, VzD = floor(Vz), VzM = floor((Vz-VzD)*60), VzS = ((Vz-VzD)*60-VzM)*60) %>%
  dplyr::select(From, To, HzD, HzM, HzS, SD, VzD, VzM, VzS)

write_xlsx(stations, path = "merenja_Avala.xlsx")








merenja_mreza <- merenja %>% filter(To %in% c("S1", "S2", "S3", "S4", "S5", "S6", "S7"))



write.csv(merenja_mreza, file = "merenja_mreza.csv")


##### Kosinusna teorema ##################
cost<-function(a,b,alfa){
  c<-sqrt((a^2)+(b^2)-(2*a*b*cos(alfa)))
  return(c)
}
###########################################

###### computin direction #################
ni<-function(pt1,pt2){
  #options(digits=2)
  x1<-pt1[2]
  y1<-pt1[1]
  x2<-pt2[2]
  y2<-pt2[1]
  dx<-as.numeric(x2-x1)
  dy<-as.numeric(y2-y1)
  atg<-ifelse(dx<0,atan(dy/dx)*180/pi+180,atan(dy/dx)*180/pi)
  direct<-ifelse(atg<0,atg+360,atg)
  deg<-floor(direct); minut<-floor((direct-deg)*60); sec<-((direct-deg)*60-minut)*60
  direkcioni<-c(deg,minut,sec)
  names(direkcioni)<-c("deg","min","sec")
  direkcioni<-as.numeric(direkcioni[1]+direkcioni[2]/60+direkcioni[3]/3600)*pi/180
  return(direkcioni)
}
###########################################

######### base ############################
disto<-function(pt1,pt2){
  #options(digits=6)
  x1<-pt1[2]
  y1<-pt1[1]
  x2<-pt2[2]
  y2<-pt2[1]
  dx<-as.numeric(x2-x1)
  dy<-as.numeric(y2-y1)
  d<-sqrt(dx^2+dy^2)
  #names(d)<-"duzina"
  return(d)
}
#############################################


######### Presecanje nazad ################

back_resection<-function(hdistl,hdistd,dir1,dir2,pr1,pr2,contd=0.01){
  base<-disto(pr1,pr2)
  left_angle<-acos((base^2+hdistl^2-hdistd^2)/(2*base*hdistl))
  ni1<-ni(pr1,pr2)
  Az_left<-ni1+left_angle
  x<-pr1[2]+hdistl*cos(Az_left)
  y<-pr1[1]+hdistl*sin(Az_left)
  yx<-c(y,x)
  c_ni<-ni(c(y,x),pr2)
  c_d<-disto(c(y,x),pr2)
  if((c_d-hdistd)>contd){return(paste("koordinate su van dozvoljenih granica"))}else
  {
    return(yx)
  }
}

####################################################################
