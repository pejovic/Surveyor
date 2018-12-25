# Project: Surveyer
# Description: Package of Land and Engineering Surveying utilities
# Authors: Milutin Pejovic, Milan Kilibarda, Branislav Bajat, Aleksandar Sekulic and Petar Bursac
#

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


surveynet.shp <- function(tacke, plan_opazanja, fix_x = list(), fix_y = list(), st_dir, st_dist, dest_crs = NA){
  for(i in names(tacke)){
    if(i == "Name"){
      tacke <- tacke %>% rename("Naziv" = "Name")
    }
  }
  for(i in names(plan_opazanja)){
    if(i == "Name"){
      plan_opazanja <- plan_opazanja %>% rename("id" = "Name")
    }
  }
  # TODO Check funkcija ide ovde
  # Transformacija u dest CRS

  if(is.na(plan_opazanja$id[[1]])){
    plan_opazanja$id <- (1:length(plan_opazanja$geometry))
  }

  # Definisanje datuma za tacke
  tacke %<>% mutate(FIX_X = FALSE, FIX_Y = FALSE)

  tacke$FIX_X[tacke$Naziv %in% fix_x] <- TRUE
  tacke$FIX_Y[tacke$Naziv %in% fix_y] <- TRUE

  # Plan opazanja - dodeljivanje novih kolona
  plan_opazanja %<>% mutate(duzina = TRUE,
                            pravac = TRUE,
                            standard_p = st_dir,
                            standard_d = st_dist,
                            od = NA,
                            do = NA)

  # Plan opazanja - definisanje naziva pocetne i krajnje tacke
  # Kreiranje data frame-a na osnovu sf plana opazanja u cilju dobijanja nazaiva pocetne i krajnje tacke
  observational_plan <- data.frame(station = NA,obs.point = NA, x_station = NA,y_station = NA,x_obs.point = NA,y_obs.point = NA)

  # Line string to multipoint
  pl_op_pocetne_tac <- st_line_sample(plan_opazanja,sample = 0)
  pl_op_krajnje_tac <- st_line_sample(plan_opazanja,sample = 1)
  koord_1 <- as.data.frame(st_coordinates(pl_op_pocetne_tac))
  koord_2 <- as.data.frame(st_coordinates(pl_op_krajnje_tac))

  # Multipoint to point
  pnts_1 = st_cast(pl_op_pocetne_tac, "POINT")
  pnts_2 = st_cast(pl_op_krajnje_tac, "POINT")
  # X- East Y - North
  observational_plan <- data.frame(station = NA,obs.point = NA, x_station = st_coordinates(pnts_1)[,1], y_station = st_coordinates(pnts_1)[,2], x_obs.point = st_coordinates(pnts_2)[,1], y_obs.point = st_coordinates(pnts_2)[,2])

  # Popunjavanje kolona Naziva stanice i opazane tacke sa vrednostima na osnovu uslova podudarnosti koordinata (dodati da je uslov podudarnost po obe koordinate)
  # TODO srediti da radi i za y
  observational_plan$station <- tacke$Naziv[match(observational_plan$x_station, st_coordinates(tacke)[,1])]
  observational_plan$obs.point <- tacke$Naziv[match(observational_plan$x_obs.point, st_coordinates(tacke)[,1])]
  observational_plan$id <- koord_1$L1

  plan_opazanja$od <- observational_plan$station[match(plan_opazanja$id, observational_plan$id)]
  plan_opazanja$do <- observational_plan$obs.point[match(plan_opazanja$id, observational_plan$id)]

  # Kreranje liste
  ul_podaci_lista <- list(tacke,plan_opazanja)

  return(ul_podaci_lista)
}

#

