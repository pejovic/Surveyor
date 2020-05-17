
# Packages
library(tidyverse)
library(magrittr)
library(ggplot2)
# library(geomnet)
library(ggnetwork)
library(sf)
library(ggmap)
library(sp)
library(rgdal)
library(leaflet)
# library(xlsx)
library(data.table)
library(mapview)
library(mapedit)
library(matlib)
library(nngeo)
library(writexl)
library(here)
library(plotly)


source("./R/Simulations_functions.r")
source("./R/functions.r")

file_path <- here::here("Data/Input/With_observations/Brana/Brana.xlsx")
brana.snet <- read_surveynet(file = file_path)
plot_surveynet(snet = brana.snet, webmap = FALSE, net.1D = FALSE, net.2D = TRUE)
plot_surveynet(snet = brana.snet, webmap = TRUE, net.1D = FALSE, net.2D = TRUE)
brana.snet.adj <- adjust.snet(adjust = TRUE, survey.net = brana.snet, dim_type = "2D", sd.apriori = 1, ellipse.scale = 10, all = FALSE) # promeniti sd pravca i duzina
plot_surveynet(snet.adj = brana.snet.adj, webmap = TRUE, net.1D = FALSE, net.2D = TRUE)

file_path <- here::here("Data/Input/With_observations/Makis/Makis_observations.xlsx")
makis.snet <- read_surveynet(file = file_path)
# TO DO: set_srs unutar read_surveynet
plot_surveynet(snet = makis.snet, webmap = TRUE, net.1D = FALSE, net.2D = TRUE)
makis.snet.adj <- adjust.snet(adjust = TRUE, survey.net = makis.snet, dim_type = "2D", sd.apriori = 1 ,  all = FALSE)

file_path <- here::here("Data/Input/With_observations/Zadatak 1/Zadatak_1.xlsx")
zadatak1.snet <- read_surveynet(file = file_path)
plot_surveynet(snet = zadatak1.snet, webmap = FALSE, net.1D = FALSE, net.2D = TRUE)
zadatak1.snet.adj <- adjust.snet(adjust = TRUE, survey.net = zadatak1.snet, dim_type = "2D", sd.apriori = 1 ,  all = FALSE)

file_path <- here::here("Data/Input/Without_observations/xlsx/TETO_plan opazanja1.xlsx")
teto.snet <- read_surveynet(file = file_path)
plot_surveynet(snet = teto.snet, webmap = TRUE, net.1D = FALSE, net.2D = TRUE)
teto.snet.adj <- adjust.snet(adjust = FALSE, survey.net = teto.snet, dim_type = "2D", sd.apriori = 1 ,  all = FALSE)


adj.net_spatial_view_web(ellipses = brana.snet.adj[[1]]$ellipse.net, observations = brana.snet.adj[[2]], points = brana.snet.adj[[1]]$net.points, sp_bound = 2, rii_bound = 1)



file_path <- here::here("Data/Input/With_observations/Brana_Gorica/Brana_Gorica_nulta_serija.xlsx")
gorica0.snet <- read_surveynet(file = file_path)
plot_surveynet(snet = gorica0.snet, webmap = FALSE, net.1D = FALSE, net.2D = TRUE)
gorica0.snet.adj <- adjust.snet(adjust = TRUE, survey.net = gorica0.snet, dim_type = "2D", sd.apriori = 1 ,  all = FALSE)

file_path <- here::here("Data/Input/With_observations/Brana_Gorica/Brana_Gorica_april_2019.xlsx")
gorica1.snet <- read_surveynet(file = file_path)
plot_surveynet(snet = gorica1.snet, webmap = FALSE, net.1D = FALSE, net.2D = TRUE)
gorica1.snet.adj <- adjust.snet(adjust = TRUE, survey.net = gorica1.snet, dim_type = "2D", sd.apriori = 1 ,  all = FALSE)




# 1D design and adjustment
file_path <- here::here("Data/Input/With_observations/DNS_1D/DNS_1D_nulta.xlsx")
dns.snet <- read_surveynet(file = file_path)
plot_surveynet(snet = dns.snet, webmap = FALSE, net.1D = TRUE, net.2D = FALSE)
dns.snet.adj <- adjust.snet(adjust = FALSE, survey.net = dns.snet, wdh_model = "n_dh", dim_type = "1D", sd.apriori = 0.2 ,  all = FALSE, result.units = "mm")
plot_surveynet(snet.adj = dns.snet.adj, webmap = FALSE, net.1D = TRUE, net.2D = FALSE)

dns.snet.adj <- adjust.snet(adjust = TRUE, survey.net = dns.snet, wdh_model = "n_dh", dim_type = "1D", sd.apriori = 0.2 ,  all = FALSE, result.units = "mm")
plot_surveynet(snet.adj = dns.snet.adj, webmap = FALSE, net.1D = TRUE, net.2D = FALSE)


fixed_points <- survey1net$points[(survey1net$points$FIX_1D == TRUE), ]$Name %>% .[!is.na(.)]





# proba 1d adjust=T
dns.snet.adj$Observations$id <- row_number(dns.snet.adj$Observations$from)

ggplotly(
  ggplot()+
    geom_ribbon(data = dns.snet.adj$Observations,
                aes(x = id,
                    ymin = 0,
                    ymax = f) )+
    #geom_area(data = dns.snet.adj$Observations,
    #          aes(x = from_to,
    #              y = f, fill = "blue"))+
    scale_colour_gradient(low="orange",
                          high="red", guide = FALSE)+
    xlab("Name") +
    ylab("Residuals [mm]") +
    ggtitle("GEODETIC 1D NETWORK")+
    labs(colour = "Residuals [mm]")+
    theme_bw()+
    ylim(min(dns.snet.adj$Observations$f)-sd(dns.snet.adj$Observations$f),
         max(dns.snet.adj$Observations$f)+sd(dns.snet.adj$Observations$f)), showlegend = TRUE
)


