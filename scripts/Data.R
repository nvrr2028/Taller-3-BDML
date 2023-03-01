#**************************************************************************************#
#                                    TALLER 3 BDML                                     #
#                        Uniandes: Sofia Charry Tobar                                  #
#                                  Laura Manuela Rodríguez Morales                     #
#                                  Nicol Valeria Rodríguez Rodríguez                   #
#                                  Brayan Alexander Vargas Rojas                       #
#                          Fuente: Properati,                                            #
#**************************************************************************************#

# Limpiar el espacio
rm(list = ls(all.names = TRUE))

#setwd("C:/Users/nicol/Documents/GitHub/Repositorios/Taller-3-BDML")
#setwd("/Users/bray/Desktop/Big Data/Talleres/Taller-3-BDML")
#setwd("C:/Users/lmrod/OneDrive/Documentos/GitHub/Taller-3-BDML")


require(pacman)
p_load(tidyverse, sf, tmaptools, osmdata)

chapinero<-opq(bbox = getbb("Chapinero Bogotá Colombia"))
chapinero

#chapinero_bbox <- c(-74.066598, 4.631753, -74.061713, 4.658588)
"chapinero_osm <- osmdata::osmdata_download(bbox = chapinero_bbox, timeout = 60)
