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

# ------------------------------------------------------------------------------------ #
# Cargar librerias.
# ------------------------------------------------------------------------------------ #

#setwd("C:/Users/nicol/Documents/GitHub/Repositorios/Taller-3-BDML")
#setwd("/Users/bray/Desktop/Big Data/Talleres/Taller-3-BDML")
#setwd("C:/Users/lmrod/OneDrive/Documentos/GitHub/Taller-3-BDML")


require(pacman)
p_load(tidyverse, sf, tmaptools, osmdata)

chapinero<-opq(bbox = getbb("Chapinero Bogotá Colombia"))
chapinero

# ------------------------------------------------------------------------------------ #
# Compilar la base de datos
# ------------------------------------------------------------------------------------ #

available_features() %>% head(20)
available_tags() %>% head(20)

available_tags("amenity")

#Obtenenemos las universidades
universidades <- chapinero %>% 
  add_osm_feature(key="amenity",value="university") %>% # de las amenities disponibles, seleccionamos las universidades
  osmdata_sf() #transformamos a un objeto sf

puntos_universidades<-universidades$osm_point
head(puntos_universidades)

ggplot()+
  geom_sf(data=puntos_universidades) +
  theme_bw()


p_load("leaflet")

leaflet() %>% 
  addTiles() %>%  #capa base
  addCircles(data=puntos_universidades, popup = ~name)

#chapinero_bbox <- c(-74.066598, 4.631753, -74.061713, 4.658588)
#chapinero_osm <- osmdata::osmdata_download(bbox = chapinero_bbox, timeout = 60)


