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

setwd("C:/Users/nicol/Documents/GitHub/Repositorios/Taller-3-BDML")
#setwd("/Users/bray/Desktop/Big Data/Talleres/Taller-3-BDML")
#setwd("C:/Users/lmrod/OneDrive/Documentos/GitHub/Taller-3-BDML")


list.of.packages = c("pacman", "readr","tidyverse", "dplyr", "arsenal", "fastDummies", 
                     "caret", "glmnet", "MLmetrics", "skimr", "plyr", "stargazer", 
                     "ggplot2", "corrplot", "Hmisc", "sf", "tmaptools", "osmdata", "leaflet")

new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
sapply(list.of.packages, require, character.only = TRUE)

# ------------------------------------------------------------------------------------ #
# 1. Descripción del problema
# ------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------ #
# 2. Data
# ------------------------------------------------------------------------------------ #

# Template submission: precio 
precio_bog <- read.csv("./data/submission_template.csv")
# Train
train_bog <- read_csv("./data/train.csv")
# Test 
test_bog <- read_csv("./data/test.csv")

# ------------------------------------------------------------------------------------ #
# Compilar la base de datos
# ------------------------------------------------------------------------------------ #

available_features() %>% head(20)
available_tags() %>% head(20)

available_tags("amenity")

available_tags("building")

#Obtenenemos las universidades
universidades <- chapinero %>% 
  add_osm_feature(key="amenity",value="university") %>% # de las amenities disponibles, seleccionamos las universidades
  osmdata_sf() #transformamos a un objeto sf

puntos_universidades<-universidades$osm_point
head(puntos_universidades)

ggplot()+
  geom_sf(data=puntos_universidades) +
  theme_bw()

leaflet() %>% 
  addTiles() %>%  #capa base
  addCircles(data=puntos_universidades, popup = ~name)


#Obtenenemos transporte
transporte <- chapinero %>% 
  add_osm_feature(key="building",value="transportation") %>% # de las amenities disponibles, seleccionamos las universidades
  osmdata_sf() #transformamos a un objeto sf

puntos_transporte<-transporte$osm_point
head(puntos_transporte)

ggplot()+
  geom_sf(data=puntos_transporte) +
  theme_bw()

leaflet() %>% 
  addTiles() %>%  #capa base
  addCircles(data=puntos_transporte, popup = ~name)
#chapinero_bbox <- c(-74.066598, 4.631753, -74.061713, 4.658588)
#chapinero_osm <- osmdata::osmdata_download(bbox = chapinero_bbox, timeout = 60)

# ------------------------------------------------------------------------------------ #
# 3. Estadísticas descriptivas
# ------------------------------------------------------------------------------------ #


