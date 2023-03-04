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

# 2.1 Limpieza de datos -------------------------------------------------------------- #
train <- train_bog
test <- test_bog

# ¿Cómo se comporta el precio de venta? 
summary(train$price) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(V1 = scales::dollar(V1))

p <- ggplot(train) +
  geom_boxplot(aes(y = price), fill = "darkblue", alpha = 0.4) +
  labs(y = "Valor del mt2 (log-scale)") +
  scale_y_log10(labels = scales::number) +
  scale_x_discrete() +
  theme_bw()
ggplotly(p)

# No hay datos atipicos que deban eliminarse 

# 2.2 Variables adicionales de la descripción de las propiedades --------------------- #

# PARA TRAIN
# Todo en minuscula
train$description <- tolower(train$description)
# Eliminamos tildes
train$description <- iconv(train$description, from = "UTF-8", to = "ASCII//TRANSLIT")
# Eliminamos caracteres especiales
train$description <- str_replace_all(train$description, "[^[:alnum:]]", " ")
# Eliminamos espacios extras
train$description <- gsub("\\s+", " ", str_trim(train$description))

# PARA TEST
# Todo en minuscula
test$description <- tolower(test$description)
# Eliminamos tildes
test$description <- iconv(test$description, from = "UTF-8", to = "ASCII//TRANSLIT")
# Eliminamos caracteres especiales
test$description <- str_replace_all(test$description, "[^[:alnum:]]", " ")
# Eliminamos espacios extras
test$description <- gsub("\\s+", " ", str_trim(test$description))

# Metros cuadrados
mts1 <- as.numeric(str_extract(train$description, "(\\d)+(?= mts)"))
mts2 <- as.numeric(str_extract(train$description, "(\\d)+(?=mts)"))
mts3 <- as.numeric(str_extract(train$description, "(\\d)+(?= m2)"))
mts4 <- as.numeric(str_extract(train$description, "(\\d)+(?=m2)"))
mts5 <- as.numeric(str_extract(train$description, "(\\d)+(?= metros cuadrados)"))
mts6 <- as.numeric(str_extract(train$description, "(\\d)+(?=metros cuadrados)"))
mts7 <- as.numeric(str_extract(train$description, "(\\d)+(?=metros)"))
mts8 <- as.numeric(str_extract(train$description, "(\\d)+(?= metros)"))

# 2.3 Variables adicionales de fuentes externas -------------------------------------- #

available_features() %>% head(20)
available_tags() %>% head(20)

available_tags("amenity")
available_tags("building")

chapinero <- getbb(place_name="UPZ Chapinero, Bogota",
                   featuretype="boundary:administrative",
                   format_out="sf_polygon") %>% .$multipolygon

# PARA TRAIN
available_tags("leisure")

# Distancia a parques ----------------------------------------------------------
Parques <- opq(bbox = getbb("UPZ Chapinero, Bogota")) %>%
  add_osm_feature(key = "leisure" , value = "park") 
Parques_sf <- osmdata_sf(Parques)
Parques_geometria <- Parques_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada parque
cent_parques <- gCentroid(as(Parques_geometria$geometry, "Spatial"), byid = T)

# Ahora vamos a calcular la distancia de cada apartamento al centroide de cada parque
train_sf <- st_as_sf(train, coords = c("lon", "lat"))
st_crs(train_sf) <- 4326
cent_parques_sf <- st_as_sf(cent_parques, coords = c("x", "y"))
# Esto va a ser demorado!
dist_matrix <- st_distance(x = train_sf, y = cent_parques_sf)

# Encontramos la distancia mínima a un parque
dist_min <- apply(dist_matrix, 1, min)
train$distancia_parque <- dist_min
train_sf$distancia_parque <- dist_min

# Distancia a un gimnasio ----------------------------------------------------------
Gym <- opq(bbox = getbb("UPZ Chapinero, Bogota")) %>%
  add_osm_feature(key = "leisure" , value = "fitness_centre") 
Gym_sf <- osmdata_sf(Gym)
Gym_geometria <- Gym_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada parque
cent_gym <- gCentroid(as(Parques_geometria$geometry, "Spatial"), byid = T)

# Ahora vamos a calcular la distancia de cada apartamento al centroide de cada parque
train_sf <- st_as_sf(train, coords = c("lon", "lat"))
st_crs(train_sf) <- 4326
centroides_sf <- st_as_sf(centroides, coords = c("x", "y"))
# Esto va a ser demorado!
dist_matrix <- st_distance(x = train_sf, y = centroides_sf)

# Encontramos la distancia mínima a un parque
dist_min <- apply(dist_matrix, 1, min)
train$distancia_parque <- dist_min
train_sf$distancia_parque <- dist_min


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


