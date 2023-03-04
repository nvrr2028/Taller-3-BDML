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
setwd("/Users/bray/Desktop/Big Data/Talleres/Taller-3-BDML")
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

################################# PARA TRAIN ###########################################
# Todo en minuscula
train$description <- tolower(train$description)
# Eliminamos tildes
train$description <- iconv(train$description, from = "UTF-8", to = "ASCII//TRANSLIT")
# Eliminamos caracteres especiales
train$description <- str_replace_all(train$description, "[^[:alnum:]]", " ")
# Eliminamos espacios extras
train$description <- gsub("\\s+", " ", str_trim(train$description))

# Dummy: chapinero ------------------------------------------------------------------

cond1 <- grepl(" chapinero ", train$description)
cond2 <- grepl("chapinero", train$description)
cond3 <- grepl(" chapinero ", train$title)
cond4 <- grepl("chapinero", train$title)

filtro <- cond1 | cond2 | cond3 | cond4
train$Chapinero <- ifelse()
sum(filtro==TRUE)
# Metros cuadrados ------------------------------------------------------------------
mts11 <- as.numeric(str_extract(train$description, "(\\d)+(?= mts)"))
mts12 <- as.numeric(str_extract(train$description, "(\\d)+(?=mts)"))
mts13 <- as.numeric(str_extract(train$description, "(\\d)+(?=  mts)"))
mts21 <- as.numeric(str_extract(train$description, "(\\d)+(?= m2)"))
mts22 <- as.numeric(str_extract(train$description, "(\\d)+(?=m2)"))
mts23 <- as.numeric(str_extract(train$description, "(\\d)+(?=  m2)"))
mts31 <- as.numeric(str_extract(train$description, "(\\d)+(?= metros cuadrados)"))
mts32 <- as.numeric(str_extract(train$description, "(\\d)+(?=metros cuadrados)"))
mts33 <- as.numeric(str_extract(train$description, "(\\d)+(?=  metros cuadrados)"))
mts41 <- as.numeric(str_extract(train$description, "(\\d)+(?=metros)"))
mts42 <- as.numeric(str_extract(train$description, "(\\d)+(?= metros)"))
mts43 <- as.numeric(str_extract(train$description, "(\\d)+(?=  metros)"))
mts51 <- as.numeric(str_extract(train$description, "(\\d)+(?= mt)"))
mts52 <- as.numeric(str_extract(train$description, "(\\d)+(?=mt)"))
mts53 <- as.numeric(str_extract(train$description, "(\\d)+(?=  mt)"))
mts61 <- as.numeric(str_extract(train$description, "(\\d)+(?= mtrs)"))
mts62 <- as.numeric(str_extract(train$description, "(\\d)+(?=mtrs)"))
mts63 <- as.numeric(str_extract(train$description, "(\\d)+(?=  mtrs)"))
mts71 <- as.numeric(str_extract(train$description, "(\\d)+(?= m )"))
mts72 <- as.numeric(str_extract(train$description, "(\\d)+(?=m )"))
mts73 <- as.numeric(str_extract(train$description, "(\\d)+(?=  m )"))
mts_train <- as.numeric(rbind(mts11, mts12, mts13, mts21, mts22, mts23, mts31, mts32, 
                              mts33, mts41, mts42, mts43, mts51, mts52, mts53, 
                              mts61, mts62, mts63, mts71, mts72, mts73))
train$surface_covered <- ifelse(is.na(train$surface_covered), mts_train, train$surface_covered)




################################# PARA TEST ###########################################
# Todo en minuscula
test$description <- tolower(test$description)
# Eliminamos tildes
test$description <- iconv(test$description, from = "UTF-8", to = "ASCII//TRANSLIT")
# Eliminamos caracteres especiales
test$description <- str_replace_all(test$description, "[^[:alnum:]]", " ")
# Eliminamos espacios extras
test$description <- gsub("\\s+", " ", str_trim(test$description))

# Metros cuadrados ------------------------------------------------------------------
mts11 <- as.numeric(str_extract(test$description, "(\\d)+(?= mts)"))
mts12 <- as.numeric(str_extract(test$description, "(\\d)+(?=mts)"))
mts13 <- as.numeric(str_extract(test$description, "(\\d)+(?=  mts)"))
mts21 <- as.numeric(str_extract(test$description, "(\\d)+(?= m2)"))
mts22 <- as.numeric(str_extract(test$description, "(\\d)+(?=m2)"))
mts23 <- as.numeric(str_extract(test$description, "(\\d)+(?=  m2)"))
mts31 <- as.numeric(str_extract(test$description, "(\\d)+(?= metros cuadrados)"))
mts32 <- as.numeric(str_extract(test$description, "(\\d)+(?=metros cuadrados)"))
mts33 <- as.numeric(str_extract(test$description, "(\\d)+(?=  metros cuadrados)"))
mts41 <- as.numeric(str_extract(test$description, "(\\d)+(?=metros)"))
mts42 <- as.numeric(str_extract(test$description, "(\\d)+(?= metros)"))
mts43 <- as.numeric(str_extract(test$description, "(\\d)+(?=  metros)"))
mts51 <- as.numeric(str_extract(test$description, "(\\d)+(?= mt)"))
mts52 <- as.numeric(str_extract(test$description, "(\\d)+(?=mt)"))
mts53 <- as.numeric(str_extract(test$description, "(\\d)+(?=  mt)"))
mts61 <- as.numeric(str_extract(test$description, "(\\d)+(?= mtrs)"))
mts62 <- as.numeric(str_extract(test$description, "(\\d)+(?=mtrs)"))
mts63 <- as.numeric(str_extract(test$description, "(\\d)+(?=  mtrs)"))
mts71 <- as.numeric(str_extract(test$description, "(\\d)+(?= m )"))
mts72 <- as.numeric(str_extract(test$description, "(\\d)+(?=m )"))
mts73 <- as.numeric(str_extract(test$description, "(\\d)+(?=  m )"))
mts_test <- as.numeric(rbind(mts11, mts12, mts13, mts21, mts22, mts23, mts31, mts32, 
                             mts33, mts41, mts42, mts43, mts51, mts52, mts53, 
                             mts61, mts62, mts63, mts71, mts72, mts73))
test$surface_covered <- ifelse(is.na(test$surface_covered), mts_test, test$surface_covered)


# 2.3 Variables adicionales de fuentes externas -------------------------------------- #

available_features() %>% head(20)
available_tags() %>% head(20)

available_tags("amenity")
available_tags("building")
available_tags("leisure")

chapinero <- getbb(place_name="UPZ Chapinero, Bogota",
                   featuretype="boundary:administrative",
                   format_out="sf_polygon") %>% .$multipolygon
p_load("leaflet")
##### trasmi bgt
trasmi <- bogota %>% 
  add_osm_feature(key="amenity",value="bus_station") %>% # de las amenities disponibles, seleccionamos las universidades
  osmdata_sf() #transformamos a un objeto sf

puntos_trasmi<-trasmi$osm_point
head(puntos_bus)
## ver buses bgt
alltrasmi<-bus$osm_polygons 
ggplot()+
  geom_sf(data=alltrasmi) +
  theme_bw()

##### cai bgt
cai <- bogota %>% 
  add_osm_feature(key="amenity",value="police") %>% # de las amenities disponibles, seleccionamos las universidades
  osmdata_sf() #transformamos a un objeto sf

puntos_cai<-cai$osm_point
head(puntos_cai)
## ver edificio uniandes 
allcai<-cai$osm_polygons 
ggplot()+
  geom_sf(data=allcai) +
  theme_bw()

##### supermercados bgt
super <- bogota %>% 
  add_osm_feature(key="shop",value="supermarket") %>% # de las amenities disponibles, seleccionamos las universidades
  osmdata_sf() #transformamos a un objeto sf

puntos_super<-super$osm_point
head(puntos_super)

allsuper<-cai$osm_polygons 
ggplot()+
  geom_sf(data=allsuper) +
  theme_bw()

##### supermercados bgt
super <- bogota %>% 
  add_osm_feature(key="shop",value="supermarket") %>% # de las amenities disponibles, seleccionamos las universidades
  osmdata_sf() #transformamos a un objeto sf

puntos_super<-super$osm_point
head(puntos_super)

allsuper<-cai$osm_polygons 
ggplot()+
  geom_sf(data=allsuper) +
  theme_bw()

##### bar bgt
bar <- bogota %>% 
  add_osm_feature(key="amenity",value="bar") %>% # de las amenities disponibles, seleccionamos las universidades
  osmdata_sf() #transformamos a un objeto sf

puntos_bar<-bar$osm_point
head(puntos_bar)

allbar<-bar$osm_polygons 
ggplot()+
  geom_sf(data=allbar) +
  theme_bw()

##### cc bgt
cc <- bogota %>% 
  add_osm_feature(key="shop",value="Mall") %>% # de las amenities disponibles, seleccionamos las universidades
  osmdata_sf() #transformamos a un objeto sf

puntos_cc<-cc$osm_point
head(puntos_cc)

allcc<-cc$osm_polygons 
ggplot()+
  geom_sf(data=allcc) +
  theme_bw()

#importamos la librería
p_load("leaflet")

leaflet() %>% 
  addTiles() %>%  #capa base
  addPolygons(data=ML) #capa edificio ML

################################# PARA TRAIN ###########################################

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
disttrain_matrix_parques <- st_distance(x = train_parques_sf, y = cent_parques_sf)

# Encontramos la distancia mínima a un parque
dist_min_parques <- apply(disttrain_matrix_parques, 1, min)
train$distancia_parque <- dist_min_parques
train_parques_sf$distancia_parque <- dist_min_parques

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
cent_gym_sf <- st_as_sf(cent_gym, coords = c("x", "y"))
# Esto va a ser demorado!
disttrain_matrix_gym <- st_distance(x = train_sf, y = cent_gym_sf)

# Encontramos la distancia mínima a un parque
dist_min_gym <- apply(disttrain_matrix_gym, 1, min)
train$distancia_gym <- dist_min_gym
train_sf$distancia_gym <- dist_min_gym

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

################################# PARA TEST ###########################################

# Distancia a parques --------------------------------------------------------------

# Ahora vamos a calcular la distancia de cada apartamento al centroide de cada parque
test_sf <- st_as_sf(test, coords = c("lon", "lat"))
st_crs(test_sf) <- 4326
cent_parques_sf <- st_as_sf(cent_parques, coords = c("x", "y"))
# Esto va a ser demorado!
disttest_matrix_parques <- st_distance(x = test_sf, y = cent_parques_sf)

# Encontramos la distancia mínima a un parque
disttest_min_parques <- apply(disttest_matrix_parques, 1, min)
test$distancia_parque <- disttest_min_parques
test_sf$distancia_parque <- disttest_min_parques

# Distancia a un gimnasio ----------------------------------------------------------

# Ahora vamos a calcular la distancia de cada apartamento al centroide de cada parque
test_sf <- st_as_sf(test, coords = c("lon", "lat"))
st_crs(test_sf) <- 4326
cent_gym_sf <- st_as_sf(cent_gym, coords = c("x", "y"))
# Esto va a ser demorado!
disttest_matrix_gym <- st_distance(x = test_sf, y = cent_gym_sf)

# Encontramos la distancia mínima a un parque
disttest_min_gym <- apply(disttest_matrix_gym, 1, min)
test$distancia_gym <- disttest_min_gym
test_sf$distancia_gym <- disttest_min_gym

# ------------------------------------------------------------------------------------ #
# 3. Estadísticas descriptivas
# ------------------------------------------------------------------------------------ #




