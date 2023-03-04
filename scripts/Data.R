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
cond1_train <- grepl(" chapinero |chapinero|el verjn bajo|el refugio|chic reservado|bellavista|chic alto|el nogal|el refugio|la cabrera|los rosales|seminario|toscana|san isidro patios|la esperanza nororiental|la surea|san isidro|san luis altos del cabo|pardo rubio|bosque caldern|bosque caldern tejada|chapinero alto|el castillo|el paraso|emaus| granada|ingemar|juan xxiii|la salle|las acacias|los olivos|mara cristina|mariscal sucre|nueva granada| palomar|pardo rubio|san martn de porres|villa anita|villa del cerro|chic lago|antiguo country|chic norte|chic norte ii|chic norte iii|chic occidental|el chic|el retiro|espartillal|la cabrera|lago gaitn|porcincula|quinta camacho|chapinero centro|catalua|chapinero central|chapinero norte|marly|sucre", train$description)
cond2_train <- grepl(" chapinero |chapinero|el verjn bajo|el refugio|chic reservado|bellavista|chic alto|el nogal|el refugio|la cabrera|los rosales|seminario|toscana|san isidro patios|la esperanza nororiental|la surea|san isidro|san luis altos del cabo|pardo rubio|bosque caldern|bosque caldern tejada|chapinero alto|el castillo|el paraso|emaus| granada|ingemar|juan xxiii|la salle|las acacias|los olivos|mara cristina|mariscal sucre|nueva granada| palomar|pardo rubio|san martn de porres|villa anita|villa del cerro|chic lago|antiguo country|chic norte|chic norte ii|chic norte iii|chic occidental|el chic|el retiro|espartillal|la cabrera|lago gaitn|porcincula|quinta camacho|chapinero centro|catalua|chapinero central|chapinero norte|marly|sucre", train$title)
filtro_train <- cond1_train | cond2_train 
train$Chapinero <- ifelse(filtro_train==TRUE, 1, 0)

# Metros cuadrados ------------------------------------------------------------------
# Se completa surface_covered, con el objetivo de no perder tantas observaciones
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
mts_train <-  as.data.frame(cbind(mts11, mts12, mts13, mts21, mts22, mts23, mts31, mts32,
               mts33, mts41, mts42, mts43, mts51, mts52, mts53, 
               mts61, mts62, mts63, mts71, mts72, mts73))
for (j in 1:nrow(train)) {
  for(i in 1:ncol(mts_train)){
    if (is.na(mts_train[j,i])) {
      train$surface_covered[j] <- train$surface_covered[j]
    } else {
      train$surface_covered[j] <- mts_train[j,i]
    }
  }
}
sum(is.na(train$surface_covered))

# Dummy: terraza o balcon ------------------------------------------------------------------
dumm1_train <- grepl("balcon|balcn|terraza|mirador", train$description)
train$terraza <- ifelse(dumm1_train==TRUE, 1, 0)

# Dummy: espacios sociales ------------------------------------------------------------------
dumm2_train <- grepl("salon comunal|saln comunal|bbq|piscina|piscinas|zona comun|zona comn|zona comn|pin pon|ascensor|ascensores", train$description)
train$social <- ifelse(dumm2_train==TRUE, 1, 0)

# Dummy: parqueadero ------------------------------------------------------------------------
dumm3_train <- grepl("garaje|garajes|sotano|sotanos|parqueadero|parqueaderos|cochera|cocheras|patio|patios", train$description)
train$parqueadero <- ifelse(dumm3_train==TRUE, 1, 0)

# Categórica: parqueadero ------------------------------------------------------------------------
parq11 <- as.numeric(str_extract(train$description, "(\\d)+(?= garaje)"))
parq12 <- as.numeric(str_extract(train$description, "(\\d)+(?=garaje)"))
parq13 <- as.numeric(str_extract(train$description, "(\\d)+(?=  garaje)"))
parq21 <- as.numeric(str_extract(train$description, "(\\d)+(?= garajes)"))
parq22 <- as.numeric(str_extract(train$description, "(\\d)+(?=garajes)"))
parq23 <- as.numeric(str_extract(train$description, "(\\d)+(?=  garajes)"))
parq31 <- as.numeric(str_extract(train$description, "(\\d)+(?= sotano)"))
parq32 <- as.numeric(str_extract(train$description, "(\\d)+(?=sotano)"))
parq33 <- as.numeric(str_extract(train$description, "(\\d)+(?=  sotano)"))
parq41 <- as.numeric(str_extract(train$description, "(\\d)+(?=sotanos)"))
parq42 <- as.numeric(str_extract(train$description, "(\\d)+(?= sotanos)"))
parq43 <- as.numeric(str_extract(train$description, "(\\d)+(?=  sotanos)"))
parq51 <- as.numeric(str_extract(train$description, "(\\d)+(?= parqueadero)"))
parq52 <- as.numeric(str_extract(train$description, "(\\d)+(?=parqueadero)"))
parq53 <- as.numeric(str_extract(train$description, "(\\d)+(?=  parqueadero)"))
parq61 <- as.numeric(str_extract(train$description, "(\\d)+(?= parqueaderos)"))
parq62 <- as.numeric(str_extract(train$description, "(\\d)+(?=parqueaderos)"))
parq63 <- as.numeric(str_extract(train$description, "(\\d)+(?=  parqueaderos)"))
parq71 <- as.numeric(str_extract(train$description, "(\\d)+(?= cochera)"))
parq72 <- as.numeric(str_extract(train$description, "(\\d)+(?=cochera)"))
parq73 <- as.numeric(str_extract(train$description, "(\\d)+(?=  cochera)"))
parq81 <- as.numeric(str_extract(train$description, "(\\d)+(?= cocheras)"))
parq82 <- as.numeric(str_extract(train$description, "(\\d)+(?=cocheras)"))
parq83 <- as.numeric(str_extract(train$description, "(\\d)+(?=  cocheras)"))
parq91 <- as.numeric(str_extract(train$description, "(\\d)+(?= patio)"))
parq92 <- as.numeric(str_extract(train$description, "(\\d)+(?=patio)"))
parq93 <- as.numeric(str_extract(train$description, "(\\d)+(?=  patio)"))
parq101 <- as.numeric(str_extract(train$description, "(\\d)+(?= patios)"))
parq102 <- as.numeric(str_extract(train$description, "(\\d)+(?=patios)"))
parq103 <- as.numeric(str_extract(train$description, "(\\d)+(?=  patios)"))
parq_train <- as.data.frame(cbind(parq11, parq12, parq13, parq21, parq22, parq23, parq31, parq32, 
                               parq33, parq41, parq42, parq43, parq51, parq52, parq53, 
                               parq61, parq62, parq63, parq71, parq72, parq73, parq81, parq82, parq83,
                               parq91, parq92, parq93, parq101, parq102, parq103))
train$cat_parqueadero <- NA
for (j in 1:nrow(train)) {
  for(i in 1:ncol(parq_train)){
    if (!is.na(parq_train[j,i])){
      train$cat_parqueadero[j] <- parq_train[j,i]
    }
  }
}
train$cat_parqueadero[train$cat_parqueadero>10] <- NA
train$cat_parqueadero <- as.factor(train$cat_parqueadero)
sum(is.na(train$cat_parqueadero))
n <- train %>% 
  group_by(cat_parqueadero) %>%
  dplyr::summarise(n=n())

################################# PARA TEST ###########################################
# Todo en minuscula
test$description <- tolower(test$description)
# Eliminamos tildes
test$description <- iconv(test$description, from = "UTF-8", to = "ASCII//TRANSLIT")
# Eliminamos caracteres especiales
test$description <- str_replace_all(test$description, "[^[:alnum:]]", " ")
# Eliminamos espacios extras
test$description <- gsub("\\s+", " ", str_trim(test$description))

# Dummy: chapinero ------------------------------------------------------------------
cond1_test <- grepl(" chapinero |chapinero|el verjn bajo|el refugio|chic reservado|bellavista|chic alto|el nogal|el refugio|la cabrera|los rosales|seminario|toscana|san isidro patios|la esperanza nororiental|la surea|san isidro|san luis altos del cabo|pardo rubio|bosque caldern|bosque caldern tejada|chapinero alto|el castillo|el paraso|emaus| granada|ingemar|juan xxiii|la salle|las acacias|los olivos|mara cristina|mariscal sucre|nueva granada| palomar|pardo rubio|san martn de porres|villa anita|villa del cerro|chic lago|antiguo country|chic norte|chic norte ii|chic norte iii|chic occidental|el chic|el retiro|espartillal|la cabrera|lago gaitn|porcincula|quinta camacho|chapinero centro|catalua|chapinero central|chapinero norte|marly|sucre", test$description)
cond2_test <- grepl(" chapinero |chapinero|el verjn bajo|el refugio|chic reservado|bellavista|chic alto|el nogal|el refugio|la cabrera|los rosales|seminario|toscana|san isidro patios|la esperanza nororiental|la surea|san isidro|san luis altos del cabo|pardo rubio|bosque caldern|bosque caldern tejada|chapinero alto|el castillo|el paraso|emaus| granada|ingemar|juan xxiii|la salle|las acacias|los olivos|mara cristina|mariscal sucre|nueva granada| palomar|pardo rubio|san martn de porres|villa anita|villa del cerro|chic lago|antiguo country|chic norte|chic norte ii|chic norte iii|chic occidental|el chic|el retiro|espartillal|la cabrera|lago gaitn|porcincula|quinta camacho|chapinero centro|catalua|chapinero central|chapinero norte|marly|sucre", test$title)
filtro_test <- cond1_test | cond2_test 
test$Chapinero <- ifelse(filtro_test==TRUE, 1, 0)

# Metros cuadrados ------------------------------------------------------------------
# Se completa surface_covered, con el objetivo de no perder tantas observaciones
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
mts_test <- as.data.frame(cbind(mts11, mts12, mts13, mts21, mts22, mts23, mts31, mts32,
                  mts33, mts41, mts42, mts43, mts51, mts52, mts53, 
                  mts61, mts62, mts63, mts71, mts72, mts73))
for (j in 1:nrow(test)) {
  for(i in 1:ncol(mts_test)){
    if (is.na(mts_test[j,i])) {
      test$surface_covered[j] <- test$surface_covered[j]
    } else {
      test$surface_covered[j] <- mts_test[j,i]
    }
  }
}
sum(is.na(test$surface_covered))

# Dummy: terraza o balcon ------------------------------------------------------------------
dumm1_test <- grepl("balcon|balcn|terraza|mirador", test$description)
train$terraza <- ifelse(dumm1_train==TRUE, 1, 0)

# Dummy: espacios sociales ------------------------------------------------------------------
dumm2_test <- grepl("salon comunal|saln comunal|bbq|piscina|piscinas|zona comun|zona comn|zona comn|pin pon|ascensor|ascensores", test$description)
test$social <- ifelse(dumm2_test==TRUE, 1, 0)

# Dummy: parqueadero ------------------------------------------------------------------
dumm3_test <- grepl("garaje|garajes|sotano|sotanos|parqueadero|parqueaderos|cochera|cocheras", test$description)
test$parqueadero <- ifelse(dumm3_test==TRUE, 1, 0)

# Categórica: parqueadero ------------------------------------------------------------------------
parq11 <- as.numeric(str_extract(test$description, "(\\d)+(?= garaje)"))
parq12 <- as.numeric(str_extract(test$description, "(\\d)+(?=garaje)"))
parq13 <- as.numeric(str_extract(test$description, "(\\d)+(?=  garaje)"))
parq21 <- as.numeric(str_extract(test$description, "(\\d)+(?= garajes)"))
parq22 <- as.numeric(str_extract(test$description, "(\\d)+(?=garajes)"))
parq23 <- as.numeric(str_extract(test$description, "(\\d)+(?=  garajes)"))
parq31 <- as.numeric(str_extract(test$description, "(\\d)+(?= sotano)"))
parq32 <- as.numeric(str_extract(test$description, "(\\d)+(?=sotano)"))
parq33 <- as.numeric(str_extract(test$description, "(\\d)+(?=  sotano)"))
parq41 <- as.numeric(str_extract(test$description, "(\\d)+(?=sotanos)"))
parq42 <- as.numeric(str_extract(test$description, "(\\d)+(?= sotanos)"))
parq43 <- as.numeric(str_extract(test$description, "(\\d)+(?=  sotanos)"))
parq51 <- as.numeric(str_extract(test$description, "(\\d)+(?= parqueadero)"))
parq52 <- as.numeric(str_extract(test$description, "(\\d)+(?=parqueadero)"))
parq53 <- as.numeric(str_extract(test$description, "(\\d)+(?=  parqueadero)"))
parq61 <- as.numeric(str_extract(test$description, "(\\d)+(?= parqueaderos)"))
parq62 <- as.numeric(str_extract(test$description, "(\\d)+(?=parqueaderos)"))
parq63 <- as.numeric(str_extract(test$description, "(\\d)+(?=  parqueaderos)"))
parq71 <- as.numeric(str_extract(test$description, "(\\d)+(?= cochera)"))
parq72 <- as.numeric(str_extract(test$description, "(\\d)+(?=cochera)"))
parq73 <- as.numeric(str_extract(test$description, "(\\d)+(?=  cochera)"))
parq81 <- as.numeric(str_extract(test$description, "(\\d)+(?= cocheras)"))
parq82 <- as.numeric(str_extract(test$description, "(\\d)+(?=cocheras)"))
parq83 <- as.numeric(str_extract(test$description, "(\\d)+(?=  cocheras)"))
parq91 <- as.numeric(str_extract(test$description, "(\\d)+(?= patio)"))
parq92 <- as.numeric(str_extract(test$description, "(\\d)+(?=patio)"))
parq93 <- as.numeric(str_extract(test$description, "(\\d)+(?=  patio)"))
parq101 <- as.numeric(str_extract(test$description, "(\\d)+(?= patios)"))
parq102 <- as.numeric(str_extract(test$description, "(\\d)+(?=patios)"))
parq103 <- as.numeric(str_extract(test$description, "(\\d)+(?=  patios)"))
parq_test <- as.data.frame(cbind(parq11, parq12, parq13, parq21, parq22, parq23, parq31, parq32, 
                                  parq33, parq41, parq42, parq43, parq51, parq52, parq53, 
                                  parq61, parq62, parq63, parq71, parq72, parq73, parq81, parq82, parq83,
                                  parq91, parq92, parq93, parq101, parq102, parq103))

test$cat_parqueadero <- NA
for (j in 1:nrow(test)) {
  for(i in 1:ncol(parq_test)){
    if (!is.na(parq_test[j,i])) {
      test$cat_parqueadero[j] <- parq_test[j,i]
    }
  }
}
test$cat_parqueadero[test$cat_parqueadero>10] <- NA
test$cat_parqueadero <- as.factor(test$cat_parqueadero)
sum(is.na(test$cat_parqueadero))
nn <- test %>% 
  group_by(cat_parqueadero) %>%
  dplyr::summarise(n=n())

# 2.3 Variables adicionales de fuentes externas -------------------------------------- #
## Examinamos con qué datos contamos
available_features() %>% head(20)
available_tags() %>% head(20)

available_tags("amenity")
available_tags("building")
available_tags("leisure")

# Creamos la unidad geográfica Chapinero
chapinero <- getbb(place_name="UPZ Chapinero, Bogota",
                   featuretype="boundary:administrative",
                   format_out="sf_polygon") %>% .$multipolygon

p_load("omsdata")
bogota<-opq(bbox = getbb("Bogotá Colombia"))
bogota

p_load("leaflet")
##### transmi bgt
transmi <- bogota %>% 
  add_osm_feature(key="amenity",value="bus_station") %>% # de las amenities disponibles, seleccionamos las universidades
  osmdata_sf() #transformamos a un objeto sf

puntos_transmi<-transmi$osm_point
head(puntos_transmi)

TR<-transmi$osm_polygons 
ggplot()+
  geom_sf(data=TR) +
  theme_bw()
## ver buses bgt

leaflet() %>% 
  addTiles() %>%  #capa base
  addPolygons(data=TR) #capa TR


##### cai bgt
cai <- bogota %>% 
  add_osm_feature(key="amenity",value="police") %>% # de las amenities disponibles, seleccionamos las universidades
  osmdata_sf() #transformamos a un objeto sf

puntos_cai<-cai$osm_point
head(puntos_cai)

CAI<-cai$osm_polygons 
ggplot()+
  geom_sf(data=CAI) +
  theme_bw()

## ver cais bgt

leaflet() %>% 
  addTiles() %>%  #capa base
  addPolygons(data=CAI) #capa CAI

##### Bar bgt
bar <- bogota %>% 
  add_osm_feature(key="amenity",value="bar") %>% # de las amenities disponibles, seleccionamos las universidades
  osmdata_sf() #transformamos a un objeto sf

puntos_bar<-bar$osm_point
head(puntos_bar)

BARES<-bar$osm_polygons 
ggplot()+
  geom_sf(data=BARES) +
  theme_bw()

## ver bares en bgt

leaflet() %>% 
  addTiles() %>%  #capa base
  addPolygons(data=BARES) #capa bares

##### supermercados bgt
super <- bogota %>% 
  add_osm_feature(key="shop",value="supermarket") %>% # de las amenities disponibles, seleccionamos las universidades
  osmdata_sf() #transformamos a un objeto sf

puntos_super<-super$osm_point
head(puntos_super)

SM<-super$osm_polygons 
ggplot()+
  geom_sf(data=SM) +
  theme_bw()

## ver supermercados en bgt
leaflet() %>% 
  addTiles() %>%  #capa base
  addPolygons(data=SM) #capa SM

##### cc bgt
cc <- bogota %>% 
  add_osm_feature(key="shop",value="Mall") %>% # de las amenities disponibles, seleccionamos las universidades
  osmdata_sf() #transformamos a un objeto sf

puntos_cc<-cc$osm_point
head(puntos_cc)

CC<-cc$osm_polygons 
ggplot()+
  geom_sf(data=CC) +
  theme_bw()

## ver Centroscomerciales en bgt
leaflet() %>% 
  addTiles() %>%  #capa base
  addPolygons(data=CC) #capa SM

################################# PARA TRAIN ###########################################

# Distancia a parques ----------------------------------------------------------
Parques <- opq(bbox = getbb("Bogota Colombia")) %>%
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
disttrain_matrix_parques <- st_distance(x = train_sf, y = cent_parques_sf)

# Encontramos la distancia mínima a un parque
dist_min_parques <- apply(disttrain_matrix_parques, 1, min)
train$distancia_parque <- dist_min_parques
train_sf$distancia_parque <- dist_min_parques

# Distancia a un gimnasio ----------------------------------------------------------
Gym <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "fitness_centre") 
Gym_sf <- osmdata_sf(Gym)
Gym_geometria <- Gym_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada parque
cent_gym <- gCentroid(as(Parques_geometria$geometry, "Spatial"), byid = T)

# Ahora vamos a calcular la distancia de cada apartamento al centroide de cada gimnasio
train_sf <- st_as_sf(train, coords = c("lon", "lat"))
st_crs(train_sf) <- 4326
cent_gym_sf <- st_as_sf(cent_gym, coords = c("x", "y"))
# Esto va a ser demorado!
disttrain_matrix_gym <- st_distance(x = train_sf, y = cent_gym_sf)

# Encontramos la distancia mínima a un gimnasio
dist_min_gym <- apply(disttrain_matrix_gym, 1, min)
train$distancia_gym <- dist_min_gym
train_sf$distancia_gym <- dist_min_gym

# Distancia a estaciones de TR ----------------------------------------------------------
transmi <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "bus_station") 
transmi_sf <- osmdata_sf(transmi)
transmi_geometria <- transmi_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada estacion de transmi
cent_transmi <- gCentroid(as(transmi_geometria$geometry, "Spatial"), byid = T)

# Ahora vamos a calcular la distancia de cada apartamento al centroide de estacion de transmi
train_sf <- st_as_sf(train, coords = c("lon", "lat"))
st_crs(train_sf) <- 4326
cent_transmi_sf <- st_as_sf(cent_transmi, coords = c("x", "y"))
# Esto va a ser demorado!
disttrain_matrix_transmi <- st_distance(x = train_sf, y = cent_transmi_sf)

# Encontramos la distancia mínima a una estacion de transmi
dist_min_transmi <- apply(disttrain_matrix_transmi, 1, min)
train$distancia_transmi <- dist_min_transmi
train_sf$distancia_transmi <- dist_min_transmi

# Distancia a estaciones de policia  ----------------------------------------------------------
cai <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "police") 
cai_sf <- osmdata_sf(cai)
cai_geometria <- cai_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada cai
cent_cai <- gCentroid(as(cai_geometria$geometry, "Spatial"), byid = T)

# Ahora vamos a calcular la distancia de cada apartamento al centroide de cada cai
train_sf <- st_as_sf(train, coords = c("lon", "lat"))
st_crs(train_sf) <- 4326
cent_cai_sf <- st_as_sf(cent_cai, coords = c("x", "y"))
# Esto va a ser demorado!
disttrain_matrix_cai <- st_distance(x = train_sf, y = cent_cai_sf)

# Encontramos la distancia mínima a un cai
dist_min_cai <- apply(disttrain_matrix_cai, 1, min)
train$distancia_cai <- dist_min_cai
train_sf$distancia_cai <- dist_min_cai

# Distancia a centros comerciales  ----------------------------------------------------------
cc <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "shop" , value = "mall") 
cc_sf <- osmdata_sf(cc)
cc_geometria <- cc_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada centro comercial
cent_cc <- gCentroid(as(cc_geometria$geometry, "Spatial"), byid = T)

# Ahora vamos a calcular la distancia de cada apartamento al centroide de cada centro comercial
train_sf <- st_as_sf(train, coords = c("lon", "lat"))
st_crs(train_sf) <- 4326
cent_cc_sf <- st_as_sf(cent_cc, coords = c("x", "y"))
# Esto va a ser demorado!
disttrain_matrix_cc <- st_distance(x = train_sf, y = cent_cc_sf)

# Encontramos la distancia mínima a un centro comercial
dist_min_cc <- apply(disttrain_matrix_cc, 1, min)
train$distancia_cc <- dist_min_cc
train_sf$distancia_cc <- dist_min_cc

# Distancia a centros bares ----------------------------------------------------------
bar <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "bar") 
bar_sf <- osmdata_sf(bar)
bar_geometria <- bar_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada bar
cent_bar <- gCentroid(as(bar_geometria$geometry, "Spatial"), byid = T)

# Ahora vamos a calcular la distancia de cada apartamento al centroide de cada bar
train_sf <- st_as_sf(train, coords = c("lon", "lat"))
st_crs(train_sf) <- 4326
cent_bar_sf <- st_as_sf(cent_bar, coords = c("x", "y"))
# Esto va a ser demorado!
disttrain_matrix_bar <- st_distance(x = train_sf, y = cent_bar_sf)

# Encontramos la distancia mínima a un bar
dist_min_bar <- apply(disttrain_matrix_bar, 1, min)
train$distancia_bar <- dist_min_bar
train_sf$distancia_bar <- dist_min_bar

# Distancia a supermercados ----------------------------------------------------------
SM <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "shop" , value = "supermarket") 
SM_sf <- osmdata_sf(SM)
SM_geometria <- SM_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada bar
cent_SM <- gCentroid(as(SM_geometria$geometry, "Spatial"), byid = T)

# Ahora vamos a calcular la distancia de cada apartamento al centroide de cada bar
train_sf <- st_as_sf(train, coords = c("lon", "lat"))
st_crs(train_sf) <- 4326
cent_SM_sf <- st_as_sf(cent_SM, coords = c("x", "y"))
# Esto va a ser demorado!
disttrain_matrix_SM <- st_distance(x = train_sf, y = cent_SM_sf)

# Encontramos la distancia mínima a un bar
dist_min_SM <- apply(disttrain_matrix_SM, 1, min)
train$distancia_SM <- dist_min_SM
train_sf$distancia_SM <- dist_min_SM


# Distancia a colegios ----------------------------------------------------------
colegios <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "school") 
colegios_sf <- osmdata_sf(colegios)
colegios_geometria <- colegios_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada colegio
cent_colegios <- gCentroid(as(colegios_geometria$geometry, "Spatial"), byid = T)

# Ahora vamos a calcular la distancia de cada apartamento al centroide de cada colegio
train_sf <- st_as_sf(train, coords = c("lon", "lat"))
st_crs(train_sf) <- 4326
cent_colegios_sf <- st_as_sf(cent_colegios, coords = c("x", "y"))
# Esto va a ser demorado!
disttrain_matrix_colegios <- st_distance(x = train_sf, y = cent_colegios_sf)

# Encontramos la distancia mínima a un colegio
dist_min_colegios <- apply(disttrain_matrix_colegios, 1, min)
train$distancia_colegios <- dist_min_colegios
train_sf$distancia_colegios <- dist_min_colegios

# Distancia a universidades ----------------------------------------------------------
universidades <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "universities") 
universidades_sf <- osmdata_sf(universidades)
universidades_geometria <- universidades_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada universidad
cent_universidades <- gCentroid(as(universidades_geometria$geometry, "Spatial"), byid = T)

# Ahora vamos a calcular la distancia de cada apartamento al centroide de cada universidad
train_sf <- st_as_sf(train, coords = c("lon", "lat"))
st_crs(train_sf) <- 4326
cent_universidades_sf <- st_as_sf(cent_universidades, coords = c("x", "y"))
# Esto va a ser demorado!
disttrain_matrix_universidades <- st_distance(x = train_sf, y = cent_universidades_sf)

# Encontramos la distancia mínima a un universidad
dist_min_universidades <- apply(disttrain_matrix_universidades, 1, min)
train$distancia_universidades <- dist_min_universidades
train_universidades_sf$distancia_universidades <- dist_min_universidades

# Distancia a hospitales ----------------------------------------------------------
hospitales <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "hospital") 
hospitales_sf <- osmdata_sf(hospitales)
hospitales_geometria <- hospitales_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada hospital
cent_hospitales <- gCentroid(as(hospitales_geometria$geometry, "Spatial"), byid = T)

# Ahora vamos a calcular la distancia de cada apartamento al centroide de cada hospital
train_sf <- st_as_sf(train, coords = c("lon", "lat"))
st_crs(train_sf) <- 4326
cent_hospitales_sf <- st_as_sf(cent_hospitales, coords = c("x", "y"))
# Esto va a ser demorado!
disttrain_matrix_hospitales <- st_distance(x = train_sf, y = cent_hospitales_sf)

# Encontramos la distancia mínima a un hospital
dist_min_hospitales <- apply(disttrain_matrix_hospitales, 1, min)
train$distancia_hospitales <- dist_min_hospitales
train_sf$distancia_hospitales <- dist_min_hospitales


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

# Distancia a transmi --------------------------------------------------------------

# Ahora vamos a calcular la distancia de cada apartamento al centroide de estacion de transmi
test_sf <- st_as_sf(test, coords = c("lon", "lat"))
st_crs(test_sf) <- 4326
cent_transmi_sf <- st_as_sf(cent_transmi, coords = c("x", "y"))
# Esto va a ser demorado!
disttest_matrix_transmi <- st_distance(x = test_sf, y = cent_transmi_sf)

# Encontramos la distancia mínima a una estacion de transmi
disttest_min_transmi <- apply(disttest_matrix_transmi, 1, min)
test$distancia_transmi <- disttest_min_transmi
test_sf$distancia_transmi <- disttest_min_transmi
# Distancia a cai --------------------------------------------------------------

# Ahora vamos a calcular la distancia de cada apartamento al centroide de estacion de policia 
test_sf <- st_as_sf(test, coords = c("lon", "lat"))
st_crs(test_sf) <- 4326
cent_cai_sf <- st_as_sf(cent_cai, coords = c("x", "y"))
# Esto va a ser demorado!
disttest_matrix_cai <- st_distance(x = test_sf, y = cent_cai_sf)

# Encontramos la distancia mínima a una estacion de policia
disttest_min_cai <- apply(disttest_matrix_cai, 1, min)
test$distancia_cai <- disttest_min_cai
test_sf$distancia_cai <- disttest_min_cai

# Distancia a un centro comercial --------------------------------------------------------------

# Ahora vamos a calcular la distancia de cada apartamento al centroide de un centro comercial
test_sf <- st_as_sf(test, coords = c("lon", "lat"))
st_crs(test_sf) <- 4326
cent_cc_sf <- st_as_sf(cent_cc, coords = c("x", "y"))
# Esto va a ser demorado!
disttest_matrix_cc <- st_distance(x = test_sf, y = cent_cc_sf)

# Encontramos la distancia mínima a un cetro comercial
disttest_min_cc <- apply(disttest_matrix_cc, 1, min)
test$distancia_cc <- disttest_min_cc
test_sf$distancia_cc <- disttest_min_cc

# Distancia a un bar --------------------------------------------------------------

# Ahora vamos a calcular la distancia de cada apartamento al centroide de un bar
test_sf <- st_as_sf(test, coords = c("lon", "lat"))
st_crs(test_sf) <- 4326
cent_bar_sf <- st_as_sf(cent_bar, coords = c("x", "y"))
# Esto va a ser demorado!
disttest_matrix_bar <- st_distance(x = test_sf, y = cent_bar_sf)

# Encontramos la distancia mínima a un bar
disttest_min_bar <- apply(disttest_matrix_bar, 1, min)
test$distancia_bar <- disttest_min_bar
test_sf$distancia_bar <- disttest_min_bar

# Distancia a un supermercado --------------------------------------------------------------

# Ahora vamos a calcular la distancia de cada apartamento al centroide de un supermercado
test_sf <- st_as_sf(test, coords = c("lon", "lat"))
st_crs(test_sf) <- 4326
cent_SM_sf <- st_as_sf(cent_SM, coords = c("x", "y"))
# Esto va a ser demorado!
disttest_matrix_SM <- st_distance(x = test_sf, y = cent_SM_sf)

# Encontramos la distancia mínima a un supermercado
disttest_min_SM <- apply(disttest_matrix_SM, 1, min)
test$distancia_SM <- disttest_min_SM
test_sf$distancia_SM <- disttest_min_SM

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

# Distancia a colegios ----------------------------------------------------------

# Ahora vamos a calcular la distancia de cada apartamento al centroide de cada colegio
test_sf <- st_as_sf(test, coords = c("lon", "lat"))
st_crs(test_sf) <- 4326
cent_colegios_sf <- st_as_sf(cent_colegios, coords = c("x", "y"))
# Esto va a ser demorado!
disttest_matrix_colegios <- st_distance(x = test_sf, y = cent_colegios_sf)

# Encontramos la distancia mínima a un colegio
dist_min_colegios <- apply(disttest_matrix_colegios, 1, min)
test$distancia_colegios <- dist_min_colegios
test_sf$distancia_colegios <- dist_min_colegios

# Distancia a universidades ----------------------------------------------------------

# Ahora vamos a calcular la distancia de cada apartamento al centroide de cada universidad
test_sf <- st_as_sf(test, coords = c("lon", "lat"))
st_crs(test_sf) <- 4326
cent_universidades_sf <- st_as_sf(cent_universidades, coords = c("x", "y"))
# Esto va a ser demorado!
disttest_matrix_universidades <- st_distance(x = test_sf, y = cent_universidades_sf)

# Encontramos la distancia mínima a un universidad
dist_min_universidades <- apply(disttest_matrix_universidades, 1, min)
test$distancia_universidades <- dist_min_universidades
test_sf$distancia_universidades <- dist_min_universidades

# Distancia a hospitales ----------------------------------------------------------
hospitales <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "hospital") 
hospitales_sf <- osmdata_sf(hospitales)
hospitales_geometria <- hospitales_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada hospital
cent_hospitales <- gCentroid(as(hospitales_geometria$geometry, "Spatial"), byid = T)

# Ahora vamos a calcular la distancia de cada apartamento al centroide de cada hospital
test_sf <- st_as_sf(test, coords = c("lon", "lat"))
st_crs(test_sf) <- 4326
cent_hospitales_sf <- st_as_sf(cent_hospitales, coords = c("x", "y"))
# Esto va a ser demorado!
disttest_matrix_hospitales <- st_distance(x = test_sf, y = cent_hospitales_sf)

# Encontramos la distancia mínima a un hospital
dist_min_hospitales <- apply(disttest_matrix_hospitales, 1, min)
test$distancia_hospitales <- dist_min_hospitales
test_sf$distancia_hospitales <- dist_min_hospitales

# 2.4 EXPORTAR LAS BASES DE DATOS FINALES -------------------------------------------- #t
# Train
train_final <- as.data.frame(train)
write.csv(train_final,"./data/train_final")
# Test
test_final <- as.data.frame(test)
write.csv(test_final,"./data/test_final")

# ------------------------------------------------------------------------------------ #
# 3. Estadísticas descriptivas
# ------------------------------------------------------------------------------------ #




