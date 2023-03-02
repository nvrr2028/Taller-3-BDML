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



list.of.packages = c("pacman", "readr","tidyverse", "dplyr", "arsenal", "fastDummies", 
                     "caret", "glmnet", "MLmetrics", "skimr", "plyr", "stargazer", 
                     "ggplot2", "corrplot", "Hmisc")

new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
sapply(list.of.packages, require, character.only = TRUE)

# Precio 
precio_bog <- read.csv("./data/submission_template.csv")
# Train
train_bog <- read_csv("./data/train.csv")
# Test 
test_bog <- read_csv("./data/test.csv")


chapinero<-opq(bbox = getbb("Chapinero Bogotá Colombia"))
chapinero

#chapinero_bbox <- c(-74.066598, 4.631753, -74.061713, 4.658588)
"chapinero_osm <- osmdata::osmdata_download(bbox = chapinero_bbox, timeout = 60)














