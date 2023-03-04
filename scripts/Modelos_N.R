#**************************************************************************************#
#                                    TALLER 3 BDML                                     #
#                        Uniandes: Sofia Charry Tobar                                  #
#                                  Laura Manuela Rodríguez Morales                     #
#                                  Nicol Valeria Rodríguez Rodríguez                   #
#                                  Brayan Alexander Vargas Rojas                       #
#                          Fuente: Properati                                           #
#**************************************************************************************#

# Limpiar el espacio
rm(list = ls(all.names = TRUE))

# ------------------------------------------------------------------------------------ #
# Cargar librerias.
# ------------------------------------------------------------------------------------ #

setwd("C:/Users/nicol/Documents/GitHub/Repositorios/Taller-3-BDML")

list.of.packages = c("pacman", "readr","tidyverse", "dplyr", "arsenal", "fastDummies", 
                     "caret", "glmnet", "MLmetrics", "skimr", "plyr", "stargazer", 
                     "ggplot2", "plotly", "corrplot", "Hmisc", "sf", "tmaptools", 
                     "osmdata", "leaflet", "rgeos", "yardstick")


new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
sapply(list.of.packages, require, character.only = TRUE)

# ------------------------------------------------------------------------------------ #
# 1. Descripción del problema
# ------------------------------------------------------------------------------------ #

# ------------------------------------------------------------------------------------ #
# 2. Data
# ------------------------------------------------------------------------------------ #

# Train
train_bog <- read_csv("./data/train_final.csv")
train_bog$cat_parqueadero <- as.factor(train_bog$cat_parqueadero)
attach(train_bog)
# Test 
test_bog <- read_csv("./data/test_final.csv")
test_bog$cat_parqueadero <- as.factor(test_bog$cat_parqueadero)
attach(test_bog)

# ------------------------------------------------------------------------------------ #
# 3. Modelos
# ------------------------------------------------------------------------------------ #

set.seed(0000)

# Partición de la base de datos train, con el objetivo de evaluar el performance de los modelos.
inTrain <- createDataPartition(
  y = train_bog$price,## Nuestra  
  p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)

training <- train_bog[ inTrain,] # Set de datos de entrenamiento
testing  <- train_bog[-inTrain,] # Set de datos de evaluación
nrow(training) # El conjunto de entrenamiento contiene el 70% de la base original (115473/164960*100)

# Cross-validation
ctrl <- trainControl(
  method = "cv", 
  number = 10) # número de folds

# Fórmula de los modelos
fmla <- formula(price~Chapinero+property_type+terraza+social+parqueadero+
                  distancia_parque+distancia_gym+distancia_transmi+distancia_cai+distancia_cc+distancia_bar+distancia_SM+
                  distancia_colegios+distancia_universidades+distancia_hospitales)

### 3.1 Modelo benchmark: regresión lineal ------------------------------------------------------------
ModeloRL <- train(fmla, 
                  data = training, method = 'lm',
                  trControl= ctrl,
                  preProcess = c("center", "scale"), 
                  na.action = na.pass)

## Predicción 1: Predicciones con hog_testing
pred_test1_ModeloRL <- predict(ModeloRL, newdata = testing) # Predicción
eva_ModeloRL <- data.frame(obs=testing$price, pred=pred_test1_ModeloRL) # Data frame con observados y predicciones
metrics_ModeloRL <- metrics(eva_ModeloRL, obs, pred); metrics_ModeloRL # Cálculo del medidas de precisión

## Predicción 2: Predicciones con test_hogares
pred_test2_ModeloRL <- predict(ModeloRL, newdata = test_bog)

# Exportar para prueba en Kaggle
Kaggle_ModeloRL <- data.frame(property_id=test_bog$property_id, price=pred_test2_ModeloRL)
write.csv(Kaggle_ModeloRL,"./stores/Kaggle_ModeloRL_N.csv", row.names = FALSE)
# RMSE = 327172092.77510

### 3.2 Lasso -----------------------------------------------------------------------------------------

Modelolasso<-train(fmla,
                   data=training,
                   method = 'glmnet', 
                   trControl = ctrl,
                   tuneGrid = expand.grid(alpha = 1, #lasso
                                          lambda = seq(0.001,0.1,by = 0.001)),
                   preProcess = c("center", "scale")
) 

summary(Modelolasso) # Resumen del modelo
coef_lasso<-coef(Modelolasso$finalModel, Modelolasso$bestTune$lambda)
coef_lasso

## Predicción 1: Predicciones con hog_testing
pred_test1_Modelolasso <- predict(Modelolasso, newdata = testing) # Predicción
eva_Modelolasso <- data.frame(obs=testing$price, pred=pred_test1_Modelolasso) # Data frame con observados y predicciones
metrics_Modelolasso <- metrics(eva_Modelolasso, obs, pred); metrics_Modelolasso # Cálculo del medidas de precisión

## Predicción 2: Predicciones con test_hogares
pred_test2_Modelolasso <- predict(Modelolasso, newdata = test_bog)

# Exportar para prueba en Kaggle
Kaggle_Modelolasso <- data.frame(property_id=test_bog$property_id, price=pred_test2_Modelolasso)
write.csv(Kaggle_Modelolasso,"./stores/Kaggle_ModeloLS_N.csv", row.names = FALSE)

# RMSE: 327258864.85935

### 3.3 Elastic net -----------------------------------------------------------------------------------
ModeloEN<-caret::train(fmla,
                       data=training,
                       method = 'glmnet', 
                       trControl = ctrl,
                       tuneGrid = expand.grid(alpha = seq(0,1,by = 0.01), #Lasso
                                              lambda = seq(0.001,0.1,by = 0.001)),
                       preProcess = c("center", "scale")
) 

summary(ModeloEN) # Resumen del modelo
ggplot(varImp(ModeloEN)) # Gráfico de importancia de las variables
ModeloEN$bestTune

## Predicción 1: Predicciones con hog_testing
pred_test1_ModeloEN <- predict(ModeloEN, newdata = testing) # Predicción
eva_ModeloEN <- data.frame(obs=testing$price, pred=pred_test1_ModeloEN) # Data frame con observados y predicciones
metrics_ModeloEN <- metrics(eva_ModeloEN, obs, pred); metrics_ModeloEN # Cálculo del medidas de precisión

## Predicción 2: Predicciones con test_hogares
pred_test2_ModeloEN <- predict(ModeloEN, newdata = test_bog)

# Exportar para prueba en Kaggle
Kaggle_ModeloEN <- data.frame(property_id=test_bog$property_id, price=pred_test2_ModeloEN)
write.csv(Kaggle_ModeloEN,"./stores/Kaggle_ModeloEN_N.csv", row.names = FALSE)
# Accuracy: 0.75462

### 3.4 Random Forest ---------------------------------------------------------------------------------

# fmla_RF <- formula(Ingtotug~P5000+P5010+P5090+Nper+Npersug+Depto+prop_P6585s1h+prop_P6585s3h+prop_Desh+prop_contributivo+
#                      prop_media+prop_superior+prop_mayoriatiempotrabajo+prop_obreroemplempresa+prop_obreroemplgobierno+prop_empldomestico+
#                      prop_trabajadorcuentapropia+prop_patronempleador)
# ctrl_RF <- trainControl(method = "cv",
#                     number = 10, # Es recomendable correr 10
#                     )
# 
# #### Hiperparámetros
# mtry_grid <- expand.grid(mtry = seq(1, 18, 2))
# mtry_grid
# 
# ModeloRF <- caret::train(fmla_RF, 
#                 data = hog_training, 
#                 method = 'rf',
#                 trControl = ctrl_RF,
#                 metric="RMSE",
#                 tuneGrid = mtry_grid,
#                 preProcess = c("center", "scale"),
#                 ntree=500)
# 
# ModeloRF #mtry es el número de predictores.
# plot(ModeloRF)
# ModeloRF$finalModel
# 
# ### Variable Importance
# varImp(ModeloRF,scale=TRUE)
# 
# ## Predicción 1: Predicciones con hog_testing
# pred_test1_ModeloRF <- predict(ModeloRF, newdata = hog_testing, type="raw")
# eva_ModeloRF <- data.frame(obs=hog_testing$Ingtotug, pred=pred_test1_ModeloRF) # Data frame con observados y predicciones
# metrics_ModeloRF <- metrics(eva_ModeloRF, obs, pred); metrics_ModeloRF # Cálculo del medidas de precisión
# 
# # Identificación de pobres y no pobres en hog_testing
# pob1_ModeloRF <- ifelse(pred_test1_ModeloRF<hog_testing$Lp, 1, 0)
# 
# # Evaluación de clasificación
# eva_ModeloRF <- data.frame(obs=as.factor(hog_testing$Pobre), pred=as.factor(pob1_ModeloRF)) # Data frame con observados y predicciones
# confmatrix_ModeloRF <- confusionMatrix(data = as.factor(pob1_ModeloRF), reference = as.factor(hog_testing$Pobre)) ; confmatrix_ModeloRF # Matriz de confusión
# 
# ## Predicción 2: Predicciones con test_hogares
# pred_test2_ModeloRF <- predict(ModeloRF, newdata = test_hogares)
# 
# # Identificación de pobres y no pobres en test_hogares
# pob2_ModeloRF <- ifelse(pred_test2_ModeloRF<test_hogares$Lp, 1, 0)
# 
# # Exportar para prueba en Kaggle
# Kaggle_ModeloEN <- data.frame(id=test_hogares$id, pobre=pob2_ModeloRF)
# write.csv(Kaggle_ModeloRF,"./stores/Kaggle_ModeloRF.csv", row.names = FALSE)

### 3.6 GBM -------------------------------------------------------------------------------------------
p_load(gbm)
grid_gbm<-expand.grid(n.trees=c(300,700,1000),interaction.depth=c(1:4),shrinkage=c(0.01,0.001),n.minobsinnode
                      =c(10,30))

ModeloGBM <- train(fmla,
                   data = training, 
                   method = "gbm", 
                   trControl = ctrl,
                   tuneGrid=grid_gbm,
                   metric = "RMSE"
)            

ModeloGBM #mtry es el número de predictores.
plot(ModeloGBM)
ModeloGBM$finalModel

### Variable Importance
plot(varImp(ModeloGBM,scale=TRUE))

## Predicción 1: Predicciones con hog_testing
pred_test1_ModeloGBM <- predict(ModeloGBM, newdata = testing, type="raw")
eva_ModeloGBM <- data.frame(obs=testing$price, pred=pred_test1_ModeloGBM) # Data frame con observados y predicciones
metrics_ModeloGBM <- metrics(eva_ModeloGBM, obs, pred); metrics_ModeloGBM # Cálculo del medidas de precisión

## Predicción 2: Predicciones con test_hogares
pred_test2_ModeloGBM <- predict(ModeloGBM, newdata = test_bog)

# Exportar para prueba en Kaggle
Kaggle_ModeloGBM <- data.frame(property_id=test_bog$property_id, pobre=pred_test2_ModeloGBM)
write.csv(Kaggle_ModeloGBM,"./stores/Kaggle_ModeloGBM_N.csv", row.names = FALSE)

