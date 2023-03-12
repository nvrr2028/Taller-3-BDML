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

#setwd("C:/Users/nicol/Documents/GitHub/Repositorios/Taller-3-BDML")
setwd("/Users/bray/Desktop/Big Data/Talleres/Taller-3-BDML")

list.of.packages = c("pacman", "readr","tidyverse", "dplyr", "arsenal", "fastDummies", 
                     "caret", "glmnet", "MLmetrics", "skimr", "plyr", "stargazer", 
                     "ggplot2", "plotly", "corrplot", "Hmisc", "sf", "tmaptools", 
                     "osmdata", "leaflet", "rgeos", "yardstick", "SuperLearner", 
                     "adabag")

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

# Ajustes adicionales de las bases de datos
lm_surface <- lm(surface_covered ~ bedrooms + property_type, data = train_bog)
train_bog$surface_covered2 <- ifelse(is.na(train_bog$surface_covered), 
                                     coef(lm_surface)[2]*train_bog$bedrooms+coef(lm_surface)[3]*train_bog$property_type, 
                                     train_bog$surface_covered)
test_bog$surface_covered2 <- ifelse(is.na(test_bog$surface_covered), 
                                    coef(lm_surface)[2]*test_bog$bedrooms+coef(lm_surface)[3]*test_bog$property_type, 
                                    test_bog$surface_covered)
lm_bathroom <- lm(bathrooms ~ bedrooms + property_type, data = train_bog)
train_bog$bathrooms2 <- ifelse(is.na(train_bog$bathrooms), 
                                     coef(lm_bathroom)[2]*train_bog$bedrooms+coef(lm_bathroom)[3]*train_bog$property_type, 
                                     train_bog$bathrooms)
test_bog$bathrooms2 <- ifelse(is.na(test_bog$bathrooms), 
                              coef(lm_bathroom)[2]*test_bog$bedrooms+coef(lm_bathroom)[3]*test_bog$property_type, 
                              test_bog$bathrooms)
# ------------------------------------------------------------------------------------ #
# 3. Estadísticas descriptivas
# ------------------------------------------------------------------------------------ #

stargazer(train_bog, type = "latex", title = "Estadísticas descriptivas", align = TRUE)

base <- train_bog %>%
  select(price,surface_covered2,bedrooms,bathrooms2,Chapinero,property_type,terraza,parqueadero,
         distancia_parque,distancia_gym,distancia_transmi,distancia_cai,distancia_cc,distancia_bar,distancia_SM,
         distancia_colegios,distancia_universidades,distancia_hospitales) %>% # Seleccionar variables de interés
  drop_na()
any(is.na(base)) # No hay datos vacíos

estadisticas10 <- base %>%
  summarise(across(c(price,surface_covered2,bedrooms,bathrooms2,Chapinero,property_type,terraza,parqueadero,
                     distancia_parque,distancia_gym,distancia_transmi,distancia_cai,distancia_cc,distancia_bar,distancia_SM,
                     distancia_colegios,distancia_universidades,distancia_hospitales), 
                   list(media = mean, desv_est = sd, minimo = min, maximo = max)))

stats <- sapply(base[, c("price","surface_covered2", "bedrooms", "bathrooms2", "Chapinero", "property_type", "terraza", "parqueadero",
                              "distancia_parque", "distancia_gym", "distancia_transmi", "distancia_cai", "distancia_cc",
                              "distancia_bar", "distancia_SM", "distancia_colegios", "distancia_universidades", "distancia_hospitales")],
                summary)

stats_df <- as.data.frame(t(stats))
stargazer(stats_df, type = "latex", title = "Estadísticas descriptivas", align = TRUE)
## correlaicones  ##

corrm <- base
res2 <- rcorr(as.matrix(corrm)) # Coeficientes de correlación
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$p, sig.level = 0.05, insig = "blank", tl.col="black") # Las correlaciones no signitificativas se eliminan

## medias de las distancias ##
media_distancia_parque <- mean(base$distancia_parque)
media_distancia_gym <- mean(base$distancia_gym)
media_distancia_transmi <- mean(base$distancia_transmi)
media_distancia_cai <- mean(base$distancia_cai)
media_distancia_cc <- mean(base$distancia_cc)
media_distancia_bar <- mean(base$distancia_bar)
media_distancia_SM <- mean(base$distancia_SM)
media_distancia_colegios <- mean(base$distancia_colegios)
media_distancia_universidades <- mean(base$distancia_universidades)
media_distancia_hospitales <- mean(base$distancia_hospitales)

# Calcular medias
medias <- c(media_distancia_parque, media_distancia_gym, media_distancia_transmi, media_distancia_cai, media_distancia_cc, media_distancia_bar, media_distancia_SM, media_distancia_colegios, media_distancia_universidades, media_distancia_hospitales)

# Asignar nombres a las variables
nombres <- c("distancia_parque", "distancia_gym", "distancia_transmi", "distancia_cai", "distancia_cc", "distancia_bar", "distancia_SM", "distancia_colegios", "distancia_universidades", "distancia_hospitales")

# Reemplazar guiones bajos por espacios y convertir en títulos
nombres_arreglados <- sapply(nombres, function(x) {
  x <- gsub("_", " ", x)
  x <- str_to_title(x)
  return(x)
})

# Colores personalizados
colores <- c("#B0C4DE", "#CAE1FF", "#BCD2EE", "#A2B5CD", "#6E7B8B", "#607B8B", "#8DB6CD", "#A4D3EE", "#B0E2FF", "#87CEFA")

# Graficar
ggplot(data = data.frame(nombres_arreglados, medias), aes(x = nombres_arreglados, y = medias, fill = nombres_arreglados)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(medias, 2)), vjust = -0.5) + # Agregar etiquetas con los valores de las medias
  labs(x = "", y = "Media", title = "Media de Distancias a Servicios") +
  scale_fill_manual(values = rev(colores)) + # Invertir los colores para que vayan de oscuro a claro
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        legend.title = element_blank())
### CHAPINERO ####
# Convertir la variable Chapinero a factor
base$Chapinero <- as.factor(base$Chapinero)

# Calcular la media de precios por cada valor de la variable Chapinero
media_precio <- aggregate(base$price, by=list(base$Chapinero), FUN=mean)

# Generar el gráfico
ggplot(media_precio, aes(x=Group.1, y=x, fill=Group.1)) + 
  geom_bar(stat="identity") +
  labs(x="Barrio", y="Precio promedio") +
  ggtitle("Precio promedio en Chapinero vs Otros barrios") +
  scale_fill_manual(values = c("#69b3a2", "#404080"))

### Numero de viviendas con terraza y parqueadero 
parq_count <- table(base$parqueadero)
terraza_count <- table(base$terraza)

# Crear un data frame con los resultados
df_count <- data.frame(variable = c(rep("parqueadero", length(parq_count)),
                                    rep("terraza", length(terraza_count))),
                       valor = c(as.numeric(parq_count), as.numeric(terraza_count)),
                       tipo = factor(rep(c("0", "1"), length(parq_count) + length(terraza_count))))

# Graficar el número total de 0 y 1 en cada variable

parq_count <- table(base$parqueadero)
terraza_count <- table(base$terraza)

# Crear un data frame con los resultados
df_count <- data.frame(variable = c(rep("parqueadero", length(parq_count)),
                                    rep("terraza", length(terraza_count))),
                       valor = c(as.numeric(parq_count), as.numeric(terraza_count)),
                       Valor = factor(rep(c("0", "1"), length(parq_count) + length(terraza_count))))

# Graficar el número total de 0 y 1 en cada variable
ggplot(df_count, aes(x = variable, y = valor, fill = Valor)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#009ACD", "#00688B")) +
  labs(x = "", y = "Número de observaciones",
       fill = "") +
  ggtitle("Conteo de valores en variables dummy") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))

# ------------------------------------------------------------------------------------ #
# 4. Modelos
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
fmla <- formula(price~surface_covered2+bedrooms+bathrooms2+Chapinero+property_type+terraza+social+parqueadero+
                  distancia_parque+distancia_gym+distancia_transmi+distancia_cai+distancia_cc+distancia_bar+distancia_SM+
                  distancia_colegios+distancia_universidades+distancia_hospitales)

### 3.1 Modelo benchmark: regresión lineal ------------------------------------------------------------
ModeloRL <- train(fmla, 
                  data = training, method = 'lm',
                  trControl= ctrl,
                  preProcess = c("center", "scale"),
                  metric= "MAE",
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
# MAE = 327172092.77510

### 3.2 Lasso -----------------------------------------------------------------------------------------

Modelolasso<-train(fmla,
                   data=training,
                   method = 'glmnet', 
                   trControl = ctrl,
                   tuneGrid = expand.grid(alpha = 1, #lasso
                                          lambda = seq(0.001,0.1,by = 0.001)),
                   preProcess = c("center", "scale"), 
                   metric = "MAE"
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

# MAE: 327258864.85935

### 3.3 Elastic net -----------------------------------------------------------------------------------
ModeloEN<-caret::train(fmla,
                       data=training,
                       method = 'glmnet', 
                       trControl = ctrl,
                       tuneGrid = expand.grid(alpha = seq(0,1,by = 0.01), #Lasso
                                              lambda = seq(0.001,0.1,by = 0.001)),
                       preProcess = c("center", "scale"), 
                       metric = "MAE"
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
# MAE: 298404785.01076

### 3.4 GBM -------------------------------------------------------------------------------------------
p_load(gbm)
grid_gbm<-expand.grid(n.trees=c(300,700,1000),interaction.depth=c(1:4),shrinkage=seq(0.001,0.1,by = 0.001),n.minobsinnode
                      =c(10,30, 40))

ModeloGBM <- train(fmla,
                   data = training, 
                   method = "gbm", 
                   trControl = ctrl,
                   tuneGrid=grid_gbm,
                   metric = "MAE"
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
Kaggle_ModeloGBM <- data.frame(property_id=test_bog$property_id, price=pred_test2_ModeloGBM)
write.csv(Kaggle_ModeloGBM,"./stores/Kaggle_ModeloGBM_N.csv", row.names = FALSE)
# MAE: 263245024.82436

### 3.5 Ridge -------------------------------------------------------------------------------------------
grid=10^seq(50,-50,length=1000)

ModeloRidge<- train(fmla,
                    data = training,
                    method = 'glmnet', 
                    tuneGrid = expand.grid(alpha = 0, lambda = grid), 
                    preProcess = c("center", "scale")
)

## Predicción 1: Predicciones con testing
pred_test1_ModeloRidge <- predict(ModeloRidge, newdata = testing, type="raw")
eva_ModeloRidge <- data.frame(obs=testing$price, pred=pred_test1_ModeloRidge) # Data frame con observados y predicciones
metrics_ModeloRidge <- metrics(eva_ModeloRidge, obs, pred); metrics_ModeloRidge # Cálculo del medidas de precisión

## Predicción 2: Predicciones con test_bog
pred_test2_ModeloRidge <- predict(ModeloRidge, newdata = test_bog)

# Exportar para prueba en Kaggle
Kaggle_ModeloRidge <- data.frame(property_id=test_bog$property_id, price=pred_test2_ModeloRidge)
write.csv(Kaggle_ModeloRidge,"./stores/Kaggle_ModeloRidge_N.csv", row.names = FALSE)
# MAE: 299411431.75789

### 3.5 Superlearner -------------------------------------------------------------------------------------------
p_load("SuperLearner")
ySL<-training$price
XSL<- training  %>% select(surface_covered2,bedrooms,bathrooms2,Chapinero,property_type,terraza,social,parqueadero,
                        distancia_parque,distancia_gym,distancia_transmi,distancia_cai,distancia_cc,distancia_bar,distancia_SM,
                        distancia_colegios,distancia_universidades,distancia_hospitales)

sl.lib <- c("SL.glmnet", "SL.lm", "SL.ridge", "SL.gbm" ) #lista de los algoritmos a correr

# Fit using the SuperLearner package,
ModeloSL <- SuperLearner(Y = ySL,  X= data.frame(XSL),
                     method = "method.NNLS", # combinación convexa
                     SL.library = sl.lib)

ModeloSL

## Predicción 1: Predicciones con testing
testing <- testing  %>%  mutate(yhat_Sup=predict(ModeloSL, newdata = data.frame(testing), onlySL = T)$pred)
pred_test1_ModeloSL <- testing$yhat_Sup
eva_ModeloSL <- data.frame(obs=testing$price, pred=pred_test1_ModeloSL) # Data frame con observados y predicciones
metrics_ModeloSL <- metrics(eva_ModeloSL, obs, pred); metrics_ModeloSL # Cálculo del medidas de precisión

## Predicción 2: Predicciones con test_bog
test_bog <- test_bog  %>%  mutate(yhat_Sup=predict(ModeloSL, newdata = data.frame(test_bog), onlySL = T)$pred)
pred_test2_ModeloSL <- test_bog$yhat_Sup

# Exportar para prueba en Kaggle
Kaggle_ModeloSL <- data.frame(property_id=test_bog$property_id, price=pred_test2_ModeloSL)
write.csv(Kaggle_ModeloSL,"./stores/Kaggle_ModeloSL_N.csv", row.names = FALSE)
# MAE: 279848874.92923

################################   BRAY       ####################################
p_load("SuperLearner")
sl.lib <- c("SL.glmnet", "SL.lm", "Sl.ridge") #lista de los algoritmos a correr

# Fit using the SuperLearner package,

Super1 <- SuperLearner(Y = ySL,  X= data.frame(XSL),
                     method = "method.NNLS", # combinación convexa
                     SL.library = sl.lib)

Super1

## NET 
# Customize the defaults for random forest.
custon_glmnet = create.Learner("SL.glmnet", tune = list(alpha = seq(0, 1, length.out=5)))

# Look at the object.
custon_glmnet$names


sl.net <- c("SL.glmnet_1", "SL.glmnet_2", "SL.glmnet_3", "SL.glmnet_4", "SL.glmnet_5") #lista de los algoritmos a correr

# Fit using the SuperLearner package,

Super2 <- SuperLearner(Y = ySL,  X= data.frame(XSL),
                       method = "method.NNLS", # combinación convexa
                       SL.library = sl.net)

Super2

## Predicción 1: Predicciones con testing
testing <- testing  %>%  mutate(yhat_Sup=predict(Super2, newdata = data.frame(testing), onlySL = T)$pred)
pred_test1_Super2 <- testing$yhat_Sup
eva_ModeloSuper2 <- data.frame(obs=testing$price, pred=pred_test1_ModeloSuper2) # Data frame con observados y predicciones
metrics_ModeloSuper2 <- metrics(eva_ModeloSuper2, obs, pred); metrics_ModeloSuper2 # Cálculo del medidas de precisión

## Predicción 2: Predicciones con test_bog
test_bog <- test_bog  %>%  mutate(yhat_Sup=predict(ModeloSuper2, newdata = data.frame(test_bog), onlySL = T)$pred)
pred_test2_ModeloSuper2 <- test_bog$yhat_Sup

# Exportar para prueba en Kaggle
Kaggle_ModeloSuper2 <- data.frame(property_id=test_bog$property_id, price=pred_test2_ModeloSuper2)
write.csv(Kaggle_ModeloSuper2,"./stores/Kaggle_ModeloSuper2_N.csv", row.names = FALSE)








