#**************************************************************************************#
#                                    Taller-3-BDML                                     #
#                        Uniandes: Sofia Charry Tobar                                  #
#                                  Laura Manuela Rodríguez Morales                     #
#                                  Nicol Valeria Rodríguez Rodríguez                   #
#                                  Brayan Alexander Vargas Rojas                       #
#                          Fuente: Properati y Open Source Maps (OSM)                  #
#**************************************************************************************#


# Making Money with ML?

# Abstract

El objetivo del ejercicio es predecir los precios de venta de las viviendas para apoyar a la compañía en su toma de decisiones de compra en Chapinero.  
Se utiliza la metodología de precios hedónicos con características de la infraestructura de la vivienda y cercanía a servicios que abastecen necesidades
como la salud, seguridad, educación, abastecimiento, entre otras. Se realizan varios modelos de Machine Learning y se prefiere un modelo de Gradient Boosting
por su mejor desempeño en la predicción. Se recomienda que se actualicen los datos del modelo cuando sea posible para que capture las condiciones del mercado
inmobiliario en el momento de interés. Se concluye que (...)

# Document

La carpeta de "document" contiene el documento final con el desarrollo del ejercicio.

# Data files

Los datos fueron obtenidos del sitio oficial de la competición en "Kaggle" https://www.kaggle.com/competitions/uniandes-bdml-20231-ps3/data y de Open Street Maps (OSM).
Además, se hicieron múltiples procedimientos para la obtención de la base final, los cuales están consigados en el código "DATA" en la carpeta de "scripts".
Asímismo, las variables se describen en el anexo 1 del documento pdf final en la carpeta "document".

# Code files

El ejercicio de predicción de los precios se desarrolla en R version 4.2.2 (2022-10-31 ucrt).
Los códigos para la corrida del ejercicio se encuentran almacenados en la carpeta "scripts", archivo "Modelos_N". 
Por lo tanto, la carpeta de "scripts" contiene: 
- DATA: El código en el script "Data" contiene nuevamente el procesamiento de las bases de datos, para facilidad en el uso de los autores.
- ESTADÍSTICAS DESCRIPTIVAS: El código en el script "Modelos_N" contiene estadísticas descriptivas.
- MODELOS: El código en el script "Modelos_N" contiene los modelos utilizados: Modelo principal, Ridge, Lasso, Elastic Net, GBM y Superlearner.

# Graphs

Todas las gráficas se pueden encontrar en la carpeta "views".

# Data dictionary

# Variable explicada Y 
Price: es el precio de venta del ofertante. Está tomada de la base de los datos recopilados de https://www.properati.com.co.

# Lista de posibles variables explicativas: 

Month: es el mes de publicación de la oferta en https://www.properati.com.co.

Year: es el año de publicación de la oferta en https://www.properati.com.co.

Surface_total: es el área total del terreno en el que se construyó la casa, incluyendo áreas cubiertas y descubiertas, por lo que incluye cualquier área exterior, como un jardín, una entrada de autos o un garaje. Se construyó con extracción de texto de los datos recopilados de https://www.properati.com.co.

Surface_covered: es el área total cubierta de los espacios construidos dentro de la casa. Da cuenta del espacio habitable real dentro de la casa. Se construyó con extracción de texto de los datos recopilados de https://www.properati.com.co.

Rooms: es la cantidad de habitaciones dentro de la propiedad. Es parte de las variables pre-existentes en la base de los datos recopilados de https://www.properati.com.co.

Bedrooms: es la cantidad de cuartos para dormir dentro de la propiedad. Es parte de las variables pre-existentes en la base de los datos recopilados de https://www.properati.com.co.

Bathrooms: es la cantidad de baños dentro de la propiedad. Es parte de las variables pre-existentes en la base de los datos recopilados de https://www.properati.com.co.

Property_type: es el tipo de vivienda. Toma el valor de 1 si es una casa y de 0 si es un apartamento. Es parte de las variables pre-existentes en la base de los datos recopilados de https://www.properati.com.co.

Lat: es la latitud de la coordenada de la vivienda. Es parte de las variables de Open Source Maps (OSM).

Lon: es la longitud de la coordenada de la vivienda. Es parte de las variables de Open Source Maps (OSM).

Chapinero: toma el valor de 1 si la vivienda hace parte de Chapinero y 0 de lo contrario. Se construyó con extracción de texto de los datos recopilados de https://www.properati.com.co. La extracción se programó con múltiples posibles variaciones de los nombres de los barrios de Chapinero.

Terraza: toma el valor de 1 si la vivienda tiene terraza y 0 de lo contrario. Se construyó con extracción de texto de los datos recopilados de https://www.properati.com.co. La extracción se programó con múltiples posibles variaciones para referirse a una terraza, como balcón, balcón o mirador.

Social: toma el valor de 1 si la vivienda cuenta con espacios sociales y 0 de lo contrario. Se construyó con extracción de texto de los datos recopilados de https://www.properati.com.co. La extracción se programó con múltiples posibles variaciones para referirse a espacios sociales, como BBQ, piscina, salón comunal, entre otros

Parqueadero: toma el valor de 1 si la vivienda cuenta con parqueadero y 0 de lo contrario. Se construyó con extracción de texto de los datos recopilados de https://www.properati.com.co. La extracción se programó con múltiples posibles variaciones para referirse a parqueaderos, como cochera o garaje.
Distancia_parque: es la distancia en metros de la vivienda al parque más cercano. Se construyó utilizando la ubicación de parques de Open Source Maps (OSM).
Distancia_gym: es la distancia en metros de la vivienda al gimnasio más cercano. Se construyó utilizando la ubicación de gimnasios de Open Source Maps (OSM).
Distancia_transmi: es la distancia en metros de la vivienda a la estación de bus más cercana. Se construyó utilizando la ubicación de las estaciones de bus de Open Source Maps (OSM).

Distancia_cai: es la distancia en metros de la vivienda al Centro de Atención Inmediata (CAI) más cercano. Se construyó utilizando la ubicación de los police ammenities de Open Source Maps (OSM).

Distancia_gym: es la distancia en metros de la vivienda al centro de entrenamiento físico más cercano. Se construyó utilizando la ubicación de centro de entrenamiento físico de Open Source Maps (OSM).

Distancia_bar: es la distancia en metros de la vivienda al bar más cercano. Se construyó utilizando la ubicación de bares de Open Source Maps (OSM).

Distancia_SM: es la distancia en metros de la vivienda al supermercado más cercano. Se construyó utilizando la ubicación de supermercados de Open Source Maps (OSM).

Distancia_colegios: es la distancia en metros de la vivienda al colegio más cercano. Se construyó utilizando la ubicación de colegios de Open Source Maps (OSM).

Distancia_universidades: es la distancia en metros de la vivienda a la universidad más cercana. Se construyó utilizando la ubicación de universidades de Open Source Maps (OSM).

Distancia_hospitales: es la distancia en metros de la vivienda al hospital más cercano. Se construyó utilizando la ubicación de hospitales de Open Source Maps (OSM).

Surface_covered2: es la superficie cubierta luego de recuperar los valores faltantes con las estimaciones de regresiones lineales en función del número de cuartos para dormir y el tipo de propiedad (si es o no una casa). Se creó añadiendo las estimaciones de las regresiones a la variable de superficie cubierta pre-existente en la base de los datos recopilados de https://www.properati.com.co utilizando dos variables independientes de esta misma base de datos.

Bathrooms2: es la cantidad de baño luego de recuperar los valores faltantes con las estimaciones de regresiones lineales en función del número de cuartos para dormir y el tipo de propiedad (si es o no una casa). Se creó añadiendo las estimaciones de las regresiones a la variable de bañ pre-existente en la base de los datos recopilados de https://www.properati.com.co utilizando dos variables independientes de esta misma base de datos
