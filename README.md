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
#      Ingtotug (continua) - ingreso total de la unidad de gasto antes de imputación de arriendo a propietarios y usufructuarios.
#      Pobre (discreta) - Pobre=1 No pobre=0

# Lista de posibles variables explicativas: 
#      P5000 (discreta) - Incluyendo sala-comedor ¿de cuántos cuartos en total dispone este hogar?
#      P5010 (discreta) - ¿En cuántos de esos cuartos duermen las personas de este hogar?
#      P5090 (discreta) - La vivienda ocupada por este hogar es
#      P5130 (continua) - Si tuviera que pagar arriendo por esta vivienda, ¿cuánto estima que tendría que pagar mensualmente?
#      P5140 (continua) - ¿Cuánto pagan mensualmente por arriendo?
#      Nper (continua) - Personas en el hogar
#      Npersug (continua) - Número de personas en la unidad de gasto
#      Depto (discreta) - Departamento
#      prop_P6585s1h - Proporción de personas que recibieron un auxilio de alimentación en el hogar
#      prop_P6585s3h - Proporción de personas que recibieron un auxilio familiar en el hogar
#      prop_P7510s3h - Proporción de personas que recibieron dinero de instituciones en el hogar.
#      prop_P7505h - Proporción de personas que recibieron dinero de otros hogares en el hogar.
#      prop_P6920h - Proporción de personas en el hogar que estarían cotizando a pensión.
#      prop_Desh - Proporción de personas en el hogar desempleadas.
#      prop_Och - Proporción de personas en el hogar empleadas.  
#      Npobres (continua) - Número de pobres
#      prop_subsidiado - Proporción de personas en el hogar cotizantes al regimen subsidiado.                
#      prop_contributivo - Proporción de personas en el hogar cotizantes al regimen contributivo.    
#      prop_especial - Proporción de personas en el hogar cotizantes al regimen especial.              
#      prop_ningunoeduc - Proporción de personas en el hogar con ningún nivel de educación.                 
#      prop_preescolar - Proporción de personas en el hogar con nivel de educación preescolar.                 
#      prop_basicaprimaria - Proporción de personas en el hogar con nivel de educación básica primaria.            
#      prop_basicasecundaria - Proporción de personas en el hogar con nivel de educación básica secundaria.           
#      prop_media - Proporción de personas en el hogar con nivel de educación media.                 
#      prop_superior - Proporción de personas en el hogar con nivel de educación superior.            
#      prop_mayoriatiempotrabajo - Proporción de personas en el hogar que pasaron la mayor parte del tiempo trabajando.       
#      prop_mayoriatiempobuscandotrabajo - Proporción de personas en el hogar que pasaron la mayor parte del tiempo buscando trabajo.  
#      prop_mayoriatiempoestudiando - Proporción de personas en el hogar que pasaron la mayor parte del tiempo estudiando.
#      prop_mayoriatiempooficiohogar - Proporción de personas en el hogar que pasaron la mayor parte del tiempo haciendo oficios del hogar.   
#      prop_mayoriatiempoincapacitado - Proporción de personas en el hogar que pasaron la mayor parte del tiempo incapacitado.
#      prop_obreroemplempresa - Proporción de personas en el hogar que son empleados de una empresa privada.     
#      prop_obreroemplgobierno - Proporción de personas en el hogar que son empleados del gobierno.              
#      prop_empldomestico - Proporción de personas en el hogar que son empleados domésticos.               
#      prop_trabajadorcuentapropia - Proporción de personas en el hogar que son cuenta propia. 
#      prop_patronempleador - Proporción de personas en el hogar que son patrón.         
#      prop_trabajadorsinremunfamilia - Proporción de personas en el hogar que son trabajadores de la familia sin remuneración.
#      prop_trabajadorsinremunempresa - Proporción de personas en el hogar que son trabajadores de una empresa sin remuneración.


