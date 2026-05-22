# Taller-3
Este es el repositorio donde se encuentran todos los archivos relacionados con el taller 3 de BD&amp;ML. 


Una empresa de compra y venta de inmuebles busca adquirir propiedades en Chapinero
al menor precio posible. El reto es que la mayoria de los datos disponibles son de
otros barrios de Bogota, por lo que el modelo debe generalizar de toda la ciudad a
un vecindario especifico. La metrica de evaluacion es el MAE en pesos colombianos,
evaluada en Kaggle.

---

## Estructura del repositorio

- Code/ contiene todos los scripts en orden de ejecucion
- Input/Raw/ contiene los datos crudos y los archivos .rds de OSM
- Input/Cleaned/ contiene las bases limpias generadas por el pipeline
- Output/Figures/ contiene las graficas en PDF
- Output/Tables/ contiene las tablas en CSV
- Output/Submissions/ contiene los archivos para subir a Kaggle
- Output/Models/ contiene los modelos guardados

---

## Requisitos

R >= 4.1. Instale pacman si no lo tiene: install.packages("pacman").
El script maestro instala y carga todos los demas paquetes automaticamente.

---

## Como reproducir el analisis

1. Clone el repositorio
2. Descargue train.csv y test.csv desde Kaggle y guardelos en Input/Raw/
3. Abra el proyecto en RStudio (.Rproj en la raiz)
4. Ejecute unicamente el script 00_rundirectory.R

Los archivos .rds de OpenStreetMap ya estan incluidos en Input/Raw/ para evitar
errores de limite de solicitudes (HTTP 429). El script los carga desde disco
automaticamente si existen.

---

## Scripts

00_rundirectory.R — script maestro, limpia el entorno, carga paquetes, crea carpetas
y ejecuta todos los demas scripts en orden

01_load_data.R — carga train.csv y test.csv

02_clean_inspect_data.R — limpieza, extraccion de features de texto y OSM,
imputacion con medianas del train y construccion del dataset final

03_descriptivas.R — estadisticas descriptivas y figuras

04_linear_regression.R — regresion lineal con CV normal y espacial

05_CART.R — arbol de decision con grid search de 288 combinaciones

06_random_forest.R — Random Forest

09_Elastic_Net.R — Ridge, Elastic Net y Lasso con cv.glmnet

10_NN.R — red neuronal con Keras

11_xgboost.R — XGBoost con validacion cruzada espacial y DTM del texto

12_superlearner.R — SuperLearner combinando CART y XGBoost con pesos NNLS

---

## Resultados

Mejor modelo: XGBoost con validacion espacial y variables de texto (DTM)
MAE Kaggle: 175,637,963 COP