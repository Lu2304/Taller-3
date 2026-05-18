# =============================================================================
# 00_rundirectory.R
# Descripcion: limpieza del entorno de trabajo, carga de paquetes, creación de
# paths para inputs y outputs, y script desde donde se deben correr los otros 
# scripts
# =============================================================================

rm(list = ls())

#Cargue de librerías
library(pacman)

p_load(dplyr, stringr, stringi, osmdata, sf, tidyr, caret, xgboost, purrr, readr, spatialsample, rsample, keras,
       quanteda, quanteda.textplots, Matrix)

# Establecimiento de paths
path_input  <- "Input"
path_raw <- file.path(path_input, "Raw")
path_cleaned  <- file.path(path_input, "Cleaned")

path_output  <- "Output"
path_figures <- file.path(path_output, "Figures")
path_tables  <- file.path(path_output, "Tables")
path_submissions  <- file.path(path_output, "Submissions")
path_models <- file.path(path_output, "Models")

dir.create(path_raw,     recursive = TRUE, showWarnings = FALSE)
dir.create(path_cleaned, recursive = TRUE, showWarnings = FALSE)
dir.create(path_figures, recursive = TRUE, showWarnings = FALSE)
dir.create(path_tables,  recursive = TRUE, showWarnings = FALSE)
dir.create(path_submissions, recursive = TRUE, showWarnings = FALSE)
dir.create(path_models, recursive = TRUE, showWarnings = FALSE)

# Correr scripts
source(file.path("Code", "01_load_data.R"))
source(file.path("Code", "02_clean_inspect_data.R"))