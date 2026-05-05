# =============================================================================
# 01_load_data.R
# Descripcion: carga de bases de datos
# =============================================================================

# Cargar bases de datos
test <- read.csv("test.csv")
train <- read.csv("train.csv")

# Revisar nombres y estructura general de los datos
colnames(test)
colnames(train)
glimpse(test)
glimpse(train)