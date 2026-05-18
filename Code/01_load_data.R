# =============================================================================
# 01_load_data.R
# Descripcion: carga de bases de datos
# =============================================================================

# Cargar bases de datos
test <- read.csv(file.path(path_raw, "test.csv"))
train <- read.csv(file.path(path_raw, "train.csv"))

# Revisar nombres y estructura general de los datos
colnames(test)
colnames(train)
glimpse(test)
glimpse(train)
