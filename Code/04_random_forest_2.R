# =============================================================================
# 04_random_forest_2.R
# Descripcion: Random Forest 2 con hiperparametros mas flexibles
# =============================================================================

library(ranger)
library(readr)
library(dplyr)

# Paths --------------------------------------------------------------------

path_input <- "Input"
path_cleaned <- file.path(path_input, "Cleaned")

path_output <- "Output"
path_submissions <- file.path(path_output, "Submissions")

dir.create(
  path_submissions,
  recursive = TRUE,
  showWarnings = FALSE
)

# Cargar bases limpias -----------------------------------------------------

base_train_final <- read_rds(
  file.path(path_cleaned, "base_train_final.rds")
)

base_test_final <- read_rds(
  file.path(path_cleaned, "base_test_final.rds")
)

# Preparar datos -----------------------------------------------------------

train_rf <- base_train_final %>%
  select(-property_id)

test_rf <- base_test_final %>%
  select(-property_id)

# Modelo RF 2: mas complejo ------------------------------------------------

set.seed(123)

rf_2 <- ranger(
  price ~ .,
  data = train_rf,
  num.trees = 1000,
  mtry = 12,
  min.node.size = 3,
  importance = "impurity"
)

# Predicciones -------------------------------------------------------------

pred_rf_2 <- predict(
  rf_2,
  data = test_rf
)$predictions

submission_rf_2 <- tibble(
  property_id = base_test_final$property_id,
  price = pred_rf_2
)

write_csv(
  submission_rf_2,
  file.path(path_submissions, "submission_rf_2.csv")
)