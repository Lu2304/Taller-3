# =============================================================================
# 03_random_forest.R
# Descripcion: Modelos Random Forest para prediccion de precios
# =============================================================================

library(ranger)
library(readr)
library(dplyr)

# Paths --------------------------------------------------------------------

path_input <- "Input"
path_cleaned <- file.path(path_input, "Cleaned")

path_output <- "Output"
path_submissions <- file.path(path_output, "Submissions")

dir.create(path_submissions,
           recursive = TRUE,
           showWarnings = FALSE)

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

# Modelo RF 1: baseline ----------------------------------------------------

set.seed(123)

rf_1 <- ranger(
  price ~ .,
  data = train_rf,
  num.trees = 500,
  mtry = 8,
  min.node.size = 5,
  importance = "impurity"
)

pred_rf_1 <- predict(
  rf_1,
  data = test_rf
)$predictions

submission_rf_1 <- tibble(
  property_id = base_test_final$property_id,
  price = pred_rf_1
)

write_csv(
  submission_rf_1,
  file.path(path_submissions, "submission_rf_1.csv")
)