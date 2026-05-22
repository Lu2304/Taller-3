# =============================================================================
# 07_gbm.R
# Descripcion: Gradient Boosting Machine para prediccion de precios
# =============================================================================

library(gbm)
library(dplyr)
library(readr)

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

train_gbm <- base_train_final %>%
  select(-property_id)

test_gbm <- base_test_final %>%
  select(-property_id)

# Modelo GBM ---------------------------------------------------------------

set.seed(123)

gbm_model <- gbm(
  formula = price ~ .,
  data = train_gbm,
  distribution = "gaussian",
  n.trees = 1500,
  interaction.depth = 6,
  shrinkage = 0.01,
  n.minobsinnode = 10,
  cv.folds = 5,
  verbose = FALSE
)

# Mejor numero de arboles --------------------------------------------------

best_iter <- gbm.perf(
  gbm_model,
  method = "cv",
  plot.it = FALSE
)

# Predicciones -------------------------------------------------------------

pred_gbm <- predict(
  gbm_model,
  newdata = test_gbm,
  n.trees = best_iter
)

submission_gbm <- tibble(
  property_id = base_test_final$property_id,
  price = pred_gbm
)

write_csv(
  submission_gbm,
  file.path(path_submissions, "submission_gbm.csv")
)