# =============================================================================
# 11_gbm.R
# Descripcion: Gradient Boosting Machine para prediccion de precios
# =============================================================================

# ------------------------------------------------------------
# 1. Preparar datos
# ------------------------------------------------------------

train_gbm <- base_train_final %>% select(-property_id)
test_gbm  <- base_test_final  %>% select(-property_id)

# ------------------------------------------------------------
# 2. Modelo GBM
# ------------------------------------------------------------

set.seed(123)

gbm_model <- gbm(
  formula           = price ~ .,
  data              = train_gbm,
  distribution      = "gaussian",
  n.trees           = 1500,
  interaction.depth = 6,
  shrinkage         = 0.01,
  n.minobsinnode    = 10,
  cv.folds          = 5,
  verbose           = FALSE
)

# ------------------------------------------------------------
# 3. Mejor número de árboles por CV
# ------------------------------------------------------------

best_iter <- gbm.perf(gbm_model, method = "cv", plot.it = FALSE)
cat("Mejor número de árboles:", best_iter, "\n")

# ------------------------------------------------------------
# 4. Predicción y submission
# ------------------------------------------------------------

pred_gbm <- predict(gbm_model, newdata = test_gbm, n.trees = best_iter)
pred_gbm <- pmax(pred_gbm, 0)

write_csv(
  tibble(property_id = base_test_final$property_id, price = pred_gbm),
  file.path(path_submissions, "submission_gbm.csv")
)

cat("Submission guardado: submission_gbm.csv\n")