# =============================================================================
# xx_linear regression
# =============================================================================

# ------------------------------------------------------------
# 1. Preparar variable dependiente y predictores
# ------------------------------------------------------------

y_train_original <- base_train_final$price
y_train          <- log(y_train_original)   

vars_lm <- base_train_final %>%
  select(-price, -property_id) %>%
  select(where(is.numeric)) %>%
  names()

x_train_raw <- base_train_final %>% select(all_of(vars_lm))
x_test_raw  <- base_test_final  %>% select(all_of(vars_lm))

# ------------------------------------------------------------
# 2. Imputar NAs con medianas del train
# ------------------------------------------------------------

medianas_train <- x_train_raw %>%
  summarise(across(everything(), ~ median(., na.rm = TRUE)))

for (v in vars_lm) {
  x_train_raw[[v]][is.na(x_train_raw[[v]])] <- medianas_train[[v]]
  x_test_raw[[v]][is.na(x_test_raw[[v]])]   <- medianas_train[[v]]
}

# ------------------------------------------------------------
# 3. Armar data frames para lm()
# ------------------------------------------------------------

df_train <- x_train_raw %>%
  mutate(log_price = y_train)

df_test <- x_test_raw

# ============================================================
# PARTE A: Cross-Validation normal (k = 5)
# ============================================================

set.seed(123)

k        <- 5
fold_id  <- sample(rep(1:k, length.out = nrow(df_train)))
mae_folds <- numeric(k)

for (fold in 1:k) {
  
  idx_tr <- which(fold_id != fold)
  idx_va <- which(fold_id == fold)
  
  df_tr <- df_train[idx_tr, ]
  df_va <- df_train[idx_va, ]
  
  modelo_lm <- lm(log_price ~ ., data = df_tr)
  
  pred_log <- predict(modelo_lm, newdata = df_va)
  pred     <- exp(pred_log)
  
  mae_folds[fold] <- mean(abs(y_train_original[idx_va] - pred))
  
  cat("Fold", fold, "- MAE:", format(mae_folds[fold], big.mark = ","), "\n")
}

mae_cv_mean <- mean(mae_folds)
mae_cv_sd   <- sd(mae_folds)

cat("\n--- CV Normal ---\n")
cat("MAE medio:", format(mae_cv_mean, big.mark = ","), "\n")
cat("MAE SD:   ", format(mae_cv_sd,   big.mark = ","), "\n")

# ============================================================
# PARTE B: Spatial Cross-Validation (spatial_block_cv, k = 5)
# ============================================================

train_sf_lm <- base_train_final %>%
  mutate(log_price = y_train) %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs    = 4326,
    remove = FALSE
  ) %>%
  st_transform(3116)

# Imputar NAs en el sf también
for (v in vars_lm) {
  train_sf_lm[[v]][is.na(train_sf_lm[[v]])] <- medianas_train[[v]]
}

set.seed(123)

spatial_folds_lm <- spatial_block_cv(train_sf_lm, v = 5)

mae_spatial_folds <- numeric(5)

for (fold in seq_along(spatial_folds_lm$splits)) {
  
  split  <- spatial_folds_lm$splits[[fold]]
  df_tr  <- training(split)   %>% st_drop_geometry()
  df_va  <- assessment(split) %>% st_drop_geometry()
  
  # Mantener solo columnas del modelo
  df_tr_model <- df_tr %>% select(all_of(vars_lm), log_price)
  df_va_model <- df_va %>% select(all_of(vars_lm), log_price)
  
  y_va_original <- exp(df_va_model$log_price)
  
  modelo_lm_sp <- lm(log_price ~ ., data = df_tr_model)
  
  pred_log <- predict(modelo_lm_sp, newdata = df_va_model)
  pred     <- exp(pred_log)
  
  mae_spatial_folds[fold] <- mean(abs(y_va_original - pred))
  
  cat("Fold espacial", fold, "- MAE:", format(mae_spatial_folds[fold], big.mark = ","), "\n")
}

mae_spatial_mean <- mean(mae_spatial_folds)
mae_spatial_sd   <- sd(mae_spatial_folds)

cat("\n--- Spatial CV ---\n")
cat("MAE medio:", format(mae_spatial_mean, big.mark = ","), "\n")
cat("MAE SD:   ", format(mae_spatial_sd,   big.mark = ","), "\n")

# ============================================================
# PARTE C: Modelo final entrenado con todo el train
# ============================================================

modelo_lm_final <- lm(log_price ~ ., data = df_train)

summary(modelo_lm_final)



coefs <- broom::tidy(modelo_lm_final) %>%
  arrange(p.value) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

write.csv(
  coefs,
  file.path(path_tables, "lm_coeficientes.csv"),
  row.names = FALSE
)

# ============================================================
# PARTE D: Predicción en test y submission para Kaggle
# ============================================================

pred_test_log <- predict(modelo_lm_final, newdata = df_test)
pred_test     <- exp(pred_test_log)
pred_test     <- pmax(pred_test, 0)

submission_lm <- data.frame(
  property_id = base_test_final$property_id,
  price       = pred_test
)

write.csv(
  submission_lm,
  file.path(path_submissions, "linear_regression.csv"),
  row.names = FALSE
)

cat("\nSubmission guardado en:", file.path(path_submissions, "linear_regression.csv"), "\n")

# ============================================================
# PARTE E: Resumen comparativo CV normal vs spatial CV
# ============================================================

resumen_lm <- data.frame(
  modelo        = "Linear Regression",
  cv_mae_mean   = mae_cv_mean,
  cv_mae_sd     = mae_cv_sd,
  sp_mae_mean   = mae_spatial_mean,
  sp_mae_sd     = mae_spatial_sd
)

print(resumen_lm)

write.csv(
  resumen_lm,
  file.path(path_tables, "lm_cv_resumen.csv"),
  row.names = FALSE
)
