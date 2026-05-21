# =============================================================================
# 07_superlearner.R
# Descripcion: SuperLearner combinando CART + XGBoost + promedio simple
# Supuesto: base_train_final, base_test_final, best_xgb_spatial, 
#           best_cart_cv en memoria
# =============================================================================

library(nnls)
library(rpart)

# ------------------------------------------------------------
# 1. Preparar datos
# ------------------------------------------------------------

y_train_original <- base_train_final$price
y_train          <- log(y_train_original)

# Para CART
vars_sl <- base_train_final %>%
  select(-price, -property_id) %>%
  select(where(is.numeric)) %>%
  names()

df_train_cart <- base_train_final %>%
  select(all_of(vars_sl)) %>%
  mutate(log_price = y_train)

df_test_cart <- base_test_final %>%
  select(all_of(vars_sl))

# Para XGBoost
x_train_mat <- base_train_final %>%
  select(-price, -property_id) %>%
  as.matrix()

x_test_mat <- base_test_final %>%
  select(all_of(colnames(x_train_mat))) %>%
  as.matrix()

# ------------------------------------------------------------
# 2. Folds (k = 5)
# ------------------------------------------------------------

set.seed(123)

k       <- 5
fold_id <- sample(rep(1:k, length.out = nrow(df_train_cart)))

# ------------------------------------------------------------
# 3. Construir matriz Z (n x 3)
#    Columnas: CART, XGBoost, Promedio simple
# ------------------------------------------------------------

Z <- matrix(NA, nrow = nrow(df_train_cart), ncol = 3)
colnames(Z) <- c("cart", "xgb", "promedio")

for (fold in 1:k) {
  
  cat("Fold", fold, "de", k, "\n")
  
  idx_tr <- which(fold_id != fold)
  idx_va <- which(fold_id == fold)
  
  # --- CART ---
  modelo_cart <- rpart(
    formula = log_price ~ .,
    data    = df_train_cart[idx_tr, ],
    method  = "anova",
    control = rpart.control(
      cp       = best_cart_cv$cp,
      minsplit = best_cart_cv$minsplit,
      maxdepth = best_cart_cv$maxdepth
    )
  )
  
  pred_cart <- exp(predict(modelo_cart, newdata = df_train_cart[idx_va, ]))
  Z[idx_va, "cart"] <- pred_cart
  
  # --- XGBoost ---
  dtrain <- xgb.DMatrix(data = x_train_mat[idx_tr, ], label = y_train[idx_tr], missing = NA)
  dvalid <- xgb.DMatrix(data = x_train_mat[idx_va, ], missing = NA)
  
  modelo_xgb <- xgb.train(
    params = list(
      objective        = "reg:squarederror",
      eval_metric      = "mae",
      eta              = best_xgb_spatial$eta,
      max_depth        = best_xgb_spatial$max_depth,
      gamma            = best_xgb_spatial$gamma,
      colsample_bytree = best_xgb_spatial$colsample_bytree,
      min_child_weight = best_xgb_spatial$min_child_weight,
      subsample        = best_xgb_spatial$subsample
    ),
    data    = dtrain,
    nrounds = best_xgb_spatial$nrounds,
    verbose = 0
  )
  
  pred_xgb <- exp(predict(modelo_xgb, newdata = dvalid))
  Z[idx_va, "xgb"] <- pred_xgb
  
  # --- Promedio simple ---
  Z[idx_va, "promedio"] <- (pred_cart + pred_xgb) / 2
}

cat("\nMAE individual en CV:\n")
cat("CART:     ", format(mean(abs(y_train_original - Z[, "cart"])),     big.mark = ","), "\n")
cat("XGB:      ", format(mean(abs(y_train_original - Z[, "xgb"])),      big.mark = ","), "\n")
cat("Promedio: ", format(mean(abs(y_train_original - Z[, "promedio"])), big.mark = ","), "\n")

# ------------------------------------------------------------
# 4. Pesos óptimos con NNLS
# ------------------------------------------------------------

fit_nnls  <- nnls(A = Z, b = y_train_original)
alpha_raw <- coef(fit_nnls)
alpha     <- alpha_raw / sum(alpha_raw)
names(alpha) <- c("cart", "xgb", "promedio")

cat("\nPesos SuperLearner:\n")
cat("CART:    ", round(alpha["cart"],     4), "\n")
cat("XGB:     ", round(alpha["xgb"],      4), "\n")
cat("Promedio:", round(alpha["promedio"], 4), "\n")

pred_sl_cv <- Z %*% alpha
cat("\nMAE SuperLearner (CV):", format(mean(abs(y_train_original - pred_sl_cv)), big.mark = ","), "\n")

# ------------------------------------------------------------
# 5. Modelos finales entrenados en todo el train
# ------------------------------------------------------------

modelo_cart_final_sl <- rpart(
  formula = log_price ~ .,
  data    = df_train_cart,
  method  = "anova",
  control = rpart.control(
    cp       = best_cart_cv$cp,
    minsplit = best_cart_cv$minsplit,
    maxdepth = best_cart_cv$maxdepth
  )
)

dtrain_full <- xgb.DMatrix(data = x_train_mat, label = y_train, missing = NA)

modelo_xgb_final_sl <- xgb.train(
  params = list(
    objective        = "reg:squarederror",
    eval_metric      = "mae",
    eta              = best_xgb_spatial$eta,
    max_depth        = best_xgb_spatial$max_depth,
    gamma            = best_xgb_spatial$gamma,
    colsample_bytree = best_xgb_spatial$colsample_bytree,
    min_child_weight = best_xgb_spatial$min_child_weight,
    subsample        = best_xgb_spatial$subsample
  ),
  data    = dtrain_full,
  nrounds = best_xgb_spatial$nrounds,
  verbose = 0
)

# ------------------------------------------------------------
# 6. Predicción en test
# ------------------------------------------------------------

dtest <- xgb.DMatrix(data = x_test_mat, missing = NA)

pred_test_cart <- exp(predict(modelo_cart_final_sl, newdata = df_test_cart))
pred_test_xgb  <- exp(predict(modelo_xgb_final_sl,  newdata = dtest))
pred_test_prom <- (pred_test_cart + pred_test_xgb) / 2

# Combinar con pesos NNLS
pred_test_sl <- alpha["cart"]     * pred_test_cart +
  alpha["xgb"]      * pred_test_xgb  +
  alpha["promedio"] * pred_test_prom

pred_test_sl <- pmax(pred_test_sl, 0)

write.csv(
  data.frame(property_id = base_test_final$property_id, price = as.numeric(pred_test_sl)),
  file.path(path_submissions, "superlearner.csv"),
  row.names = FALSE
)

cat("\nSubmission guardado: superlearner.csv\n")

# ------------------------------------------------------------
# 7. Resumen
# ------------------------------------------------------------

resumen_sl <- data.frame(
  modelo       = "SuperLearner",
  alpha_cart   = round(alpha["cart"],     4),
  alpha_xgb    = round(alpha["xgb"],      4),
  alpha_prom   = round(alpha["promedio"], 4),
  mae_cv       = mean(abs(y_train_original - pred_sl_cv))
)

print(resumen_sl)

write.csv(resumen_sl, file.path(path_tables, "superlearner_resumen.csv"), row.names = FALSE)
