# ------------------------------------------------------------
# 1. Definir variable dependiente y matriz de predictores
# ------------------------------------------------------------

y_train <- base_train_final$price

x_train <- base_train_final %>%
  select(-price) %>%
  select(-property_id)

# ------------------------------------------------------------
# 2. Grilla
# ------------------------------------------------------------

grid_xgb <- expand.grid(
  eta = c(0.01, 0.05),
  max_depth = c(2, 4, 8),
  gamma = c(0.001, 0.01, 0.1, 0),
  colsample_bytree = c(0.66, 0.75),
  min_child_weight = c(20, 40),
  subsample = c(0.66, 0.75),
  nrounds = c(250, 500, 1000)
)

# ------------------------------------------------------------
# 3. Folds
# ------------------------------------------------------------

set.seed(123)

k <- 5
fold_id <- sample(rep(1:k, length.out = nrow(x_train)))

# ------------------------------------------------------------
# 4. Grid search con CV y MAE
# ------------------------------------------------------------

resultados_grid <- vector("list", nrow(grid_xgb))

for (g in seq_len(nrow(grid_xgb))) {
  
  cat("Modelo", g, "de", nrow(grid_xgb), "\n")
  
  pars <- grid_xgb[g, ]
  resultados_folds <- vector("list", k)
  
  for (fold in 1:k) {
    
    idx_tr <- which(fold_id != fold)
    idx_va <- which(fold_id == fold)
    
    x_tr <- x_train[idx_tr, , drop = FALSE]
    y_tr <- y_train[idx_tr]
    
    x_va <- x_train[idx_va, , drop = FALSE]
    y_va <- y_train[idx_va]
    
    dtrain <- xgb.DMatrix(data = x_tr, label = y_tr)
    dvalid <- xgb.DMatrix(data = x_va, label = y_va)
    
    modelo <- xgb.train(
      params = list(
        objective = "reg:squarederror",
        eval_metric = "mae",
        eta = pars$eta,
        max_depth = pars$max_depth,
        gamma = pars$gamma,
        colsample_bytree = pars$colsample_bytree,
        min_child_weight = pars$min_child_weight,
        subsample = pars$subsample
      ),
      data = dtrain,
      nrounds = pars$nrounds,
      verbose = 0
    )
    
    pred_va <- predict(modelo, newdata = dvalid)
    
    mae_valid_fold <- mean(abs(y_va - pred_va))
    
    resultados_folds[[fold]] <- data.frame(
      fold = fold,
      mae_valid = mae_valid_fold
    )
  }
  
  folds_df <- bind_rows(resultados_folds)
  
  resultados_grid[[g]] <- cbind(
    grid_xgb[g, ],
    data.frame(
      mae_valid_mean = mean(folds_df$mae_valid),
      mae_valid_sd = sd(folds_df$mae_valid)
    )
  )
}

# ------------------------------------------------------------
# 5. Resultados ordenados
# ------------------------------------------------------------

resultados_grid_df <- bind_rows(resultados_grid) %>%
  arrange(mae_valid_mean, mae_valid_sd)

resultados_grid_df

best_xgb <- resultados_grid_df[1, ]
best_xgb

dtrain_full <- xgb.DMatrix(data = x_train, label = y_train)

modelo_xgb_final <- xgb.train(
  params = list(
    objective = "reg:squarederror",
    eval_metric = "mae",
    eta = best_xgb$eta,
    max_depth = best_xgb$max_depth,
    gamma = best_xgb$gamma,
    colsample_bytree = best_xgb$colsample_bytree,
    min_child_weight = best_xgb$min_child_weight,
    subsample = best_xgb$subsample
  ),
  data = dtrain_full,
  nrounds = best_xgb$nrounds,
  verbose = 1
)

# ------------------------------------------------------------
# 6. Preparar matriz de test con las mismas variables del modelo
# ------------------------------------------------------------

vars_xgb <- colnames(x_train)

x_test <- base_test_final %>%
  select(all_of(vars_xgb)) %>%
  as.matrix()

dtest <- xgb.DMatrix(data = x_test)

# ------------------------------------------------------------
# 7. Predecir precios
# ------------------------------------------------------------

pred_test <- predict(modelo_xgb_final, newdata = dtest)

# ------------------------------------------------------------
# 8. Crear archivo de submission
# ------------------------------------------------------------

submission_xgb <- data.frame(
  property_id = base_test_final$property_id,
  price = pred_test
)

# ------------------------------------------------------------
# 4. Guardar CSV para Kaggle
# ------------------------------------------------------------

write.csv(
  submission_xgb,
  file.path(path_submissions, "xgboost1.csv"),
  row.names = FALSE
)