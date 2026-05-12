# ------------------------------------------------------------
# 1. Definir variable dependiente y matriz de predictores
# ------------------------------------------------------------

y_train_original <- base_train_final$price
y_train <- log(y_train_original)

x_train <- base_train_final %>%
  select(-price) %>%
  select(-property_id) %>%
  as.matrix()

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
    y_va_original <- y_train_original[idx_va]
    
    dtrain <- xgb.DMatrix(data = x_tr, label = y_tr, missing = NA)
    dvalid <- xgb.DMatrix(data = x_va, missing = NA)
    
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
    
    pred_va_log <- predict(modelo, newdata = dvalid)
    pred_va <- exp(pred_va_log)
    
    mae_valid_fold <- mean(abs(y_va_original - pred_va))
    
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

dtrain_full <- xgb.DMatrix(data = x_train, label = y_train, missing = NA)

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

dtest <- xgb.DMatrix(data = x_test, missing = NA)

# ------------------------------------------------------------
# 7. Predecir precios
# ------------------------------------------------------------

pred_test_log <- predict(modelo_xgb_final, newdata = dtest)

pred_test <- exp(pred_test_log)

pred_test <- pmax(pred_test, 0)

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
  file.path(path_submissions, "xgboost11.csv"),
  row.names = FALSE
)
#------------------------------------------------------------------------------------------------
# Spatial Cross Validation

# ------------------------------------------------------------
# 1. Definir variable dependiente y matriz de predictores
# ------------------------------------------------------------

base_train_model <- base_train_final %>%
  mutate(row_id = row_number())

y_train_original <- base_train_model$price
y_train <- log(y_train_original)

x_train <- base_train_model %>%
  select(-price, -property_id, -row_id) %>%
  as.matrix()

# ------------------------------------------------------------
# 2. Crear folds espaciales
# ------------------------------------------------------------

train_sf <- base_train_model %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(3116)

set.seed(123)

spatial_folds <- spatial_block_cv(
  train_sf,
  v = 5
)

folds_valid <- lapply(spatial_folds$splits, function(split) {
  assessment(split)$row_id
})

k <- length(folds_valid)

# ------------------------------------------------------------
# 3. Grilla
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
# 4. Grid search con spatial CV y MAE
# ------------------------------------------------------------

resultados_grid_spatial <- vector("list", nrow(grid_xgb))

for (g in seq_len(nrow(grid_xgb))) {
  
  cat("Modelo", g, "de", nrow(grid_xgb), "\n")
  
  pars <- grid_xgb[g, ]
  resultados_folds <- vector("list", k)
  
  for (fold in seq_len(k)) {
    
    idx_va <- folds_valid[[fold]]
    idx_tr <- setdiff(seq_len(nrow(x_train)), idx_va)
    
    x_tr <- x_train[idx_tr, , drop = FALSE]
    y_tr <- y_train[idx_tr]
    
    x_va <- x_train[idx_va, , drop = FALSE]
    y_va_original <- y_train_original[idx_va]
    
    dtrain <- xgb.DMatrix(data = x_tr, label = y_tr, missing = NA)
    dvalid <- xgb.DMatrix(data = x_va, missing = NA)
    
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
    
    pred_va_log <- predict(modelo, newdata = dvalid)
    pred_va <- exp(pred_va_log)
    
    mae_valid_fold <- mean(abs(y_va_original - pred_va))
    
    resultados_folds[[fold]] <- data.frame(
      fold = fold,
      mae_valid = mae_valid_fold
    )
  }
  
  folds_df <- bind_rows(resultados_folds)
  
  resultados_grid_spatial[[g]] <- cbind(
    grid_xgb[g, ],
    data.frame(
      mae_valid_mean = mean(folds_df$mae_valid),
      mae_valid_sd = sd(folds_df$mae_valid)
    )
  )
}

# ------------------------------------------------------------
# 5. Resultados spatial CV
# ------------------------------------------------------------

resultados_grid_spatial_df <- bind_rows(resultados_grid_spatial) %>%
  arrange(mae_valid_mean, mae_valid_sd)

resultados_grid_spatial_df

best_xgb_spatial <- resultados_grid_spatial_df[1, ]
best_xgb_spatial

# ------------------------------------------------------------
# 6. Entrenar modelo final con mejores parámetros spatial CV
# ------------------------------------------------------------

dtrain_full <- xgb.DMatrix(
  data = x_train,
  label = y_train,
  missing = NA
)

modelo_xgb_final_spatial <- xgb.train(
  params = list(
    objective = "reg:squarederror",
    eval_metric = "mae",
    eta = best_xgb_spatial$eta,
    max_depth = best_xgb_spatial$max_depth,
    gamma = best_xgb_spatial$gamma,
    colsample_bytree = best_xgb_spatial$colsample_bytree,
    min_child_weight = best_xgb_spatial$min_child_weight,
    subsample = best_xgb_spatial$subsample
  ),
  data = dtrain_full,
  nrounds = best_xgb_spatial$nrounds,
  verbose = 1
)

# ------------------------------------------------------------
# 7. Preparar matriz de test con las mismas variables del modelo
# ------------------------------------------------------------

vars_xgb <- colnames(x_train)

x_test <- base_test_final %>%
  select(all_of(vars_xgb)) %>%
  as.matrix()

dtest <- xgb.DMatrix(
  data = x_test,
  missing = NA
)

# ------------------------------------------------------------
# 8. Predecir precios
# ------------------------------------------------------------

pred_test_log <- predict(modelo_xgb_final_spatial, newdata = dtest)

pred_test <- exp(pred_test_log)

pred_test <- pmax(pred_test, 0)

# ------------------------------------------------------------
# 9. Crear archivo de submission
# ------------------------------------------------------------

submission_xgb_spatial <- data.frame(
  property_id = base_test_final$property_id,
  price = pred_test
)

# ------------------------------------------------------------
# 10. Guardar CSV para Kaggle
# ------------------------------------------------------------

write.csv(
  submission_xgb_spatial,
  file.path(path_submissions, "xgboost_spatial_cv.csv"),
  row.names = FALSE
)