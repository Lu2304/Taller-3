# =============================================================================
# 05_cart.R
# =============================================================================

# ------------------------------------------------------------
# 1. Preparar datos
# ------------------------------------------------------------

y_train_original <- base_train_final$price
y_train          <- log(y_train_original)

vars_cart <- base_train_final %>%
  select(-price, -property_id) %>%
  select(where(is.numeric)) %>%
  names()

df_train <- base_train_final %>%
  select(all_of(vars_cart)) %>%
  mutate(log_price = y_train)

df_test <- base_test_final %>%
  select(all_of(vars_cart))

# CART maneja NAs internamente con surrogate splits — no se imputa

# ------------------------------------------------------------
# 2. Grilla de hiperparámetros (288 combinaciones)
# rpart tiene límite interno de maxdepth = 30
# ------------------------------------------------------------

set.seed(123)

grid_cart <- expand.grid(
  cp       = c(0.00001, 0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1),
  minsplit = c(5, 10, 20, 30, 40, 50),
  maxdepth = c(5, 10, 15, 20, 25, 30)
)

cat("Combinaciones a evaluar:", nrow(grid_cart), "\n")

# ============================================================
# PARTE A: CV normal (k = 5)
# ============================================================

set.seed(123)

k       <- 5
fold_id <- sample(rep(1:k, length.out = nrow(df_train)))

resultados_cv <- vector("list", nrow(grid_cart))

for (g in seq_len(nrow(grid_cart))) {
  
  if (g %% 50 == 0) cat("CV normal: modelo", g, "de", nrow(grid_cart), "\n")
  
  pars      <- grid_cart[g, ]
  mae_folds <- numeric(k)
  
  for (fold in 1:k) {
    
    idx_tr <- which(fold_id != fold)
    idx_va <- which(fold_id == fold)
    
    modelo <- rpart(
      formula = log_price ~ .,
      data    = df_train[idx_tr, ],
      method  = "anova",
      control = rpart.control(
        cp       = pars$cp,
        minsplit = pars$minsplit,
        maxdepth = pars$maxdepth
      )
    )
    
    pred          <- exp(predict(modelo, newdata = df_train[idx_va, ]))
    mae_folds[fold] <- mean(abs(y_train_original[idx_va] - pred))
  }
  
  resultados_cv[[g]] <- cbind(
    grid_cart[g, ],
    data.frame(mae_cv_mean = mean(mae_folds), mae_cv_sd = sd(mae_folds))
  )
}

resultados_cv_df <- bind_rows(resultados_cv) %>%
  arrange(mae_cv_mean)

cat("\n--- Top 5 CV Normal ---\n")
print(head(resultados_cv_df, 5))

best_cart_cv <- resultados_cv_df[1, ]

# ============================================================
# PARTE B: Spatial CV (k = 5)
# ============================================================

train_sf_cart <- base_train_final %>%
  mutate(log_price = y_train) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(3116)

set.seed(123)

spatial_folds_cart <- spatial_block_cv(train_sf_cart, v = 5)

resultados_spatial <- vector("list", nrow(grid_cart))

for (g in seq_len(nrow(grid_cart))) {
  
  if (g %% 50 == 0) cat("Spatial CV: modelo", g, "de", nrow(grid_cart), "\n")
  
  pars      <- grid_cart[g, ]
  mae_folds <- numeric(5)
  
  for (fold in seq_along(spatial_folds_cart$splits)) {
    
    split <- spatial_folds_cart$splits[[fold]]
    df_tr <- training(split)   %>% st_drop_geometry() %>% select(all_of(vars_cart), log_price)
    df_va <- assessment(split) %>% st_drop_geometry() %>% select(all_of(vars_cart), log_price)
    
    modelo <- rpart(
      formula = log_price ~ .,
      data    = df_tr,
      method  = "anova",
      control = rpart.control(
        cp       = pars$cp,
        minsplit = pars$minsplit,
        maxdepth = pars$maxdepth
      )
    )
    
    pred            <- exp(predict(modelo, newdata = df_va))
    mae_folds[fold] <- mean(abs(exp(df_va$log_price) - pred))
  }
  
  resultados_spatial[[g]] <- cbind(
    grid_cart[g, ],
    data.frame(mae_sp_mean = mean(mae_folds), mae_sp_sd = sd(mae_folds))
  )
}

resultados_spatial_df <- bind_rows(resultados_spatial) %>%
  arrange(mae_sp_mean)

cat("\n--- Top 5 Spatial CV ---\n")
print(head(resultados_spatial_df, 5))

best_cart_spatial <- resultados_spatial_df[1, ]

# ============================================================
# PARTE C: Modelo final y submission
# ============================================================

modelo_cart_final <- rpart(
  formula = log_price ~ .,
  data    = df_train,
  method  = "anova",
  control = rpart.control(
    cp       = best_cart_cv$cp,
    minsplit = best_cart_cv$minsplit,
    maxdepth = best_cart_cv$maxdepth
  )
)

# Visualizar el árbol
rpart.plot(
  modelo_cart_final,
  type  = 4,
  extra = 101,
  main  = "CART - Árbol final"
)

# Importancia de variables
importancia <- modelo_cart_final$variable.importance %>%
  as.data.frame() %>%
  tibble::rownames_to_column("variable") %>%
  rename(importancia = ".") %>%
  arrange(desc(importancia))

cat("\n--- Importancia de variables ---\n")
print(importancia)

write.csv(importancia, file.path(path_tables, "cart_importancia.csv"), row.names = FALSE)

# Submission
pred_test <- pmax(exp(predict(modelo_cart_final, newdata = df_test)), 0)

write.csv(
  data.frame(property_id = base_test_final$property_id, price = pred_test),
  file.path(path_submissions, "cart.csv"),
  row.names = FALSE
)

cat("\nSubmission guardado: cart.csv\n")

# ============================================================
# PARTE D: Resumen CV normal vs spatial CV
# ============================================================

resumen_cart <- data.frame(
  modelo      = "CART",
  cp          = best_cart_cv$cp,
  minsplit    = best_cart_cv$minsplit,
  maxdepth    = best_cart_cv$maxdepth,
  cv_mae_mean = best_cart_cv$mae_cv_mean,
  cv_mae_sd   = best_cart_cv$mae_cv_sd,
  sp_mae_mean = best_cart_spatial$mae_sp_mean,
  sp_mae_sd   = best_cart_spatial$mae_sp_sd
)

print(resumen_cart)

write.csv(resumen_cart, file.path(path_tables, "cart_cv_resumen.csv"), row.names = FALSE)
