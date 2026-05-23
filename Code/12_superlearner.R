# =============================================================================
#Superlearner.R
# Descripcion: SuperLearner combinando CART + XGBoost + promedio simple
# =============================================================================

# ------------------------------------------------------------
# 1. Preparar datos
# ------------------------------------------------------------

y_train_original <- base_train_final$price
y_train          <- log(y_train_original)

vars_sl <- base_train_final %>%
  select(-price, -property_id) %>%
  select(where(is.numeric)) %>%
  names()

df_train_cart <- base_train_final %>%
  select(all_of(vars_sl)) %>%
  mutate(log_price = y_train)

df_test_cart <- base_test_final %>%
  select(all_of(vars_sl))

x_train_mat <- base_train_final %>%
  select(-price, -property_id) %>%
  as.matrix()

x_test_mat <- base_test_final %>%
  select(all_of(colnames(x_train_mat))) %>%
  as.matrix()

# ------------------------------------------------------------
# 2. Folds normales (k = 5)
# ------------------------------------------------------------

set.seed(123)
k       <- 5
fold_id <- sample(rep(1:k, length.out = nrow(df_train_cart)))

# ------------------------------------------------------------
# 3. Construir matriz Z
# ------------------------------------------------------------

Z <- matrix(NA, nrow = nrow(df_train_cart), ncol = 3)
colnames(Z) <- c("cart", "xgb", "promedio")

for (fold in 1:k) {
  cat("Fold", fold, "de", k, "\n")
  idx_tr <- which(fold_id != fold)
  idx_va <- which(fold_id == fold)
  
  modelo_cart <- rpart(log_price ~ ., data = df_train_cart[idx_tr, ],
                       method = "anova",
                       control = rpart.control(cp = best_cart_cv$cp, minsplit = best_cart_cv$minsplit,
                                               maxdepth = best_cart_cv$maxdepth))
  pred_cart <- exp(predict(modelo_cart, newdata = df_train_cart[idx_va, ]))
  Z[idx_va, "cart"] <- pred_cart
  
  dtrain <- xgb.DMatrix(data = x_train_mat[idx_tr, ], label = y_train[idx_tr], missing = NA)
  dvalid <- xgb.DMatrix(data = x_train_mat[idx_va, ], missing = NA)
  modelo_xgb <- xgb.train(
    params = list(objective = "reg:squarederror", eval_metric = "mae",
                  eta = best_xgb_spatial$eta, max_depth = best_xgb_spatial$max_depth,
                  gamma = best_xgb_spatial$gamma, colsample_bytree = best_xgb_spatial$colsample_bytree,
                  min_child_weight = best_xgb_spatial$min_child_weight,
                  subsample = best_xgb_spatial$subsample),
    data = dtrain, nrounds = best_xgb_spatial$nrounds, verbose = 0)
  pred_xgb <- exp(predict(modelo_xgb, newdata = dvalid))
  Z[idx_va, "xgb"] <- pred_xgb
  Z[idx_va, "promedio"] <- (pred_cart + pred_xgb) / 2
}

cat("\nMAE individual en CV:\n")
cat("CART:     ", format(mean(abs(y_train_original - Z[, "cart"])),     big.mark = ","), "\n")
cat("XGB:      ", format(mean(abs(y_train_original - Z[, "xgb"])),      big.mark = ","), "\n")
cat("Promedio: ", format(mean(abs(y_train_original - Z[, "promedio"])), big.mark = ","), "\n")

# ------------------------------------------------------------
# 4. Pesos NNLS
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
cat("\nMAE SuperLearner (CV normal):", format(mean(abs(y_train_original - pred_sl_cv)), big.mark = ","), "\n")

# ------------------------------------------------------------
# 4b. Spatial CV
# ------------------------------------------------------------

train_sf_sl <- base_train_final %>%
  mutate(row_id = row_number()) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(3116)

set.seed(123)
spatial_folds_sl <- spatial_block_cv(train_sf_sl, v = 5)
folds_valid_sp   <- lapply(spatial_folds_sl$splits, function(s) assessment(s)$row_id)

mae_spatial_sl <- numeric(5)

for (fold in seq_len(5)) {
  cat("Spatial fold", fold, "de 5\n")
  idx_va <- folds_valid_sp[[fold]]
  idx_tr <- setdiff(seq_len(nrow(df_train_cart)), idx_va)
  
  m_cart <- rpart(log_price ~ ., data = df_train_cart[idx_tr, ],
                  method = "anova",
                  control = rpart.control(cp = best_cart_cv$cp, minsplit = best_cart_cv$minsplit,
                                          maxdepth = best_cart_cv$maxdepth))
  p_cart <- exp(predict(m_cart, newdata = df_train_cart[idx_va, ]))
  
  dtrain <- xgb.DMatrix(data = x_train_mat[idx_tr, ], label = y_train[idx_tr], missing = NA)
  dvalid <- xgb.DMatrix(data = x_train_mat[idx_va, ], missing = NA)
  m_xgb  <- xgb.train(
    params = list(objective = "reg:squarederror", eval_metric = "mae",
                  eta = best_xgb_spatial$eta, max_depth = best_xgb_spatial$max_depth,
                  gamma = best_xgb_spatial$gamma, colsample_bytree = best_xgb_spatial$colsample_bytree,
                  min_child_weight = best_xgb_spatial$min_child_weight,
                  subsample = best_xgb_spatial$subsample),
    data = dtrain, nrounds = best_xgb_spatial$nrounds, verbose = 0)
  p_xgb <- exp(predict(m_xgb, newdata = dvalid))
  
  p_sl <- alpha["cart"] * p_cart + alpha["xgb"] * p_xgb +
    alpha["promedio"] * (p_cart + p_xgb) / 2
  
  mae_spatial_sl[fold] <- mean(abs(y_train_original[idx_va] - p_sl))
  cat("MAE fold", fold, ":", format(mae_spatial_sl[fold], big.mark = ","), "\n")
}

cat("\nMAE Spatial CV SuperLearner:", format(mean(mae_spatial_sl), big.mark = ","), "\n")

# ------------------------------------------------------------
# 5. Modelos finales
# ------------------------------------------------------------

modelo_cart_final_sl <- rpart(log_price ~ ., data = df_train_cart,
                              method = "anova",
                              control = rpart.control(cp = best_cart_cv$cp, minsplit = best_cart_cv$minsplit,
                                                      maxdepth = best_cart_cv$maxdepth))

dtrain_full <- xgb.DMatrix(data = x_train_mat, label = y_train, missing = NA)

modelo_xgb_final_sl <- xgb.train(
  params = list(objective = "reg:squarederror", eval_metric = "mae",
                eta = best_xgb_spatial$eta, max_depth = best_xgb_spatial$max_depth,
                gamma = best_xgb_spatial$gamma, colsample_bytree = best_xgb_spatial$colsample_bytree,
                min_child_weight = best_xgb_spatial$min_child_weight,
                subsample = best_xgb_spatial$subsample),
  data = dtrain_full, nrounds = best_xgb_spatial$nrounds, verbose = 0)

# ------------------------------------------------------------
# 6. Predicción en test
# ------------------------------------------------------------

dtest <- xgb.DMatrix(data = x_test_mat, missing = NA)

pred_test_cart <- exp(predict(modelo_cart_final_sl, newdata = df_test_cart))
pred_test_xgb  <- exp(predict(modelo_xgb_final_sl,  newdata = dtest))
pred_test_prom <- (pred_test_cart + pred_test_xgb) / 2

pred_test_sl <- alpha["cart"]     * pred_test_cart +
  alpha["xgb"]      * pred_test_xgb  +
  alpha["promedio"] * pred_test_prom
pred_test_sl <- pmax(pred_test_sl, 0)

write.csv(
  data.frame(property_id = base_test_final$property_id, price = as.numeric(pred_test_sl)),
  file.path(path_submissions, "superlearner.csv"),
  row.names = FALSE)

cat("\nSubmission guardado: superlearner.csv\n")

# ------------------------------------------------------------
# 7. Resumen
# ------------------------------------------------------------

resumen_sl <- data.frame(
  modelo      = "SuperLearner",
  alpha_cart  = round(alpha["cart"],     4),
  alpha_xgb   = round(alpha["xgb"],      4),
  alpha_prom  = round(alpha["promedio"], 4),
  mae_cv      = mean(abs(y_train_original - pred_sl_cv)),
  mae_spatial = mean(mae_spatial_sl)
)

print(resumen_sl)
write.csv(resumen_sl, file.path(path_tables, "superlearner_resumen.csv"), row.names = FALSE)

# ------------------------------------------------------------
# 8. Gráfica de pesos
# ------------------------------------------------------------

azul_oscuro <- "#071D49"
gris_texto  <- "#4D4D4D"

pesos_sl <- tibble(
  modelo = c("CART", "XGBoost", "Promedio simple"),
  peso   = as.numeric(alpha)
) %>%
  mutate(modelo = fct_reorder(modelo, peso))

p_pesos_sl <- ggplot(pesos_sl, aes(x = modelo, y = peso)) +
  geom_col(fill = azul_oscuro, width = 0.7) +
  geom_text(aes(label = paste0(round(peso * 100, 1), "%")),
            hjust = -0.1, size = 5, color = azul_oscuro) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title    = "Pesos aprendidos por el SuperLearner",
       subtitle = "El ensemble asigna mayor peso al componente XGBoost",
       x = NULL, y = "Peso NNLS",
       caption  = "Fuente: elaboración propia. Pesos estimados mediante mínimos cuadrados no negativos sobre predicciones out-of-fold.") +
  theme_minimal(base_size = 16) +
  theme(plot.title       = element_text(size = 24, face = "bold", color = azul_oscuro),
        plot.subtitle    = element_text(size = 16, color = gris_texto),
        axis.text.y      = element_text(size = 15, face = "bold", color = azul_oscuro),
        axis.text.x      = element_text(size = 13, color = gris_texto),
        axis.title.x     = element_text(size = 15, face = "bold", color = azul_oscuro),
        plot.caption     = element_text(size = 11, color = gris_texto, hjust = 0),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank())

ggsave(file.path(path_figures, "superlearner_pesos.png"),
       p_pesos_sl, width = 10, height = 6, dpi = 300)

message("✓ Gráfica de pesos guardada: superlearner_pesos.png")