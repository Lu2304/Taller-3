# ------------------------------------------------------------
# 1. Preparar variables
# ------------------------------------------------------------

y_train_original <- base_train_final$price
y_train <- log(y_train_original)

vars_nn <- base_train_final %>%
  select(-price, -property_id) %>%
  select(where(is.numeric)) %>%
  names()

x_train_raw <- base_train_final %>%
  select(all_of(vars_nn))

x_test_raw <- base_test_final %>%
  select(all_of(vars_nn))

# ------------------------------------------------------------
# 2. Imputar NA con medianas del train
# ------------------------------------------------------------

medianas_train <- x_train_raw %>%
  summarise(across(everything(), ~ median(., na.rm = TRUE)))

for (v in vars_nn) {
  x_train_raw[[v]][is.na(x_train_raw[[v]])] <- medianas_train[[v]]
  x_test_raw[[v]][is.na(x_test_raw[[v]])] <- medianas_train[[v]]
}

# ------------------------------------------------------------
# 3. Escalar predictores usando train
# ------------------------------------------------------------

x_train_scaled <- scale(x_train_raw)

centros <- attr(x_train_scaled, "scaled:center")
escalas <- attr(x_train_scaled, "scaled:scale")

x_test_scaled <- scale(
  x_test_raw,
  center = centros,
  scale = escalas
)

x_train <- as.matrix(x_train_scaled)
x_test <- as.matrix(x_test_scaled)

# ------------------------------------------------------------
# 4. Separar train / validation
# ------------------------------------------------------------

set.seed(123)

n <- nrow(x_train)
idx_valid <- sample(seq_len(n), size = floor(0.2 * n))
idx_train <- setdiff(seq_len(n), idx_valid)

x_tr <- x_train[idx_train, ]
y_tr <- y_train[idx_train]

x_val <- x_train[idx_valid, ]
y_val <- y_train[idx_valid]

# ------------------------------------------------------------
# 5. Modelo de red neuronal
# ------------------------------------------------------------

modelo_nn <- keras_model_sequential() %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(rate = 0.20) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dropout(rate = 0.20) %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "linear")

modelo_nn %>% compile(
  optimizer = optimizer_rmsprop(learning_rate = 0.001),
  loss = "mae",
  metrics = c("mae")
)

# ------------------------------------------------------------
# 6. Early stopping
# ------------------------------------------------------------

early_stop <- callback_early_stopping(
  monitor = "val_loss",
  patience = 30,
  restore_best_weights = TRUE
)

# ------------------------------------------------------------
# 7. Entrenamiento
# ------------------------------------------------------------

history_nn <- modelo_nn %>% fit(
  x = x_tr,
  y = y_tr,
  validation_data = list(x_val, y_val),
  epochs = 500,
  batch_size = 64,
  callbacks = list(early_stop),
  verbose = 1
)

# MAE 
pred_val_log <- modelo_nn %>%
  predict(x_val)

pred_val <- exp(pred_val_log[,1])

y_val_real <- exp(y_val)

mae_real <- mean(abs(y_val_real - pred_val))

mae_real
# ------------------------------------------------------------
# 8. Predicción en test
# ------------------------------------------------------------

pred_test_log <- modelo_nn %>%
  predict(x_test)

pred_test <- exp(pred_test_log[, 1])
pred_test <- pmax(pred_test, 0)

# ------------------------------------------------------------
# 9. Submission para Kaggle
# ------------------------------------------------------------

submission_nn <- data.frame(
  property_id = base_test_final$property_id,
  price = pred_test
)

write.csv(
  submission_nn,
  file.path(path_submissions, "NN_rmsprop3.csv"),
  row.names = FALSE
)
