# =============================================================================
# Elastic net
# =============================================================================
y_train_original <- base_train_final$price
y_train          <- log(y_train_original)

vars_en <- base_train_final %>%
  select(-price, -property_id) %>%
  select(where(is.numeric)) %>%
  names()

x_train_raw <- base_train_final %>% select(all_of(vars_en))
x_test_raw  <- base_test_final  %>% select(all_of(vars_en))

# Imputar NAs con mediana del train
medianas_train <- x_train_raw %>%
  summarise(across(everything(), ~ median(., na.rm = TRUE)))

for (v in vars_en) {
  x_train_raw[[v]][is.na(x_train_raw[[v]])] <- medianas_train[[v]]
  x_test_raw[[v]][is.na(x_test_raw[[v]])]   <- medianas_train[[v]]
}

# Eliminar columnas de varianza cero (e.g. na_bedrooms constante)
vars_ok <- vars_en[sapply(x_train_raw, function(x) var(x, na.rm = TRUE) > 0)]
x_train_raw <- x_train_raw %>% select(all_of(vars_ok))
x_test_raw  <- x_test_raw  %>% select(all_of(vars_ok))

x_train_scaled <- scale(x_train_raw)
centros <- attr(x_train_scaled, "scaled:center")
escalas <- attr(x_train_scaled, "scaled:scale")
x_train_mat <- as.matrix(x_train_scaled)
x_test_mat  <- as.matrix(scale(x_test_raw, center = centros, scale = escalas))

# ------------------------------------------------------------
# 2. Entrenar los modelos
# ------------------------------------------------------------
for (alpha in c(0, 0.5, 1)) {
  
  nombre <- c("0" = "en_ridge.csv", "0.5" = "en_elastic.csv", "1" = "en_lasso.csv")[as.character(alpha)]
  
  set.seed(123)
  
  cv_fit <- cv.glmnet(
    x            = x_train_mat,
    y            = y_train,
    alpha        = alpha,
    nfolds       = 10,
    type.measure = "mae"
  )
  
  modelo <- glmnet(
    x      = x_train_mat,
    y      = y_train,
    alpha  = alpha,
    lambda = cv_fit$lambda.min
  )
  
  pred_test <- pmax(exp(predict(modelo, newx = x_test_mat)), 0)
  
  write.csv(
    data.frame(property_id = base_test_final$property_id, price = as.numeric(pred_test)),
    file.path(path_submissions, nombre),
    row.names = FALSE
  )
  
  cat("Guardado:", nombre, "| lambda.min:", round(cv_fit$lambda.min, 6), "\n")
}