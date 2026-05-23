# =============================================================================
# 06_random_forest.R
# Descripcion: Tres especificaciones de Random Forest para prediccion de precios
#   RF 1 — baseline (num.trees=500, mtry=8, min.node.size=5)
#   RF 2 — mas complejo (num.trees=1000, mtry=12, min.node.size=3)
#   RF 3 — mayor regularizacion (num.trees=1500, mtry=6, min.node.size=10)
# =============================================================================

# ------------------------------------------------------------
# 1. Preparar datos
# ------------------------------------------------------------

train_rf <- base_train_final %>%
  select(-property_id)

test_rf <- base_test_final %>%
  select(-property_id)

# ------------------------------------------------------------
# 2. RF 1 — Baseline
# ------------------------------------------------------------

set.seed(123)

rf_1 <- ranger(
  price ~ .,
  data          = train_rf,
  num.trees     = 500,
  mtry          = 8,
  min.node.size = 5,
  importance    = "impurity"
)

pred_rf_1 <- predict(rf_1, data = test_rf)$predictions

write_csv(
  tibble(property_id = base_test_final$property_id, price = pred_rf_1),
  file.path(path_submissions, "submission_rf_1.csv")
)

cat("RF 1 guardado | OOB MAE aprox:", format(sqrt(rf_1$prediction.error), big.mark = ","), "\n")

# ------------------------------------------------------------
# 3. RF 2 — Más complejo
# ------------------------------------------------------------

set.seed(123)

rf_2 <- ranger(
  price ~ .,
  data          = train_rf,
  num.trees     = 1000,
  mtry          = 12,
  min.node.size = 3,
  importance    = "impurity"
)

pred_rf_2 <- predict(rf_2, data = test_rf)$predictions

write_csv(
  tibble(property_id = base_test_final$property_id, price = pred_rf_2),
  file.path(path_submissions, "submission_rf_2.csv")
)

cat("RF 2 guardado | OOB MAE aprox:", format(sqrt(rf_2$prediction.error), big.mark = ","), "\n")

# ------------------------------------------------------------
# 4. RF 3 — Mayor regularización
# ------------------------------------------------------------

set.seed(123)

rf_3 <- ranger(
  price ~ .,
  data          = train_rf,
  num.trees     = 1500,
  mtry          = 6,
  min.node.size = 10,
  importance    = "impurity"
)

pred_rf_3 <- predict(rf_3, data = test_rf)$predictions

write_csv(
  tibble(property_id = base_test_final$property_id, price = pred_rf_3),
  file.path(path_submissions, "submission_rf_3.csv")
)

cat("RF 3 guardado | OOB MAE aprox:", format(sqrt(rf_3$prediction.error), big.mark = ","), "\n")

# ------------------------------------------------------------
# 5. Variable importance — mejor modelo (RF 2)
# ------------------------------------------------------------

pred_wrapper_rf <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

set.seed(123)

vi_rf_perm <- vip::vi_permute(
  object         = rf_2,
  train          = train_rf %>% select(-price),
  target         = train_rf$price,
  metric         = "mae",
  pred_wrapper   = pred_wrapper_rf,
  nsim           = 5,
  smaller_is_better = TRUE
)

p_varimp_rf <- vi_rf_perm %>%
  arrange(desc(Importance)) %>%
  slice(1:10) %>%
  mutate(Variable = fct_reorder(Variable, Importance)) %>%
  ggplot(aes(x = Variable, y = Importance)) +
  geom_col(fill = "#071D49", width = 0.7) +
  coord_flip() +
  labs(
    title    = "Importancia de variables — Random Forest",
    subtitle = "Top 10 variables según permutation importance (RF 2)",
    x        = NULL,
    y        = "Aumento en MAE"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(size = 22, face = "bold", color = "#071D49"),
    plot.subtitle = element_text(size = 16, color = "#4D4D4D"),
    axis.text.y   = element_text(size = 14, color = "#071D49"),
    axis.text.x   = element_text(size = 13),
    axis.title.x  = element_text(size = 15, face = "bold", color = "#071D49")
  )

ggsave(
  file.path(path_figures, "varimp_random_forest.png"),
  p_varimp_rf,
  width = 10, height = 7, dpi = 300
)

message("✓ Random Forest completado — submissions y gráfica guardados")