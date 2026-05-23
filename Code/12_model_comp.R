# =============================================================================
# Comparación de los 3 mejores modelos: XGBoost, SuperLearner, CART
# =============================================================================

# ------------------------------------------------------------
# 1. Predicciones en train
# ------------------------------------------------------------

pred_train_xgb  <- exp(predict(modelo_xgb_final_sl,
                               newdata = xgb.DMatrix(data = x_train_mat, missing = NA)))
pred_train_cart <- exp(predict(modelo_cart_final_sl, newdata = df_train_cart))
pred_train_sl   <- as.numeric(Z %*% alpha)

# ------------------------------------------------------------
# 2. Errores (predicho - real)
# Positivo = sobreestima, Negativo = subestima
# ------------------------------------------------------------

errores <- data.frame(
  real = y_train_original,
  xgb  = pred_train_xgb,
  cart = pred_train_cart,
  sl   = pred_train_sl
) %>%
  mutate(
    error_xgb  = xgb  - real,
    error_cart = cart - real,
    error_sl   = sl   - real
  )

# Resumen de bias y % que sobreestima
resumen_errores <- data.frame(
  modelo          = c("XGBoost", "CART", "SuperLearner"),
  bias            = c(mean(errores$error_xgb),
                      mean(errores$error_cart),
                      mean(errores$error_sl)),
  pct_sobreestima = c(mean(errores$error_xgb > 0),
                      mean(errores$error_cart > 0),
                      mean(errores$error_sl  > 0)) * 100
)

print(resumen_errores)

# ------------------------------------------------------------
# 3. Gráfica: distribución de errores
# ------------------------------------------------------------

errores_long <- data.frame(
  real         = y_train_original,
  XGBoost      = pred_train_xgb,
  CART         = pred_train_cart,
  SuperLearner = pred_train_sl
) %>%
  pivot_longer(-real, names_to = "modelo", values_to = "pred") %>%
  mutate(
    error  = (pred - real) / 1e6,
    modelo = factor(modelo, levels = c("CART", "SuperLearner", "XGBoost"))
  )

ggplot(errores_long, aes(x = error, fill = modelo)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  facet_wrap(~ modelo, ncol = 1) +
  scale_fill_manual(values = c(
    "XGBoost"      = "#071D49",
    "CART"         = "#D7191C",
    "SuperLearner" = "#1A9641"
  )) +
  scale_x_continuous(
    limits = c(-500, 500),
    labels = function(x) paste0(x, "M")
  ) +
  labs(
    title    = "Distribución de errores por modelo",
    subtitle = "Positivo = sobreestima (destruye capital)\nNegativo = subestima (oportunidad perdida)",
    x        = "Error (predicho − real, millones COP)",
    y        = "Densidad",
    fill     = NULL,
    caption  = "Fuente: elaboración propia"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold", size = 18, color = "#071D49"),
    plot.subtitle   = element_text(size = 13, color = "#4D4D4D"),
    legend.position = "none",
    strip.text      = element_text(face = "bold", size = 14)
  )

ggsave(file.path(path_figures, "errores_modelos.png"),
       width = 10, height = 8, dpi = 300)

# ------------------------------------------------------------
# 4. Gráfica: CV Normal vs Spatial CV
# ------------------------------------------------------------

azul_oscuro <- "#071D49"
gris_texto  <- "#4D4D4D"

cv_comparison <- tibble(
  modelo = rep(c("XGBoost", "SuperLearner", "CART"), each = 2),
  tipo   = rep(c("CV Normal", "Spatial CV"), 3),
  mae    = c(96182501,  135823643,
             107252991, 139297953,
             126786640, 160270517) / 1e6
) %>%
  mutate(modelo = factor(modelo, levels = c("CART", "SuperLearner", "XGBoost")))

ggplot(cv_comparison, aes(x = modelo, y = mae, fill = tipo)) +
  geom_col(position = "dodge", width = 0.6, alpha = 0.9) +
  geom_text(
    aes(label = paste0(round(mae, 0), "M")),
    position = position_dodge(width = 0.6),
    vjust = -0.5, size = 4, color = gris_texto
  ) +
  scale_fill_manual(values = c(
    "CV Normal"  = azul_oscuro,
    "Spatial CV" = "#A8B8D8"
  )) +
  scale_y_continuous(
    labels = function(x) paste0(x, "M"),
    limits = c(0, 200)
  ) +
  labs(
    title    = "CV Normal vs Spatial CV por modelo",
    subtitle = "El Spatial CV es más informativo: anticipa mejor el desempeño en Chapinero",
    x        = NULL,
    y        = "MAE (millones COP)",
    fill     = NULL,
    caption  = "Fuente: elaboración propia"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title         = element_text(size = 20, face = "bold", color = azul_oscuro),
    plot.subtitle      = element_text(size = 13, color = gris_texto),
    axis.text          = element_text(size = 13, color = azul_oscuro),
    legend.position    = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.caption       = element_text(size = 10, color = gris_texto)
  )

ggsave(file.path(path_figures, "cv_normal_vs_spatial.png"),
       width = 10, height = 6, dpi = 300)

message("✓ Gráficas de comparación guardadas")