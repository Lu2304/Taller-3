# =============================================================================
# 08_figures_compare.R
# Descripcion: Figuras para presentacion de comparacion de modelos
# =============================================================================

library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(forcats)
library(tibble)
library(stringr)
library(ggrepel)

# =============================================================================
# Paths
# =============================================================================

path_output <- "Output"
path_figures <- file.path(path_output, "Figures")

dir.create(
  path_figures,
  recursive = TRUE,
  showWarnings = FALSE
)

# =============================================================================
# Base resumen de modelos
# =============================================================================

model_results <- tribble(
  ~modelo, ~familia, ~mae_train, ~mae_kaggle,
  
  "XGBoost baseline", "Boosting", 94116244, 459380272.49,
  "XGBoost + imputación", "Boosting", 112305618, 251808804.86,
  "XGBoost spatial CV", "Boosting", 135434740, 183644146.00,
  "XGBoost sin lat/lon", "Boosting", 135434740, 249413067.64,
  "XGBoost spatial + DTM", "Boosting", 124554398, 175637962.86,
  
  "SuperLearner", "SuperLearner", 104739812, 189061480.54,
  
  "Random Forest 1", "Random Forest", NA, 206470563.21,
  "Random Forest 2", "Random Forest", NA, 200114849.08,
  "Random Forest 3", "Random Forest", NA, 212366076.17,
  
  "CART", "CART", 126786640, 246043324.94,
  
  "Red neuronal 1", "Red neuronal", 122142601, 237152443.13,
  "Red neuronal 2", "Red neuronal", 142369043, 262782527.15,
  "Red neuronal 3", "Red neuronal", 129073151, 238568458.66,
  
  "Regresión lineal", "Lineal", 202184245, 317386733.65,
  
  "Ridge", "Elastic Net", NA, 320031734.70,
  "Elastic Net", "Elastic Net", NA, 317612456.90,
  "Lasso", "Elastic Net", NA, 317587252.64,
  
  "Naive Bayes 1", "Naive Bayes", NA, 438159965.88,
  "Naive Bayes 2", "Naive Bayes", NA, 434632540.50,
  "Naive Bayes 3", "Naive Bayes", NA, 440659113.18
)

# =============================================================================
# Estilo corporativo
# =============================================================================

azul_oscuro <- "#071D49"
azul_medio  <- "#1F5AA6"
azul_claro  <- "#7BAFEA"

gris_texto  <- "#4D4D4D"
gris_claro  <- "#EAECEF"

paleta_familias <- c(
  "Boosting" = "#071D49",
  "SuperLearner" = "#174A7C",
  "Random Forest" = "#1F77B4",
  "CART" = "#4A90E2",
  "Red neuronal" = "#7BAFEA",
  "Elastic Net" = "#8A9BAE",
  "Lineal" = "#B0B7C3",
  "Naive Bayes" = "#D0D5DD"
)

theme_corporativo <- function(base_size = 12) {
  
  theme_minimal(
    base_size = base_size,
    base_family = "Helvetica"
  ) +
    
    theme(
      
      plot.title = element_text(
        face = "bold",
        size = 16,
        color = azul_oscuro
      ),
      
      plot.subtitle = element_text(
        size = 11,
        color = gris_texto,
        margin = margin(b = 10)
      ),
      
      plot.caption = element_text(
        size = 9,
        color = gris_texto,
        hjust = 0
      ),
      
      axis.title = element_text(
        size = 11,
        color = azul_oscuro
      ),
      
      axis.text = element_text(
        size = 10,
        color = gris_texto
      ),
      
      panel.grid.major.y = element_blank(),
      
      panel.grid.major.x = element_line(
        color = gris_claro
      ),
      
      panel.grid.minor = element_blank(),
      
      legend.position = "bottom",
      
      legend.title = element_text(
        face = "bold"
      ),
      
      legend.text = element_text(
        size = 9
      ),
      
      plot.background = element_rect(
        fill = "white",
        color = NA
      ),
      
      panel.background = element_rect(
        fill = "white",
        color = NA
      )
    )
}

# =============================================================================
# FIGURA 1
# Ranking MAE Kaggle
# =============================================================================

p_ranking <- model_results %>%
  
  mutate(
    modelo = fct_reorder(modelo, mae_kaggle)
  ) %>%
  
  ggplot(
    aes(
      x = mae_kaggle / 1e6,
      y = modelo,
      fill = familia
    )
  ) +
  
  geom_col(width = 0.72) +
  
  geom_text(
    aes(
      label = paste0(
        round(mae_kaggle / 1e6, 1),
        "M"
      )
    ),
    hjust = -0.1,
    size = 3.4,
    color = azul_oscuro
  ) +
  
  scale_fill_manual(
    values = paleta_familias
  ) +
  
  scale_x_continuous(
    labels = label_number(suffix = "M"),
    expand = expansion(mult = c(0, 0.18))
  ) +
  
  labs(
    title = "Comparación de algoritmos según MAE en Kaggle",
    
    subtitle = "Menor MAE implica mejor desempeño predictivo fuera de muestra.",
    
    x = "MAE Kaggle, millones de COP",
    
    y = NULL,
    
    fill = "Familia algorítmica",
    
    caption =
      "Fuente: elaboración propia con resultados del public leaderboard de Kaggle.\nNota: todos los modelos utilizaron la misma base limpia y el mismo pipeline de feature engineering."
  ) +
  
  theme_corporativo()

ggsave(
  file.path(
    path_figures,
    "compare_01_ranking_mae_kaggle.png"
  ),
  p_ranking,
  width = 11,
  height = 7,
  dpi = 300
)

# =============================================================================
# FIGURA 2
# Train vs Kaggle
# =============================================================================

p_generalizacion <- model_results %>%
  
  filter(!is.na(mae_train)) %>%
  
  ggplot(
    
    aes(
      x = mae_train / 1e6,
      y = mae_kaggle / 1e6,
      color = familia,
      label = modelo
    )
    
  ) +
  
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "#9AA4B2"
  ) +
  
  geom_point(
    size = 3.2,
    alpha = 0.9
  ) +
  
  geom_text_repel(
    size = 3,
    color = azul_oscuro,
    max.overlaps = 25
  ) +
  
  scale_color_manual(
    values = paleta_familias
  ) +
  
  labs(
    
    title = "Brecha entre desempeño en entrenamiento y Kaggle",
    
    subtitle = "Modelos más alejados de la línea punteada presentan mayor dificultad para generalizar a Chapinero.",
    
    x = "MAE train, millones de COP",
    
    y = "MAE Kaggle, millones de COP",
    
    color = "Familia algorítmica",
    
    caption =
      "Fuente: elaboración propia.\nNota: desviaciones grandes respecto a la diagonal sugieren sobreajuste o cambio de distribución espacial."
  ) +
  
  theme_corporativo()

ggsave(
  file.path(
    path_figures,
    "compare_02_train_vs_kaggle.png"
  ),
  p_generalizacion,
  width = 10,
  height = 7,
  dpi = 300
)

# =============================================================================
# FIGURA 3
# Evolucion XGBoost
# =============================================================================

xgb_evolution <- model_results %>%
  
  filter(
    str_detect(modelo, "XGBoost")
  ) %>%
  
  mutate(
    
    modelo = factor(
      
      modelo,
      
      levels = c(
        "XGBoost baseline",
        "XGBoost + imputación",
        "XGBoost spatial CV",
        "XGBoost sin lat/lon",
        "XGBoost spatial + DTM"
      )
    )
  )

p_xgb <- xgb_evolution %>%
  
  ggplot(
    aes(
      x = modelo,
      y = mae_kaggle / 1e6,
      group = 1
    )
  ) +
  
  geom_line(
    color = azul_oscuro,
    linewidth = 1.2
  ) +
  
  geom_point(
    color = azul_medio,
    size = 3.5
  ) +
  
  geom_text(
    
    aes(
      label = paste0(
        round(mae_kaggle / 1e6, 1),
        "M"
      )
    ),
    
    vjust = -0.9,
    size = 3.5,
    color = azul_oscuro
  ) +
  
  labs(
    
    title = "Evolución del desempeño de XGBoost",
    
    subtitle = "La mayor mejora provino de la validación espacial y del uso del texto como DTM.",
    
    x = NULL,
    
    y = "MAE Kaggle, millones de COP",
    
    caption =
      "Fuente: elaboración propia.\nNota: DTM corresponde a una matriz documento-término construida a partir del texto de títulos y descripciones."
  ) +
  
  theme_corporativo() +
  
  theme(
    axis.text.x = element_text(
      angle = 25,
      hjust = 1
    )
  )

ggsave(
  file.path(
    path_figures,
    "compare_03_xgboost_evolution.png"
  ),
  p_xgb,
  width = 10,
  height = 6,
  dpi = 300
)

# =============================================================================
# FIGURA 4
# Mejor modelo por familia
# =============================================================================

family_best <- model_results %>%
  
  group_by(familia) %>%
  
  slice_min(
    mae_kaggle,
    n = 1,
    with_ties = FALSE
  ) %>%
  
  ungroup() %>%
  
  mutate(
    familia = fct_reorder(
      familia,
      mae_kaggle
    )
  )

p_family <- family_best %>%
  
  ggplot(
    aes(
      x = mae_kaggle / 1e6,
      y = familia,
      fill = familia
    )
  ) +
  
  geom_col(
    width = 0.7,
    show.legend = FALSE
  ) +
  
  geom_text(
    
    aes(
      label = paste0(
        modelo,
        "  |  ",
        round(mae_kaggle / 1e6, 1),
        "M"
      )
    ),
    
    hjust = -0.05,
    size = 3.4,
    color = azul_oscuro
  ) +
  
  scale_fill_manual(
    values = paleta_familias
  ) +
  
  scale_x_continuous(
    labels = label_number(suffix = "M"),
    expand = expansion(mult = c(0, 0.35))
  ) +
  
  labs(
    
    title = "Mejor modelo por familia algorítmica",
    
    subtitle = "Comparación sobre un pipeline común de limpieza, imputación y feature engineering.",
    
    x = "MAE Kaggle, millones de COP",
    
    y = NULL,
    
    caption =
      "Fuente: elaboración propia.\nNota: esta figura resume la mejor iteración observada por familia de modelos."
  ) +
  
  theme_corporativo()

ggsave(
  file.path(
    path_figures,
    "compare_04_best_by_family.png"
  ),
  p_family,
  width = 11,
  height = 6,
  dpi = 300
)

# =============================================================================
# FIGURA 5
# Brecha de generalizacion
# =============================================================================

p_gap <- model_results %>%
  
  filter(!is.na(mae_train)) %>%
  
  mutate(
    
    gap = mae_kaggle - mae_train,
    
    modelo = fct_reorder(
      modelo,
      gap
    )
  ) %>%
  
  ggplot(
    aes(
      x = gap / 1e6,
      y = modelo,
      fill = familia
    )
  ) +
  
  geom_col(width = 0.72) +
  
  geom_text(
    
    aes(
      label = paste0(
        round(gap / 1e6, 1),
        "M"
      )
    ),
    
    hjust = -0.1,
    size = 3.4,
    color = azul_oscuro
  ) +
  
  scale_fill_manual(
    values = paleta_familias
  ) +
  
  scale_x_continuous(
    labels = label_number(suffix = "M"),
    expand = expansion(mult = c(0, 0.18))
  ) +
  
  labs(
    
    title = "Brecha de generalización: Kaggle menos train",
    
    subtitle = "Una brecha alta sugiere sobreajuste o dificultad para trasladar el modelo a Chapinero.",
    
    x = "Diferencia MAE Kaggle - MAE train, millones de COP",
    
    y = NULL,
    
    fill = "Familia algorítmica",
    
    caption =
      "Fuente: elaboración propia.\nNota: Kaggle evalúa exclusivamente propiedades en Chapinero, mientras que train incluye una geografía más amplia de Bogotá."
  ) +
  
  theme_corporativo()

ggsave(
  file.path(
    path_figures,
    "compare_05_generalization_gap.png"
  ),
  p_gap,
  width = 10,
  height = 6.5,
  dpi = 300
)

# =============================================================================
# Mensaje final
# =============================================================================

cat("\n")
cat("=============================================\n")
cat("Figuras generadas correctamente en:\n")
cat("Output/Figures\n")
cat("=============================================\n")
cat("\n")
