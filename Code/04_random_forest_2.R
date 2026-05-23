# =============================================================================
# 04_random_forest_2.R
# Descripcion: Random Forest 2 con hiperparametros mas flexibles
# =============================================================================

library(ranger)
library(readr)
library(dplyr)

# Paths --------------------------------------------------------------------

path_input <- "Input"
path_cleaned <- file.path(path_input, "Cleaned")

path_output <- "Output"
path_submissions <- file.path(path_output, "Submissions")

dir.create(
  path_submissions,
  recursive = TRUE,
  showWarnings = FALSE
)

# Cargar bases limpias -----------------------------------------------------

base_train_final <- read_rds(
  file.path(path_cleaned, "base_train_final.rds")
)

base_test_final <- read_rds(
  file.path(path_cleaned, "base_test_final.rds")
)

# Preparar datos -----------------------------------------------------------

train_rf <- base_train_final %>%
  select(-property_id)

test_rf <- base_test_final %>%
  select(-property_id)

# Modelo RF 2: mas complejo ------------------------------------------------

set.seed(123)

rf_2 <- ranger(
  price ~ .,
  data = train_rf,
  num.trees = 1000,
  mtry = 12,
  min.node.size = 3,
  importance = "impurity"
)

# Predicciones -------------------------------------------------------------

pred_rf_2 <- predict(
  rf_2,
  data = test_rf
)$predictions

submission_rf_2 <- tibble(
  property_id = base_test_final$property_id,
  price = pred_rf_2
)

write_csv(
  submission_rf_2,
  file.path(path_submissions, "submission_rf_2.csv")
)

# ------------------------------------------------------------
# Variable importance por permutación
# ------------------------------------------------------------

library(vip)
library(ggplot2)
library(forcats)

path_figures <- file.path(path_output, "Figures")

dir.create(
  path_figures,
  recursive = TRUE,
  showWarnings = FALSE
)

# Wrapper de predicción para ranger
pred_wrapper_rf <- function(object, newdata) {
  predict(
    object,
    data = newdata
  )$predictions
}

set.seed(123)

vi_rf_perm <- vip::vi_permute(
  object = rf_2,
  train = train_rf %>% select(-price),
  target = train_rf$price,
  metric = "mae",
  pred_wrapper = pred_wrapper_rf,
  nsim = 5,
  smaller_is_better = TRUE
)

vi_rf_perm

p_varimp_rf <- vi_rf_perm %>%
  arrange(desc(Importance)) %>%
  slice(1:10) %>%
  mutate(
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  ggplot(
    aes(
      x = Variable,
      y = Importance
    )
  ) +
  geom_col(
    fill = "#071D49",
    width = 0.7
  ) +
  coord_flip() +
  labs(
    title = "Importancia de variables - Random Forest 2",
    subtitle = "Top 10 variables según permutation importance",
    x = NULL,
    y = "Aumento en MAE"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      size = 22,
      face = "bold",
      color = "#071D49"
    ),
    plot.subtitle = element_text(
      size = 16,
      color = "#4D4D4D"
    ),
    axis.text.y = element_text(
      size = 14,
      color = "#071D49"
    ),
    axis.text.x = element_text(
      size = 13
    ),
    axis.title.x = element_text(
      size = 15,
      face = "bold",
      color = "#071D49"
    ),
    legend.text = element_text(
      size = 13
    )
  )
