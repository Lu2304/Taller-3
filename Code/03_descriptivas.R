# =============================================================================
#Estadísticas descriptivas
# =============================================================================


# Tema base para todas las figuras 
theme_taller <- theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 11),
    axis.title    = element_text(size = 11),
    legend.position = "bottom",
    plot.caption  = element_text(color = "grey50", size = 9),
    panel.grid.minor = element_blank()
  )


# =============================================================================
# 1. DISTRIBUCIÓN DEL PRECIO
# =============================================================================

# --- 1.1 Histograma del precio en train ---
p1_hist <- ggplot(base_train, aes(x = price / 1e6)) +
  geom_histogram(bins = 50, fill = "#2C7BB6", color = "white", alpha = 0.85) +
  scale_x_continuous(labels = comma_format(suffix = "M")) +
  labs(
    title    = "Distribución del precio de venta",
    subtitle = "Bogotá — datos de entrenamiento",
    x        = "Precio (millones COP)",
    y        = "Número de propiedades",
    caption  = "Fuente: Properati"
  ) +
  theme_taller

ggsave(file.path(path_figures, "01_hist_precio.pdf"),
       p1_hist, width = 8, height = 5)


# --- 1.2 Precio por tipo de propiedad ---
p1_box <- ggplot(base_train, aes(x = property_type, y = price / 1e6, fill = property_type)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.size = 1.5) +
  scale_y_continuous(labels = comma_format(suffix = "M")) +
  scale_fill_manual(values = c("Apartamento" = "#2C7BB6", "Casa" = "#D7191C")) +
  labs(
    title    = "Precio por tipo de propiedad",
    subtitle = "Bogotá — datos de entrenamiento",
    x        = NULL,
    y        = "Precio (millones COP)",
    fill     = NULL,
    caption  = "Fuente: Properati"
  ) +
  theme_taller +
  theme(legend.position = "none")

ggsave(file.path(path_figures, "02_box_precio_tipo.pdf"),
       p1_box, width = 6, height = 5)




# =============================================================================
# 2. VARIABLES ESTRUCTURALES
# =============================================================================

# --- 2.1 Distribución de habitaciones ---
p2_bed <- base_train %>%
  filter(bedrooms <= 8) %>%
  count(bedrooms) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = factor(bedrooms), y = pct)) +
  geom_col(fill = "#2C7BB6", alpha = 0.85) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -0.5, size = 3.5) +
  labs(
    title    = "Distribución de habitaciones",
    subtitle = "Bogotá — datos de entrenamiento",
    x        = "Número de habitaciones",
    y        = "Porcentaje (%)",
    caption  = "Fuente: Properati"
  ) +
  theme_taller

ggsave(file.path(path_figures, "04_dist_habitaciones.pdf"),
       p2_bed, width = 7, height = 5)


# --- 2.2 Precio vs habitaciones ---
p2_price_bed <- base_train %>%
  filter(bedrooms >= 1, bedrooms <= 6) %>%
  group_by(bedrooms) %>%
  summarise(precio_medio = mean(price, na.rm = TRUE) / 1e6,
            n = n()) %>%
  ggplot(aes(x = factor(bedrooms), y = precio_medio)) +
  geom_col(fill = "#2C7BB6", alpha = 0.85) +
  geom_text(aes(label = comma(round(precio_medio), suffix = "M")),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = comma_format(suffix = "M")) +
  labs(
    title    = "Precio promedio por número de habitaciones",
    subtitle = "Bogotá — datos de entrenamiento",
    x        = "Número de habitaciones",
    y        = "Precio promedio (millones COP)",
    caption  = "Fuente: Properati"
  ) +
  theme_taller

ggsave(file.path(path_figures, "05_precio_habitaciones.pdf"),
       p2_price_bed, width = 7, height = 5)


# =============================================================================
# 3. MISSINGNESS
# =============================================================================

# --- 3.1 Porcentaje de missings por variable en train ---
missing_df <- base_train %>%
  summarise(
    across(
      c(surface_total, surface_covered, rooms, bathrooms, estrato),
      ~ mean(is.na(.)) * 100
    )
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_missing") %>%
  mutate(variable = recode(variable,
                           surface_total   = "Área total",
                           surface_covered = "Área cubierta",
                           rooms           = "Habitaciones",
                           bathrooms       = "Baños"
  )) %>%
  arrange(desc(pct_missing))

p3_missing <- ggplot(missing_df, aes(x = reorder(variable, pct_missing), y = pct_missing)) +
  geom_col(fill = "#D7191C", alpha = 0.85) +
  geom_text(aes(label = paste0(round(pct_missing, 1), "%")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Porcentaje de valores faltantes",
    subtitle = "Bogotá — datos de entrenamiento",
    x        = NULL,
    y        = "% de observaciones con valor faltante",
    caption  = "Fuente: Properati"
  ) +
  theme_taller

ggsave(file.path(path_figures, "06_missingness.pdf"),
       p3_missing, width = 7, height = 5)


# =============================================================================
# 4. VARIABLES DE TEXTO
# =============================================================================

# --- 4.1 Frecuencia de variables binarias extraídas del texto ---
text_vars <- base_train %>%
  summarise(
    across(
      c(gym, elevator, parking, balcony, pool,
        security, green_area, remodeled, new_property, luxury, storage),
      ~ mean(.) * 100
    )
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct") %>%
  mutate(variable = recode(variable,
                           gym         = "Gimnasio",
                           elevator    = "Ascensor",
                           parking     = "Parqueadero",
                           balcony     = "Balcón/Terraza",
                           pool        = "Piscina",
                           security    = "Seguridad/Portería",
                           green_area  = "Zona verde/Parque",
                           remodeled   = "Remodelado",
                           new_property = "Propiedad nueva",
                           luxury      = "Acabados de lujo",
                           storage     = "Depósito/Bodega"
  )) %>%
  arrange(desc(pct))

p4_text <- ggplot(text_vars, aes(x = reorder(variable, pct), y = pct)) +
  geom_col(fill = "#1A9641", alpha = 0.85) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 80), labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Frecuencia de amenidades mencionadas en los anuncios",
    subtitle = "% de propiedades que mencionan cada amenidad — datos de entrenamiento",
    x        = NULL,
    y        = "% de propiedades",
    caption  = "Fuente: Properati — elaboración propia a partir del texto"
  ) +
  theme_taller

ggsave(file.path(path_figures, "07_text_vars.pdf"),
       p4_text, width = 8, height = 6)


# =============================================================================
# 5. VARIABLES ESPACIALES (OSM)
# =============================================================================

# --- 5.1 Distribución de distancias ---
osm_long <- base_train_osm %>%
  select(dist_transporte_m, dist_colegio_m, dist_parque_m) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "distancia_m") %>%
  mutate(variable = recode(variable,
                           dist_transporte_m = "Transporte público",
                           dist_colegio_m    = "Colegio más cercano",
                           dist_parque_m     = "Parque más cercano"
  ))

p5_osm <- ggplot(osm_long, aes(x = distancia_m / 1000, fill = variable)) +
  geom_histogram(bins = 40, alpha = 0.85, color = "white") +
  facet_wrap(~ variable, scales = "free") +
  scale_fill_manual(values = c("#2C7BB6", "#D7191C", "#1A9641")) +
  scale_x_continuous(labels = function(x) paste0(x, " km")) +
  labs(
    title    = "Distribución de distancias a amenidades",
    subtitle = "Bogotá — datos de entrenamiento",
    x        = "Distancia (km)",
    y        = "Número de propiedades",
    caption  = "Fuente: OpenStreetMap"
  ) +
  theme_taller +
  theme(legend.position = "none")

ggsave(file.path(path_figures, "08_osm_distancias.pdf"),
       p5_osm, width = 10, height = 4)


# --- 5.2 Distribución de restaurantes en 500m ---
p5_restaurantes <- ggplot(base_train_osm, aes(x = n_restaurantes_500m)) +
  geom_histogram(bins = 30, fill = "#2C7BB6", color = "white", alpha = 0.85) +
  scale_x_continuous(breaks = seq(0, max(base_train_osm$n_restaurantes_500m, na.rm = TRUE), by = 5)) +
  labs(
    title    = "Restaurantes en un radio de 500 metros",
    subtitle = "Bogotá — datos de entrenamiento",
    x        = "Número de restaurantes",
    y        = "Número de propiedades",
    caption  = "Fuente: OpenStreetMap"
  ) +
  theme_taller

ggsave(file.path(path_figures, "08b_restaurantes_500m.pdf"),
       p5_restaurantes, width = 7, height = 5)


# --- 5.2 Precio vs distancia al transporte ---
p5_transport <- base_train_osm %>%
  filter(!is.na(dist_transporte_m)) %>%
  mutate(dist_bin = cut(dist_transporte_m,
                        breaks = c(0, 250, 500, 1000, 2000, Inf),
                        labels = c("<250m", "250-500m", "500m-1km", "1-2km", ">2km"))) %>%
  group_by(dist_bin) %>%
  summarise(precio_medio = mean(price, na.rm = TRUE) / 1e6) %>%
  ggplot(aes(x = dist_bin, y = precio_medio)) +
  geom_col(fill = "#2C7BB6", alpha = 0.85) +
  geom_text(aes(label = comma(round(precio_medio), suffix = "M")),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = comma_format(suffix = "M")) +
  labs(
    title    = "Precio promedio por distancia al transporte público",
    subtitle = "Bogotá — datos de entrenamiento",
    x        = "Distancia a parada de transporte",
    y        = "Precio promedio (millones COP)",
    caption  = "Fuente: Properati + OpenStreetMap"
  ) +
  theme_taller

ggsave(file.path(path_figures, "09_precio_transporte.pdf"),
       p5_transport, width = 7, height = 5)


# =============================================================================
# 6. TRAIN VS CHAPINERO (TEST)
# =============================================================================

# --- 6.1 Mapa de puntos train vs test ---
# (requiere que base_train_osm y base_test_osm tengan lat/lon)
map_data <- bind_rows(
  base_train_osm %>% select(lat, lon, price) %>% mutate(muestra = "Bogotá (train)"),
  base_test_osm  %>% select(lat, lon) %>% mutate(price = NA, muestra = "Chapinero (test)")
)

p6_map <- ggplot(map_data, aes(x = lon, y = lat, color = muestra)) +
  geom_point(size = 0.4, alpha = 0.4) +
  scale_color_manual(values = c("Bogotá (train)" = "#2C7BB6",
                                "Chapinero (test)" = "#D7191C")) +
  labs(
    title    = "Distribución espacial de las propiedades",
    subtitle = "Train (toda Bogotá) vs Test (Chapinero)",
    x        = "Longitud",
    y        = "Latitud",
    color    = NULL,
    caption  = "Fuente: Properati"
  ) +
  theme_taller +
  theme(legend.position = "right")

ggsave(file.path(path_figures, "10_mapa_train_test.pdf"),
       p6_map, width = 7, height = 6)


# --- 6.2 Comparación de precio train vs test ---
# (test no tiene precio, comparamos variables estructurales)
comp_beds <- bind_rows(
  base_train_osm %>% select(bedrooms) %>% mutate(muestra = "Bogotá (train)"),
  base_test_osm  %>% select(bedrooms) %>% mutate(muestra = "Chapinero (test)")
) %>%
  filter(bedrooms >= 1, bedrooms <= 6) %>%
  count(muestra, bedrooms) %>%
  group_by(muestra) %>%
  mutate(pct = n / sum(n) * 100)

p6_comp <- ggplot(comp_beds, aes(x = factor(bedrooms), y = pct, fill = muestra)) +
  geom_col(position = "dodge", alpha = 0.85) +
  scale_fill_manual(values = c("Bogotá (train)" = "#2C7BB6",
                               "Chapinero (test)" = "#D7191C")) +
  labs(
    title    = "Distribución de habitaciones: Bogotá vs Chapinero",
    subtitle = "Diferencias en la composición del stock de vivienda",
    x        = "Número de habitaciones",
    y        = "% de propiedades",
    fill     = NULL,
    caption  = "Fuente: Properati"
  ) +
  theme_taller

ggsave(file.path(path_figures, "11_comp_habitaciones.pdf"),
       p6_comp, width = 8, height = 5)


# --- 6.3 Comparación tipo de propiedad train vs test ---
comp_type <- bind_rows(
  base_train %>% select(property_type) %>% mutate(muestra = "Bogotá (train)"),
  base_test  %>% select(property_type) %>% mutate(muestra = "Chapinero (test)")
) %>%
  count(muestra, property_type) %>%
  group_by(muestra) %>%
  mutate(pct = n / sum(n) * 100)

p6_type <- ggplot(comp_type, aes(x = muestra, y = pct, fill = property_type)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_manual(values = c("Apartamento" = "#2C7BB6", "Casa" = "#D7191C")) +
  labs(
    title    = "Tipo de propiedad: Bogotá vs Chapinero",
    subtitle = "Chapinero tiene casi exclusivamente apartamentos",
    x        = NULL,
    y        = "% de propiedades",
    fill     = NULL,
    caption  = "Fuente: Properati"
  ) +
  theme_taller

ggsave(file.path(path_figures, "12_comp_tipo.pdf"),
       p6_type, width = 6, height = 5)

text_compare <- bind_rows(
  base_train %>%
    summarise(across(c(gym, elevator, parking, balcony, pool,
                       security, green_area, remodeled, new_property, 
                       luxury, storage),
                     ~ mean(.) * 100)) %>%
    mutate(muestra = "Bogotá (train)"),
  
  base_test %>%
    summarise(across(c(gym, elevator, parking, balcony, pool,
                       security, green_area, remodeled, new_property,
                       luxury, storage),
                     ~ mean(.) * 100)) %>%
    mutate(muestra = "Chapinero (test)")
) %>%
  pivot_longer(-muestra, names_to = "variable", values_to = "pct") %>%
  mutate(variable = recode(variable,
                           gym          = "Gimnasio",
                           elevator     = "Ascensor",
                           parking      = "Parqueadero",
                           balcony      = "Balcón/Terraza",
                           pool         = "Piscina",
                           security     = "Seguridad/Portería",
                           green_area   = "Zona verde/Parque",
                           remodeled    = "Remodelado",
                           new_property = "Propiedad nueva",
                           luxury       = "Acabados de lujo",
                           storage      = "Depósito/Bodega"
  ))

ggplot(text_compare, aes(x = reorder(variable, pct), y = pct, fill = muestra)) +
  geom_col(position = "dodge", alpha = 0.85) +
  coord_flip() +
  scale_fill_manual(values = c("Bogotá (train)" = "#2C7BB6",
                               "Chapinero (test)" = "#D7191C")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Amenidades mencionadas: Bogotá vs Chapinero",
    subtitle = "% de propiedades que mencionan cada amenidad",
    x        = NULL,
    y        = "% de propiedades",
    fill     = NULL,
    caption  = "Fuente: Properati — elaboración propia a partir del texto"
  ) +
  theme_taller
# =============================================================================
# TABLA RESUMEN (para diapositivas)
# =============================================================================

tabla_resumen <- bind_rows(
  base_train %>% summarise(
    muestra = "Bogotá (train)",
    n = n(),
    precio_medio = mean(price, na.rm = TRUE) / 1e6,
    precio_mediana = median(price, na.rm = TRUE) / 1e6,
    pct_apartamento = mean(property_type == "Apartamento") * 100,
    bedrooms_medio = mean(bedrooms, na.rm = TRUE)
  ),
  base_test %>% summarise(
    muestra = "Chapinero (test)",
    n = n(),
    precio_medio = NA,
    precio_mediana = NA,
    pct_apartamento = mean(property_type == "Apartamento") * 100,
    bedrooms_medio = mean(bedrooms, na.rm = TRUE)
  )
)

write.csv(tabla_resumen,
          file.path(path_tables, "01_tabla_resumen.csv"),
          row.names = FALSE)

message("✓ Figuras guardadas en: ", path_figures)
message("✓ Tablas guardadas en: ", path_tables)