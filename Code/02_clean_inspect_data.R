# =============================================================================
# 02_clean_inspect_data.R
# Descripcion: Limpieza, renombre y analisis exploratorio de las bases
#              train y test
# =============================================================================

length(unique(train$property_id))
length(unique(train$city))
unique(train$city)
length(unique(train$price))
length(unique(train$month))
unique(train$month)
length(unique(train$year))
unique(train$year)
length(unique(train$surface_total))
length(unique(train$surface_covered))
length(unique(train$rooms))
length(unique(train$bedrooms))
length(unique(train$bathrooms))
length(unique(train$property_type))
unique(train$property_type)
length(unique(train$operation_type))
unique(train$operation_type)
length(unique(train$lat))
length(unique(train$lon))
length(unique(train$title))
length(unique(train$description))

range(train$price)
range(train$surface_total, na.rm = TRUE)
range(train$surface_covered, na.rm = TRUE)
range(train$bedrooms, na.rm = TRUE)
range(train$rooms, na.rm = TRUE)
range(train$bathrooms, na.rm = TRUE)

mean(train$price, na.rm = TRUE)
mean(train$surface_total, na.rm = TRUE)
mean(train$surface_covered, na.rm = TRUE)
mean(train$rooms, na.rm = TRUE)
mean(train$bedrooms, na.rm = TRUE)
mean(train$bathrooms, na.rm = TRUE)

colSums(is.na(train))


# 1. Train ----------------------------------------------------------------



# ------------------------------------------------------------
# Función para limpiar texto
# ------------------------------------------------------------
limpiar_texto <- function(x) {
  x %>%
    str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[^a-z0-9\\s]", " ") %>%
    str_squish()
}

# ------------------------------------------------------------
# Crear variables a partir del texto
# ------------------------------------------------------------

base_train <- train %>%
  mutate(
    texto = limpiar_texto(str_c(title, description, sep = " ")),
    
    gym = if_else(
      str_detect(texto, "\\b(gimnasio|gym)\\b"),
      1, 0
    ),
    
    elevator = if_else(
      str_detect(texto, "\\b(ascensor|elevador)\\b"),
      1, 0
    ),
    
    parking = if_else(
      str_detect(texto, "\\b(parking|parqueadero|garaje|garage)\\b"),
      1, 0
    ),
  )


base_train <- train %>%
  mutate(
    title = coalesce(title, ""),
    description = coalesce(description, ""),
    
    texto = limpiar_texto(str_c(title, description, sep = " ")),
    
    gym = as.integer(str_detect(texto, "\\b(gimnasio|gym)\\b")),
    elevator = as.integer(str_detect(texto, "\\b(ascensor|elevador)\\b")),
    parking = as.integer(str_detect(texto, "\\b(parking|parqueadero|garaje|garage)\\b")),
    balcony = as.integer(str_detect(texto, "\\b(balcon|balcones|terraza)\\b")),
    pool = as.integer(str_detect(texto, "\\b(piscina|pool)\\b")),
    security = as.integer(str_detect(texto, "\\b(vigilancia|seguridad|porteria|portero|recepcion)\\b")),
    green_area = as.integer(str_detect(texto, "\\b(zona verde|zonas verdes|parque|jardin)\\b")),
    remodeled = as.integer(str_detect(texto, "\\b(remodelado|remodelada|renovado|renovada)\\b")),
    new_property = as.integer(str_detect(texto, "\\b(nuevo|nueva|estrenar|para estrenar)\\b"))
  )

base_train %>%
  summarise(across(
    c(gym, elevator, parking, balcony, pool, security, green_area, remodeled, new_property),
    sum
  ))

# ------------------------------------------------------------
# Crear variables a partir de OpenStreetMap
# ------------------------------------------------------------

propiedades_sf <- base_train %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(3116)  # coordenadas en metros para Colombia

bbox_bogota <- getbb("Bogota, Colombia")

sf::sf_use_s2(FALSE)

extraer_geometrias_osm <- function(osm_obj) {
  bind_rows(
    osm_obj$osm_points,
    st_centroid(osm_obj$osm_polygons),
    st_centroid(osm_obj$osm_multipolygons)
  ) %>%
    filter(!st_is_empty(.)) %>%
    st_transform(3116)
}

consultar_osm_cache <- function(nombre_archivo, key, value) {
  
  archivo <- file.path(path_raw, nombre_archivo)
  
  if (file.exists(archivo)) {
    
    message("Cargando desde archivo local: ", nombre_archivo)
    osm_obj <- readRDS(archivo)
    
  } else {
    
    message("Consultando OSM: ", nombre_archivo)
    
    osm_obj <- opq(bbox = bbox_bogota, timeout = 120) %>%
      add_osm_feature(key = key, value = value) %>%
      osmdata_sf()
    
    saveRDS(osm_obj, archivo)
  }
  
  extraer_geometrias_osm(osm_obj)
}

# Transporte público
transporte <- consultar_osm_cache(
  nombre_archivo = "transporte_osm.rds",
  key = "public_transport",
  value = c("station", "stop_position", "platform")
)

# Restaurantes
restaurantes <- consultar_osm_cache(
  nombre_archivo = "restaurantes_osm.rds",
  key = "amenity",
  value = "restaurant"
)

# Colegios
colegios <- consultar_osm_cache(
  nombre_archivo = "colegios_osm.rds",
  key = "amenity",
  value = "school"
)

# Parques
parques <- consultar_osm_cache(
  nombre_archivo = "parques_osm.rds",
  key = "leisure",
  value = "park"
)

# ------------------------------------------------------------
# Crear variables espaciales
# ------------------------------------------------------------

# Distancia al transporte público más cercano

# Limpiar geometrías de transporte
transporte <- transporte %>%
  filter(!st_is_empty(.)) %>%
  filter(!is.na(st_geometry(.)))

# Índice del punto de transporte más cercano
idx_transporte_cercano <- st_nearest_feature(propiedades_sf, transporte)

# Distancia al transporte público más cercano
propiedades_sf$dist_transporte_m <- as.numeric(
  st_distance(
    propiedades_sf,
    transporte[idx_transporte_cercano, ],
    by_element = TRUE
  )
)


# Número de restaurantes en un radio de 500 metros
buffer_500 <- st_buffer(propiedades_sf, dist = 500)

propiedades_sf$n_restaurantes_500m <- lengths(
  st_intersects(buffer_500, restaurantes)
)


# Distancia al colegio más cercano
idx_colegio_cercano <- st_nearest_feature(propiedades_sf, colegios)

propiedades_sf$dist_colegio_m <- as.numeric(
  st_distance(
    propiedades_sf,
    colegios[idx_colegio_cercano, ],
    by_element = TRUE
  )
)


# Distancia al parque más cercano

idx_parque_cercano <- st_nearest_feature(propiedades_sf, parques)

propiedades_sf$dist_parque_m <- as.numeric(
  st_distance(
    propiedades_sf,
    parques[idx_parque_cercano, ],
    by_element = TRUE
  )
)


# ------------------------------------------------------------
# Volver a formato data frame normal
# ------------------------------------------------------------

base_train_osm <- propiedades_sf %>%
  st_drop_geometry()

# Ajustes finales 
base_train_osm <- base_train_osm %>%
  mutate(
    house = as.integer(property_type == "Casa")
  )

base_train_final <- base_train_osm %>%
  select(
    -city,
    -property_type,
    -operation_type,
    -title,
    -description,
    -texto
  ) %>%
  drop_na()

# 2. Test -----------------------------------------------------------------

# ------------------------------------------------------------
# Crear variables a partir del texto
# ------------------------------------------------------------

base_test <- test %>%
  mutate(
    title = coalesce(title, ""),
    description = coalesce(description, ""),
    
    texto = limpiar_texto(str_c(title, description, sep = " ")),
    
    gym = as.integer(str_detect(texto, "\\b(gimnasio|gym)\\b")),
    elevator = as.integer(str_detect(texto, "\\b(ascensor|elevador)\\b")),
    parking = as.integer(str_detect(texto, "\\b(parking|parqueadero|garaje|garage)\\b")),
    balcony = as.integer(str_detect(texto, "\\b(balcon|balcones|terraza)\\b")),
    pool = as.integer(str_detect(texto, "\\b(piscina|pool)\\b")),
    security = as.integer(str_detect(texto, "\\b(vigilancia|seguridad|porteria|portero|recepcion)\\b")),
    green_area = as.integer(str_detect(texto, "\\b(zona verde|zonas verdes|parque|jardin)\\b")),
    remodeled = as.integer(str_detect(texto, "\\b(remodelado|remodelada|renovado|renovada)\\b")),
    new_property = as.integer(str_detect(texto, "\\b(nuevo|nueva|estrenar|para estrenar)\\b"))
  )

# ------------------------------------------------------------
# Crear variables a partir del texto
# ------------------------------------------------------------

test_sf <- base_test %>%
  mutate(row_id = row_number()) %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(3116)

# Distancia al transporte público más cercano
idx_transporte_cercano_test <- st_nearest_feature(test_sf, transporte)

test_sf$dist_transporte_m <- as.numeric(
  st_distance(
    test_sf,
    transporte[idx_transporte_cercano_test, ],
    by_element = TRUE
  )
)

# Número de restaurantes en un radio de 500 metros
buffer_500_test <- st_buffer(test_sf, dist = 500)

test_sf$n_restaurantes_500m <- lengths(
  st_intersects(buffer_500_test, restaurantes)
)

# Distancia al colegio más cercano
idx_colegio_cercano_test <- st_nearest_feature(test_sf, colegios)

test_sf$dist_colegio_m <- as.numeric(
  st_distance(
    test_sf,
    colegios[idx_colegio_cercano_test, ],
    by_element = TRUE
  )
)

# Distancia al parque más cercano
idx_parque_cercano_test <- st_nearest_feature(test_sf, parques)

test_sf$dist_parque_m <- as.numeric(
  st_distance(
    test_sf,
    parques[idx_parque_cercano_test, ],
    by_element = TRUE
  )
)

# ------------------------------------------------------------
# Volver a formato normal y unir con base_test
# ------------------------------------------------------------

test_osm_vars <- test_sf %>%
  st_drop_geometry() %>%
  select(
    row_id,
    dist_transporte_m,
    n_restaurantes_500m,
    dist_colegio_m,
    dist_parque_m
  )

base_test_osm <- base_test %>%
  mutate(row_id = row_number()) %>%
  left_join(test_osm_vars, by = "row_id") %>%
  select(-row_id)

# Ajustes finales 

base_test_osm <- base_test_osm %>%
  mutate(
    house = as.integer(property_type == "Casa")
  )

base_test_final <- base_test_osm %>%
  select(
    -city,
    -price,
    -property_type,
    -operation_type,
    -lat,
    -lon,
    -title,
    -description,
    -texto
  )
