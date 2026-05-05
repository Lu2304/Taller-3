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

# Nuevas variables
# 1. Función para limpiar texto
limpiar_texto <- function(x) {
  x %>%
    str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[^a-z0-9\\s]", " ") %>%
    str_squish()
}

# 2. Crear variables
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

# Variables de OpenStreetMap
propiedades_sf <- base_train %>%
  filter(!is.na(lon), !is.na(lat)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(3116)  # coordenadas en metros para Colombia

bbox_bogota <- getbb("Bogota, Colombia")

extraer_geometrias_osm <- function(osm_obj) {
  bind_rows(
    osm_obj$osm_points,
    st_centroid(osm_obj$osm_polygons),
    st_centroid(osm_obj$osm_multipolygons)
  ) %>%
    st_transform(3116)
}

sf::sf_use_s2(FALSE)

# Transporte público
transporte_osm <- opq(bbox = bbox_bogota) %>%
  add_osm_feature(
    key = "public_transport",
    value = c("station", "stop_position", "platform")
  ) %>%
  osmdata_sf()

transporte <- extraer_geometrias_osm(transporte_osm)


# Restaurantes
restaurantes_osm <- opq(bbox = bbox_bogota) %>%
  add_osm_feature(key = "amenity", value = "restaurant") %>%
  osmdata_sf()

restaurantes <- extraer_geometrias_osm(restaurantes_osm)


# Colegios
colegios_osm <- opq(bbox = bbox_bogota) %>%
  add_osm_feature(key = "amenity", value = "school") %>%
  osmdata_sf()

colegios <- extraer_geometrias_osm(colegios_osm)


# Parques
parques_osm <- opq(bbox = bbox_bogota) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf()

parques <- extraer_geometrias_osm(parques_osm)

# ------------------------------------------------------------
# 5. Crear variables espaciales
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
dist_colegio <- st_distance(propiedades_sf, colegios)

propiedades_sf$dist_colegio_m <- apply(dist_colegio, 1, min) %>%
  as.numeric()


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
# 6. Volver a formato data frame normal
# ------------------------------------------------------------

base_train_osm <- propiedades_sf %>%
  st_drop_geometry()


