# pacotes -----------------------------------------------------------------
library(SpatialKDE)
library(sp)
library(sf)
library(dplyr)
library(tmap)

data(meuse)


# DESCRITIVA --------------------------------------------------------------

# Exemplo de dados
dados <- data.frame(
  nome = c("Ponto A", "Ponto B"),
  lon = c(-46.6333, -43.2096),
  lat = c(-23.5505, -22.9035)
)

# Criar objeto sf
pontos_sf <- st_as_sf(
  flora,
  coords = c("latitude", "longitude"),
  crs = 4326   # EPSG:4326 = sistema latitude/longitude
)

pontos_sf

plot(pontos_sf["phylum"], pch = 19)

# KDE ---------------------------------------------------------------------

meuse <- meuse %>%
  st_as_sf(coords = c("x", "y"), dim = "XY") %>%
  st_set_crs(28992) %>%
  select()

meuse

cell_size <- 100
band_width <- 150

grid_meuse <- meuse %>%
  create_grid_rectangular(
    cell_size = cell_size,
    side_offset = band_width)

kde <- meuse %>%
  kde(band_width = band_width, kernel = "quartic", grid = grid_meuse)

tm_shape(kde) +
  tm_polygons(col = "kde_value", palette = "viridis", title = "KDE Estimate") +
  tm_shape(meuse) +
  tm_bubbles(size = 0.1, col = "red")


