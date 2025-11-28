
# pacotes -----------------------------------------------------------------
library(dplyr)
library(SpatialKDE)
library(sp)
library(sf)
library(dplyr)
library(tmap)

# leitura -----------------------------------------------------------------
caminho <- "bases/Flora_final.parquet"

flora <- 
  arrow::read_parquet(caminho) %>% 
  janitor::clean_names() %>% 
  filter(phylum == "Tracheophyta") 


# amostra para teste ------------------------------------------------------
set.seed(12)
amostra <- sample(1:nrow(flora), 100)

flora <- flora %>% 
  slice(amostra)

# DESCRITIVA --------------------------------------------------------------
flora %>% 
  select(decimal_lat, decimal_lon, latitude, longitude) %>% 
  head(100) 

# DESCRITIVA ESPACIAL --------------------------------------------------------------

pontos_sf <- st_as_sf(
  flora,
  coords = c("latitude", "longitude"),
  crs = 4326   # EPSG:4326 = sistema latitude/longitude
)

pontos_sf

plot(pontos_sf["phylum"], pch = 19)



# kde ---------------------------------------------------------------------
flora_sf <- flora %>%
  st_as_sf(coords = c("longitude", "latitude"), dim = "XY") %>%
  st_set_crs(4326) %>%     # CRS WGS84 (latitude/longitude)
  select() 

flora_m <- st_transform(flora_sf, 31984)  

cell_size <- 2000     
band_width <- 10000 #50000      

grid_flora <- flora_m %>%
  create_grid_rectangular(
    cell_size = cell_size,
    side_offset = band_width
  )

kde <- flora_m %>%
  kde(band_width = band_width, kernel = "quartic", grid = grid_flora)


tm_shape(kde) +
  tm_polygons(
    col = "kde_value", 
    title = "KDE",
    border.col = NA,    interpolate = TRUE 
              ) +
  tm_shape(flora_m) +
  tm_bubbles(size = 0.05, col = "red") +
  tm_layout(legend.show = TRUE)

