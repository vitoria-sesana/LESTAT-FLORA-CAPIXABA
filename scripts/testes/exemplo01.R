library(terra)

# Read a raster file (e.g., elevation data)
path_raster <- system.file("ex/elev.tif", package = "terra")
elevation_raster <- rast(path_raster)

# Read a vector file (e.g., administrative boundaries)
path_vector <- system.file("ex/lux.shp", package = "terra")
boundaries_vector <- vect(path_vector)

# Plotting
plot(elevation_raster, main = "Elevation Raster")
plot(boundaries_vector, add = TRUE, col = "transparent", border = "blue")

elevation_raster |> class()
elevation_raster$elevation |> class()
elevation_raster$elevation 
