library(dplyr)
library(sf)

# leitura -----------------------------------------------------------------
# base flora
caminho_flora <- "bases/2025-10-01_Estatísticas Básica.xlsx"

base_flora <- 
  readxl::read_xlsx(caminho, sheet = "Flora")


map <- sf::st_read("bases/QGis_Vetores-Matriciais/vetor/geofisic/Clima-Brasil_IBGE_2002.shp")
class(map)
head(map)
plot(map)
