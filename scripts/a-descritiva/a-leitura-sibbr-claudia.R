
# pacotes -----------------------------------------------------------------
library(dplyr)
library(sf)

# leitura -----------------------------------------------------------------
# base flora
caminho_flora <- "bases/2025-10-01_Estatísticas Básica.xlsx"

base_flora <- 
  readxl::read_xlsx(caminho_flora, sheet = "Flora")

# leitura dos mapas -------------------------------------------------------

# map <- sf::st_read("bases/QGis_Vetores-Matriciais/vetor/geofisic/Clima-Brasil_IBGE_2002.shp")
# class(map)
# head(map)
# plot(map)
# 
# 
# map2 <- sf::st_read("bases/QGis_Vetores-Matriciais/vetor/2025-01-18_Flora Final_SHP/Flora_separando_dados_fora_ES.shp")
# class(map2)
# head(map2)
# plot(map2)
# 
# map3 <- sf::st_read("bases/QGis_Vetores-Matriciais/vetor/geofisic/Clima-Brasil_IBGE_2002.shp")
# class(map3)
# head(map3)
# plot(map3)
# 
# 
# map4 <- sf::st_read("bases/QGis_Vetores-Matriciais/vetor/")
# class(map4)
# head(map4)
# plot(map4)
# 
# map4 <- sf::st_read("bases/QGis_Vetores-Matriciais/vetor/Recursos Naturais_UCs/UCs_Estaduais_IEMA_2022_editado.shp")
# class(map4)
# head(map4)
# plot(map4)
