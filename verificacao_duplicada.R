
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


# flora %>% head() %>% View
# 
# duplicadas <- flora %>% 
#   select(institutio, species, latitude, longitude) %>% 
#   group_by(species, latitude, longitude) %>% 
#   filter(n() > 1)
# 
# flora %>% 
#   filter(institutio == "CNCFlora" &
#         species == "Acianthera auriculata") %>% 
#         #  latitude == "-20.17694" &
#         #  longitude == "-40.92417") %>% 
#   View()
# 
# flora %>% 
#   filter( 
#    latitude == as.numeric(-20.17694) ) %>%
#   View()
# 
# duplicadas_unicas <- df %>%
#   count(col1, col2, col3) %>%
#   filter(n > 1)
# 

