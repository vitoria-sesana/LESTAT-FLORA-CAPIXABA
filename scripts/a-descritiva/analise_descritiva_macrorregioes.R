
# pacotes -----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(sf)       
library(dplyr)    
library(geobr)

# leitura --------------------------------------------------------------------
# base flora
caminho <- "outputs/reino_plantae_es/base_filtrada.parquet"
base_flora <- arrow::read_parquet(caminho)

# coordenadas estaduais
municipios <- geobr::read_municipality(year = 2020)
municipios_es <- municipios %>% filter(code_state == 32)

# tratamento espacial ----------------------------------------------------------
# deixando os pontos de latitude e longitude legíveis
pontos_sf = 
  sf::st_as_sf(
    base_flora,
    coords = c("decimal_longitude", "decimal_latitude"), 
    crs = 4326,  
    remove = FALSE
    )

# info sistema de referência
print(sf::st_crs(pontos_sf))
print(sf::st_crs(municipios_es))

# relacionando com as coordenadas estaduais 
pontos_sf <- 
  sf::st_transform(pontos_sf, st_crs(municipios_es))

# base tratada (falta macrorregiões)
base_flora_tratada <- 
  sf::st_join(pontos_sf, municipios_es, join = st_within)

View(base_flora_tratada)

base_flora_tratada$name_muni %>% table()

base_flora_tratada$name_muni %>% is.na %>% table()


# gráficos ----------------------------------------------------------------

quantidade_registros_municipais <- 
  base_flora_tratada %>% 
  group_by(code_muni) %>% 
  summarise( n = n()) %>% 
  na.omit() %>% 
  mutate(percentual = n / sum(n) * 100)


ggplot() +
  geom_sf(data = quantidade_registros_municipais, aes(fill = percentual), color = "white", size = 0.2, na.rm = FALSE) +
  coord_sf(xlim = c(-42,-39.5)) +
  labs(title = "", 
       size=8) +
  scale_fill_distiller(palette = "Reds",
                       name="Ipa Urb m2",
                       direction = 1, 
                       na.value = "white") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.4), 
        panel.background = element_rect(fill = '#E5E5E5'),
        legend.margin = margin(r=2,l=2,t=2,b=2),
        legend.position = c(0.85,0.2),
        legend.background = element_rect(size=0.1, linetype="solid", 
                                         colour ="black"))

library(ggplot2)
library(sf)

# Verifique o bbox se quiser ajustar visualmente
st_bbox(quantidade_registros_municipais)

# Mapa ajustado
ggplot() +
  geom_sf(data = quantidade_registros_municipais,
          aes(fill = percentual),
          color = "white",
          size = 0.2,
          na.rm = FALSE) +
  
  # Coordenadas ajustadas com X e Y (para garantir corte certo)
  coord_sf(
    xlim = c(-42, -39.5),
    ylim = c(-21.5, -18.5),
    expand = FALSE
  ) +
  
  # Escala de preenchimento
  scale_fill_distiller(
    palette = "Reds",
    name = "% Registros",
    direction = 1,
    na.value = "white"
  ) +
  
  # Títulos e legenda
  labs(
    title = "Distribuição percentual por município",
    subtitle = "Espírito Santo",
    fill = "% registros"
  ) +
  
  # Tema customizado
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.4),
    panel.background = element_rect(fill = '#E5E5E5'),
    legend.position = c(0.85, 0.2),
    legend.background = element_rect(size = 0.1, linetype = "solid", colour = "black"),
    legend.margin = margin(r = 2, l = 2, t = 2, b = 2),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

