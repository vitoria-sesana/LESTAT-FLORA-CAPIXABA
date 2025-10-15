# Carregamento de pacotes
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)
library(gt)
library(webshot2)
library(treemapify)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(cowplot)
library(stringr)
library(glue)


# Carregando dados da Flora Capixaba
plantae <- read_csv("data/reino_plantae_es.csv")

# Filtrando variáveis que serão usadas (com base no texto de qualificação)
dados <- plantae |>
  select(recordID, DatasetName, Institution, Collection, kingdom, phylum,
         Class, order, family, genus, Species, Subspecies, institutionCode,
         collectionCode, locality, `Latitude - original`, `Longitude - original`,
         decimalLatitude, decimalLongitude, stateProvince, year, month, day,
         occurrenceStatus) |>
  clean_names() |>
  filter(state_province == "Espirito Santo")

write.csv(dados, "data/base_filtrada.csv")


############### INCOMPLETUDE ####################################
# Número de linhas incompletas e completas para cada coluna
# Apenas algumas variáveis foram selecionadas (as mais relevantes para os objetivos)

dados_na <- dados |>
  select(institution, collection, kingdom, phylum, class, order,
         family, genus, species, subspecies) |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(cols = everything(),
               names_to = "variavel",
               values_to = "n_na") |>
  mutate(total = nrow(dados),
         n_preenchidos = total - n_na) |>
  pivot_longer(cols = c(n_na, n_preenchidos),
               names_to = "tipo",
               values_to = "n") |>
  mutate(percentual = round(n / total *100, 2),
         tipo = recode(tipo,
                       n_na = "Faltantes",
                       n_preenchidos = "Preenchidos"))
  
# Ordenar variáveis da menor % de faltantes para o menor
ordem_vars <- dados_na |>
  filter(tipo == "Faltantes") |>
  arrange(desc(percentual)) |>
  pull(variavel)

dados_na$variavel <- factor(dados_na$variavel, levels = ordem_vars)

# Gráfico empilhado
p1 <- ggplot(dados_na, aes(x = variavel, y = percentual, fill = tipo)) +
  geom_bar(stat = "identity", width = 0.7, color = "white") +
  geom_text(
    aes(label = paste0(round(percentual, 1), "%")),
    position = position_stack(vjust = 0.5),
    color = "#444444",
    size = 4.2,
    fontface = "bold"
  ) +
  coord_flip() +
  labs(
    title = "Porcentagem de dados faltantes por variável",
    x = "Variável",
    y = "Percentual (%)",
    fill = "Tipo de dado"
  ) +
  scale_fill_manual(values = c("Faltantes" = "#e74c3c", "Preenchidos" = "#2ecc71")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  )

ggsave("outputs/incompletude_empilhado.png", p1)

# Tabela de Incompletude
tabela_incompletude <- dados |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(cols = everything(),
               names_to = "Variável",
               values_to = "Faltantes") |>
  mutate(
    Total = nrow(dados),
    Preenchidos = Total - Faltantes,
    `% Faltantes` = paste0(round(100 * Faltantes / Total, 1), "%"),
    `% Preenchidos` = paste0(round(100 * Preenchidos / Total, 1), "%")
  ) |>
  select(Variável, Total, Faltantes, `% Faltantes`, Preenchidos, `% Preenchidos`) |>
  arrange(desc(Faltantes))

# Tabela estilizada
tabela_gt <- gt(tabela_incompletude) |>
  tab_header(
    title = md("**Resumo de Incompletude dos Dados**")
  ) |>
  fmt_number(
    columns = starts_with("%"),
    decimals = 1
  ) |>
  tab_options(
    table.border.top.color = "#1b263b",
    table.border.bottom.color = "#1b263b",
    table.border.top.width = px(2),
    table.border.bottom.width = px(2),
    column_labels.background.color = "#1b263b",
    heading.background.color = "#1b263b",
    table.font.names = "Arial",
    table.font.size = px(13),
    data_row.padding = px(6)
  ) |>
  # --- Estilo do cabeçalho e rótulos ---
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = list(
      cells_column_labels(),
      cells_title(groups = "title")
    )
  ) |>
  tab_style(
    style = cell_fill(color = "#f9f9f9"),
    locations = cells_body()
  )

# Visualizar no RStudio Viewer
tabela_gt

# Exportar como imagem PNG
gtsave(tabela_gt, "outputs/tabela_incompletude.png")

############# FREQUÊNCIA DE COLEÇÃO E INSTITUIÇÃO ###########

# 10 instituições mais frequentes
dados_top10_instituicoes <- dados |>
  count(institution_code) |>
  mutate(perc = n / sum(n)) |>
  arrange(desc(n)) |>
  slice_head(n = 10) |>
  mutate(
    institution_code = case_when(
      is.na(institution_code) ~ "Sem informação",
      !is.na(institution_code) ~ institution_code)
  ) |>
  mutate(
    label = paste0(
      institution_code, "\n",
      n, " (", percent(perc, accuracy = 0.1), ")"
    )
  )

# Treemap para instituições
p2 <- ggplot(dados_top10_instituicoes, aes(area = n, fill = n, label = label)) +
  geom_treemap(color = "white") +
  geom_treemap_text(
    colour = "white",
    place = "center",
    grow = FALSE,         # mantém o tamanho do texto fixo
    reflow = TRUE,        # quebra de linha automática
    size = 14             # tamanho fixo da fonte
  ) +
  scale_fill_gradient(
    low = "#6baed6", high = "#08306b",
    name = "Frequência"
  ) +
  labs(
    title = "10 Instituições Mais Frequentes"  ) +
  theme_void(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5)
  )

ggsave("outputs/treeplot_instituicoes.png", p2)

# 10 coleções mais frequentes
dados_top10_colecoes <- dados |>
  count(collection_code) |>
  mutate(perc = n / sum(n)) |>
  arrange(desc(n)) |>
  slice_head(n = 10) |>
  mutate(
    collection_code = case_when(
      is.na(collection_code) ~ "Sem informação",
      !is.na(collection_code) ~ collection_code)
  ) |>
  mutate(
    label = paste0(
      collection_code, "\n",
      n, " (", percent(perc, accuracy = 0.1), ")"
    )
  )

# Treemap para coleções
p3 <- ggplot(dados_top10_colecoes, aes(area = n, fill = n, label = label)) +
  geom_treemap(color = "white") +
  geom_treemap_text(
    colour = "white",
    place = "center",
    grow = FALSE,         # mantém o tamanho do texto fixo
    reflow = TRUE,        # quebra de linha automática
    size = 14             # tamanho fixo da fonte
  ) +
  scale_fill_gradient(
    low = "#6baed6", high = "#08306b",
    name = "Frequência"
  ) +
  labs(
    title = "10 Coleções Mais Frequentes") +
  theme_void(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5)
  )

ggsave("outputs/treeplot_colecoes.png", p3)

# Tabela para instituições

tabela_instituicoes <- dados |>
  mutate(institution_code = case_when(
    is.na(institution_code) ~ "Sem informação",
    !is.na(institution_code) ~ institution_code))|>
  count(institution_code, name = "Frequência") |>
  arrange(desc(Frequência)) |>
  mutate(Rank = row_number()) |>
  mutate(
    Instituição = ifelse(Rank <= 20, institution_code, "Outros")
  ) |>
  group_by(Instituição) |>
  summarise(Frequência = sum(Frequência), .groups = "drop") |>
  mutate(
    `%` = paste0(round(Frequência / sum(Frequência)*100, 1), "%")
  ) |>
  arrange(desc(Frequência))

# Criar tabela GT estilizada
tabela_gt_inst <- gt(tabela_instituicoes) |>
  tab_header(
    title = md("**20 Instituições Mais Frequentes**")
  ) |>
  fmt_percent(
    columns = "%", decimals = 1
  ) |>
  tab_options(
    table.border.top.color = "#1b263b",
    table.border.bottom.color = "#1b263b",
    table.border.top.width = px(2),
    table.border.bottom.width = px(2),
    column_labels.background.color = "#1b263b",
    heading.background.color = "#1b263b",
    table.font.names = "Arial",
    table.font.size = px(13),
    data_row.padding = px(6)
  ) |>
  # Estilo do cabeçalho e rótulos
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = list(
      cells_column_labels(),
      cells_title(groups = "title")
    )
  ) |>
  tab_style(
    style = cell_fill(color = "#f9f9f9"),
    locations = cells_body()
  )

tabela_gt_inst

gtsave(tabela_gt_inst, "outputs/tabela_instituicoes.png")

# Tabela para coleções

tabela_colecoes <- dados |>
  mutate(collection_code = case_when(
    is.na(collection_code) ~ "Sem informação",
    !is.na(collection_code) ~ collection_code))|>
  count(collection_code, name = "Frequência") |>
  arrange(desc(Frequência)) |>
  mutate(Rank = row_number()) |>
  mutate(
    Coleção = ifelse(Rank <= 20, collection_code, "Outros")
  ) |>
  group_by(Coleção) |>
  summarise(Frequência = sum(Frequência), .groups = "drop") |>
  mutate(
    `%` = paste0(round(Frequência / sum(Frequência)*100, 1), "%")
  ) |>
  arrange(desc(Frequência))

# Criar tabela GT estilizada
tabela_gt_collec <- gt(tabela_colecoes) |>
  tab_header(
    title = md("**20 Coleções Mais Frequentes**")
  ) |>
  fmt_percent(
    columns = "%", decimals = 1
  ) |>
  tab_options(
    table.border.top.color = "#1b263b",
    table.border.bottom.color = "#1b263b",
    table.border.top.width = px(2),
    table.border.bottom.width = px(2),
    column_labels.background.color = "#1b263b",
    heading.background.color = "#1b263b",
    table.font.names = "Arial",
    table.font.size = px(13),
    data_row.padding = px(6)
  ) |>
  # Estilo do cabeçalho e rótulos
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = list(
      cells_column_labels(),
      cells_title(groups = "title")
    )
  ) |>
  tab_style(
    style = cell_fill(color = "#f9f9f9"),
    locations = cells_body()
  )

tabela_gt_collec

gtsave(tabela_gt_collec, "outputs/tabela_colecoes.png")


########### MAPA DE DISTRIBUIÇÃO DOS FILOS - CÓDIGO ÚNICO PARA MACRORREGIÃO ############

# Coordenadas dos registros
dados_registros <- data.frame(
  filo = dados$phylum,
  latitude = dados$decimal_latitude,
  longitude = dados$decimal_longitude
) |>
  mutate(
    filo = case_when(
      is.na(filo) ~ "Sem informação",
      !is.na(filo) ~ filo
      
    )
  )

# Tranformando registros em shapefile
dados_sf <- st_as_sf(
  dados_registros,
  coords = c("longitude", "latitude"),
  crs = 4326 # WGS84 (sistema padrão de coordenadas geográficas)
)

es <- st_read("vetores/Macrorregiao.shp")
es <- st_transform(es, 4326)



mapa_principal <- ggplot() +
  # Espírito Santo
  geom_sf(data = es, fill = "gray98", color = "#444444", linewidth = 0.3) +
  # Pontos das observações
  geom_sf(data = dados_sf, aes(color = filo), size = 1.5, alpha = 0.9) +
  # Escala e norte
  annotation_scale(location = "bl", width_hint = 0.35, line_width = 0.5, text_cex = 0.9) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(1.1, "cm"),
    width = unit(1.1, "cm")
  ) +
  # Paleta de cores mais equilibrada
  scale_color_manual(
    name = "Filos",
    values = c(
      "Anthocerotophyta" = "#5B5EA6",
      "Bryophyta" = "#009B77",
      "Charophyta" = "#F6A600",
      "Chlorophyta" = "#7BB661",
      "Marchantiophyta" = "#92A8D1",
      "Rhodophyta" = "#DD4124",
      "Sem informação" = "#444444",
      "Tracheophyta" = "#45B8AC"
    )
  ) +
  coord_sf(
    xlim = st_bbox(es)[c("xmin", "xmax")],
    ylim = st_bbox(es)[c("ymin", "ymax")],
    expand = TRUE
  ) +
  labs(
    title = "Distribuição da Flora por Filo no \n Espírito Santo: ",
    subtitle = "Fonte: SiBBr",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 22),
    legend.text = element_text(size = 20),
    legend.key.size = unit(2.5, "cm"),
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5),
    plot.subtitle = element_text(size = 24, hjust = 0.5),
    plot.margin = margin(10, 30, 10, 30)
  )

# Mini mapa do Brasil (em destaque)
brasil <- ne_states(country = "brazil", returnclass = "sf")

localizacao <- ggplot() +
  geom_sf(data = brasil, fill = "gray85", color = "white") +
  geom_sf(data = es, fill = "#DD4124", color = "black", linewidth = 0.3) +
  theme_void() +
  theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7))

# Combinar o mapa principal e do Brasil
mapa_completo <- ggdraw() +
  draw_plot(mapa_principal) +
  draw_plot(localizacao, x = 0.70, y = 0.63, width = 0.25, height = 0.25)

# Exportar imagem final
# ggsave(
#   filename = "mapa_ES_final.png",
#   plot = mapa_completo,
#   width = 30, height = 45, units = "cm", dpi = 400
# )


########### MAPA DE DISTRIBUIÇÃO DOS FILOS - ITERAÇÃO PARA MAPAS ############

# Arquivos (usei todos os arquivos da pasta do Drive LESTAT FLORA CAPIXABA>Arquivos Cláudia>QGIS>_Vetor)
vetores_dir <- "vetores/"
output_dir <- "outputs/"

if (!dir.exists(output_dir)) dir.create(output_dir)

shapefiles <- list.files(vetores_dir, pattern = "\\.shp$", full.names = TRUE)

# Vetor de títulos automático
titulos <- shapefiles |>
  basename() |>
  tools::file_path_sans_ext() |>
  gsub("_", " ", x = _) |>
  gsub("\\s+", " ", x = _) |>
  trimws() |>
  str_to_title()

dados_registros <- data.frame(
  filo = dados$phylum,
  latitude = dados$decimal_latitude,
  longitude = dados$decimal_longitude
) |>
  mutate(filo = ifelse(is.na(filo), "Sem informação", filo))

dados_sf <- st_as_sf(dados_registros, coords = c("longitude", "latitude"), crs = 4326)

# Mini mapa do Brasil 
brasil <- ne_states(country = "brazil", returnclass = "sf")

# Mapa território ES

territorio <- st_read("vetores/0 Territorio.shp")
territorio <- st_transform(territorio, 4326)

oceano <- st_read("vetores/Oceano.shp")
oceano <- st_transform(oceano, 4326)

vizinhos <- st_read("vetores/Estados_Vizinhos_INPE_Mt-Atlant.shp")
vizinhos <- st_transform(vizinhos, 4326)


# Função para ler shapefile
ler_shp_seguro <- function(path) {
  tryCatch({
    shp <- st_read(path, quiet = TRUE)
    if (is.na(st_crs(shp))) {
      message(glue("CRS indefinido em {basename(path)}, aplicando 4326"))
      st_crs(shp) <- 4326
    }
    shp <- st_transform(shp, 4326)
    shp <- st_make_valid(shp)
    shp <- shp[!st_is_empty(shp), ]
    shp
  }, error = function(e) {
    message(glue("Erro ao ler {basename(path)}: {e$message}"))
    NULL
  })
}


# Loop principal — um mapa por shapefile
for (i in seq_along(shapefiles)) {
  shp_path <- shapefiles[i]
  shp_name <- tools::file_path_sans_ext(basename(shp_path))
  titulo_extra <- titulos[i]
  
  message(glue::glue(" Gerando mapa para: {shp_name}..."))
  
  es <- ler_shp_seguro(shp_path)
  if (is.null(es)) next
  
  mapa_principal <- ggplot() +
    # --- Camadas geográficas ---
    geom_sf(data = territorio, fill = "#D9D9D9", color = "#555555", linewidth = 0.4) +  
    geom_sf(data = oceano, fill = "#CFE2F3", color = NA) +                               
    geom_sf(data = vizinhos, fill = "#F5F5F5", color = "#999999", linewidth = 0.3) +     
    geom_sf(data = es, fill = "#C4A484", color = "#2B2B2B", linewidth = 0.5) +           
    
    # --- Observações ---
    geom_sf(
      data = dados_sf,
      aes(color = filo, shape = filo),
      size = 2.3, alpha = 0.85, stroke = 0.6
    ) +
    
    # --- Escala e norte ---
    annotation_scale(location = "bl", width_hint = 0.35, line_width = 0.5, text_cex = 0.9) +
    annotation_north_arrow(
      location = "tr",
      which_north = "true",
      style = north_arrow_fancy_orienteering,
      height = unit(1.1, "cm"),
      width = unit(1.1, "cm")
    ) +
    
    # --- Paleta de cores harmoniosa e contrastante ---
    scale_color_manual(
      name = "Filos",
      values = c(
        "Tracheophyta"      = "#1B9E77",  
        "Bryophyta"         = "#66A61E",  
        "Charophyta"        = "#E6AB02",  
        "Chlorophyta"       = "#7570B3",  
        "Marchantiophyta"   = "#A6761D",  
        "Rhodophyta"        = "#D95F02",  
        "Anthocerotophyta"  = "#E7298A",  
        "Sem informação"    = "#666666"   
      )
    ) +
    
    # --- Formatos distintos por filo ---
    scale_shape_manual(
      name = "Filos",
      values = c(
        "Tracheophyta"      = 16,  # círculo
        "Bryophyta"         = 17,  # triângulo
        "Charophyta"        = 18,  # losango
        "Chlorophyta"       = 15,  # quadrado
        "Marchantiophyta"   = 3,   # cruz
        "Rhodophyta"        = 8,   # estrela
        "Anthocerotophyta"  = 4,   # X
        "Sem informação"    = 1    # círculo vazado
      )
    ) +
    
    coord_sf(
      xlim = st_bbox(es)[c("xmin", "xmax")],
      ylim = st_bbox(es)[c("ymin", "ymax")],
      expand = TRUE
    ) +
    
    labs(
      title = paste0("Distribuição da Flora por Filo no \nEspírito Santo: ", titulo_extra),
      subtitle = "Fonte: SiBBr",
      x = NULL, y = NULL
    ) +
    
    theme_minimal(base_size = 18) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 22),
      legend.text = element_text(size = 20),
      legend.key.size = unit(2.2, "cm"),
      plot.title = element_text(face = "bold", size = 30, hjust = 0.5),
      plot.subtitle = element_text(size = 24, hjust = 0.5, color = "#333333"),
      plot.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = "#CCCCCC"),
      plot.margin = margin(10, 30, 10, 30)
    )
  
  # Mini mapa do Brasil
  localizacao <- ggplot() +
    geom_sf(data = brasil, fill = "gray85", color = "white") +
    geom_sf(data = es, fill = "#DD4124", color = "black", linewidth = 0.3) +
    theme_void() +
    theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7))
  
  # Combinar os mapas
  mapa_completo <- ggdraw() +
    draw_plot(mapa_principal) +
    draw_plot(localizacao, x = 0.70, y = 0.63, width = 0.25, height = 0.25)
  
  # Salvar resultado
  output_path <- file.path(output_dir, paste0("mapa_", shp_name, ".png"))
  
  ggsave(
    filename = output_path,
    plot = mapa_completo,
    width = 30, height = 45, units = "cm", dpi = 400
  )
  
  message(glue::glue("Mapa salvo em: {output_path}"))
}
