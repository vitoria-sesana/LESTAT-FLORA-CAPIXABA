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
library(readxl)

dados <- read_excel("data/2025-10-01_Estatísticas Básica.xlsx", 
                    sheet = "Flora")[1:120183,]

############### INCOMPLETUDE ####################################
# Número de linhas incompletas e completas para cada coluna
# Apenas algumas variáveis foram selecionadas (as mais relevantes para os objetivos)

dados_na <- dados |>
  select(phylum, Class, order, family, genus, Species) |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(cols = everything(),
               names_to = "variavel",
               values_to = "n_na") |>
  mutate(total = nrow(dados),
         n_preenchidos = total - n_na,
         variavel = case_when(
           variavel == "phylum" ~ "Filo",
           variavel == "Class" ~ "Classe",
           variavel == "order" ~ "Ordem",
           variavel == "family" ~ "Família",
           variavel == "genus" ~ "Gênero",
           variavel == "Species" ~ "Espécie"
         )) |>
  pivot_longer(cols = c(n_na, n_preenchidos),
               names_to = "tipo",
               values_to = "n") |>
  mutate(percentual = round(n / total *100, 2),
         tipo = recode(tipo,
                       n_na = "Faltantes",
                       n_preenchidos = "Preenchidos"),
         variavel = factor(variavel, levels = c("Espécie", "Gênero", "Família", "Ordem", "Classe", "Filo")))



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
    title = "Porcentagem de Dados Faltantes por Variável de \n Classificação Biológica",
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
  filter(`Variável` %in% c("phylum", "Class", "order", "family", "genus", "Species")) |>
  mutate(
    Total = nrow(dados),
    Preenchidos = Total - Faltantes,
    `% Faltantes` = paste0(round(100 * Faltantes / Total, 1), "%"),
    `% Preenchidos` = paste0(round(100 * Preenchidos / Total, 1), "%"),
    `Variável` = factor(case_when(
      `Variável` == "phylum" ~ "Filo",
      `Variável` == "Class" ~ "Classe",
      `Variável` == "order" ~ "Ordem",
      `Variável` == "family" ~ "Família",
      `Variável` == "genus" ~ "Gênero",
      `Variável` == "Species" ~ "Espécie"
    ), levels =  c("Filo", "Classe", "Ordem", "Família", "Gênero", "Espécie"))
  ) |>
  select(Variável, Total, Faltantes, `% Faltantes`, Preenchidos, `% Preenchidos`) 

# Tabela estilizada
tabela_gt <- gt(tabela_incompletude) |>
  tab_header(
    title = md("**Resumo de Incompletude dos Dados de Classificação Biológica**")
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
  count(instituto) |>
  mutate(perc = n / sum(n)) |>
  arrange(desc(n)) |>
  slice_head(n = 10) |>
  mutate(
    instituto = case_when(
      is.na(instituto) ~ "Sem informação",
      !is.na(instituto) ~ instituto)
  ) |>
  mutate(
    label = paste0(
      instituto, "\n",
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
    title = "10 Institutos Mais Frequentes"  ) +
  theme_void(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5)
  )

ggsave("outputs/treeplot_instituicoes.png", p2)

# 10 coleções mais frequentes
dados_top10_colecoes <- dados |>
  count(collection) |>
  mutate(perc = n / sum(n)) |>
  arrange(desc(n)) |>
  slice_head(n = 10) |>
  mutate(
    collection = case_when(
      is.na(collection) ~ "Sem informação",
      !is.na(collection) ~ collection)
  ) |>
  mutate(
    label = paste0(
      collection, "\n",
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
  mutate(instituto = case_when(
    is.na(instituto) ~ "Sem informação",
    !is.na(instituto) ~ instituto))|>
  count(instituto, name = "Frequência") |>
  arrange(desc(Frequência)) |>
  mutate(Rank = row_number()) |>
  mutate(
    Instituto = ifelse(Rank <= 20, instituto, "Outros")
  ) |>
  group_by(Instituto) |>
  summarise(Frequência = sum(Frequência), .groups = "drop") |>
  mutate(
    `%` = paste0(round(Frequência / sum(Frequência)*100, 1), "%")
  ) |>
  arrange(desc(Frequência))

# Criar tabela GT estilizada
tabela_gt_inst <- gt(tabela_instituicoes) |>
  tab_header(
    title = md("**20 Institutos Mais Frequentes**")
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
  mutate(collection = case_when(
    is.na(collection) ~ "Sem informação",
    !is.na(collection) ~ collection))|>
  count(collection, name = "Frequência") |>
  arrange(desc(Frequência)) |>
  mutate(Rank = row_number()) |>
  mutate(
    Coleção = ifelse(Rank <= 20, collection, "Outros")
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

############# FREQUÊNCIA DE FAMÍLIAS DENTRE AS TRAQUEÓFITAS

traqueofitas <- dados |>
  filter(phylum == "Tracheophyta")

# 10 instituições mais frequentes
n_familias_traq <- traqueofitas |>
  count(family) |>
  mutate(perc = n / sum(n)) |>
  arrange(desc(n)) |>
  slice_head(n = 50) |>  mutate(
    `Família` = case_when(
      is.na(family) ~ "Sem informação",
      !is.na(family) ~ family)
  ) |>
  mutate(
    label = paste0(
      `Família`, "\n",
      n, " (", percent(perc, accuracy = 0.1), ")"
    )
  )

# Treemap para famílias das traqueófitas
p4 <- ggplot(n_familias_traq, aes(area = n, fill = n, label = label)) +
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
    title = "50 Famílias Mais Frequentes no Filo Traquófitas"  ) +
  theme_void(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5)
  )

p4

ggsave("outputs/treeplot_familas_traqueofitas.png", p4)


# Tabela para famílias dentre as traqueófitas

tabela_familia <- traqueofitas |>
  mutate(family = case_when(
    is.na(family) ~ "Sem informação",
    !is.na(family) ~ family))|>
  count(family, name = "Frequência") |>
  arrange(desc(Frequência)) |>
  mutate(Rank = row_number()) |>
  mutate(
    `Família` = family
  ) |>
  group_by(`Família`) |>
  summarise(Frequência = sum(Frequência), .groups = "drop") |>
  mutate(
    `%` = paste0(round(Frequência / sum(Frequência)*100, 1), "%")
  ) |>
  arrange(desc(Frequência))

# Criar tabela GT estilizada
tabela_gt_familia <- gt(tabela_familia) |>
  tab_header(
    title = md("**Famílias do Filo Traqueófita**")
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

tabela_gt_familia

gtsave(tabela_gt_familia, "outputs/tabela_familia_traqueofita.png")






