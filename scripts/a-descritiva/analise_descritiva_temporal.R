
# pacotes -----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyverse)
library(gt)

# leitura --------------------------------------------------------------------
caminho <- "outputs/reino_plantae_es/base_filtrada.parquet"

base_flora <- arrow::read_parquet(caminho)

write.csv(base_flora, "bases/sibbr.csv")

# analises simples --------------------------------------------------------

base_flora$year %>% na.omit %>% min()
base_flora$year %>% na.omit %>% max()

library(ggplot2)

ggplot(base_flora, aes(x = year)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  labs(title = "Histograma observações por ano", x = "Ano", y = "Frequência") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1800,2025, 25))

ggplot(base_flora, aes(x = year)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot observações por ano", x = "Ano") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1800,2025, 25))


# tratamento --------------------------------------------------------------

base_flora_tratado <- 
  base_flora %>% 
  mutate(
    seculo = case_when(
      is.na(year) ~ "S.I.",
      year %in% c(1800:1899) ~ "Século XIX",
      year %in% c(1900:1999) ~ "Século XX",
      year %in% c(2000:2099) ~ "Século XXI",
      ),
    decada_10 = case_when(
      is.na(year) ~ "S.I.",
      TRUE ~ paste0(floor(year / 10) * 10)
    ),
    decada_25 = case_when(
      is.na(year) ~ "S.I.",
      TRUE ~ paste0(floor(year / 25) * 25)
    ),
    decada_50 = case_when(
      is.na(year) ~ "S.I.",
      TRUE ~ paste0(floor(year / 50) * 50)
    )
    ) %>% 
  mutate(across(everything(), ~ ifelse(is.na(.) | . == "", "S.I.", .)))

    
# tratamento --------------------------------------------------------------

## analise anual agrupada ------
resultados_seculos <- 
  base_flora_tratado %>%
  group_by(seculo) %>%
  summarise(n = n()) %>%
  mutate(percentual = round(n / sum(n) * 100,3)) %>% 
  as_tibble()

resultados_decada_10 <- 
  base_flora_tratado %>%
  group_by(decada_10) %>%
  summarise(n = n()) %>%
  mutate(percentual = round(n / sum(n) * 100,3)) %>% 
  as_tibble()

resultados_decada_25 <- 
  base_flora_tratado %>%
  group_by(decada_25) %>%
  summarise(n = n()) %>%
  mutate(percentual = round(n / sum(n) * 100,3)) %>% 
  as_tibble() %>% 
  mutate(
    decada_25 = as.numeric(decada_25),
    decada = paste(decada_25, "-", decada_25+24)
  )

resultados_decada_50 <- 
  base_flora_tratado %>%
  group_by(decada_50) %>%
  summarise(n = n()) %>%
  mutate(percentual = round(n / sum(n) * 100,3)) %>% 
  as_tibble()

resultados_anuais <- 
  base_flora_tratado %>%
  group_by(year) %>%
  summarise(n = n()) %>%
  mutate(percentual =round( n / sum(n) * 100, 2)) %>% 
  as_tibble()

resultados_anuais_filo <- 
  base_flora_tratado %>% 
  group_by(year, phylum) %>% 
  summarise(n = n(), .groups = "drop_last") %>% 
  mutate(percentual = n / sum(n) * 100) %>% 
  mutate(phylum = ifelse(is.na(phylum), "S.I.", phylum)) %>% 
  as_tibble()

resultados_decada_filo <- 
  base_flora_tratado %>% 
  group_by(decada_25, phylum) %>% 
  summarise(n = n(), .groups = "drop_last") %>% 
  mutate(percentual = n / sum(n) * 100) %>% 
  mutate(phylum = ifelse(is.na(phylum), "S.I.", phylum)) %>% 
  as_tibble()


# gráficos ----------------------------------------------------------------

resultados_anuais_filo <- resultados_anuais_filo %>%
  mutate(decada_num = as.numeric(stringr::str_remove(year, "s")))

ggplot(resultados_anuais_filo, aes(x = decada_num, y = n, color = phylum)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Evolução das observações por ano e por filo",
       x = "Ano",
       y = "Quantidade (n)",
       color = "Phylum") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1800,2025, 25))

resultados_decada_filo <- resultados_decada_filo %>%
  mutate(decada_num = as.numeric(stringr::str_remove(decada_25, "s")))

ggplot(resultados_decada_filo, aes(x = decada_num, y = n, color = phylum)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Evolução por ano e por phylum",
       x = "Ano",
       y = "Quantidade (n)",
       color = "Phylum") +
  theme_minimal() 

# sem o filo que mais aparece
resultados_anuais_filo %>% 
  filter(phylum == "Tracheophyta") %>% 
  ggplot( aes(x = decada_num, y = n, color = phylum)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Evolução por ano (sómente Tracheophyta)",
       x = "Década",
       y = "Quantidade (n)",
       color = "Phylum") +
  theme_minimal()

resultados_decada_filo <- resultados_decada_filo %>%
  mutate(decada_num = as.numeric(stringr::str_remove(decada_25, "s"))) 

resultados_decada_filo %>% 
  filter(phylum != "Tracheophyta") %>% 
ggplot(aes(x = decada_num, y = n, color = phylum)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Evolução por década e por phylum",
       x = "Década",
       y = "Quantidade (n)",
       color = "Phylum") +
  theme_minimal()


# saídas ------------------------------------------------------------------

# tabela
# gráficos

resultados_anuais <- 
  as_tibble(resultados_decada_10)

resultados_decada_25_2 <- resultados_decada_25 %>% 
  # arrange(desc(decada_25)) %>% 
  select(decada, n, percentual) %>% 
  rename(ano = decada, `Frequência` = n, Percentual = percentual) %>% 
  mutate(`Percentual Acumulado` = cumsum(Percentual)) %>% 
  mutate(Percentual = paste0(Percentual, "%"),
         `Percentual Acumulado` = paste0(`Percentual Acumulado`, "%"))

# Tabela estilizada
tabela_gt <- 
  gt(resultados_decada_25_2) |>
  tab_header(
    title = md("**Resumo de Frequência de\n Observações Anuais dos Dados**")
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
gtsave(tabela_gt, "")


resultados_anuais_2 <- resultados_anuais %>% 
  arrange(desc(n)) %>%
  select(year, n, percentual) %>% 
  rename(Ano = year, `Frequência` = n, Percentual = percentual) %>% 
  # mutate(`Percentual Acumulado` = cumsum(Percentual)) %>% 
  mutate(Percentual = paste0(Percentual, "%")) %>% 
         # `Percentual Acumulado` = paste0(`Percentual Acumulado`, "%")) %>% 
  head(10) 

# Tabela estilizada
tabela_gt <- 
  gt(resultados_anuais_2) |>
  tab_header(
    title = md("**Resumo de Frequência de\n Observações Anuais dos Dados**")
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
gtsave(tabela_gt, "")
