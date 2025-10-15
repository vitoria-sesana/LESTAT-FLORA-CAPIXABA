
# pacotes -----------------------------------------------------------------
library(dplyr)
library(ggplot2)

# leitura --------------------------------------------------------------------
caminho <- "outputs/reino_plantae_es/base_filtrada.parquet"

base_flora <- arrow::read_parquet(caminho)

# analises simples --------------------------------------------------------

base_flora$year %>% na.omit %>% min()
base_flora$year %>% na.omit %>% max()

hist(base_flora$year)
boxplot(base_flora$year, horizontal = TRUE)


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
  mutate(percentual = round(n / sum(n) * 100,3))

resultados_decada_10 <- 
  base_flora_tratado %>%
  group_by(decada_10) %>%
  summarise(n = n()) %>%
  mutate(percentual = round(n / sum(n) * 100,3))

resultados_decada_25 <- 
  base_flora_tratado %>%
  group_by(decada_25) %>%
  summarise(n = n()) %>%
  mutate(percentual = round(n / sum(n) * 100,3))

resultados_decada_50 <- 
  base_flora_tratado %>%
  group_by(decada_50) %>%
  summarise(n = n()) %>%
  mutate(percentual = round(n / sum(n) * 100,3))

resultados_anuais <- 
  base_flora_tratado %>%
  group_by(year) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(percentual = round(n / sum(n) * 100,1))

resultados_anuais_filo <- 
  base_flora_tratado %>% 
  group_by(year, phylum) %>% 
  summarise(n = n(), .groups = "drop_last") %>% 
  mutate(percentual = n / sum(n) * 100) %>% 
  mutate(phylum = ifelse(is.na(phylum), "S.I.", phylum))

resultados_decada_filo <- 
  base_flora_tratado %>% 
  group_by(decada_25, phylum) %>% 
  summarise(n = n(), .groups = "drop_last") %>% 
  mutate(percentual = n / sum(n) * 100) %>% 
  mutate(phylum = ifelse(is.na(phylum), "S.I.", phylum))


# gráficos ----------------------------------------------------------------

resultados_anuais_filo <- resultados_anuais_filo %>%
  mutate(decada_num = as.numeric(stringr::str_remove(year, "s")))

ggplot(resultados_anuais_filo, aes(x = decada_num, y = n, color = phylum)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Evolução por década e por phylum",
       x = "Década",
       y = "Quantidade (n)",
       color = "Phylum") +
  theme_minimal()

# sem o filo que mais aparece
resultados_anuais_filo %>% 
  filter(phylum != "Tracheophyta") %>% 
  ggplot( aes(x = decada_num, y = n, color = phylum)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Evolução por década e por phylum",
       x = "Década",
       y = "Quantidade (n)",
       color = "Phylum") +
  theme_minimal()

resultados_decada_filo <- resultados_decada_filo %>%
  mutate(decada_num = as.numeric(stringr::str_remove(decada_25, "s")))

ggplot(resultados_decada_filo, aes(x = decada_num, y = n, color = phylum)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(title = "Evolução por década e por phylum",
       x = "Década",
       y = "Quantidade (n)",
       color = "Phylum") +
  theme_minimal()

