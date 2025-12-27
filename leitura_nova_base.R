require(dplyr)
require(data.table)

# leitura da base mais atualizada -----------------------------------------

base <- read.csv("bases/Faceta-2025-12-14.csv")


# tratamento --------------------------------------------------------------

base <- 
  base %>% 
  janitor::clean_names() %>% 
  as.data.table() %>% 
  select(
    decimal_latitude,
    decimal_longitude,
    phylum,
    family,
    genus,
    species,
    year
  )


# analise das duplicadas --------------------------------------------------
base_tratado <- base[ phylum == "Tracheophyta"]

# definindo a quantidade de casas decimais
base_tratado[, lat_r := round(decimal_latitude, 3)]
base_tratado[, lon_r := round(decimal_longitude, 3)]

# agrupando e quantificando as duplicadas de acordo com as covariáveis
base_agrupada <- base_tratado[,.(qntd=.N), by = .(lat_r, lon_r, family, genus, species, year)]

# observações que possuem 2 ou mais ocorrências
duplicadas <-
  base_agrupada %>%
  filter(qntd > 1)


# resultado das analises ----------------------------------------------------------------
print(paste("Total de observações coletadas:", nrow(base_tratado)))
print(paste("Quantidade de pontos com 2 ou mais ocorrências:", length(duplicadas$family)))
print(paste("Frequência de pontos com 2 ou mais ocorrências:", sum(duplicadas$qntd)))
print(paste("Quantidade de observações a serem deletadas:", sum(duplicadas$qntd) - length(duplicadas$family)))
print(paste("Quantidade de observações restantes:", nrow(base_tratado) - (sum(duplicadas$qntd) - length(duplicadas$family))))



# retirando duplicadas ----------------------------------------------------

# selecionando apenas a Tracheophyta
flora <- base[ phylum == "Tracheophyta"]

# definindo a quantidade de casas decimais
flora[, lat_r := round(decimal_latitude, 3)]
flora[, lon_r := round(decimal_longitude, 3)]

# quantificando a duplicidade
flora[, qntd_duplicadas := .N, by = .(lat_r, lon_r, family, genus, species, year)]
flora[, ind_duplicada := qntd_duplicadas > 1]

# base final
flora_final <- flora[
  , .SD[1], 
  by = .(lat_r, lon_r, family, genus, species, year)
]


# saídas ------------------------------------------------------------------
openxlsx::write.xlsx(flora_final, "outputs/Faceta-2025-12-14_sem_duplicadas.xlsx")



