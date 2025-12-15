
base <- read.csv("bases/Faceta-2025-12-14.csv")


base_tratado <- 
  base %>% 
  janitor::clean_names() %>% 
  as.data.table()


flora <- base_tratado[ phylum == "Tracheophyta"]

# definindo a quantidade de casas decimais
flora[, lat_r := round(decimal_latitude, 3)]
flora[, lon_r := round(decimal_longitude, 3)]

# agrupando e quantificando as duplicadas de acordo com as covariáveis
base_agrupada <- flora[,.(qntd=.N), by = .(lat_r, lon_r, family, genus, species, year)] 

# observações que possuem 2 ou mais ocorrências
duplicadas <- 
  base_agrupada %>% 
  filter(qntd > 1)


# analises ----------------------------------------------------------------
print(paste("Total de observações coletadas:", nrow(flora)))
print(paste("Quantidade de pontos com 2 ou mais ocorrências:", length(duplicadas$family)))
print(paste("Frequência de pontos com 2 ou mais ocorrências:", sum(duplicadas$qntd)))
print(paste("Quantidade de observações a serem deletadas:", sum(duplicadas$qntd) - length(duplicadas$family)))
print(paste("Quantidade de observações restantes:", nrow(flora) - (sum(duplicadas$qntd) - length(duplicadas$family))))

