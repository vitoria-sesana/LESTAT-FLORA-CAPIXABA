library(data.table)

# leitura -----------------------------------------------------------------
caminho <- "bases/Flora_final.parquet"

base <- 
  arrow::read_parquet(caminho) %>% 
  janitor::clean_names() %>% 
  as.data.table()
  
flora <- base[ phylum == "Tracheophyta" & year >= 1965]

base[, lat_r := round(latitude, 6)]
base[, lon_r := round(longitude, 6)]


base[,.(qntd=.N) ,by = .(family)]

duplicadas <- base[,.(qntd=.N) ,by = .(collection, family, lat_r, lon_r)] 

x <- duplicadas %>% 
  filter(qntd > 1)

length(x$family)
sum(x$qntd)
sum(x$qntd) - length(x$family)

nrow(flora)
nrow(flora) - (sum(x$qntd) - length(x$family))



# saídas ------------------------------------------------------------------
openxlsx::write.xlsx(duplicadas, 'duplicadas.xlsx')

########### Avaliando NA


