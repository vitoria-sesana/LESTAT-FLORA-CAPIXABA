# leitura, pré seleção e saida da base de dados coletada no site do SIBBR

# pacotes -----------------------------------------------------------------
#install.packages("arrow")

# leitura --------------------------------------------------------------------
caminho <- "data/reino_plantae_es/reino_plantae_es.csv"

dados <- 
  read.csv(caminho) 

dados_filtrados <- 
  dados |>
  janitor::clean_names() |>
  select(record_id, dataset_name, institution, collection, kingdom, phylum,
         class, order, family, genus, species, subspecies, institution_code,
         collection_code, locality, latitude_original, longitude_original,
         decimal_latitude, decimal_longitude, state_province, year, month, day,
         occurrence_status) |>
  filter(state_province == "Espirito Santo")


# saidas ------------------------------------------------------------------
arrow::write_parquet(dados, "outputs/reino_plantae_es/reino_plantae_es.parquet")
arrow::write_parquet(dados_filtrados, "outputs/reino_plantae_es/base_filtrada.parquet")


# para ler a base, basta apenas: ------------------------------------------
caminho <- "data/reino_plantae_es/base_filtrada.parquet"

base_flora <- arrow::read_parquet(caminho)



