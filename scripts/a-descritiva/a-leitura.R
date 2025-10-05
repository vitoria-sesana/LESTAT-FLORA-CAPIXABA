library(dplyr)

# leitura -----------------------------------------------------------------

caminho_flora <- "bases/2025-10-01_Estatísticas Básica.xlsx"

base_flora <- 
  readxl::read_xlsx(caminho, sheet = "Flora")

