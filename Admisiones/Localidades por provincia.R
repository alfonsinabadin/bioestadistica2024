library(dplyr)
library(readxl)

setwd("~/5to año/Bioestadística/bioestadistica2024/Admisiones")

df <- read_excel("Listado de Localidades - 19-06-2024.xlsx")

df <- df %>% 
  mutate(Provincia = tools::toTitleCase(tolower(Provincia))) %>% 
  mutate(Nombre = tools::toTitleCase(tolower(Nombre)))

provinces_localities <- df %>%
  group_by(Provincia) %>%
  summarise(Localidades = list(Nombre))

localidades <- setNames(as.list(provinces_localities$Localidades), provinces_localities$Provincia)

saveRDS(localidades,"localidades.RDS")
