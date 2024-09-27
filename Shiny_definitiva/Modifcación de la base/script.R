# Cargar las librerías necesarias
library(readxl)
library(writexl)
library(dplyr)
setwd("~/5to año/Bioestadística/bioestadistica2024/Shiny_definitiva")

# Cargar la base
data <- read_excel("Base completa.xlsx")

# Convertir a formato de fecha las columnas necesarias
data$`Fecha de registro` <- as.Date(data$`Fecha de registro`, format="%Y-%m-%d")
data$`Fecha de Nacimiento` <- as.Date(data$`Fecha de Nacimiento`, format="%Y-%m-%d")

# Completar la "Primer fecha de registro" y "Edad del primer registro"
data <- data %>%
  group_by(DNI) %>%
  mutate(
    `Primer fecha de registro` = min(`Fecha de registro`, na.rm = TRUE),
    `Edad del primer registro` = ifelse(!is.na(`Fecha de Nacimiento`),
                                        as.numeric(difftime(`Primer fecha de registro`, `Fecha de Nacimiento`, units = "weeks")) / 52.25,
                                        0),
    # Completar el campo "Recuerda DNI" con "Sí" si hay un DNI presente
    `Recuerda DNI` = ifelse(!is.na(DNI) & DNI != "", "Sí", `Recuerda DNI`)
  ) %>%
  ungroup()

# Redondear la edad del primer registro
data$`Edad del primer registro` <- round(data$`Edad del primer registro`, 0)

data$`Sexo biológico` <- "Masculino"

data$Género <- "Masculino"

# Guardar la base actualizada en un nuevo archivo Excel
write_xlsx(data, "Base completa.xlsx")
