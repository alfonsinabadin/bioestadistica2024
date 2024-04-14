# Librerias
library(tidyverse)
library(validate)
library(readxl)

# Base de datos
parto <- read_excel("Trabajo práctico 1/parto_sec1_n50.xlsx")

# Reglas
reglas_parto <- tribble(
  ~name, ~description, ~rule,
  "R01", "{del01} no puede ser vacía", "is.na(del01)",
  "R02", "Si {del01} no es vacío, no puede ser mayor o igual a 60", "del01 >= 60",
  "R03", "{del02} no puede ser vacía", "is.na(del02)",
  "R04", "{del02} no puede ser menor a 20", "del02 <= 20",
  "R05", "{del02} no puede ser mayor a 50", "del02 >= 50",
  "R06", "{del03} no puede ser vacía", "is.na(del03)",
  "R07", "si {del03} es 0, {del04} debe ser vacía", "del03 ==  0 & !is.na(del04)",
  "R08", "si {del03} es 0, {del05} debe ser vacía", "del03 ==  0 & !is.na(del05)",
  "R09", "si {del03} no es 0, {del04} no puede ser vacía", "del03 !=  0 & is.na(del04)",
  "R10", "si {del03} no es 0, {del05} no puede ser vacía", "del03 !=  0 & is.na(del05)",
  "R11", "{del06} no puede ser vacía", "is.na(del06)",
  "R12", "Si {del06} = 0, todas las 6a son vacías", "del06 == 0 & !is.na(del06a1) & !is.na(del06a2) & !is.na(del06a3) & !is.na(del06a4) & !is.na(del06as)",
  "R13", "Si {del06a4} = 1, {del06as} no puede ser vacío", "del06a4 == 1 & is.na(del06as)",
  "R14", "Si {del06} = 1, {del06a1} no puede ser vacía", "del06 == 1 & is.na(del06a1)",
  "R15", "Si {del06} = 1, {del06a2} no puede ser vacía", "del06 == 1 & is.na(del06a2)",
  "R16", "Si {del06} = 1, {del06a3} no puede ser vacía", "del06 == 1 & is.na(del06a3)",
  "R17", "Si {del06} = 1, {del06a4} no puede ser vacía", "del06 == 1 & is.na(del06a4)",
  "R18", "{del07} no puede ser vacía", "is.na(del07)"
)
reglas <- validator(.data = reglas_parto)

# Validación de reglas
validacion <- confront(parto, reglas)
errors(validacion)
valores = values(validacion)

# Resultados
resultados <- validacion %>% 
  values() %>% 
  as_tibble() %>% 
  mutate(id = parto$part_id) %>% 
  pivot_longer(cols = -id, names_to = "regla", values_to = "status") %>%
  mutate(
    status = factor(
      if_else(status, "vacio", "limpio", "no disponible"),
      levels = c("limpio", "no disponible", "vacio"), ordered = TRUE
    )
  )
resultados

## Análisis de reglas 
resultados %>% 
      group_by(regla) %>% 
      summarise(
      registros = n(),
      casos = sum(status == "vacio"),
      `(%)` = casos/registros*100
      )

ggplot(resultados, aes(y = regla, fill = status))+
  geom_bar(position = "fill") +
  scale_x_continuous(labels = scales::percent, name = "Porcentaje") +
  scale_fill_manual(values = c("#EBC4E1", "#B17AA0", "#8A5D86")) +
  theme_minimal()

## Pacientes con inconsistencias
inconsistencias <- resultados %>%
  group_by(id) %>%
  summarise(
    inconsistencia = sum(status == "vacio")
  )

ggplot(inconsistencias,
       aes(x = as.factor(id), y = inconsistencia)) +
  geom_bar(stat = "identity", fill = "#C995B7") +
  labs(title = "Cantidad de inconsistencias por paciente",
       x = "ID del paciente",
       y = "Cantidad de inconsistencias") +
  theme_grey() +
  theme(plot.title = element_text(hjust = 0.5))

## Pacientes limpios
### Inconsistencias (vacío)
comunicacion_vacio <- resultados %>% 
  left_join(y = reglas_parto, by = c("regla" = "name")) %>% 
  select(id, regla, description, status) %>% 
  filter(status == "vacio") %>% 
  arrange(id)

### Inconsistencias (no disponible)
comunicacion_nd <- resultados %>% 
  left_join(y = reglas_parto, by = c("regla" = "name")) %>% 
  select(id, regla, description, status) %>% 
  filter(status == "no disponible") %>% 
  arrange(id)

inconsistencias_vacias <- comunicacion_vacio %>%
  group_by(id) %>%
  summarize(inconsistencias_vacias = n())

inconsistencias_nd <- comunicacion_nd %>%
  group_by(id) %>%
  summarize(inconsistencias_nd = n())

resultado <- full_join(inconsistencias_vacias, inconsistencias_nd, by = "id") %>%
  replace(is.na(.), 0)  # Reemplazar NA con 0 para aquellos pacientes sin inconsistencias de un tipo

resultado$inconsistencia_total <- resultado$inconsistencias_vacias + resultado$inconsistencias_nd

todos_los_pacientes <- 1:50
pacientes_con_inconsistencias <- resultado$id
pacientes_sin_inconsistencias <- setdiff(todos_los_pacientes, pacientes_con_inconsistencias)
