library(readxl)
Base_completa <- read_excel("Shiny_definitiva/Base completa.xlsx")

data <- Base_completa %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>% 
  ungroup() %>%
  mutate(
    EdadCategorica = factor(
      ifelse(`Edad del registro` >= 0 & `Edad del registro` <= 12, "0 a 12",
             ifelse(`Edad del registro` >= 13 & `Edad del registro` <= 17, "13 a 17",
                    ifelse(`Edad del registro` >= 18 & `Edad del registro` <= 29, "18 a 29",
                           ifelse(`Edad del registro` >= 30 & `Edad del registro` <= 60, "30 a 60", 
                                  ifelse(`Edad del registro` >= 61, "+ 60", NA))))),
      levels = c("0 a 12", "13 a 17", "18 a 29", "30 a 60", "+ 60"),
      ordered = TRUE
    ),
    `Nivel Máximo Educativo Alcanzado` = factor(`Nivel Máximo Educativo Alcanzado`,
                                                levels = c("Sin instrucción formal", 
                                                           "Primario incompleto","Primario en curso", 
                                                           "Primario completo","Secundario incompleto", 
                                                           "Secundario en curso","Secundario completo", 
                                                           "Nivel superior incompleto","Nivel superior en curso", 
                                                           "Nivel superior completo"), 
                                                ordered = TRUE
    ),
    `Situación Laboral Actual` = factor(
      `Situación Laboral Actual`,
      levels = c("Estable", "Esporádico", "No tiene", "No informado"),
      ordered = TRUE
    ),
    `Ingreso Económico` = factor(`Ingresos Económicos`,
                                 levels = c(
                                   "No informado",
                                   "AlimentAR",
                                   "AUH",
                                   "AUHD",
                                   "Jubilación",
                                   "PNC nacional",
                                   "PNC provincial",
                                   "Salario formal", 
                                   "Salario informal", 
                                   "Sin ingresos", 
                                   "Otro subsidio/plan social", 
                                   "Otro tipo de pensión", 
                                   "Otro tipo de ingreso"),
                                 ordered = TRUE
    ),
    `Situación Judicial` = factor(`Situación Judicial`,
                                  levels = c("Sin causas", 
                                             "Con causa cerrada", 
                                             "Con causa abierta", 
                                             "Desconoce", 
                                             "No informada", 
                                             "Otra"),
                                  ordered = TRUE),
    `Situación Habitacional Actual` = factor(`Situación Habitacional Actual`,
                                             levels = c("No informada",
                                                        "Casa/Departamento alquilado", 
                                                        "Casa/Departamento cedido", 
                                                        "Casa/Departamento propio", 
                                                        "Institución de salud mental", 
                                                        "Institución penal", 
                                                        "Institución terapéutica", 
                                                        "Pensión", 
                                                        "Refugio", 
                                                        "Situación de calle", 
                                                        "Otra"),
                                             ordered = TRUE),
    CUD = factor(CUD,
                 levels = c("No informado",
                            "Si", 
                            "No"),
                 ordered = TRUE),
    EdadInicioCategorica = factor(
      ifelse(`Edad de Inicio de Consumo` >= 0 & `Edad de Inicio de Consumo` <= 12, "0 a 12",
             ifelse(`Edad de Inicio de Consumo` >= 13 & `Edad de Inicio de Consumo` <= 17, "13 a 17",
                    ifelse(`Edad de Inicio de Consumo` >= 18 & `Edad de Inicio de Consumo` <= 29, "18 a 29",
                           ifelse(`Edad de Inicio de Consumo` >= 30 & `Edad de Inicio de Consumo` <= 60, "30 a 60", 
                                  ifelse(`Edad de Inicio de Consumo` >= 61, "+ 60", NA))))),
      levels = c("0 a 12", "13 a 17", "18 a 29", "30 a 60", "+ 60"),
      ordered = TRUE
    )
  )

c("Alcohol","Crack","Cocaína","Marihuana","Nafta",
  "Pegamento","Psicofármacos","Otra")

df <- data %>%
  pivot_longer(
    cols = c("Consumo actual con Alcohol",
             "Consumo actual con Crack",
             "Consumo actual con Cocaína",
             "Consumo actual con Marihuana",
             "Consumo actual con Nafta Aspirada",
             "Consumo actual con Pegamento",
             "Consumo actual con Psicofármacos",
             "Consumo actual con Otras"
             ),
    names_to = "Sustancia",
    values_to = "Consume"
  ) %>%
  filter(Consume == "Si") %>%
  mutate(
    Sustancia = case_when(
      Sustancia == "Consumo actual con Alcohol" ~ "Alcohol",
      Sustancia == "Consumo actual con Crack" ~ "Crack",
      Sustancia == "Consumo actual con Cocaína" ~ "Cocaína",
      Sustancia == "Consumo actual con Marihuana" ~ "Marihuana",
      Sustancia == "Consumo actual con Nafta Aspirada" ~ "Nafta Aspirada",
      Sustancia == "Consumo actual con Pegamento" ~ "Pegamento",
      Sustancia == "Consumo actual con Psicofármacos" ~ "Psicofármacos",
      Sustancia == "Consumo actual con Otras" ~ "Otras"
    )
  ) %>%
  select(`ID de registro`, Sustancia, `Sustancia de inicio`)
  
