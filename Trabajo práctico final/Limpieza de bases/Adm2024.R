library(readxl)
library(writexl)
library(tidyverse)

base <- read_excel("Trabajo práctico final/2024 ADMISIÓN_ANONIMIZADA.xlsx")[1:97,]

fecha_psiq <- base$Fecha...6
fecha_psiq <- as.Date(as.numeric(fecha_psiq), origin = "1899-12-30")
fecha_psiq <- format(fecha_psiq, "%d/%m/%Y")

fecha_ts <- base$Fecha...8
fecha_ts <- as.Date(as.numeric(fecha_ts), origin = "1899-12-30")
fecha_ts <- format(fecha_ts, "%d/%m/%Y")

# Convertir fecha de entrevista psicológica
fecha_psico <- base$Fecha...4
fecha_psico <- format(fecha_psico, "%d/%m/%Y")

# Convertir fecha de nacimiento
fecha_nacimiento <- base$`Fecha de Nacimiento`
fecha_nacimiento <- format(fecha_nacimiento, "%d/%m/%Y")

adm24 <- data.frame(
  Apellido_nombres = base$`Apellido, Nombre`,
  DNI = base$`DNI (ficticio)`,
  Entrevista_psicológica_fecha = fecha_psico,
  # consideramos que si la fecha psico no tiene registro, es porque no se
  # asignó, si hay registro de fecha y hay registro del profesional, la
  # persona estuvo presente. Tomamos el NA como ausente.
  Entrevista_psicológica_asistencia = ifelse(is.na(fecha_psico),"No asignada",
                                             ifelse(base$`Entrevista psicológica` == "Presente" | base$`Entrevista psicológica` == "Presente (gise)", "Presente", NA)),
  Entrevista_psiquiátrica_fecha = fecha_psiq,
  Entrevista_psiquiátrica_asistencia = ifelse(is.na(fecha_psiq) | base$`Entrevista psiquiátrica` == "No asignada", "No asignada",
                                              ifelse(!is.na(fecha_psiq) & base$`Entrevista psiquiátrica` == "Presente", "Presente", NA)),
  Entrevista_ts_fecha = fecha_ts, 
  Entrevista_ts_asistencia = ifelse(is.na(fecha_ts), "No asignada",
                                    ifelse(base$`Entrevista T.S.` == "Presente", "Presente", NA)), 
  Tratamiento = base$`Tratamiento elegido`,
  Contacto = base$`Tel. de contacto`,
  Fecha_de_nacimiento = fecha_nacimiento, # completa con NA ya que no hay registros
  Edad = base$Edad,
  Nivel_educativo = base$`Nivel educativo`,
  Situacion_habitacional = base$`Situación habitacional`,
  Redes_de_apoyo = base$`Redes de apoyo`,
  Tiene_CUD = base$`Tiene CUD`,
  Trabajo = base$Trabajo,
  Ingresos_económicos = base$`Ingresos económicos`,
  Situación_Judicial = base$`Situación Judicial`,
  Referencia_APS = base$`Referencia a APS`,
  Equipo_referencia = base$`Derivado de:`,
  Consumo_actual = base$`Consumo actual`,
  Edad_de_inicio = base$`Edad de inicio`,
  Sustancia_de_inicio = base$`Sustancia de inicio`,
  Tratamientos_previos = base$`Tratamientos previos`,
  Observaciones = base$OBSERVACIONES,
  stringsAsFactors = FALSE
)


write_xlsx(adm24,"Trabajo práctico final/Limpieza de bases/2024 ADMISIÓN_LIMPIA.xlsx")

base <- rbind(
  read_excel("Trabajo práctico final/Limpieza de bases/2022 ADMISIÓN_LIMPIA.xlsx"),
  read_excel("Trabajo práctico final/Limpieza de bases/2023 ADMISIÓN_LIMPIA.xlsx"),
  read_excel("Trabajo práctico final/Limpieza de bases/2024 ADMISIÓN_LIMPIA.xlsx")
  )

Policonsumo <- ifelse(base$Consumo_actual == "Policonsumo" |
                             base$Consumo_actual == "Cocaína y alcohol" |
                             base$Consumo_actual == "Crack y marihuana" |
                             base$Consumo_actual == "Crack y alcohol" |
                             base$Consumo_actual == "Cocaína y psicofármacos" |
                             base$Consumo_actual == "Cocaína y marihuana" |
                             base$Consumo_actual == "Marihuana y alcohol",
                           "Si", "No"
                             )

Sustancia_actual <- ifelse(base$Consumo_actual == "Policonsumo", "Policonsumo",
                                ifelse(grepl("y",base$Consumo_actual), sapply(strsplit(base$Consumo_actual, "y"), `[`, 1),
                                       base$Consumo_actual))

base <- base %>% 
  add_column(Policonsumo, .before = "Consumo_actual") %>% 
  add_column(Sustancia_actual, .before = "Consumo_actual") %>% 
  select(!(Consumo_actual)) %>% 
  select(!(Edad)) %>%
  add_column(Provincia = NA, .after = "Situacion_habitacional") %>%
  add_column(Localidad = NA, .after = "Provincia") %>%
  add_column(Barrio = NA, .after = "Localidad") %>%
  add_column(Edad = round(as.numeric(difftime(Sys.Date(), as.Date(base$Fecha_de_nacimiento, format = "%d/%m/%Y"),units = "days"))/365,0), .after = "Fecha_de_nacimiento")

base$Edad_de_inicio <-ifelse(base$Edad_de_inicio == "Antes de los 12", "Niños/as de hasta 12 años", 
                             ifelse(base$Edad_de_inicio == "Antes de los 11", "Niños/as de hasta 12 años",
                                    ifelse(base$Edad_de_inicio == "entre 13 y 17", "Adolescentes entre 13 a 17 años",
                                           ifelse(base$Edad_de_inicio == "entre 15/16/17", "Adolescentes entre 13 a 17 años",
                                                  ifelse(base$Edad_de_inicio == "entre 12/13/14", "Adolescentes entre 13 a 17 años",
                                                         ifelse(base$Edad_de_inicio == "de 18 en adelante", "Jóvenes de 18 a 29 años", NA)
                                                  )))))

colnames(base)[24] <- "Derivado_de"

write_xlsx(base,"Trabajo práctico final/Limpieza de bases/ADMISIÓN.xlsx")
