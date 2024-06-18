# Vamos a completar las bases previas a las de 2024 de modo que queden con el 
# mismo formato y mismas columnas

# Información de la persona ----------------------------------------------------
# Apellido, nombres
# DNI
# Contacto
# Fecha de nacimiento
# Edad
# Nivel educativo
# Situacion habitacional
# Redes de apoyo
# Tiene CUD
# Trabajo
# Ingresos económicos
# Situación Judicial

# Tratamiento ------------------------------------------------------------------
# Entrevista psicológica - fecha
# Entrevista psicológica - presente/ausente
# Entrevista psiquiátrica - fecha
# Entrevista psiquiátrica - presente/ausente
# Entrevista con ts - fecha
# Entrevista con ts - presente/ausente
# Tratamiento

# Historial --------------------------------------------------------------------
# Referencia APS
# Equipo referencia
# Consumo actual
# Edad de inicio
# Sustancia de inicio
# Tratamientos previos
# Observaciones

library(readxl)
library(lubridate)
library(writexl)

base <- read_excel("Trabajo práctico final/2022 ADMISIÓN_ANONIMIZADA.xlsx", sheet = "10 - 11- 12")

# hay que cambiar la fecha de psiqui a formato dd/mm/YY

fecha_psi <- base$Fecha...7

fecha_psi[32] <- NA # dice 16/11/2022 23/11, nos vamos a quedar con la ultima

fecha_psi[46] <- NA # dice 25/11/2022 02/12, nos vamos a quedar con la ultima

fecha_psi <- as.Date(as.numeric(fecha_psi), origin = "1899-12-30")

fecha_psi <- format(fecha_psi, "%d/%m/%Y")

fecha_psi[32] <- format("23/11/2022", format = "%d/%m/%Y")
fecha_psi[46] <- format("02/12/2023", format = "%d/%m/%Y")

adm22 <- data.frame(
  Apellido_nombres = base$`Apellido, nombres`,
  DNI = base$`DNI (ficticio)`,
  Entrevista_psicológica_fecha = format(base$Fecha...4, format = "%d/%m/%Y"),
  # consideramos que si la fecha psico no tiene registro, es porque no se
  # asignó, si hay registro de fecha y hay registro del profesional, la
  # persona estuvo presente. Tomamos el NA como ausente.
  Entrevista_psicológica_asistencia = ifelse(is.na(base$Fecha...4),"No asignada",
                                             ifelse(!is.na(base$Psicólogx),"Presente",NA)),
  Entrevista_psiquiátrica_fecha = fecha_psi,
  Entrevista_psiquiátrica_asistencia = ifelse(is.na(base$Fecha...7) | base$`Entrevista psiquiátrica`== "No asignada","No asignada",
                                              ifelse(!is.na(base$Fecha...7) & base$`Entrevista psiquiátrica`=="Presente","Presente",NA)),
  Entrevista_ts_fecha = NA, # no hay registros
  Entrevista_ts_asistencia = ifelse(base$`Intervino T.S.`=="si" | base$`Intervino T.S.`=="SI", "Presente","No asignada"), 
  Tratamiento = base$`Tratamiento elegido`,
  Contacto = base$`Tel. de contacto`,
  Fecha_de_nacimiento = NA, # completa con NA ya que no hay registros
  Edad = NA,
  Nivel_educativo = base$`Nivel educativo alcanzado`,
  Situacion_habitacional = base$`Situación habitacional`,
  Redes_de_apoyo = NA,
  Tiene_CUD = NA,
  Trabajo = NA,
  Ingresos_económicos = NA,
  Situación_Judicial = NA,
  Referencia_APS = base$`Referencia a APS`,
  Equipo_referencia = base$`Derivado de:`,
  Consumo_actual = base$Sustancias,
  Edad_de_inicio = base$`Edad de inicio`,
  Sustancia_de_inicio = NA,
  Tratamientos_previos = base$`Tratamientos previos`,
  Observaciones = ifelse(!is.na(base$`Intervenciones de T.S.`),paste(base$`Intervenciones de T.S.`,base$OBSERVACIONES,"-"),base$OBSERVACIONES)
)

write_xlsx(adm22,"Trabajo práctico final/Limpieza de bases/2022 ADMISIÓN_LIMPIA_4.xlsx")

adm22 <- rbind(
  read_excel("Trabajo práctico final/Limpieza de bases/2022 ADMISIÓN_LIMPIA_1.xlsx"),
  read_excel("Trabajo práctico final/Limpieza de bases/2022 ADMISIÓN_LIMPIA_2.xlsx"),
  read_excel("Trabajo práctico final/Limpieza de bases/2022 ADMISIÓN_LIMPIA_3.xlsx"),
  read_excel("Trabajo práctico final/Limpieza de bases/2022 ADMISIÓN_LIMPIA_4.xlsx"))

write_xlsx(adm22,"Trabajo práctico final/Limpieza de bases/2022 ADMISIÓN_LIMPIA.xlsx")
