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

base <- read_excel("Trabajo práctico final/2022 ADMISIÓN_ANONIMIZADA.xlsx", sheet = "4- 5 - 6")

# hay que cambiar la fecha psicológica a formato dd/mm/YY

fecha_psi <- base$Día

fecha_psi <- ifelse(!is.na(base$Día),paste(base$Día,base$`Entrevista psicológica`,"2022"),NA)

fecha_psi <- dmy(fecha_psi)

fecha_psi <- format(fecha_psi, format = "%d/%m/%Y")

adm22 <- data.frame(
  Apellido_nombres = base$`Apellido, nombres`,
  DNI = base$`DNI (ficticio)`,
  Entrevista_psicológica_fecha = fecha_psi,
  # consideramos que si la fecha psico no tiene registro, es porque no se
  # asignó, si hay registro de fecha y hay registro del profesional, la
  # persona estuvo presente. Tomamos el NA como ausente.
  Entrevista_psicológica_asistencia = ifelse(is.na(base$Día),"No asignada",
                                             ifelse(!is.na(base$Profesional),"Presente",NA)),
  Entrevista_psiquiátrica_fecha = format(base$Fecha, format = "%d/%m/%Y"),
  Entrevista_psiquiátrica_asistencia = ifelse(is.na(base$Fecha),"No asignada",
                                              ifelse(!is.na(base$Fecha) & base$`Entrevista psiquiátrica`=="Presente","Presente",NA)),
  Entrevista_ts_fecha = format(base$T.S., "%d/%m/%Y"),
  Entrevista_ts_asistencia = NA, # no hay registros
  Tratamiento = base$`Tratamiento elegido`,
  Contacto = base$`Tel. de contacto`,
  Fecha_de_nacimiento = NA, # completa con NA ya que no hay registros
  Edad = base$Edad,
  Nivel_educativo = base$`Nivel educativo alcanzado`,
  Situacion_habitacional = base$`Situación habitacional`,
  Redes_de_apoyo = NA,
  Tiene_CUD = NA,
  Trabajo = NA,
  Ingresos_económicos = NA,
  Situación_Judicial = NA,
  Referencia_APS = base$`Referencia a APS`,
  Equipo_referencia = base$`Derivado de:`,
  Consumo_actual = NA,
  Edad_de_inicio = NA,
  Sustancia_de_inicio = NA,
  Tratamientos_previos = NA,
  Observaciones = base$OBSERVACIONES
)

write_xlsx(adm22,"Trabajo práctico final/Limpieza de bases/2022 ADMISIÓN_LIMPIA_2.xlsx")
