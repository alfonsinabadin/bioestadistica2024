library(readxl)


base <- read_excel("Trabajo práctico final/2024 ADMISIÓN_ANONIMIZADA.xlsx")[1:97,]

fecha_psiq <- base$Fecha...6

fecha_psiq <- as.Date(as.numeric(fecha_psiq), origin = "1899-12-30")

fecha_psiq <- format(fecha_psiq, "%d/%m/%Y")

fecha_ts <- base$Fecha...8

fecha_ts <- as.Date(as.numeric(fecha_ts), origin = "1899-12-30")

fecha_ts <- format(fecha_ts, "%d/%m/%Y")

adm24 <- data.frame(
  Apellido_nombres = base$`Apellido, Nombre`,
  DNI = base$`DNI (ficticio)`,
  Entrevista_psicológica_fecha = format(base$Fecha...4,format = "%d/%m/%Y"),
  # consideramos que si la fecha psico no tiene registro, es porque no se
  # asignó, si hay registro de fecha y hay registro del profesional, la
  # persona estuvo presente. Tomamos el NA como ausente.
  Entrevista_psicológica_asistencia = ifelse(is.na(base$Fecha...4),"No asignada",
                                             ifelse(base$`Entrevista psicológica`=="Presente" |base$`Entrevista psicológica`=="Presente (gise)","Presente",NA)),
  Entrevista_psiquiátrica_fecha = fecha_psiq,
  Entrevista_psiquiátrica_asistencia = ifelse(is.na(base$Fecha...6) | base$`Entrevista psiquiátrica`== "No asignada","No asignada",
                                              ifelse(!is.na(base$Fecha...6) & base$`Entrevista psiquiátrica`=="Presente","Presente",NA)),
  Entrevista_ts_fecha = fecha_ts, 
  Entrevista_ts_asistencia = ifelse(is.na(base$Fecha...8), "No asignada",
                                    ifelse(base$`Entrevista T.S.`=="Presente", "Presente",NA)), 
  Tratamiento = base$`Tratamiento elegido`,
  Contacto = base$`Tel. de contacto`,
  Fecha_de_nacimiento = format(base$`Fecha de Nacimiento`,format = "%d/%m/%Y"), # completa con NA ya que no hay registros
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
  Observaciones = base$OBSERVACIONES
)

write_xlsx(adm24,"Trabajo práctico final/Limpieza de bases/2024 ADMISIÓN_LIMPIA.xlsx")

base <- rbind(
  read_excel("Trabajo práctico final/Limpieza de bases/2022 ADMISIÓN_LIMPIA.xlsx"),
  read_excel("Trabajo práctico final/Limpieza de bases/2023 ADMISIÓN_LIMPIA.xlsx"),
  read_excel("Trabajo práctico final/Limpieza de bases/2024 ADMISIÓN_LIMPIA.xlsx")
  )

write_xlsx(base,"Trabajo práctico final/Limpieza de bases/ADMISIÓN.xlsx")
