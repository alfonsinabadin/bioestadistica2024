library(readxl)

base <- read_excel("Trabajo práctico final/2023 ADMISIÓN_ANONIMIZADA.xlsx", 
                    sheet = "Registro admisiones")

fecha_psi <- base$Fecha...4

fecha_psi <- as.Date(as.numeric(fecha_psi), origin = "1899-12-30")

fecha_psi <- format(fecha_psi, "%d/%m/%Y")

fecha_psiq <- base$Fecha...6

fecha_psiq <- as.Date(as.numeric(fecha_psiq), origin = "1899-12-30")

fecha_psiq <- format(fecha_psiq, "%d/%m/%Y")

adm23 <- data.frame(
  Apellido_nombres = base$`Apellido y Nombre`,
  DNI = base$`DNI (ficticio)`,
  Entrevista_psicológica_fecha = fecha_psi,
  # consideramos que si la fecha psico no tiene registro, es porque no se
  # asignó, si hay registro de fecha y hay registro del profesional, la
  # persona estuvo presente. Tomamos el NA como ausente.
  Entrevista_psicológica_asistencia = ifelse(is.na(base$Fecha...4),"No asignada",
                                             ifelse(base$`Entrevista psicológica`=="Ausente",NA,"Presente")),
  Entrevista_psiquiátrica_fecha = fecha_psiq,
  Entrevista_psiquiátrica_asistencia = ifelse(is.na(base$Fecha...6) | base$`Entrevista psiquiátrica`== "No asignada","No asignada",
                                              ifelse(!is.na(base$Fecha...6) & base$`Entrevista psiquiátrica`=="Presente","Presente",NA)),
  Entrevista_ts_fecha = format(base$Fecha...8, format = "%d/%m/%Y"), 
  Entrevista_ts_asistencia = ifelse(is.na(base$Fecha...8) | base$`Entrevista T.S.` == "No asignada", "No asignada",
                                    ifelse(base$`Entrevista T.S.`=="si" | base$`Entrevista T.S.`=="SI" | base$`Entrevista T.S.`=="Presente", "Presente",NA)), 
  Tratamiento = base$`Tratamiento elegido`,
  Contacto = base$`Tel. de contacto`,
  Fecha_de_nacimiento = NA, # completa con NA ya que no hay registros
  Edad = base$Edad,
  Nivel_educativo = base$`Nivel educativo alcanzado`,
  Situacion_habitacional = base$`Situación habitacional`,
  Redes_de_apoyo = base$`Redes de apoyo`,
  Tiene_CUD = NA,
  Trabajo = base$Trabajo,
  Ingresos_económicos = NA,
  Situación_Judicial = NA,
  Referencia_APS = base$`Referencia a APS`,
  Equipo_referencia = base$`Derivado de:`,
  Consumo_actual = base$`Consumo actual`,
  Edad_de_inicio = base$`Edad de inicio`,
  Sustancia_de_inicio = base$`Sustancia de inicio`,
  Tratamientos_previos = base$`Tratamientos previos`,
  Observaciones = base$OBSERVACIONES
)

write_xlsx(adm23,"Trabajo práctico final/Limpieza de bases/2023 ADMISIÓN_LIMPIA.xlsx")
