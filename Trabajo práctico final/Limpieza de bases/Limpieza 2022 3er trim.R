library(readxl)
library(lubridate)

adm22 <- read_excel("Trabajo práctico final/2022 ADMISIÓN_ANONIMIZADA.xlsx", 
           sheet = "4- 5 - 6")

# aux 

fecha_psi <- ifelse(!is.na(adm22$Día),paste(adm22$Día,adm22$`Entrevista psicológica`,"2022"),NA)

fecha_psi <- dmy(fecha_psi)

fecha_psi <- format(fecha_psi, format = "%d/%m/%Y")

# nueva base

# agrega columnas de la base 2024 con NA

adm22_2trim <- data.frame(
  `Apellido, nombres` = adm22$`Apellido, nombres`,
  DNI = adm22$`DNI (ficticio)`,
  Contacto = adm22$`Tel. de contacto`,
  `Nivel educativo alcanzado` = adm22$`Nivel educativo alcanzado`,
  `Fecha de nacimiento` = numeric(nrow(adm22)),
  Edad = adm22$Edad,
  `Situación habitacional` = adm22$`Situación habitacional`,
  Sustancias = numeric(nrow(adm22)),
  `Edad de inicio` = numeric(nrow(adm22)),
  `Sustancia de inicio` = numeric(nrow(adm22)),
  `Red de apoyo` = numeric(nrow(adm22)),
  Trabajo = numeric(nrow(adm22)),
  `Ingresos económicos` = numeric(nrow(adm22)),
  `Tiene CUD` = numeric(nrow(adm22)),
  `Situación judicial` = numeric(nrow(adm22)),
  # ENTREVISTA PSICOLÓGICA -----------------------------------------------------
  # cambia a formato fecha dd/mm/YYYY
  `Psicológica - fecha` = fecha_psi,
  # tomamos No se le asifna si no tiene información del día (no hay fecha programada/registrada)
  # tomamos presente si tiene info del profesional (¿está bien?)
  # tomamos NA = ausente si no tiene información del personal
  `Psicológica - presentismo` = ifelse(is.na(adm22$Día),"No se le asigna",ifelse(!is.na(adm22$Profesional),"Presente","Ausente")),
  # ENTREVISTA PSIQUIÁTRICA ----------------------------------------------------  
  `Psiquiátrica - fecha` = format(adm22$Fecha,format = "%d/%m/%Y"),
  `Psiquiácrita - presentismo` = ifelse(is.na(adm22$Fecha),"No se le asigna",
                                              ifelse(!is.na(adm22$Fecha) & adm22$`Entrevista psiquiátrica` == "Ausente",NA,"Presente")),
  # ENTREVISTA TS --------------------------------------------------------------
  `TS - fecha` = format(adm22$T.S., format = "%d/%m/%Y"),
  # agregamos con NA porque no tenemos info
  `TS - presentismo` = numeric(nrow(adm22)),
  # TRATAMIENTO FINAL ----------------------------------------------------------
  Tratamiento = adm22$`Tratamiento elegido`,
  
  `Tratamientos previos` = numeric(nrow(adm22)),
  `Referencia APS` = adm22$`Referencia a APS`,
  `Derivado de` = adm22$`Derivado de:`,
  
  Observaciones = adm22$OBSERVACIONES
)
