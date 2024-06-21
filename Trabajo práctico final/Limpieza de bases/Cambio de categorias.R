# Cambios de nombres como estan en el formulario a la base ADMISION
library(readxl)
ADMISIÓN <- read_excel("Trabajo práctico final/Limpieza de bases/ADMISIÓN.xlsx")

## Tratamiento
ADMISIÓN <- ADMISIÓN %>%
  mutate(Tratamiento = recode(Tratamiento, "INTERNACIÓN CRISTALERÍA" = "Internación Cristalería", 
                              "NO FINALIZÓ ADMISIÓN" = "No finalizó el proceso", 
                              "CENTRO DE DÍA ZEBALLOS" = "Centro de día Zeballos",
                              "DERIVADO" = "Derivado",
                              "INTERNACIÓN BAIGORRIA" = "Internación Baigorria",
                              "SEGUIMIENTO" = "Continúa en seguimiento",
                              "RECHAZA TTO." = "Rechaza tratamiento",
                              "CDD BUEN PASTOR" = "Centro de día Buen Pastor",
                              "CDD BAIGORRIA" = "Centro de día Baigorria",
                              "INTERNACION BUEN PASTOR" = "Internación Buen Pastor",
                              "INTERNACION B.P / Baig" = "Internación Buen Pastor"))

## Nivel educativo
ADMISIÓN <- ADMISIÓN %>%
  mutate(Nivel_educativo = recode(Nivel_educativo, "Primaria incompleta" = "Primario incompleto (incluye educación especial)",
                                  "Secundaria incompleta" = "Secundario incompleto",
                                  "Nivel Superior incompleto" = "Superior terciario o universitario incompleto",
                                  "Secundaria en curso" = "Secundario en curso",
                                  "Secundaria completa" = "Secundario completo",
                                  "Primaria completa" = "Primario completo",
                                  "Primaria en curso" = "Primario en curso",
                                  "Ningún nivel educativo" = "Sin instrucción formal",
                                  "Nivel superior completo" = "Superior terciario o universitario completo",
                                  "Nivel superior incompleto" = "Superior terciario o universitario incompleto",
                                  "Nivel superior en curso" = "Superior terciario o universitario en curso"))


## Situacion habitacional
ADMISIÓN <- ADMISIÓN %>%
  mutate(Situacion_habitacional = recode(Situacion_habitacional, "Internado en efector de salud" = "Internado / Institucionalizado",
                                         "Casa/depto" = "Casa/depto propio",
                                         "Refugio" = "Refugio",
                                         "Situación de calle" = "Situación de calle",
                                         "Pensión" = "Pensión",
                                         "Institucionalizado" = "Internado / Institucionalizado",
                                         "En inst. penal" = "Internado / Institucionalizado",
                                         "En inst. de SM" = "Internado / Institucionalizado",
                                         "Casa/depto propio" = "Casa/depto propio",
                                         "Casa/depto alquilado" = "Casa/depto alquilado",
                                         "Casa/depto cedido" = "Casa/depto cedido",
                                         "En inst. terapéutica" = "Internado / Institucionalizado"))
### Juntamos todo lo que decia en la base inst. algo en la categoria Internado / Institucionalizado

## Redes de apoyo
ADMISIÓN <- ADMISIÓN %>%
  mutate(Redes_de_apoyo = recode(Redes_de_apoyo, "Buena" = "Buena",
                                 "Escasa" = "Escasa",
                                 "Nula" = "Ninguna",
                                 "Familia + Institucionales" = "Familia/Amigos e Institución",
                                 "Familia" = "Familia/Amigos",
                                 "Sin vínculos actualmente" = "Ninguna",
                                 "Institucionales" = "Institución",
                                 "Familia + Amistades" = "Familia/Amigos",
                                 "38" = NULL))
### Borramos ese 38, ademas junte Nula y Sin vinculos actualmente en "ninguna" y a Familia lo agregue a "Familia/Amigos" 

## Trabajo
ADMISIÓN <- ADMISIÓN %>%
  mutate(Trabajo = recode(Trabajo, "No tiene" = "No tiene",
                          "Estable" = "Estable",
                          "Esporádico" = "Esporádico",
                          "Secundaria incompleta" = NULL))
### Borramos ese secundaria incompleta

## Ingresos Economicos
ADMISIÓN <- ADMISIÓN %>%
  mutate(Ingresos_económicos = recode(Ingresos_económicos, "Sin ingresos" = "Sin ingresos",
                                      "PNC nacional" = "Pensión no Contributiva (PNC)",
                                      "Salario informal + subsidio" = "Salario informal + subsidio",
                                      "Salario informal" = "Salario informal"))
## Referencia
ADMISIÓN <- ADMISIÓN %>%
  mutate(Referencia_APS = recode(Referencia_APS, "Sólo clínica médica" = "Sólo clínica médica",
                                 "Referencia con seguimiento" = "Referencia con seguimiento",
                                 "Referencia sin seguimiento" = "Referencia sin seguimiento",
                                 "No está referenciado" = "No está referenciado"))

## Tratamientos previos
ADMISIÓN <- ADMISIÓN %>%
  mutate(Tratamientos_previos = recode(Tratamientos_previos, "No" = "No",
                                       "entre 1 y 2" = "Entre 1 y 2",
                                       "más de 3" = "3 o más"))

ADMISIÓN$DNI[6] <- NA

write_xlsx(ADMISIÓN,"Trabajo práctico final/Limpieza de bases/ADMISIÓN.xlsx")
write_xlsx(ADMISIÓN,"Admisiones/ADMISIÓN.xlsx")
