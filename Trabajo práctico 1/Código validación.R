library(tidyverse)
library(validate)
library(readxl)

parto <- read_excel("Trabajo práctico 1/parto_sec1_n50.xlsx")

reglas_parto <- tribble(
  ~name, ~description, ~rule,
  "R1", "{del01} no puede ser vacía", "is.na(del01)",
  "R2", "Si {del01} no es vacío, no puede ser mayor o igual a 100", "!is.na(del01) & del01 >= 100",
  "R3", "{del02} no puede ser vacía", "is.na(del02)",
  "R4", "{del02} no puede ser menor a 20", "!is.na(del02) & del02 <= 20",
  "R5", "{del02} no puede ser mayor a 50", "!is.na(del02) & del02 >= 50",
  "R6", "{del03} no puede ser vacía", "is.na(del03)",
  "R7", "si {del03} es 0, {del04} debe ser vacía", "del03 ==  0 & !is.na(del04)",
  "R8", "si {del03} es 0, {del05} debe ser vacía", "del03 ==  0 & !is.na(del05)",
  "R9", "si {del03} no es 0, {del04} no puede ser vacía", "del03 !=  0 & is.na(del04)",
  "R10", "si {del03} no es 0, {del05} no puede ser vacía", "del03 !=  0 & is.na(del05)",
  "R11", "si {del03} no es 0, {del05} no puede ser vacía", "del03 !=  0 & is.na(del05)",
  "R12", "{del04} debe ser numérica", "!is.numeric(del04)",
  "R13", "{del04} debe ser 0,1 o 9", "!any(grepl(del04,c(0,1,9)))",
  "R14", "{del05} debe ser numérica", "!is.numeric(del05)",
  "R15", "{del05} debe ser 0,1 o 9", "!any(grepl(del05,c(0,1,9)))",
  "R16", "Si hubo cesárea previamente ({del05} = 1) no se puede inducir el siguiente parto ({del06} = 0)", "!is.na(del05) & del05 == 1 & del06 == 1",
  "R17", "{del06} no puede ser vacía", "is.na(del06)",
  "R18", "{del06} debe ser numérica", "!is.numeric(del06)",
  "R19", "{del06} debe ser 0 o 1", "!any(grepl(del06,c(0,1)))",
  "R20", "Si {del06} = 0, todas las 6a son vacías", "del06 == 0 & !is.na(del06a1) & !is.na(del06a2) & !is.na(del06a3) & !is.na(del06a4) & !is.na(del06as)",
  "R21", "Si {del06a4} = 1, {del06as} no puede ser vacío", "del06a4 == 1 & is.na(del06as)",
  "R22", "Si {del06} = 1, ninguna de las 6a puede ser vacia", "del06 == 1 & is.na(del06a1)",
  "R23", "Si {del06} = 1, ninguna de las 6a puede ser vacia", "del06 == 1 & is.na(del06a2)",
  "R24", "Si {del06} = 1, ninguna de las 6a puede ser vacia", "del06 == 1 & is.na(del06a3)",
  "R25", "Si {del06} = 1, ninguna de las 6a puede ser vacia", "del06 == 1 & is.na(del06a4)",
  "R26", "{del06a1} debe ser 0 o 1", "!any(grepl(del06a1,c(0,1)))",
  "R27", "{del06a2} debe ser 0 o 1", "!any(grepl(del06a2,c(0,1)))",
  "R27", "{del06a3} debe ser 0 o 1", "!any(grepl(del06a3,c(0,1)))",
  "R27", "{del06a4} debe ser 0 o 1", "!any(grepl(del06a4,c(0,1)))",
  "R28", "{del07} no puede ser vacía", "is.na(del07)",
  "R29", "Si {del06} = 0, {del07} = 0", "del06 == 0 & del07 != 0",
  "R30", "Si {del06} = 1 y {del06a1} = 1, {del07} debe ser 1", "del06 == 1 & del06a1 == 1 & del07 == 1"
)

reglas_parto <- validator(.data = reglas_parto)

validacion <- confront(parto, reglas_parto)

errors(validacion)

valores = values(validacion)

# Consultar :
## por que algunas reglas no las imprime ¿es q estan mal?
## por que separa las reglas en dos listas en el values()
## lo del dato de Cami (cesarea previa ==> no inducción futura?)
## Pregunta 6 y 7 ¿No son lo mismo?