library(readxl)
library(dplyr)

defun <- read.csv("Trabajo práctico 3/AÑO 2019 - Defunciones.csv", 
                        comment.char="#")

# Limpio base
defun <- defun %>%
  filter(defun$CAUSA == "C43") %>% # filtro causa de muerte
  group_by(PROVRES,GRUPEDAD) %>% # agrupo por cada linea que tiene misma prov y
                                 # mismo grupo de edad (saco sexo)
  summarise(C43 = sum(CUENTA))   # guardo al suma de casos en una var C43

# Limpio nombre de grupo de edad
for (i in 1:nrow(defun)) {
  # saco los primeros 3 caracteres
  defun$GRUPEDAD[i] = substr(defun$GRUPEDAD[i],4,nchar(defun$GRUPEDAD[i]))
  # cambio el a por un guion
  defun$GRUPEDAD[i] = sub("a","-",defun$GRUPEDAD[i])
  # los que dicen más, lo escribo bien
  #if(!grepl("-",defun$GRUPEDAD[i])){
  #  defun$GRUPEDAD[i] = paste(substr(defun$GRUPEDAD[i],4,5)," y más")
  #}
}

# Importo base provincias

# acá tengo codigo de provincia y nombre de la hoja en el excel
provres <- read.csv("Trabajo práctico 3/Descripción de los archivos de defunciones.xlsx - PROVRES.csv")
# dataframe vacio
poblaciones = data.frame()

# para cada provincia en la base de defunciones
for (prov in unique(defun$PROVRES)) {
  # importar la base de poblacion
  base <- read_excel("Trabajo práctico 3/Población por sexo y grupos quinquenales de edad para el total del país y provincias. Años 2010-2040.xls",
                    sheet = paste(provres$NOMBRE.SHEET[provres$CODIGO==prov]), skip = 31)
  # guardar la info para cada grupo de edad en ambos sexos al año 2019 (columna 14)
  base <- data.frame(
    PROVRES = prov,
    GRUPEDAD = base[4:24,1],
    POBLACION = base[4:24,14]
  )
  colnames(base) <- c("PROVRES","GRUPEDAD","POBLACION")
  # voy registrando en una base larga
  poblaciones = rbind(poblaciones,base)
}



