library(readxl)
library(dplyr)
library(openxlsx)

# Grupo etario
edades <- c("0-14","15-29","30-44","45-59","60-74","75 y más")

# Poblaciones x provincia ------------------------------------------------------

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

poblaciones$POBLACION <- as.numeric(poblaciones$POBLACION)

poblaciones2 <- poblaciones %>%
  mutate(GRUPEDAD = ifelse(GRUPEDAD %in% c("0-4", "5-9", "10-14"), "0-14", GRUPEDAD)) %>%
  mutate(GRUPEDAD = ifelse(GRUPEDAD %in% c("15-19", "20-24", "25-29"), "15-29", GRUPEDAD)) %>%
  mutate(GRUPEDAD = ifelse(GRUPEDAD %in% c("30-34", "35-39", "40-44"), "30-44", GRUPEDAD)) %>%
  mutate(GRUPEDAD = ifelse(GRUPEDAD %in% c("45-49", "50-54", "55-59"), "45-59", GRUPEDAD)) %>%
  mutate(GRUPEDAD = ifelse(GRUPEDAD %in% c("60-64", "65-69", "70-74"), "60-74", GRUPEDAD)) %>%
  mutate(GRUPEDAD = ifelse(GRUPEDAD %in% c("75-79", "80-84", "85-89", "90-94", "95-99", "100 y más"), "75 y más", GRUPEDAD)) %>%
  group_by(PROVRES, GRUPEDAD) %>%
  summarise(POBLACION = sum(POBLACION)) %>%
  ungroup()  

# Poblacion tipo ---------------------------------------------------------------

pob_tipo <- read_excel("Trabajo práctico 3/Población por sexo y grupos quinquenales de edad para el total del país y provincias. Años 2010-2040.xls",
                       sheet = "01-TOTAL DEL PAÍS", skip = 31)
pob_tipo <- data.frame(
  GRUPEDAD = pob_tipo[4:24,1],
  POBLACION = pob_tipo[4:24,14]
)
colnames(pob_tipo) <- c("GRUPEDAD","POBLACION")
pob_tipo$POBLACION <- as.numeric(pob_tipo$POBLACION)

pob_tipo2 <- pob_tipo %>%
  mutate(GRUPEDAD = ifelse(GRUPEDAD %in% c("0-4", "5-9", "10-14"), "0-14", GRUPEDAD)) %>%
  mutate(GRUPEDAD = ifelse(GRUPEDAD %in% c("15-19", "20-24", "25-29"), "15-29", GRUPEDAD)) %>%
  mutate(GRUPEDAD = ifelse(GRUPEDAD %in% c("30-34", "35-39", "40-44"), "30-44", GRUPEDAD)) %>%
  mutate(GRUPEDAD = ifelse(GRUPEDAD %in% c("45-49", "50-54", "55-59"), "45-59", GRUPEDAD)) %>%
  mutate(GRUPEDAD = ifelse(GRUPEDAD %in% c("60-64", "65-69", "70-74"), "60-74", GRUPEDAD)) %>%
  mutate(GRUPEDAD = ifelse(GRUPEDAD %in% c("75-79", "80-84", "85-89", "90-94", "95-99", "100 y más"), "75 y más", GRUPEDAD)) %>%
  group_by(GRUPEDAD) %>%
  summarise(POBLACION = sum(POBLACION)) %>%
  ungroup()  

# Defunciones ------------------------------------------------------------------

defun <- read.csv("Trabajo práctico 3/AÑO 2019 - Defunciones.csv", 
                  fileEncoding = "Latin1")

# Limpio base
defun <- defun %>%
  filter(defun$CAUSA == "C43") %>% # filtro causa de muerte
  group_by(PROVRES,GRUPEDAD) %>% # agrupo por cada linea que tiene misma prov y
  # mismo grupo de edad (saco division x sexo)
  summarise(C43 = sum(CUENTA))   # guardo al suma de casos en una var C43

# Limpio nombre de grupo de edad
for (i in 1:nrow(defun)) {
  # saco los primeros 3 caracteres
  defun$GRUPEDAD[i] = substr(defun$GRUPEDAD[i],4,nchar(defun$GRUPEDAD[i]))
  # cambio el a por un guion
  defun$GRUPEDAD[i] = sub("a","-",defun$GRUPEDAD[i])
  # saco espacios en blanco
  defun$GRUPEDAD[i] = ifelse(!grepl("y más",defun$GRUPEDAD[i]),sub(" ","",defun$GRUPEDAD[i]),defun$GRUPEDAD[i])
  defun$GRUPEDAD[i] = ifelse(!grepl("y más",defun$GRUPEDAD[i]),sub(" ","",defun$GRUPEDAD[i]),defun$GRUPEDAD[i])
}

defun2 <- defun %>%
  mutate(GRUPEDAD = ifelse(GRUPEDAD %in% c("0-4", "5-9", "10-14"), "0-14", GRUPEDAD)) %>%
  mutate(GRUPEDAD = ifelse(GRUPEDAD %in% c("15-19", "20-24", "25-29"), "15-29", GRUPEDAD)) %>%
  mutate(GRUPEDAD = ifelse(GRUPEDAD %in% c("30-34", "35-39", "40-44"), "30-44", GRUPEDAD)) %>%
  mutate(GRUPEDAD = ifelse(GRUPEDAD %in% c("45-49", "50-54", "55-59"), "45-59", GRUPEDAD)) %>%
  mutate(GRUPEDAD = ifelse(GRUPEDAD %in% c("60-64", "65-69", "70-74"), "60-74", GRUPEDAD)) %>%
  mutate(GRUPEDAD = ifelse(GRUPEDAD %in% c("75-79", "80 y más"), "75 y más", GRUPEDAD)) %>%
  group_by(PROVRES, GRUPEDAD) %>%
  summarise(C43 = sum(C43)) %>%
  ungroup()  

casos <- merge(poblaciones2, defun2, by = c("PROVRES","GRUPEDAD"), all.x = TRUE)
for(i in 1:nrow(casos)) {if(is.na(casos$C43[i])) {casos$C43[i]=0}}

# Tasas M.I --------------------------------------------------------------------

# Limpio base
tasas <- defun2 %>%
  group_by(GRUPEDAD) %>% # agrupo por cada linea que tiene misma prov y
  # mismo grupo de edad (saco division x sexo)
  summarise(CASOS = sum(C43))   # guardo al suma de casos en una var C43

pob_tipo2 <- merge(pob_tipo2,tasas,by = "GRUPEDAD", all.x=TRUE)
pob_tipo2$CASOS <- ifelse(is.na(pob_tipo2$CASOS),0,pob_tipo2$CASOS)
pob_tipo2$TASAS <- 100000*pob_tipo2$CASOS/pob_tipo2$POBLACION

# guardo
write.xlsx(casos,"Trabajo práctico 3/Bases limpias/Defunciones por provincia.xlsx")
write.xlsx(pob_tipo2,"Trabajo práctico 3/Bases limpias/Población tipo.xlsx")
write.xlsx(poblaciones2,"Trabajo práctico 3/Bases limpias/Poblaciones.xlsx")
write.xlsx(provres,"Trabajo práctico 3/Bases limpias/Códigos de poblacion.xlsx")
