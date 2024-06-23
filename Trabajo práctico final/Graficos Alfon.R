library(readxl)
library(ggplot2)
library(dplyr)


data <- read_excel("Trabajo práctico final/Limpieza de bases/ADMISIÓN.xlsx")

# Grafico fechas y tto ---------------------------------------------------------
primera_entrevista <- numeric()
for(i in 1:nrow(data)) {
  fechas <- t(data[i,c(3,5,7)])
  fechas <- subset(fechas,!is.na(fechas))
  primera_entrevista[i] <- ifelse(nrow(fechas)==0,NA,min(fechas))
}
primera_entrevista <- data.frame(
  mes = format(as.Date(primera_entrevista,"%d/%m/%Y"),"%m"),
  anio = format(as.Date(primera_entrevista,"%d/%m/%Y"),"%Y"),
  mes_anio = format(as.Date(primera_entrevista,"%d/%m/%Y"),"%m/%Y"),
  Tratamiento = factor(ifelse(grepl("Internación",data$Tratamiento),"Internación",
                       ifelse(grepl("Centro de día", data$Tratamiento), "Centro de día",
                              ifelse(grepl("No finalizó el proceso", data$Tratamiento), "Abandona",
                                     ifelse(grepl("Rechaza", data$Tratamiento), "Rechaza",
                                            ifelse(grepl("Derivado",data$Tratamiento), "Derivado",NA))))),
                       levels = c("Abandona","Rechaza","Internación","Centro de día","Derivado"),
                       ordered = TRUE)
  )

primera_entrevista <- primera_entrevista %>%
  filter(!is.na(mes_anio)) %>%
  filter(anio!="2022") %>%
  filter(!is.na(Tratamiento)) %>%
  group_by(anio,mes,Tratamiento) %>%
  summarize(frecuencia = n()) %>%
  ungroup() %>%
  arrange(anio,mes,desc(Tratamiento)) %>%
  group_by(anio,mes) %>%
  mutate(label = cumsum(frecuencia))

ggplot(primera_entrevista) +
  geom_bar(aes(x = mes, y = frecuencia, fill = Tratamiento), 
           stat = "identity") +
  geom_text(aes(x = mes, y = label, label = frecuencia), 
            color = "white", size = 3.5, vjust = 1.5) +
  labs(x = "Fecha de la primera entrevista", y = "Frecuencia") +
  facet_wrap(~anio) +
  scale_fill_manual(values = c("#FFE0B2","#FFB74D","#FF9800","#F57C00","#E65100")) +
  theme_grey()

# caminito
primera_entrevista <- numeric()
for(i in 1:nrow(data)) {
  fechas <- t(data[i,c(3,5,7)])
  fechas <- subset(fechas,!is.na(fechas))
  primera_entrevista[i] <- ifelse(nrow(fechas)==0,NA,min(fechas))
}
data$primera <- primera_entrevista

library(ggbump)
library(tidyverse)

data_processed <- data %>%
  select(primera, Tratamiento) %>%
  filter(!is.na(primera)) %>%
  filter(!is.na(Tratamiento)) %>%
  mutate(anio = format(as.Date(primera, "%d/%m/%Y"), "%Y")) %>%
  group_by(anio, Tratamiento) %>%
  summarise(conteo = n(), .groups = 'drop') %>%
  filter(Tratamiento != "Continúa en seguimiento")
data_processed$Tratamiento <- factor(data_processed$Tratamiento, 
                                     levels = c("Internación Cristalería",
                                               "Internación Baigorria",
                                               "Internación Buen Pastor",
                                               "Internación B.P",
                                               "Centro de día Zeballos",
                                               "Centro de día Buen Pastor",
                                               "Centro de día Baigorria",
                                               "Derivado",
                                               "No finalizó el proceso",
                                               "Rechaza tratamiento"
                                               ),
                                     ordered = TRUE)

# Crear el gráfico
ggplot(data_processed, aes(x = as.numeric(anio), y = conteo, color = Tratamiento, group = Tratamiento)) +
  geom_bump(size = 2) +  # Usar geom_bump para el estilo de líneas curvas
  geom_point(size = 6) +
  labs(x = "Año", y = "Conteo") +
  scale_color_manual(values = c("#fff3e0","#FFE0B2","#FFCC80","#FFB74D","#ffa726","#FF9800",
                                "#fb8c00","#F57C00","#ef6c00","#E65100")) +
  theme_minimal()
