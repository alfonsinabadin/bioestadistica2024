library(readxl)
library(ggplot2)
library(dplyr)

data <- read_excel("Trabajo práctico final/Limpieza de bases/ADMISIÓN.xlsx")

# Grafico fechas y tto ---------------------------------------------------------
primera_entrevista <- numeric()
for(i in 1:nrow(data)) {
  fechas <- t(data[i,c(3,5,7)])
  fechas <- subset(fechas,!is.na(fechas))
  primera_entrevista[i] <- ifelse(nrow(fechas)==0,NA,format(first(as.Date(fechas,"%d/%m/%Y")),"%d/%m/%Y"))
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
  filter(anio!="2021") %>%
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
  labs(x = "Fecha de la primera entrevista", y = "Frecuencia",
       fill = " ") +
  facet_wrap(~anio) +
  scale_fill_manual(values = c("#FFB74D","#FF9800","#F57C00","#ef6c00","#E65100")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "palegoldenrod",
                                        color = "palegoldenrod"))



# graficos con %
library(tidyverse)
library(ggforce)
library(scales)
library(ggplot2)

# Año anterior
data <- read_excel("Trabajo práctico final/Limpieza de bases/ADMISIÓN.xlsx")
primera_entrevista <- numeric()
for(i in 1:nrow(data)) {
  fechas <- t(data[i,c(3,5,7)])
  fechas <- subset(fechas,!is.na(fechas))
  primera_entrevista[i] <- ifelse(nrow(fechas)==0,NA,format(first(as.Date(fechas,"%d/%m/%Y")),"%d/%m/%Y"))
}
data$primera <- primera_entrevista

anio_anterior <- as.numeric(format(Sys.Date(),"%Y"))-1
anio_anterior <- subset(data,as.numeric(format(as.Date(data$primera,"%d/%m/%Y"),"%Y"))==anio_anterior)
  filter()


anio_actual <- subset(data,as.numeric(format(as.Date(data$primera,"%d/%m/%Y"),"%Y")) == format(Sys.Date(),"%Y"))

df_ant <- data.frame(
  policonsumo = , B)

# Ensure A is a factor (we'll be using it to fill the pie)
df$A <- factor(df$A)

# compute the individual proportion in this case using var C
df$prop <- df$B/sum(df$B) 

# compute the cumulative proportion and use that to plot ymax 
df$p_end <- cumsum(df$prop)

# generate a y-min between 0 and 1 less value than p_end (using p_end) 
df$p_start <- c(0, head(df$p_end ,-1))



# -------------------------------------------------------------------------

# plot 
df %>%
  mutate_at(c("p_start", "p_end"), rescale, to=pi*c(-.5,.5), from=0:1) %>%
  ggplot + 
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = .5, r = 1, start = p_start, end = p_end, fill=A)) + 
  coord_fixed() +xlab("X_label") + ylab("Y_lablel") + guides(fill=guide_legend(title="Legend Title"))
