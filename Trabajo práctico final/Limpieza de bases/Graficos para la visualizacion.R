# Cambio el formado edad para filtro
data$Entrevista_psicológica_fecha <- as.Date(data$Entrevista_psicológica_fecha, format="%d/%m/%Y")
data$anio <- format(data$Entrevista_psicológica_fecha, "%Y")

# Graficos
# Grafico sustancia de inicio
conteo <- table(data$Sustancia_de_inicio)
conteo_df <- as.data.frame(conteo)
names(conteo_df) <- c("Sustancia", "Cantidad")
ggplot(datos, aes(x = Sustancia, y = Cantidad, fill = Sustancia)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Cantidad", title = "Consumo según sustancia de inicio", fill = "Sustancia de inicio") +
  theme_minimal() +
  theme(axis.text.x = element_blank())+
  scale_fill_manual(values = c("Cocaína" = "#e9c46a", "Pegamento" = "#f4d35e",
                               "Marihuana" = "#e5a845", "Alcohol" = "#5e503f",
                               "Psicofármacos" = "#b08a61", "Otras" = "#FDC500"))

# Gráfico  policonsumo
frecuencia_policonsumo <- as.data.frame(table(data$Policonsumo))
names(frecuencia_policonsumo) <- c("Policonsumo", "Cantidad")

ggplot(frecuencia_policonsumo, aes(x = "", y = Cantidad, fill = Policonsumo)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribución de Policonsumo", fill = "Policonsumo", y = "Cantidad de jóvenes",
       x = NULL) +
  geom_text(aes(label = Cantidad), position = position_stack(vjust = 0.5),
            colour = "white") +
  theme_minimal() +
  scale_fill_manual(values = c("Si" = "#e9c46a", "No" = "#5e503f"))

# Edad de inicio vs tratamiento
conteo_tratamiento_edad <- as.data.frame(table(data$Tratamiento, data$Edad_de_inicio))
names(conteo_tratamiento_edad) <- c("Tratamiento", "Edad_de_inicio", "Cantidad")

ggplot(conteo_tratamiento_edad, aes(x = Tratamiento, y = Cantidad, fill = Edad_de_inicio)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Relación entre Tratamiento y Edad de Inicio",
       x = "Tratamiento",
       y = "",
       fill = "Edad de Inicio") +
  theme_minimal()+
  geom_text(aes(label=Cantidad),
            position=position_stack(vjust=0.5),
            colour = "white")+
  coord_flip()+
  scale_fill_manual(values = c("Adolescentes entre 13 a 17 años" = "#e9c46a", 
                               "Jóvenes de 18 a 29 años" = "#e5a845",
                               "Niños/as de hasta 12 años" = "#b08a61"))


# Tratamiento vs CUD
]frecuencia_cud <- as.data.frame(table(data$Tiene_CUD))
names(frecuencia_cud) <- c("CUD", "Cantidad")

ggplot(frecuencia_cud, aes(x = "", y = Cantidad, fill = CUD)) +
  geom_bar(stat = "identity") +
  labs(title = "", fill = "CUD", y = "Cantidad de jóvenes", x = NULL) +
  geom_text(aes(label = Cantidad), position = position_stack(vjust = 0.5)) +
  theme_minimal() +
  scale_fill_manual(values = c("Si" = "#dfba74", "No" = "#e8a249"))
