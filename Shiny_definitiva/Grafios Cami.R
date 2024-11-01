library(ggplot2)
library(dplyr)

# Cantidad de tratamientos Previos (Grafico Dot-Plot) ----------------------------------------------
### Para mi no tiene sentido hacer un Dot Plot con categorica
data$`Número de Tratamientos Previos` <- as.factor(data$`Número de Tratamientos Previos`)
data_summary <- data %>%
  group_by(`Número de Tratamientos Previos`) %>%
  summarise(count = n(), .groups = 'drop')
ggplot(data_summary, aes(x = count, y = `Número de Tratamientos Previos`)) +
  geom_point(size = 3, color = "#C57412") +  # Cambia el color según tus preferencias
  labs(title = "Cantidad de Tratamientos Previos",
       x = "Cantidad",
       y = "Número de Tratamientos Previos") +
  theme_minimal()

### Haría grafico de barras
data$`Número de Tratamientos Previos` <- as.factor(data$`Número de Tratamientos Previos`)

conteo_na <- sum(is.na(data$`Número de Tratamientos Previos`))

data_summary <- data %>%
  filter(!is.na(`Número de Tratamientos Previos`)) %>%
  group_by(`Número de Tratamientos Previos`) %>%
  summarise(conteo = n(), .groups = 'drop') %>%
  complete(`Número de Tratamientos Previos`, fill = list(conteo = 0))  # Completa con ceros donde no hay tratamientos

total_count <- sum(data_summary$conteo)
data_summary <- data_summary %>%
  mutate(porcentaje = (conteo / total_count) * 100)

g <- ggplot(data_summary, aes(x = porcentaje, y = reorder(`Número de Tratamientos Previos`, conteo))) +
  geom_bar(stat = "identity", fill = "#ec7e14", 
           aes(text = paste("Número de Tratamientos Previos:", `Número de Tratamientos Previos`, 
                            "<br>Conteo:", conteo, 
                            "<br>Porcentaje:", round(porcentaje, 2), "%"))) +
  labs(x = "Porcentaje", y = "Número de Tratamientos Previos", title = "Porcentaje por Número de Tratamientos Previos") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +  # Espaciado en porcentajes
  theme_grey() +
  theme(legend.position = 'none') +
  annotate("text", x = max(data_summary$porcentaje) * 0.8, y = 1, 
           label = paste("Datos faltantes =", conteo_na), 
           size = 5, color = "black", hjust = 0)

ggplotly(g, tooltip = 'text')

# Tratamiento asignado (Gráfico de barras) ------------------------------------------------------
data$`Tratamiento Elegido` <- as.factor(data$`Tratamiento Elegido`)

df <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>% 
  ungroup() %>%
  group_by(`Tratamiento Elegido`) %>%
  summarize(conteo = n(), .groups = 'drop') %>%
  complete(`Tratamiento Elegido`, fill = list(conteo = 0)) %>%  
  filter(!is.na(`Tratamiento Elegido`))  

total_conteo <- sum(df$conteo)

df <- df %>%
  mutate(porcentaje = (conteo / total_conteo) * 100)

g <- ggplot(df, aes(x = porcentaje, y = reorder(`Tratamiento Elegido`, conteo))) +
  geom_bar(stat = "identity", fill = "#ec7e14", 
           aes(text = paste("Tratamiento:", `Tratamiento Elegido`, 
                            "<br>Conteo:", conteo, 
                            "<br>Porcentaje:", round(porcentaje, 2), "%"))) +  
  labs(x = "Porcentaje", y = "Tratamiento Elegido", title = "Porcentaje por Tratamiento Elegido") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +  
  theme_grey() +
  theme(legend.position = 'none')

ggplotly(g, tooltip = 'text')
