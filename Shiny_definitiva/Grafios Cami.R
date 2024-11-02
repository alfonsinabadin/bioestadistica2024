library(ggplot2)
library(dplyr)

# CARACTERISTICAS DE CONSUMO
# Nivel educativo vs sustancia de inicio
df <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  select(`Nivel Máximo Educativo Alcanzado`, starts_with("Inicio con")) %>%
  pivot_longer(cols = starts_with("Inicio con"),
               names_to = "Sustancia",
               values_to = "Inicio") %>%
  filter(Inicio == "Si", !is.na(`Nivel Máximo Educativo Alcanzado`)) %>%
  group_by(Sustancia, `Nivel Máximo Educativo Alcanzado`) %>%
  summarize(conteo = n(), .groups = 'drop') %>%
  group_by(`Nivel Máximo Educativo Alcanzado`) %>%  
  mutate(porcentaje = (conteo / sum(conteo)) * 100) %>% 
  ungroup()

g <- ggplot(df, aes(x = porcentaje, y = `Nivel Máximo Educativo Alcanzado`, fill = Sustancia)) +
  geom_bar(stat = "identity", position = "stack", 
           aes(text = paste("Sustancia:", `Sustancia`, 
                            "<br>Porcentaje:", round(porcentaje, 2), "%",
                            "<br>Conteo:", conteo))) +
 
  scale_fill_manual(values = c("#ff4800", "#ff5400", "#ff6d00", "#ff9100",
                               "#ffaa00", "#ffaa00", "#ffb600","#ffd000","#ffea00" )) +
  labs(x = "Porcentaje", y = "Nivel Máximo Educativo Alcanzado", 
       title = "Distribución de Tratamientos por Sustancia de Consumo Actual") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme_grey() +
  theme(legend.position = "right")

ggplotly(g, tooltip = 'text')


# DERIVACION Y TRATAMIENTO ASIGNADO
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

# Consumo Actual vs Tratamiento elegido (Gráfico de barras subdivididas) -------------------------

df <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  select(`Tratamiento Elegido`, starts_with("Consumo actual con")) %>%
  pivot_longer(cols = starts_with("Consumo actual con"),
               names_to = "Sustancia",
               values_to = "Consumo") %>%
  filter(Consumo == "Si", !is.na(`Tratamiento Elegido`)) %>%
  group_by(Sustancia, `Tratamiento Elegido`) %>%
  summarize(conteo = n(), .groups = 'drop') %>%
  group_by(Sustancia) %>%
  mutate(porcentaje = (conteo / sum(conteo)) * 100)

g <- ggplot(df, aes(x = porcentaje, y = Sustancia, fill = `Tratamiento Elegido`)) +
  geom_bar(stat = "identity", position = "stack", 
           aes(text = paste("Tratamiento:", `Tratamiento Elegido`, 
                            "<br>Porcentaje:", round(porcentaje, 2), "%",
                            "<br>Conteo:", conteo))) +
  scale_fill_manual(values = c("#ff4800", "#ff5400", "#ff6d00", "#ff9100","#ffec51",
                               "#ffaa00", "#ffaa00", "#ffb600","#ffd000","#ffea00", "#f28f3b" )) +
  labs(x = "Porcentaje", y = "Sustancia de Consumo Actual", 
       title = "Distribución de Tratamientos por Sustancia de Consumo Actual") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  
  theme_grey() +
  theme(legend.position = "right")

ggplotly(g, tooltip = 'text')

