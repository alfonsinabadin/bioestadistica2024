library(ggplot2)
library(dplyr)
library(kableExtra)

# CARACTERISTICAS DE CONSUMO
# Edad de inicio consumo
df <- data %>%
  filter(!is.na(`Edad de Inicio de Cosumo`)) %>%
  group_by(`Edad de Inicio de Cosumo`) %>%
  summarize(conteo = n(), .groups = 'drop') %>%
  ungroup()

# Medidas descriptivas
media_edad <- mean(data$`Edad de Inicio de Cosumo`, na.rm = TRUE)
desviacion_edad <- sd(data$`Edad de Inicio de Cosumo`, na.rm = TRUE)
n <- sum(!is.na(data$`Edad de Inicio de Cosumo`))


g <- ggplot(df, aes(x = `Edad de Inicio de Cosumo`, y = conteo)) +
      geom_bar(stat = "identity", fill = "#ec7e14") +
      labs(x = "Edad de Inicio", y = "Conteo", title = "Conteo por Edad de Inicio") +
      scale_x_continuous(breaks = seq(min(df$`Edad de Inicio de Cosumo`), max(df$`Edad de Inicio de Cosumo`), by = 1)) +
      theme_grey() +
      theme(legend.position = 'none')+
      annotate("text", 
           x = max(df$`Edad de Inicio de Cosumo`) * 0.95, 
           y = max(df$conteo) * 0.95,                   
           label = paste("Media:", round(media_edad, 1), 
                         "\nDesvío:", round(desviacion_edad, 1), 
                         "\nn:", n),
           color = "black", size = 4, hjust = 0)
ggplotly(g, tooltip = 'text')

# Edad inicio y Edad de registro
data <- data %>%
  mutate(edad_inicio_cat = cut(
    `Edad de Inicio de Cosumo`,
    breaks = c(-Inf, 12, 17, 29, 60, Inf),  
    labels = c("0 a 12", "13 a 17", "18 a 29", "30 a 60", "Mayor de 60"),
    right = FALSE  
  )) %>%
    mutate(edad_registro_cat = cut(
      `Edad del registro`,
      breaks = c(-Inf, 12, 17, 29, 60, Inf),  
      labels = c("0 a 12", "13 a 17", "18 a 29", "30 a 60", "Mayor de 60"),
      right = FALSE  
    ))
tabla_cruzada <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>%  
  ungroup() %>%
  filter(!is.na(edad_inicio_cat) & !is.na(edad_registro_cat)) %>%
  count(edad_inicio_cat, edad_registro_cat, .drop = FALSE) %>% 
  pivot_wider(
    names_from = edad_registro_cat, 
    values_from = n, 
    values_fill = 0  
  ) %>%
  ungroup() %>%
  rename("Edad de Inicio\\Edad de Registro" = edad_inicio_cat) 

tabla_cruzada <- tabla_cruzada %>%
  mutate(Total_Fila = rowSums(select(., -`Edad de Inicio\\Edad de Registro`)))


totales_columna <- tabla_cruzada %>%
  summarise(across(starts_with("0 a 12"):ends_with("Mayor de 60"), sum, na.rm = TRUE))

tabla_cruzada <- bind_rows(tabla_cruzada, c(`Edad de Inicio\\Edad de Registro` = "Total_Columna", totales_columna))

suma_total <- sum(tabla_cruzada$Total_Fila, na.rm = TRUE)

tabla_cruzada <- tabla_cruzada %>%
  add_row(`Edad de Inicio\\Edad de Registro` = "Total", !!!totales_columna, Total_Fila = suma_total)

tabla_cruzada <- tabla_cruzada %>%
  filter(`Edad de Inicio\\Edad de Registro` != "Total_Columna") %>%
  rename("Total" = Total_Fila) 

kable(tabla_cruzada, format = "html", table.attr = "style='width:100%;'") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  add_header_above(c("Edad de inicio vs Edad de Registro" = ncol(tabla_cruzada))) %>%  
  column_spec(1, bold = TRUE, width = "150px", background = "#ffb600", color = "white") %>%  
  row_spec(0, background = "#ffb600", color = "white") %>%
  row_spec(nrow(tabla_cruzada), bold = TRUE)%>%  
  column_spec(7, bold = TRUE)

# Tabla sustancia de inicio
tabla_s_inicio <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  select(starts_with("Inicio con")) %>%
  pivot_longer(
    cols = starts_with("Inicio con"),
    names_to = "Sustancia",
    values_to = "Inicio"
  ) %>%
  filter(Inicio %in% c("Si", "No")) %>%  
  group_by(Sustancia, Inicio) %>%
  summarize(conteo = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Inicio, values_from = conteo, values_fill = 0) %>%
  mutate(
    Total = Si + No,                   
    Porcentaje = round((Si / Total) * 100,2),
    Porcentaje = paste0(Porcentaje, "%"))

kable(tabla_s_inicio, format = "html", table.attr = "style='width:100%;'") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, background = "#ffb600", color = "white") %>%
  add_header_above(if(ncol(tabla_s_inicio) == 6) c("Sustancia de inicio" = 5) else NULL) %>%
  column_spec(1, width = "25em", extra_css = "white-space: nowrap; overflow: hidden; text-overflow: ellipsis;")
    
# Tabla sustancia actual
tabla_s_actual <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  select(starts_with("Consumo actual con")) %>%
  pivot_longer(
    cols = starts_with("Consumo actual con"),
    names_to = "Sustancia",
    values_to = "Consumo"
  ) %>%
  filter(Consumo %in% c("Si", "No")) %>%  
  group_by(Sustancia, Consumo) %>%
  summarize(conteo = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Consumo, values_from = conteo, values_fill = 0) %>%
  mutate(
    Total = Si + No,                   
    Porcentaje = round((Si / Total) * 100,2),
    Porcentaje = paste0(Porcentaje, "%"))

kable(tabla_s_actual, format = "html", table.attr = "style='width:100%;'") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, background = "#ffb600", color = "white") %>%
  add_header_above(if(ncol(tabla_s_actual) == 5) c("Sustancia actual" = 5) else NULL) %>%
  column_spec(1, width = "25em", extra_css = "white-space: nowrap; overflow: hidden; text-overflow: ellipsis;")

# Sustancia actual vs Sustancia de inicio
df <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  select(starts_with("Consumo actual con"), starts_with("Inicio con")) %>%
  pivot_longer(cols = starts_with("Consumo actual con"),
               names_to = "Sustancia_Consumo_Actual",
               values_to = "Consumo_Actual") %>%
  pivot_longer(cols = starts_with("Inicio con"),
               names_to = "Sustancia_Inicio",
               values_to = "Inicio") %>%
  filter(Consumo_Actual == "Si" & Inicio == "Si") %>%  
  group_by(Sustancia_Consumo_Actual, Sustancia_Inicio) %>%
  summarize(conteo = n(), .groups = 'drop') %>%
  group_by(Sustancia_Consumo_Actual) %>%  
  mutate(porcentaje = (conteo / sum(conteo)) * 100) %>%
  ungroup()

g <- ggplot(df, aes(x = porcentaje, y = Sustancia_Consumo_Actual, fill = Sustancia_Inicio)) +
  geom_bar(stat = "identity", position = "stack", 
           aes(text = paste("Sustancia:", `Sustancia_Inicio`, 
                            "<br>Porcentaje:", round(porcentaje, 2), "%",
                            "<br>Conteo:", conteo))) +
  
  scale_fill_manual(values = c("#ff4800", "#ff5400", "#ff6d00", "#ff9100",
                               "#ffaa00", "#ffaa00", "#ffb600","#ffd000","#ffea00" )) +
  labs(x = "Porcentaje", y = "Sustancia de consumo actual", 
       title = "Distribución de Sustancia actual por Sustancia al inicio") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme_grey() +
  theme(legend.position = "right")

ggplotly(g, tooltip = 'text')

# Edad vs sustancia de inicio

df <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  select(`edad_inicio_cat`, starts_with("Inicio con")) %>%
  pivot_longer(cols = starts_with("Inicio con"),
               names_to = "Sustancia",
               values_to = "Inicio") %>%
  filter(Inicio == "Si", !is.na(`edad_inicio_cat`)) %>%
  group_by(Sustancia, `edad_inicio_cat`) %>%
  summarize(conteo = n(), .groups = 'drop') %>%
  group_by(`edad_inicio_cat`) %>%  
  mutate(porcentaje = (conteo / sum(conteo)) * 100) %>% 
  ungroup()
g <- ggplot(df, aes(x = porcentaje, y = `edad_inicio_cat`, fill = Sustancia)) +
  geom_bar(stat = "identity", position = "stack", 
           aes(text = paste("Sustancia:", `Sustancia`, 
                            "<br>Porcentaje:", round(porcentaje, 2), "%",
                            "<br>Conteo:", conteo))) +
  
  scale_fill_manual(values = c("#ff4800", "#ff5400", "#ff6d00", "#ff9100",
                               "#ffaa00", "#ffaa00", "#ffb600","#ffd000","#ffea00" )) +
  labs(x = "Porcentaje", y = "Edad de inicio", 
       title = "Distribución de Edad  por Sustancia de Consumo al inicio") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  theme_grey() +
  theme(legend.position = "right")

ggplotly(g, tooltip = 'text')

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

