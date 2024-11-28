library(ggplot2)
library(dplyr)
library(kableExtra)
library(readxl)
library(tidyr)
library(plotly)
library(gridExtra)
library(graphics)
library(egg)
library(ggthemes)
library(forcats)

data <- read_excel("Shiny_definitiva/Base completa.xlsx")

# CARACTERISTICAS DE CONSUMO
# Edad de inicio consumo
df <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  filter(!is.na(`Edad de Inicio de Consumo`))

# Medidas descriptivas
media_edad <- mean(df$`Edad de Inicio de Consumo`, na.rm = TRUE)
desviacion_edad <- sd(df$`Edad de Inicio de Consumo`, na.rm = TRUE)
n <- sum(!is.na(df$`Edad de Inicio de Consumo`))

# Gráfico de histograma
g <- ggplot(df, aes(x = `Edad de Inicio de Consumo`)) +
  geom_histogram(fill = "#ec7e14", binwidth = 1, color = "black") +
  labs(x = "Edad de Inicio", y = "Frecuencia", title = "Distribución por Edad de Inicio") +
  scale_x_continuous(breaks = seq(min(df$`Edad de Inicio de Consumo`), 
                                  max(df$`Edad de Inicio de Consumo`), by = 1)) +
  theme_grey() +
  theme(legend.position = 'none') +
  annotate("text",
           x = max(df$`Edad de Inicio de Consumo`) * 0.95,
           y = max(table(df$`Edad de Inicio de Consumo`)) * 0.95,  
           label = paste("Media:", round(media_edad, 1),
                         "\nDesvío:", round(desviacion_edad, 1),
                         "\nn:", n),
           color = "black", size = 4, hjust = 0)
ggplotly(g, tooltip = 'text')

# Edad inicio y Edad de registro
data <- data %>%
  mutate(edad_inicio_cat = cut(
    `Edad de Inicio de Consumo`,
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
  select(`Sustancia de inicio`) %>%
  filter(!is.na(`Sustancia de inicio`)) %>%  
  group_by(`Sustancia de inicio`) %>%
  summarize(conteo = n(), .groups = 'drop') %>%
  mutate(
    Porcentaje = round((conteo / sum(conteo)) * 100, 2)
  ) %>%
  ungroup() 
tabla_s_inicio <- tabla_s_inicio %>%
  mutate(`Sustancia de inicio` = fct_reorder(`Sustancia de inicio`,
                                                as.numeric(`Sustancia de inicio` != "Otras")))

  
kable(tabla_s_inicio, format = "html", table.attr = "style='width:100%;'") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, background = "#ffb600", color = "white") %>%
  add_header_above(if(ncol(tabla_s_inicio) == 6) c("Sustancia de inicio" = 5) else NULL) %>%
  column_spec(1, width = "25em", extra_css = "white-space: nowrap; overflow: hidden; text-overflow: ellipsis;")

# Tabla sustancia actual
tabla_s_actual <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>%
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
    Porcentaje = round((Si / Total) * 100,2)) %>%
  ungroup() 

  
kable(tabla_s_actual, format = "html", table.attr = "style='width:100%;'") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, background = "#ffb600", color = "white") %>%
  add_header_above(if(ncol(tabla_s_actual) == 5) c("Sustancia actual" = 5) else NULL) %>%
  column_spec(1, width = "25em", extra_css = "white-space: nowrap; overflow: hidden; text-overflow: ellipsis;")

# Sustancia actual vs Sustancia de inicio
df <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>%
  select(starts_with("Consumo actual con"), `Sustancia de inicio`) %>%
  pivot_longer(
    cols = starts_with("Consumo actual con"),
    names_to = "Sustancia_Consumo_Actual",
    values_to = "Consumo_Actual"
  ) %>%
  filter(Consumo_Actual == "Si" & !is.na(`Sustancia de inicio`)) %>%  # Filtra donde hay consumo actual y sustancia de inicio definida
  group_by(Sustancia_Consumo_Actual, `Sustancia de inicio`) %>%       # Agrupa por sustancia de consumo actual y sustancia de inicio específica
  summarize(conteo = n(), .groups = 'drop') %>%
  group_by(Sustancia_Consumo_Actual) %>%  
  mutate(porcentaje = (conteo / sum(conteo)) * 100) %>%
  ungroup()

df <- df %>%
  mutate(Sustancia_Consumo_Actual = fct_reorder(Sustancia_Consumo_Actual,
                                                as.numeric(Sustancia_Consumo_Actual != "Consumo actual con Otras")))


g <- ggplot(df, aes(x = porcentaje, y = Sustancia_Consumo_Actual, fill = `Sustancia de inicio`)) +
  geom_bar(stat = "identity", position = "stack",
           aes(text = paste("Sustancia:", `Sustancia de inicio`,
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
  select(`edad_inicio_cat`, `Sustancia de inicio`) %>%
  filter(!is.na(`Sustancia de inicio`), !is.na(`edad_inicio_cat`)) %>%
  group_by(`Sustancia de inicio`, `edad_inicio_cat`) %>%
  summarize(conteo = n(), .groups = 'drop') %>%
  group_by(`edad_inicio_cat`) %>%  
  mutate(porcentaje = (conteo / sum(conteo)) * 100) %>%
  ungroup()

g <- ggplot(df, aes(x = porcentaje, y = `edad_inicio_cat`, fill = `Sustancia de inicio`)) +
  geom_bar(stat = "identity", position = "stack",
           aes(text = paste("Sustancia:", `Sustancia de inicio`,
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
  select(`Nivel Máximo Educativo Alcanzado`, `Sustancia de inicio`) %>%
  filter(!is.na(`Sustancia de inicio`), !is.na(`Nivel Máximo Educativo Alcanzado`)) %>%
  group_by(`Sustancia de inicio`, `Nivel Máximo Educativo Alcanzado`) %>%
  summarize(conteo = n(), .groups = 'drop') %>%
  group_by(`Nivel Máximo Educativo Alcanzado`) %>%  
  mutate(porcentaje = (conteo / sum(conteo)) * 100) %>%
  ungroup()

g <- ggplot(df, aes(x = porcentaje, y = `Nivel Máximo Educativo Alcanzado`, fill = `Sustancia de inicio`)) +
  geom_bar(stat = "identity", position = "stack",
           aes(text = paste("Sustancia:", `Sustancia de inicio`,
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
# Cantidad de tratamientos Previos (Grafico de Bastones) ----------------------------------------------

df <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>%
  group_by(`Número de Tratamientos Previos`) %>%
  summarise(count = n(), .groups = 'drop')  %>%
  ungroup()

g <- ggplot(df, aes(x = `Número de Tratamientos Previos`, y = count)) +
  geom_bar(stat = "identity", fill = "#ffb600", width = 0.01) +
  
  geom_point(aes(y = count,text = paste("Número de Tratamientos Previos:", `Número de Tratamientos Previos`,
                                        "<br>Cantidad de Pacientes:", count)), size = 3, shape = 16, color = "#ffb600") +
  
  labs(title = "Cantidad de Tratamientos Previos por pacientes",
       y = "Cantidad de Pacientes",
       x = "Número de Tratamientos Previos")  +
  
  theme_minimal()

ggplotly(g, tooltip = "text")

# Tratamiento asignado (Gráfico de barras) ------------------------------------------------------
data$`Tratamiento Elegido` <- as.factor(data$`Tratamiento Elegido`)

df <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>%
  group_by(`Tratamiento Elegido`) %>%
  summarize(conteo = n(), .groups = 'drop') %>%
  complete(`Tratamiento Elegido`, fill = list(conteo = 0)) %>%  
  filter(!is.na(`Tratamiento Elegido`))  %>%
  ungroup()

total_conteo <- sum(df$conteo)

df <- df %>%
  mutate(porcentaje = (conteo / total_conteo) * 100)

g <- ggplot(df, aes(x = porcentaje, y = reorder(`Tratamiento Elegido`, conteo))) +
  geom_bar(stat = "identity", fill = "#ffb600",
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
  select(`Tratamiento Elegido`, starts_with("Consumo actual con")) %>%
  pivot_longer(cols = starts_with("Consumo actual con"),
               names_to = "Sustancia",
               values_to = "Consumo") %>%
  filter(Consumo == "Si", !is.na(`Tratamiento Elegido`)) %>%
  group_by(Sustancia, `Tratamiento Elegido`) %>%
  summarize(conteo = n(), .groups = 'drop') %>%
  group_by(Sustancia) %>%
  mutate(porcentaje = (conteo / sum(conteo)) * 100)%>%
  ungroup()

df <- df %>%
  mutate(Sustancia = fct_reorder(Sustancia,
                                 as.numeric(Sustancia != "Consumo actual con Otras")))

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