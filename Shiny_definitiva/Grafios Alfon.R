library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(gridExtra)
library(graphics)
library(egg)
library(ggthemes)

data <- read_excel("Shiny_definitiva/Base completa.xlsx")

# Características demográficas -------------------------------------------------

# Distribución de la edad al ingreso

df <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>% 
  ungroup()

conteo_na <- paste("Hay", sum(is.na(df$`Edad del registro`)), "registros personales\nsin información")

## histograma 

df <- df %>%
  filter(!is.na(`Edad del registro`)) %>%
  select(`Edad del registro`) 

hist <- ggplot(df) +
  geom_histogram(aes(x = `Edad del registro`, y = (..count..)),
                 position = "identity", binwidth = 2,
                 fill = "#ff8800", color = "grey1")+
  scale_x_continuous(limits = c(0,60), breaks = seq(0,60,1))  +
  labs(x = "Edad de registro", 
       y = "Frecuencia", 
       title = "Distribución de la edad de registro",
       subtitle = conteo_na) +
  theme_fivethirtyeight() + 
  theme(axis.title = element_text())

hist_plotly <- ggplotly(hist) %>%
  layout(title = list(y = 0.98,
                      text = "Distribución de la edad de registro",
                      font = list(family = "Montserrat Semibold", size = 15, color = "grey1")),
         xaxis = list(title = list(text = "Edad de registro",
                                   font = list(family = "Montserrat", size = 12, color = "grey1")),
                      tickvals = seq(0, 60, 1),
                      tickfont = list(family = "Montserrat", size = 10, color = "grey")),
         yaxis = list(title = list(text = "Frecuencia",
                                   font = list(family = "Montserrat", size = 12, color = "grey1")),
                      tickfont = list(family = "Montserrat", size = 10, color = "grey"))) %>%
  add_annotations(
    text = conteo_na,
    x = 0.05, y = 0.95,  # Posición centrada
    xref = "paper", yref = "paper",
    showarrow = FALSE,
    font = list(family = "Montserrat", size = 12, color = "white"),
    bgcolor = "grey",  # Fondo negro semi-transparente
    bordercolor = "grey",  # Color del borde
    borderwidth = 2,
    borderpad = 10,
    align = "center"
  )


## boxplot 

df <- df %>% 
  group_by(`Edad del registro`) %>%
  summarise(value = n())

box_plotly <- plot_ly(df, x = ~`Edad del registro`, type = "box",
                      marker = list(color = "grey10"),
                      line = list(color = "grey10"),
                      fillcolor = "#ff8800") %>%
  layout(xaxis = list(title = list(text = "Edad de registro",
                                   font = list(family = "Montserrat", size = 15, color = "grey1")),
                      tickvals = seq(0, 60, 10),
                      tickfont = list(family = "Montserrat", size = 10, color = "grey")),
         yaxis = list(showticklabels = FALSE)) # Oculta etiquetas en eje X

## grafico final 

subplot(box_plotly, hist_plotly, nrows = 2, heights = c(0.2, 0.8), 
        shareX = TRUE,
        titleX = TRUE,
        titleY = TRUE, 
        margin = 0)

# Edad en años cumplidos, al ingreso

df <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>% 
  ungroup() %>%
  mutate(
    Edad = factor(
      ifelse(`Edad del registro` >= 0 & `Edad del registro` <= 12, "0 a 12",
             ifelse(`Edad del registro` >= 13 & `Edad del registro` <= 17, "13 a 17",
                    ifelse(`Edad del registro` >= 18 & `Edad del registro` <= 29, "18 a 29",
                           ifelse(`Edad del registro` >= 30 & `Edad del registro` <= 60, "30 a 60", NA)))),
      levels = c("0 a 12", "13 a 17", "18 a 29", "30 a 60"),
      ordered = TRUE
    )
  ) %>%
  group_by(Edad) %>%
  summarise(conteo = n()) %>%
  complete(Edad) %>%
  mutate(conteo = ifelse(is.na(conteo),0,conteo))

Edad <- subset(df$Edad,!is.na(df$Edad))
Conteo <- subset(df$conteo,!is.na(df$Edad))

# Calcula el conteo de vacíos
conteo_na <- sum(is.na(data$`Edad del registro`))
texto_na <- paste("Vacíos: ", conteo_na)

plot_ly(
  type = 'pie',
  labels = Edad,
  values = Conteo,
  textinfo = 'none',  # Oculta etiquetas dentro de los sectores
  hole = 0.5,  # Convierte el gráfico en una dona
  marker = list(colors = c("#FF8800", "#FFA500", "#FFD700", "#FFEB3B")),
  showlegend = TRUE
) %>%
  layout(
    title = list(text = "Distribución de Edad",
                 font = list(family = "Montserrat Semibold", size = 15, color = "grey1")),
    showlegend = TRUE,
    legend = list(
      title = list(text = "Categorías de Sedronar",
                   font = list(family = "Montserrat", size = 12, color = "grey1")),
      orientation = "v",  # Alineación vertical
      x = 1.1,  # Posiciona la leyenda a la derecha
      y = 0.5,  # Centra la leyenda verticalmente
      bordercolor = "grey10",  # Color del borde
      borderwidth = 1  # Ancho del borde
    ),
    annotations = list(
      list(
        x = 0.5, y = 0.5,  # Posición centrada en el gráfico
        text = texto_na,
        showarrow = FALSE,
        font = list(size = 12, color = "white"),
        bgcolor = "grey",
        bordercolor = "grey",
        borderwidth = 2,
        borderpad = 10,
        align = "center"
      )
    )
  )





# grafico 3 

df <- data %>%
  mutate(Género = factor(Género, 
                         levels = c("No informado", "Mujer", "Hombre", "Trans (feminidades)", "Trans (masculinidades)", "Otro"),
                         ordered = TRUE)
  ) %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>% 
  ungroup() %>%
  group_by(Género) %>%
  summarize(conteo = n()) %>%
  complete(Género, fill = list(conteo = 0)) %>%
  filter(!is.na(Género))

plot_ly(
  df,
  labels = ~Género,
  values = ~conteo,
  type = 'pie',
  textinfo = 'label+percent',
  hoverinfo = 'label+value',
  marker = list(colors = c("#FFA500", "#FF8C00", "#FFD700", "#FF4500", "#FFA07A", "#FF6347"))
) %>%
  layout(
    title = "Distribución por Género",
    showlegend = TRUE
  )

## Pedir a Diego mejorar, con leyenda centrada y ordenar leyenda en el orden del 
## input de la Shiny

# grafico 4

## no se puede

# Características sociales y económicas ----------------------------------------

# grafico 5

df <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>% 
  ungroup() %>%
  mutate(`Nivel Máximo Educativo Alcanzado` = factor(`Nivel Máximo Educativo Alcanzado`,
                                                     levels = c("Sin instrucción formal", 
                                                                "Primario incompleto","Primario en curso", 
                                                                "Primario completo","Secundario incompleto", 
                                                                "Secundario en curso","Secundario completo", 
                                                                "Nivel superior incompleto","Nivel superior en curso", 
                                                                "Nivel superior completo"), 
                                                     ordered = TRUE
  )
  ) %>%
  group_by(`Nivel Máximo Educativo Alcanzado`) %>%
  summarize(conteo = n()) %>%
  complete(`Nivel Máximo Educativo Alcanzado`) %>%
  mutate(conteo = ifelse(is.na(conteo),0,conteo)) %>%
  filter(!is.na(`Nivel Máximo Educativo Alcanzado`))

g <- ggplot(df, aes(x = conteo, y = `Nivel Máximo Educativo Alcanzado`)) +
  geom_bar(stat = "identity", fill = "#ec7e14", 
           aes(text = paste("Nivel Máximo Educativo Alcanzado:", `Nivel Máximo Educativo Alcanzado`, "<br>Conteo:", conteo))) +
  labs(x = "Conteo", y = "Nivel Máximo Educativo Alcanzado", title = "Conteo por nivel máximo educativo alcanzado`") +
  scale_x_continuous(breaks = seq(min(df$conteo),max(df$conteo),by = 50)) +
  theme_grey() +
  theme(legend.position = 'none')

ggplotly(g, tooltip = 'text')

# grafico 6

df <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>% 
  ungroup() %>%
  mutate(`Nivel Máximo Educativo Alcanzado` = factor(`Nivel Máximo Educativo Alcanzado`,
                                                     levels = c("Sin instrucción formal", 
                                                                "Primario incompleto","Primario en curso", 
                                                                "Primario completo","Secundario incompleto", 
                                                                "Secundario en curso","Secundario completo", 
                                                                "Nivel superior incompleto","Nivel superior en curso", 
                                                                "Nivel superior completo"), 
                                                     ordered = TRUE
  ),
  Edad = factor(
    ifelse(`Edad del registro` >= 0 & `Edad del registro` <= 12, "0 a 12",
           ifelse(`Edad del registro` >= 13 & `Edad del registro` <= 17, "13 a 17",
                  ifelse(`Edad del registro` >= 18 & `Edad del registro` <= 29, "18 a 29",
                         ifelse(`Edad del registro` >= 30 & `Edad del registro` <= 60, "30 a 60", NA)))),
    levels = c("0 a 12", "13 a 17", "18 a 29", "30 a 60"),
    ordered = TRUE
  )
  ) %>%
  group_by(Edad, `Nivel Máximo Educativo Alcanzado`, .add=TRUE) %>%
  summarise(conteo = ifelse(is.na(n()),0,n()), .groups = "drop") %>%
  filter(!is.na(Edad),!is.na(`Nivel Máximo Educativo Alcanzado`))

g <- ggplot(df, aes(x = Edad, y = `Nivel Máximo Educativo Alcanzado`, fill = conteo)) +
  geom_tile(aes(text = paste(
    "Nivel Máximo Educativo Alcanzado:", `Nivel Máximo Educativo Alcanzado`,
    "<br>Edad:", Edad, 
    "<br>Conteo:", conteo))) +
  scale_fill_gradient(low = "#FFDC2E", high = "#ec7e14") + # probar min y max
  labs(title = "Máximo nivel educativo alcanzado según grupo de edad",
       fill = "Conteo") +
  theme_grey()
ggplotly(g, tooltip = 'text')

# grafico 7

df <- data%>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>% 
  ungroup() %>%
  mutate(`Situación Laboral Actual` = factor(
    `Situación Laboral Actual`,
    levels = c("Estable", "Esporádico", "No tiene", "No informado"),
    ordered = TRUE
  )) %>%
  group_by(`Situación Laboral Actual`,.add = TRUE) %>%
  summarise(conteo = n(), .groups = "drop") %>%
  filter(!is.na(`Situación Laboral Actual`)) %>%
  complete(`Situación Laboral Actual`) %>%
  mutate(conteo = ifelse(is.na(conteo),0,conteo))

plot_ly(
  df,
  labels = ~`Situación Laboral Actual`,
  values = ~conteo,
  type = 'pie',
  textinfo = 'label+percent',
  hoverinfo = 'label+value',
  marker = list(colors = c("#FFA500", "#FF8C00", "#FFD700"))
) %>%
  layout(
    title = "Distribución por situación laboral actual",
    showlegend = TRUE
  )
### sacar el no infnormado a un cuadro de texto

# grafico 8

df <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>% 
  ungroup() %>%
  mutate(`Ingreso Económico` = factor(`Ingresos Económicos`,
                                      levels = c(
                                        "No informado",
                                        "AlimentAR",
                                        "AUH",
                                        "AUHD",
                                        "Jubilación",
                                        "PNC nacional",
                                        "PNC provincial",
                                        "Salario formal", 
                                        "Salario informal", 
                                        "Sin ingresos", 
                                        "Otro subsidio/plan social", 
                                        "Otro tipo de pensión", 
                                        "Otro tipo de ingreso"),
                                      ordered = TRUE
  )
  ) %>%
  group_by(`Ingreso Económico`, .add = TRUE) %>%
  summarize(conteo = n(), .groups = "drop") %>%
  filter(!is.na(`Ingreso Económico`)) %>%
  complete(`Ingreso Económico`) %>%
  mutate(conteo = ifelse(is.na(conteo),0,conteo))

g <- ggplot(df, aes(x = conteo, y = `Ingreso Económico`)) +
  geom_bar(stat = "identity", fill = "#ec7e14", 
           aes(text = paste("Ingreso económico:", `Ingreso Económico`, "<br>Conteo:", conteo))) +
  labs(x = "Conteo", y = "Ingreso económico", title = "Conteo por ingreso económico`") +
  scale_x_continuous(breaks = seq(min(df$conteo),max(df$conteo),by = 50)) +
  theme_grey() +
  theme(legend.position = 'none')

ggplotly(g, tooltip = 'text')

# grafico 9

df <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>% 
  ungroup() %>%
  mutate(`Situación Habitacional Actual` = factor(`Situación Habitacional Actual`,
                                                  levels = c(
                                                    "No informada",
                                                    "Casa/Departamento alquilado", 
                                                    "Casa/Departamento cedido", 
                                                    "Casa/Departamento propio", 
                                                    "Institución de salud mental", 
                                                    "Institución penal", 
                                                    "Institución terapéutica", 
                                                    "Pensión", 
                                                    "Refugio", 
                                                    "Situación de calle", 
                                                    "Otra"),
                                                  ordered = TRUE
  )
  ) %>%
  group_by(`Situación Habitacional Actual`, .add = TRUE) %>%
  summarize(conteo = n(), .groups = "drop") %>%
  filter(!is.na(`Situación Habitacional Actual`)) %>%
  complete(`Situación Habitacional Actual`) %>%
  mutate(conteo = ifelse(is.na(conteo),0,conteo))

g <- ggplot(df, aes(x = conteo, y = `Situación Habitacional Actual`)) +
  geom_bar(stat = "identity", fill = "#ec7e14", 
           aes(text = paste("Situación habitacional actual:", `Situación Habitacional Actual`, "<br>Conteo:", conteo))) +
  labs(x = "Conteo", y = "Situación habitacional actual", title = "Conteo por situación habitacional`") +
  scale_x_continuous(breaks = seq(min(df$conteo),max(df$conteo),by = 50)) +
  theme_grey() +
  theme(legend.position = 'none')

ggplotly(g, tooltip = 'text')

# grafico 10

df <- data%>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>% 
  ungroup() %>%
  mutate(`Referencia a APS` = factor(
    `Referencia a APS`,
    levels = c("Referencia con seguimiento", 
               "Referencia sin seguimiento", 
               "No está referenciado", 
               "No informada"),
    ordered = TRUE
  )) %>%
  group_by(`Referencia a APS`,.add = TRUE) %>%
  summarise(conteo = n(), .groups = "drop") %>%
  filter(!is.na(`Referencia a APS`)) %>%
  complete(`Referencia a APS`) %>%
  mutate(conteo = ifelse(is.na(conteo),0,conteo))

p2 <- plot_ly(
  df,
  labels = ~`Referencia a APS`,
  values = ~conteo,
  type = 'pie',
  textinfo = 'label+percent',
  hoverinfo = 'label+value',
  marker = list(colors = c("#FFA500", "#FF8C00", "#FFD700"))
) %>%
  layout(
    title = "Distribución por situación laboral actual",
    showlegend = TRUE
  )

df <- data%>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>% 
  ungroup() %>%
  mutate(`Redes de Apoyo` = factor(
    `Redes de Apoyo`,
    levels = c("No informado",
               "Familiares", 
               "Amistades", 
               "Institucionalidades",
               "Sin vínculos actualmente"),
    ordered = TRUE
  )) %>%
  group_by(`Redes de Apoyo`,.add = TRUE) %>%
  summarise(conteo = n(), .groups = "drop") %>%
  filter(!is.na(`Redes de Apoyo`)) %>%
  complete(`Redes de Apoyo`) %>%
  mutate(conteo = ifelse(is.na(conteo),0,conteo))

p1 <- plot_ly(
  df,
  labels = ~`Redes de Apoyo`,
  values = ~conteo,
  type = 'pie',
  textinfo = 'label+percent',
  hoverinfo = 'label+value',
  marker = list(colors = c("#FFA500", "#FF8C00", "#FFD700"))
) %>%
  layout(
    title = "Distribución por situación laboral actual",
    showlegend = TRUE
  )

subplot(p1, p2, nrows=1)
