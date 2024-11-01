library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(gridExtra)

data <- read_excel("Shiny_definitiva/Base completa.xlsx")

# Características demográficas -------------------------------------------------

# grafico 1

## no se entiende

# grafico 2

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
  select(Edad) %>%
  group_by(Edad) %>%
  summarize(conteo = n()) %>%
  complete(Edad) %>%
  mutate(conteo = ifelse(is.na(conteo),0,conteo)) %>%
  filter(!is.na(Edad))

g <- ggplot(df, aes(x = conteo, y = rev(Edad))) +
  geom_bar(stat = "identity", fill = "#ec7e14", 
           aes(text = paste("Categoría de Edad:", rev(Edad), "<br>Conteo:", conteo))) +
  labs(x = "Conteo", y = "Categoría de Edad", title = "Conteo por categoría de Edad") +
  scale_x_continuous(breaks = seq(min(df$conteo),max(df$conteo),by = 50)) +
  theme_grey() +
  theme(legend.position = 'none')

ggplotly(g, tooltip = 'text')

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
  scale_fill_gradient(low = "#FFDC2E", high = "#ec7e14") +
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

# grafico 8

df <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>% 
  ungroup() %>%
  mutate(`Ingreso Económico` = factor(`Ingreso Económico`,
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
