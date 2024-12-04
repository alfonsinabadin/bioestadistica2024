library(readxl)
Base_completa <- read_excel("Shiny_definitiva/Base completa.xlsx")

data <- Base_completa %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>% 
  ungroup() %>%
  mutate(
    EdadCategorica = factor(
      ifelse(`Edad del registro` >= 0 & `Edad del registro` <= 12, "0 a 12",
             ifelse(`Edad del registro` >= 13 & `Edad del registro` <= 17, "13 a 17",
                    ifelse(`Edad del registro` >= 18 & `Edad del registro` <= 29, "18 a 29",
                           ifelse(`Edad del registro` >= 30 & `Edad del registro` <= 60, "30 a 60", 
                                  ifelse(`Edad del registro` >= 61, "+ 60", NA))))),
      levels = c("0 a 12", "13 a 17", "18 a 29", "30 a 60", "+ 60"),
      ordered = TRUE
    ),
    `Nivel Máximo Educativo Alcanzado` = factor(`Nivel Máximo Educativo Alcanzado`,
                                                levels = c("Sin instrucción formal", 
                                                           "Primario incompleto","Primario en curso", 
                                                           "Primario completo","Secundario incompleto", 
                                                           "Secundario en curso","Secundario completo", 
                                                           "Nivel superior incompleto","Nivel superior en curso", 
                                                           "Nivel superior completo"), 
                                                ordered = TRUE
    ),
    `Situación Laboral Actual` = factor(
      `Situación Laboral Actual`,
      levels = c("Estable", "Esporádico", "No tiene", "No informado"),
      ordered = TRUE
    ),
    `Ingreso Económico` = factor(`Ingresos Económicos`,
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
    ),
    `Situación Judicial` = factor(`Situación Judicial`,
                                  levels = c("Sin causas", 
                                             "Con causa cerrada", 
                                             "Con causa abierta", 
                                             "Desconoce", 
                                             "No informada", 
                                             "Otra"),
                                  ordered = TRUE),
    `Situación Habitacional Actual` = factor(`Situación Habitacional Actual`,
                                             levels = c("No informada",
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
                                             ordered = TRUE),
    CUD = factor(CUD,
                 levels = c("No informado",
                            "Si", 
                            "No"),
                 ordered = TRUE),
    EdadInicioCategorica = factor(
      ifelse(`Edad de Inicio de Consumo` >= 0 & `Edad de Inicio de Consumo` <= 12, "0 a 12",
             ifelse(`Edad de Inicio de Consumo` >= 13 & `Edad de Inicio de Consumo` <= 17, "13 a 17",
                    ifelse(`Edad de Inicio de Consumo` >= 18 & `Edad de Inicio de Consumo` <= 29, "18 a 29",
                           ifelse(`Edad de Inicio de Consumo` >= 30 & `Edad de Inicio de Consumo` <= 60, "30 a 60", 
                                  ifelse(`Edad de Inicio de Consumo` >= 61, "+ 60", NA))))),
      levels = c("0 a 12", "13 a 17", "18 a 29", "30 a 60", "+ 60"),
      ordered = TRUE
    ),
    `Sustancia de inicio`= factor(`Sustancia de inicio`,
                                  levels = c("Alcohol", "Crack", "Cocaína", "Marihuana",
                                             "Nafta aspirada", "Pegamento", "Psicofármacos", "Otra"),
                                  ordered = TRUE)
  )

df <- data %>%
  pivot_longer(
    cols = c("Consumo actual con Alcohol",
             "Consumo actual con Crack",
             "Consumo actual con Cocaína",
             "Consumo actual con Marihuana",
             "Consumo actual con Nafta Aspirada",
             "Consumo actual con Pegamento",
             "Consumo actual con Psicofármacos",
             "Consumo actual con Otras"
             ),
    names_to = "Sustancia",
    values_to = "Consume"
  ) %>%
  filter(Consume == "Si") %>%
  mutate(
    Sustancia = factor(case_when(
      Sustancia == "Consumo actual con Alcohol" ~ "Alcohol",
      Sustancia == "Consumo actual con Crack" ~ "Crack",
      Sustancia == "Consumo actual con Cocaína" ~ "Cocaína",
      Sustancia == "Consumo actual con Marihuana" ~ "Marihuana",
      Sustancia == "Consumo actual con Nafta Aspirada" ~ "Nafta Aspirada",
      Sustancia == "Consumo actual con Pegamento" ~ "Pegamento",
      Sustancia == "Consumo actual con Psicofármacos" ~ "Psicofármacos",
      Sustancia == "Consumo actual con Otras" ~ "Otras"
    ), levels = c("Alcohol", "Crack", "Cocaína", "Marihuana", "Nafta aspirada","Pegamento", "Psicofármacos", "Otra"),
    ordered = TRUE),
    `Sustancia de inicio`= factor(`Sustancia de inicio`,
                                  levels = c("Alcohol", "Crack", "Cocaína", "Marihuana",
                                             "Nafta aspirada", "Pegamento", "Psicofármacos", "Otra"),
                                  ordered = TRUE)
  ) %>%
  select(`ID de registro`, Sustancia, `Sustancia de inicio`) %>%
  group_by(`Sustancia de inicio`,Sustancia)%>%
  summarise(conteo = n(), .groups = "drop") %>%
  filter(!is.na(Sustancia),!is.na(`Sustancia de inicio`)) %>%
  complete(Sustancia, `Sustancia de inicio`, fill = list(conteo = 0))
  
g <- ggplot(df, aes(x = conteo, y = Sustancia, fill = `Sustancia de inicio`,
                    text = paste(
                      "\nSustancia de inicio:", ..fill..,
                      "\nFrecuencia:", x))) +
  geom_bar(stat = "identity", position = "stack") + # Usa "stack" para apilar, "dodge" para barras lado a lado
  labs(
    x = "Frecuencia",
    y = "Sustancia de consumo actual",
    fill = "Sustancia de Inicio",
    title = "Sustancia de inicio en cada sustancia de consumo actual"
  )+
  scale_fill_manual(values = c("#FBC91C", "#828a00", "#274001", "#EC7E14", "#4d8584", "#a62f03", "#400d01", "#4C443C")) +
  theme_fivethirtyeight() +
  theme(
    legend.position = "right",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5),      # Centrar el título de la leyenda
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  )
ggplotly(g, tooltip = "text") %>%
  layout(
    title = list(
      y = 0.93,
      title_x = 0.2,
      font = list(family = "Montserrat", size = 15, color = "grey1"),
      pad = list(l = -80)
    ),
    xaxis = list(
      title = list(
        text = "Frecuencia",
        font = list(family = "Montserrat", size = 12, color = "grey1")
      ),
      tickfont = list(family = "Montserrat", size = 10, color = "grey")
    ),
    yaxis = list(
      title = list(
        text = "",
        font = list(family = "Montserrat", size = 12, color = "grey1")
      ),
      tickfont = list(family = "Montserrat", size = 12, color = "grey")
    ),
    legend = list(
      title = list(
        text = "Sustancia de Inicio", # Texto del título de la leyenda
        font = list(family = "Montserrat", size = 12, color = "grey1") # Estilo del título
      ),
      font = list(family = "Montserrat", size = 10, color = "grey") # Estilo del texto de la leyenda
    ),
    hoverlabel = list(
      font = list(
        family = "Montserrat",
        size = 10,
        color = "white",
        style = "italic",
        textcase = "word caps"
      )
    )
  )

df <- data %>%
  select(EdadCategorica,`Sustancia de inicio`) %>%
  group_by(EdadCategorica,`Sustancia de inicio`) %>%
  summarise(conteo = n()) %>%
  ungroup() %>%
  filter(!is.na(EdadCategorica)) %>%
  complete(EdadCategorica, `Sustancia de inicio`, fill = list(conteo = 0))

g <- ggplot(df, aes(x = conteo, y = EdadCategorica, fill = `Sustancia de inicio`,
                    text = paste(
                      "\nSustancia de inicio:", ..fill..,
                      "\nFrecuencia:", x))) +
  geom_bar(stat = "identity", position = "stack") + # Usa "stack" para apilar, "dodge" para barras lado a lado
  labs(
    x = "Frecuencia",
    y = "Grupo de Edad (SEDRONAR)",
    fill = "Sustancia de Inicio",
    title = "Sustancia de inicio en cada grupo de edad (SEDRONAR)"
  )+
  scale_fill_manual(values = c("#FBC91C", "#828a00", "#274001", "#EC7E14", "#4d8584", "#a62f03", "#400d01", "#4C443C")) +
  theme_fivethirtyeight() +
  theme(
    legend.position = "right",
    legend.title.position = "top",
    legend.title = element_text(hjust = 0.5),      # Centrar el título de la leyenda
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)
  )
ggplotly(g, tooltip = "text") %>%
  layout(
    title = list(
      y = 0.93,
      title_x = 0.2,
      font = list(family = "Montserrat", size = 15, color = "grey1"),
      pad = list(l = -80)
    ),
    xaxis = list(
      title = list(
        text = "Frecuencia",
        font = list(family = "Montserrat", size = 12, color = "grey1")
      ),
      tickfont = list(family = "Montserrat", size = 10, color = "grey")
    ),
    yaxis = list(
      title = list(
        text = "",
        font = list(family = "Montserrat", size = 12, color = "grey1")
      ),
      tickfont = list(family = "Montserrat", size = 12, color = "grey")
    ),
    legend = list(
      title = list(
        text = "Sustancia de Inicio", # Texto del título de la leyenda
        font = list(family = "Montserrat", size = 12, color = "grey1") # Estilo del título
      ),
      font = list(family = "Montserrat", size = 10, color = "grey") # Estilo del texto de la leyenda
    ),
    hoverlabel = list(
      font = list(
        family = "Montserrat",
        size = 10,
        color = "white",
        style = "italic",
        textcase = "word caps"
      )
    )
  )
