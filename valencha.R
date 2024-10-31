library(shiny)
library(plotly)
library(dplyr)

# Suponiendo que tu base ya está cargada como `Base_completa`
# Aquí te muestro un ejemplo para cargar la base, ajusta según corresponda
# Base_completa <- read.csv("ruta/a/tu/archivo.csv")

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Análisis de Datos"),
  
  sidebarLayout(
    # Barra lateral con el menú de navegación
    sidebarPanel(
      h4("Menú de categorías"),
      tabsetPanel(
        id = "menu_tabs",
        tabPanel("Características Demográficas"),
        tabPanel("Características Sociales y Económicas"),
        tabPanel("Características de Consumo"),
        tabPanel("Derivación y Tratamiento Asignado")
      )
    ),
    
    # Panel principal donde se muestran los gráficos de acuerdo con la categoría seleccionada
    mainPanel(
      uiOutput("contenido_pestana")  # Espacio dinámico para mostrar el contenido de cada pestaña
    )
  )
)

# Servidor
server <- function(input, output) {
  
  # Características Demográficas -----------------------------------------------
  
  # Gráfico de Distribución de Edad
  
  output$dist_plot <- renderPlotly({
    edad <- Base_completa_copia$`Edad del primer registro` %>% na.omit() %>% .[. > 0]
    
    promedio <- mean(edad, na.rm = TRUE)
    desvio <- sd(edad, na.rm = TRUE)
    n <- length(na.omit(edad))
    
    fig <- plot_ly()
    
    fig <- fig %>%
      add_histogram(x = ~edad, name = "Histograma", yaxis = "y1", marker = list(color = "#ec7e14")) %>%
      layout(
        yaxis = list(title = "Frecuencia", range = c(0, 60)),
        xaxis = list(title = "Edad"),
        title = "Distribución de la Edad al Ingreso",
        annotations = list(
          text = paste("Promedio:", round(promedio, 2), "<br>",
                       "Desvío estándar:", round(desvio, 2), "<br>",
                       "n =", n),
          x = 0.95, y = 0.95, xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 12, color = "black")
        )
      )
    
    fig <- fig %>%
      add_boxplot(x = ~edad, name = "Boxplot", yaxis = "y2", boxpoints = FALSE, 
                  line = list(color = "#fbc91c"),
                  fillcolor = "rgba(251, 201, 28, 0.5)")%>%
      layout(
        yaxis2 = list(title = "", overlaying = "y", side = "right", showgrid = FALSE)
      )
    
    fig

  })
  
  # Gráfico de Distribución por Género
  
  output$gender_pie <- renderPlotly({
    genero_data <- Base_completa %>%
      filter(!is.na(Género)) %>%  
      count(Género) %>%
      mutate(porcentaje = n / sum(n) * 100) 
    
    fig <- plot_ly(genero_data, labels = ~Género, values = ~n, type = 'pie',
                   textinfo = 'label+percent', insidetextorientation = 'radial',
                   marker = list(colors = c("#ec7e14", "#fbc91c")))%>%
      layout(
        title = "Distribución por Género",
        showlegend = TRUE
      )
    
    fig
  })

  # Características Sociales y Económicas --------------------------------------
    
  output$educacion_bar <- renderPlotly({
    educacion_data <- Base_completa %>%
      filter(!is.na(`Nivel Máximo Educativo Alcanzado`)) %>%  
      count(`Nivel Máximo Educativo Alcanzado`)
    
    fig <- plot_ly(educacion_data, x = ~`Nivel Máximo Educativo Alcanzado`, y = ~n, type = 'bar',
                   marker = list(color = "#ec7e14")) %>%
      layout(
        title = "Distribución del Máximo Nivel Educativo Alcanzado",
        xaxis = list(title = "Nivel Educativo"),
        yaxis = list(title = "Frecuencia")
      )
    
    fig
  })
  
  # Gráfico de Situación Laboral
  
  output$situacion_laboral_pie <- renderPlotly({
    situacion_laboral_data <- Base_completa %>%
      filter(!is.na(`Situación Laboral Actual`)) %>%
      count(`Situación Laboral Actual`)
    
    fig <- plot_ly(situacion_laboral_data, labels = ~`Situación Laboral Actual`, values = ~n, type = 'pie',
                   textinfo = 'label+percent', insidetextorientation = 'radial',
                   marker = list(colors = c("#ec7e14", "#009E73", "#fbc91c"))) %>%
      layout(
        title = "Situación Laboral al Momento de la Entrevista",
        showlegend = TRUE
      )
    
    fig
  })
  
  # Gráfico de Barras del Tipo de Ingreso
  
  output$tipo_ingreso_bar <- renderPlotly({
    ingreso_data <- Base_completa %>%
      filter(!is.na(`Ingreso Económico`)) %>%
      count(`Ingreso Económico`)
    
    fig <- plot_ly(ingreso_data, x = ~`Ingreso Económico`, y = ~n, type = 'bar',
                   marker = list(color = "#ec7e14")) %>%
      layout(
        title = "Tipo de Ingreso Económico al Momento de la Entrevista",
        xaxis = list(title = "Tipo de Ingreso"),
        yaxis = list(title = "Frecuencia")
      )
    
    fig
  })
  
  # Gráfico de Barras de Situación Habitacional
  
  output$situacion_habitacional_bar <- renderPlotly({
    habitacion_data <- Base_completa %>%
      filter(!is.na(`Situación Habitacional Actual`)) %>%
      count(`Situación Habitacional Actual`)
    
    fig <- plot_ly(habitacion_data, x = ~`Situación Habitacional Actual`, y = ~n, type = 'bar',
                   marker = list(color = "#ec7e14")) %>%
      layout(
        title = "Situación Habitacional al Momento de la Entrevista",
        xaxis = list(title = "Situación Habitacional"),
        yaxis = list(title = "Frecuencia")
      )
    
    fig
  })
  
  # Gráfico de Red de Apoyo
  
  output$red_apoyo_pie <- renderPlotly({
    red_apoyo_data <- Base_completa %>%
      filter(!is.na(`Redes de Apoyo`)) %>%
      count(`Redes de Apoyo`)
    
    fig <- plot_ly(red_apoyo_data, labels = ~`Redes de Apoyo`, values = ~n, type = 'pie',
                   textinfo = 'label+percent', insidetextorientation = 'radial',
                   marker = list(colors = c("#56B4E9", "#fbc91c", "#009E73", "#0072B2","#CC79A7","#D55E00","#F0E442","#ec7e14","#999999"))) %>%
      layout(
        title = "Red de Apoyo",
        showlegend = TRUE
      )
    
    fig
  })
  
  # Gráfico de barras de situación judicial

  output$grafico_situacion_judicial <- renderPlotly({
    habitacion_data <- Base_completa %>%
      filter(!is.na(`Situación Judicial`)) %>%
      count(`Situación Judicial`)
    
    fig <- plot_ly(habitacion_data, x = ~`Situación Judicial`, y = ~n, type = 'bar',
                   marker = list(color = "#ec7e14")) %>%
      layout(
        title = "Situación Judicial al Momento de la Entrevista",
        xaxis = list(title = "Situación Judicial"),
        yaxis = list(title = "Frecuencia")
      )
    
    fig
  })

 
  # Gráfico de sectores de certificado de discapacidad
  
  output$grafico_cud <- renderPlotly({
    referencias_data <- Base_completa %>%
      filter(!is.na(CUD)) %>%
      count(CUD)
    
    fig <- plot_ly(referencias_data, labels = ~CUD, values = ~n, type = 'pie',
                   textinfo = 'label+percent', insidetextorientation = 'radial',
                   marker = list(colors = c("#ec7e14", "#fbc91c"))) %>%
      layout(
        title = "Certificado de Discapacidad (CUD)",
        showlegend = TRUE
      )
    
    fig
  })
  
  # Gráfico de Referencias de APS
  
  output$referencias_pie <- renderPlotly({
    referencias_data <- Base_completa %>%
      filter(!is.na(`Referencia a APS`)) %>%
      count(`Referencia a APS`)
    
    fig <- plot_ly(referencias_data, labels = ~`Referencia a APS`, values = ~n, type = 'pie',
                   textinfo = 'label+percent', insidetextorientation = 'radial',
                   marker = list(colors = c("#fbc91c", "#ec7e14", "#009E73", "#CC79A7"))) %>%
      layout(
        title = "Referencias APS",
        showlegend = TRUE
      )
    
    fig
  })
  
  output$contenido_pestana <- renderUI({
    if (input$menu_tabs == "Características Demográficas") {
      tagList(
        plotlyOutput("dist_plot"),
        tags$div(style = "margin-top: 80px;"),  # Espacio entre gráficos
        plotlyOutput("gender_pie")
      )
    } 
    
      else if (input$menu_tabs == "Características Sociales y Económicas") {
        tagList(
          plotlyOutput("educacion_bar"),
        tags$div(style = "margin-top: 80px;"),
        plotlyOutput("situacion_laboral_pie"),
        tags$div(style = "margin-top: 80px;"),
        plotlyOutput("tipo_ingreso_bar"),
        tags$div(style = "margin-top: 80px;"),
        plotlyOutput("situacion_habitacional_bar"),
        tags$div(style = "margin-top: 80px;"),
        plotlyOutput("red_apoyo_pie"),
        tags$div(style = "margin-top: 80px;"),
        plotlyOutput("grafico_situacion_judicial"),
        tags$div(style = "margin-top: 80px;"),
        plotlyOutput("grafico_cud"),
        tags$div(style = "margin-top: 80px;"),
        plotlyOutput("referencias_pie")
        )
    } 
    
      else if (input$menu_tabs == "Características de Consumo") {
      h4("Contenido de Características de Consumo")
      # Aquí puedes añadir otros gráficos relacionados con esta categoría
    } else if (input$menu_tabs == "Derivación y Tratamiento Asignado") {
      h4("Contenido de Derivación y Tratamiento Asignado")
      # Aquí puedes añadir otros gráficos relacionados con esta categoría
    }
  })
}

# Ejecuta la app
shinyApp(ui = ui, server = server)