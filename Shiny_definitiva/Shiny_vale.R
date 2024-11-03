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
      uiOutput("contenido_pestana"),  
      conditionalPanel(
        condition = "input.menu_tabs == 'Derivación y Tratamiento Asignado'",
        plotlyOutput("trat_previos"),
        plotlyOutput("trat_asignados"),
        plotlyOutput("cons_vs_trat")
      ),
      conditionalPanel(
        condition = "input.menu_tabs == 'Características de Consumo'",
        uiOutput("tabla_s_inicio"),
        uiOutput("tabla_s_actual"),
        plotlyOutput("acual_vs_inicio"),
        plotlyOutput("edad_sinicio"),
        plotlyOutput("educ_sustancia")

      )
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
  # Caracteristicas de consumo ------------------------------------------------
    # Tabla sustancia de inicio
  output$tabla_s_inicio <- renderText({
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
        Porcentaje = paste0(round((Si / Total) * 100, 2), "%")
      )
    
    kable(tabla_s_inicio, format = "html", table.attr = "style='width:100%;'") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
      row_spec(0, background = "#ffb600", color = "white") %>%
      add_header_above(c("Sustancia de inicio" = 5)) %>%
      column_spec(1, width = "25em", extra_css = "white-space: nowrap; overflow: hidden; text-overflow: ellipsis;") %>%
      as.character()  # Convertir la tabla a cadena de texto HTML
  })
    # Tabla sustancia actual
  output$tabla_s_actual <- renderText({
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
  })
    # Sustancia Actual vs Sustancia de Inicio
  output$acual_vs_inicio <- renderPlotly({
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
  })
    # Edad vs Sustancia de Inicio
  output$edad_sinicio <- renderPlotly({
    data <- data %>%
      mutate(edad_inicio_cat = cut(
        `Edad de Inicio de Cosumo`,
        breaks = c(-Inf, 12, 17, 29, 60, Inf),  
        labels = c("0 a 12", "13 a 17", "18 a 29", "30 a 60", "Mayor de 60"),
        right = FALSE  
      ))
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
  })
    # Nivel Educativo vs Sustancia de inicio
  output$educ_sustancia <- renderPlotly({
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
  })
  
  # Derivacion y tratamiento asignado
    # Cantidad de tratamientos Previos
    output$trat_previos <- renderPlotly({
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
  })
    
    # Tratamiento Asignado
    output$trat_asignados <- renderPlotly({
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
    })
    
    # Consumo actual vs Tratamiento elegido
    output$cons_vs_trat <- renderPlotly({
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
    })
}

# Ejecuta la app
shinyApp(ui = ui, server = server)