# Librerías --------------------------------------------------------------------
library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(plotly)
library(rlang)
library(readxl)
library(shinyjs)
library(shinyvalidate)
library(geoAr)
library(ggplot2)
library(shinydashboard)
library(DT)

# Importar base
base <- function(){
  data <- read_excel("Base completa.xlsx")
}
data <- base()

# NUEVO: Extraer años únicos de la columna de fecha sin modificarla
anios <- unique(year(as.Date(data$`Fecha de registro`, format="%d-%m-%Y"))) 
# Calcular la primera entrevista
primera_entrevista <- data.frame(
  mes = numeric(),
  anio = numeric(),
  mes_anio = character(),
  Tratamiento = factor(),
  frecuencia = numeric()
)

# Juntar fechas primeras entrevistas para el grafico
for(i in 1:nrow(data)) {
  fechas <- t(data[i, c(25, 27, 29)])  # Asegúrate de que los índices son correctos
  fechas <- subset(fechas, !is.na(fechas))
  primera_fecha <- ifelse(length(fechas) == 0, NA, format(as.Date(fechas), "%d/%m/%Y"))
  
  if (!is.na(primera_fecha)) {
    primera_entrevista <- rbind(primera_entrevista, data.frame(
      mes = format(as.Date(primera_fecha, "%d/%m/%Y"), "%m"),
      anio = format(as.Date(primera_fecha, "%d/%m/%Y"), "%Y"),
      mes_anio = format(as.Date(primera_fecha, "%d/%m/%Y"), "%m/%Y"),
      Tratamiento = factor(ifelse(grepl("Internación", data$`Tratamiento Elegido`[i]), "Internación",
                                  ifelse(grepl("Centro de día", data$`Tratamiento Elegido`[i]), "Centro de día",
                                         ifelse(grepl("No finalizó el proceso", data$`Tratamiento Elegido`[i]), "Abandona",
                                                ifelse(grepl("Rechaza", data$`Tratamiento Elegido`[i]), "Rechaza",
                                                       ifelse(grepl("Derivado", data$`Tratamiento Elegido`[i]), "Derivado", NA))))),
                           levels = c("Abandona", "Rechaza", "Internación", "Centro de día", "Derivado"),
                           ordered = TRUE)
    ))
  }
}

# Filtrar y organizar los datos
primera_entrevista <- primera_entrevista %>%
  filter(!is.na(mes_anio)) %>%
  filter(anio != "2021") %>%
  filter(!is.na(Tratamiento)) %>%
  group_by(anio, mes, Tratamiento) %>%
  summarize(frecuencia = n(), .groups = 'drop') %>%
  arrange(anio, mes, desc(Tratamiento)) %>%
  group_by(anio, mes) %>%
  mutate(label = cumsum(frecuencia))

# Base de provincias y localidades
provincias <- show_arg_codes()[2:25, 5]
provincias[[1]][1] <- "CABA"
localidades_por_provincia <- readRDS("localidades.rds")
## User Interface --------------------------------------------------------------
ui <- page_navbar(
  
  useShinyjs(),
  
  # Tema con función bs_theme()
  theme = bs_theme(
    bg = "#e1e1e1",
    fg = "black",
    primary = "#ec7e14",
    secondary = "#fbc91c",
    success = "#009E73",
    base_font = font_google("Montserrat")
  ),
  
  # Dejo seteados los tamaños de las tipografías de los títulos de cada campo
  tags$head(
    tags$style(HTML("
    input::placeholder {
      font-size: 11px;
    }
    .form-control {
      font-size: 11px;
    }
    .selectize-input {
      font-size: 11px;
    }
    .selectize-dropdown {
      font-size: 11px;
    }
    
     /* Ajuste del tamaño y la posición del icono dentro del valueBox */
      .small-box {
        position: relative; /* Asegura que el ícono se posicione en relación a la caja */
        padding: 20px; /* Aumenta el padding para que haya espacio alrededor del ícono */
      }
      
      .small-box .icon-large {
        position: absolute;
        top: 50%;
        right: 10px;
        transform: translateY(-50%);
        font-size: 60px; /* Cambia el tamaño del ícono según sea necesario */
        opacity: 0.4; /* Ajusta la opacidad */
      }
      
      .small-box .inner {
        text-align: left; 
        position: relative; /* Posición relativa para ajustar la superposición */
        z-index: 1; /* Asegura que el texto esté sobre el ícono */
      }
  "))
  )
  ,
  
  lang = "en",
  
  # Título de la shiny (encabezado)
  title = tags$span(
    style = "align-items: center;",
    tags$img(
      src = "pmlogo.png",
      width = "30px",
      height = "auto",
      style = "margin-bottom: 5px;"
    ),
    tags$span("Gestión de registros - Padre misericodioso", style = "font-size: 18px;color:#ec7e14;")
  ),
  
  # Espacio en la barra de arriba (en el encabezado)
  nav_spacer(),
  
  # Pestaña nuevo registro 
  nav_panel(
    tags$span("Nuevo registro", style = "font-size: 14px;"),
    class = "bslib-page-dashboard",
    icon = icon("user")
  ),
  nav_panel(
    tags$span("Consulta y modificación de registros", style = "font-size: 14px;"),
    class = "bslib-page-dashboard"
  ),
  
  # NUEVO
  nav_panel(
    tags$span("Tablero de visualización", style = "font-size: 14px;"),
    class = "bslib-page-dashboard",
    fluidRow(
      column(2, selectInput("anio", "Seleccione un año:", choices = c("Todos los años", sort(anios)), selected = "Todos los años")),
      column(2, valueBoxOutput("int_cristaleria")),
      column(2, valueBoxOutput("int_bp_baigorria")),
      column(2, valueBoxOutput("cdd_zeballos")),
      column(2, valueBoxOutput("cdd_bp")),
      column(2, valueBoxOutput("cdd_baigorria"))
    ),
    tags$head(
      tags$style(HTML("
      .dataTable th {
        font-size: 12px; /* Cambia el tamaño de la fuente de los encabezados aquí */
        color: white; /* Cambia el color de la letra si es necesario */
        background-color: #EF8D16; /* Asegúrate de que el color de fondo sea consistente */
        padding: 2px; /* Ajusta el padding para reducir la altura */
      }
      .dataTable td {
        font-size: 11px; /* Cambia este valor para el contenido */
        padding: 3px; /* Ajusta el padding de las celdas del cuerpo si es necesario */
      }
    "))
    ),
    fluidRow(
      column(7,
             plotlyOutput("grafico_interactivo")
      ),
      column(5,
             plotOutput("grafico_serie")
             ),
      dataTableOutput("tabla_resumen")
    )
  ),
  
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  )
)


# NUEVO
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$anio)  
    if (input$anio == "Todos los años") {
      return(data)  #
    } else {
      return(data %>% filter(year(as.Date(`Fecha de registro`, format="%Y-%m-%d")) == input$anio))
    }
  })
  
  output$int_bp_baigorria <- renderValueBox({
    count_bp_baigorria <- sum(filtered_data()$`Tratamiento Elegido` == "Internación Buen Pastor / Baigorria", na.rm = TRUE)
    
    valueBox(
      count_bp_baigorria,
      "Internación Buen Pastor / Baigorria",
      icon = icon("hospital-user", class = "icon-large"), color = "yellow", width = 2
    )
  })
  
  output$int_cristaleria <- renderValueBox({
    count_cristaleria <- sum(filtered_data()$`Tratamiento Elegido` == "Internación Cristalería", na.rm = TRUE)
    valueBox(
      count_cristaleria,
      "Internación Cristalería",
      icon = icon("hospital-user", class = "icon-large"), color = "yellow", width = 2
    )
  })
  
  output$cdd_zeballos <- renderValueBox({
    count_cdd_zeballos <- sum(filtered_data()$`Tratamiento Elegido` == "Centro de día Zeballos", na.rm = TRUE)
    valueBox(
      count_cdd_zeballos ,
      "Centro de día Zeballos",
      icon = icon("warehouse", class = "icon-large"), color = "yellow", width = 2
    )
  })
  
  output$cdd_bp <- renderValueBox({
    count_cdd_bp <- sum(filtered_data()$`Tratamiento Elegido` == "Centro de día Buen Pastor", na.rm = TRUE)
    valueBox(
      count_cdd_bp,
      "Centro de día Buen Pastor",
      icon = icon("warehouse", class = "icon-large"), color = "yellow", width = 2
    )
  })
  
  output$cdd_baigorria <- renderValueBox({
    count_cdd_baigorria <- sum(filtered_data()$`Tratamiento Elegido` == "Centro de día Baigorria", na.rm = TRUE)
    valueBox(
      count_cdd_baigorria,
      "Centro de día Baigorria",
      icon = icon("warehouse", class = "icon-large"), color = "yellow", width = 2
    )
  })
  # Grafico 1
  datos_filtrados <- reactive({
    data_filtrada <- primera_entrevista
    
    # Filtrar por año si no es "Todos los años"
    if (input$anio != "Todos los años") {
      data_filtrada <- data_filtrada %>% filter(anio == input$anio)
    }
    
    return(data_filtrada)
  })
  
  output$grafico_interactivo <- renderPlotly({
    # Verificar si hay datos filtrados
    if (nrow(datos_filtrados()) == 0) {
      # Si no hay datos, mostrar un mensaje en lugar del gráfico
      return(plotly::plotly_empty() %>% 
               layout(title = "No hay datos disponibles para esta selección."))
    } 
    
    # Generar el gráfico con los datos filtrados
    grafico <- ggplot(datos_filtrados()) +
      geom_bar(aes(x = mes, y = frecuencia, fill = Tratamiento), stat = "identity") +
      labs(title = "Casos y derivaciones según fecha de primera entrevista",
           x = "Mes de la primera entrevista", y = "Frecuencia", fill = "Tratamiento") +
      scale_fill_manual(values = c("#C57412","#EF8D16","#FBC91C","#BCBF1A","#5C8001")) +
      theme_minimal()
    
    ggplotly(grafico) %>%
      layout(barmode = "stack",
             legend = list(itemclick = "toggleothers"))

  })
  # GRAFICO 2
  output$grafico_serie <- renderPlot({
    
    data <- data %>%
      mutate(`Fecha de registro` = as.Date(`Fecha de registro`, format = "%Y-%m-%d"))
    
    # Agrupar por año y mes para contar las admisiones
    admisiones_por_mes <- data %>%
      mutate(anio_mes = floor_date(`Fecha de registro`, "month")) %>% # Agrupar por mes
      group_by(anio_mes) %>%
      summarise(cantidad_admisiones = n()) %>%
      ungroup()
    # Crear el gráfico de serie de tiempo
    ggplot(admisiones_por_mes, aes(x = anio_mes, y = cantidad_admisiones)) +
      geom_line(color = "#FBC91C", size = 1) +   
      geom_point(color = "#FBC91C", size = 2) + 
      labs(title = "Cantidad de Admisiones por Mes",
           x = "Fecha",
           y = "Cantidad de Admisiones") +
      theme_minimal() 
  })
  # Tabla resumen de datos
  output$tabla_resumen <- renderDT({
    # Formatear las fechas
    data$`Fecha de registro` <- format(as.Date(data$`Fecha de registro`), "%d/%m/%Y")
    data$`Fecha de Nacimiento` <- format(as.Date(data$`Fecha de Nacimiento`), "%d/%m/%Y")
    data$`Primer fecha de registro` <- format(as.Date(data$`Primer fecha de registro`), "%d/%m/%Y")
    data$`Fecha de la Entrevista con Psicológo` <- format(as.Date(data$`Fecha de la Entrevista con Psicológo`), "%d/%m/%Y")
    data$`Fecha de la Entrevista con Psiquiátra` <- format(as.Date(data$`Fecha de la Entrevista con Psiquiátra`), "%d/%m/%Y")
    data$`Fecha de la Entrevista con Trabajador Social` <- format(as.Date(data$`Fecha de la Entrevista con Trabajador Social`), "%d/%m/%Y")
    
    datatable(
      data,
      extensions = 'Scroller',
      options = list(
        lengthChange = TRUE,
        scrollX = TRUE,
        deferRender = TRUE,
        scrollY = 150,
        scroller = TRUE,
        order = list(list(3, 'desc')),
        columnDefs = list(list(width = '100px', targets = c(1:31))),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#EF8D16', 'color': 'white', 'font-size': '12px'});",
          "$(this.api().table().body()).css({'font-size': '10px'});",  # Cambia el tamaño de la fuente aquí
          "}"
        ),
        language = list(
          url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json",
          infoFiltered = "Todos" # supuestamente cambia el All por todos pero no anda
        )
      ),
      style = "bootstrap",
      #colnames = c(),
      filter = "top"
    )
  })
}

shinyApp(ui, server)