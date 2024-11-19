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
library(tibble)
library(writexl)
library(tidyr)
library(shinyWidgets)
library(ggplot2)
library(ggthemes)
library(shinydashboard)
library(showtext)
library(leaflet) # para gráfico de mapa
library(emojifont) # para icono de mujery hombre
font_add_google("Montserrat")

# Importar base ----------------------------------------------------------------
base <- function(){
  data <- read_excel("Base completa.xlsx")
}
data <- base()

# Base de provincias y localidades ---------------------------------------------
provincias <- show_arg_codes()[2:25, 5]
provincias[[1]][1] <- "CABA"
provincia_vacia <- tibble(name_iso = " ")
provincias <- bind_rows(provincia_vacia, provincias)
colnames(provincias) <- "Provincias"

localidades_por_provincia <- readRDS("localidades.rds")
for (provincia in names(localidades_por_provincia)) {
  localidades_por_provincia[[provincia]] <- c("", localidades_por_provincia[[provincia]])
}

# Datos de provincias para plot
provincias_df <- data.frame(
  Provincia = c(
    "Buenos Aires", "Catamarca", "Chaco", "Chubut", "Córdoba", "Corrientes", 
    "Entre Ríos", "Formosa", "Jujuy", "La Pampa", "La Rioja", "Mendoza", 
    "Misiones", "Neuquén", "Río Negro", "Salta", "San Juan", "San Luis", 
    "Santa Cruz", "Santa Fe", "Santiago del Estero", 
    "Tierra del Fuego, Antártida e Islas del Atlántico Sur", "Tucumán", 
    "Ciudad Autónoma de Buenos Aires"
  ),
  Latitud = c(
    -36.0000, -28.5000, -26.5000, -44.0000, -32.5000, -28.5000, 
    -32.0000, -24.5000, -23.5000, -37.0000, -29.5000, -34.5000, 
    -26.5000, -39.0000, -40.5000, -25.0000, -31.0000, -33.0000, 
    -48.5000, -31.5000, -27.5000, -54.5000, -27.0000, -34.6037
  ),
  Longitud = c(
    -60.0000, -66.0000, -60.5000, -68.5000, -64.5000, -58.5000, 
    -59.0000, -60.0000, -65.5000, -65.0000, -67.5000, -68.5000, 
    -54.5000, -70.0000, -67.5000, -65.0000, -68.5000, -66.0000, 
    -69.0000, -60.5000, -63.5000, -68.5000, -65.5000, -58.3816
  )
)

# User Interface
ui <- page_navbar(
  
  useShinyjs(),
  
  theme = bs_theme(
    bg = "#e1e1e1",
    fg = "black",
    primary = "#ec7e14",
    secondary = "#fbc91c",
    base_font = font_google("Montserrat")
  ),
  
  tags$head(
    tags$style(HTML("
      input::placeholder {
        font-size: 12px;
      }
      .form-control {
        font-size: 12px;
      }
      .selectize-input {
        font-size: 12px;
      }
      .selectize-dropdown {
        font-size: 12px;
      }
      .checkbox-inline, .checkbox-label {
        font-size: 12px;
      }
      .shiny-input-container {
        font-size: 12px;
      }
      .checkbox-group-input {
        margin-top: 10px;
      }
    "))
  ),
  
  lang = "en",
  
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
  
  nav_spacer(),
  
  # Pestaña Nuevo registro 
  nav_panel(
    tags$span("Nuevo registro", style = "font-size: 14px;"),
    class = "bslib-page-dashboard",
    icon = icon("user")
  ),
  
  nav_panel(
    tags$span("Modificación de registros", style = "font-size: 14px;"),
    class = "bslib-page-dashboard",
    icon = icon("pen-to-square")
  ),
  
  nav_menu(
    tags$span("Tablero de visualización", style = "font-size: 14px;"),
    icon = icon("chart-simple"),
    
    tabPanel(HTML("<span style='font-size:14px'>Análisis demográfico</span>"),
      
      fluidRow(
       column(
         width = 3,
         wellPanel(
           
           style = "min-height: 390px;",
           
           h4("Filtros", style = "font-size: 15px; font-weight: bold;"), 
           
           fluidRow(
             div(
               
               # Filtro año
               pickerInput(
                 "year_filter",
                 label = tags$span("Año del registro:", style = "font-size:15px;"),
                 choices = NULL,  # Los años se actualizarán en el server
                 selected = NULL,
                 multiple = TRUE,
                 options = pickerOptions(
                   actionsBox = TRUE,   # Para seleccionar/deseleccionar todo
                   #live-search = TRUE,    # Permite buscar dentro del selector
                   selectAllText = "Todos",
                   deselectAllText = "Ninguno",
                   noneSelectedText = "Seleccione"
                   )
                 ),
               
               # Filtro Edad Categorica
               checkboxGroupInput(
                 "edad_filter",
                 label = tags$span("Categoría de edad:", style = "font-size:15px;"),
                 choices = c("0 a 12", "13 a 17", "18 a 29", "30 a 60"),
                 selected = c("0 a 12", "13 a 17", "18 a 29", "30 a 60")),
               
               # Filtro Sexo
               checkboxGroupInput(
                 "sexo_filter",
                 label = tags$span("Sexo biológico:", style = "font-size:15px;"),
                 choices = c("Masculino","Femenino","No informado"),
                 selected =  c("Masculino","Femenino","No informado"))
              
               )
             )
           )
         ),
         
         column(
           width = 9,
           
           h2("Análisis demográfico", style = "font-size: 20px; font-weight: bold;"),
           
           fluidRow(
            
             column(
               width = 9,
               plotlyOutput("histbox.edad", height = "390px")
               ),
             
             column(
               width = 3,
               
               fluidRow(
                 plotOutput("box_sexo_masc", height = "130px")  # Ajusta el alto según lo que necesites
               ),
               
               fluidRow(
                 plotOutput("box_sexo_fem", height = "130px")  # Ajusta el alto según lo que necesites
               ),
               
               fluidRow(
                 plotOutput("box_sexo_ni", height = "130px")  # Ajusta el alto según lo que necesites
               )
             )
             ),
           
           fluidRow(
             style = "margin-top:15px;",
             column(
               width = 6
             ),
             
             column(
               
               width = 6,
               h4(tags$span("Provincias de origen (excepto Santa Fe)", style = "font-size:15px;")),
               fluidRow(
               column(
                 width = 6,
                 leafletOutput("map", height = "200px")
                 ),
               
               column(width = 6,
                      tableOutput("map.table")
                      )
               )
               )
             )
           )
       )
      ),
    tabPanel(HTML("<span style='font-size:14px'>Análisis socioeconómico</span>"),
             
             h2("Análisis socioeconómico", style = "font-size: 20px; font-weight: bold;"),
             
             fluidRow(
               column(
                 width = 3,
                 wellPanel(
                   
                   style = "min-height: 390px;",
                   
                   h4("Filtros", style = "font-size: 15px; font-weight: bold;"),
                   
                   fluidRow(
                     div(
                       
                       # Filtro año
                       pickerInput(
                         "year_filter_2",
                         label = tags$span("Año del registro:", style = "font-size:15px;"),
                         choices = NULL,  # Los años se actualizarán en el server
                         selected = NULL,
                         multiple = TRUE,
                         options = pickerOptions(
                           actionsBox = TRUE,   # Para seleccionar/deseleccionar todo
                           #live-search = TRUE,    # Permite buscar dentro del selector
                           selectAllText = "Todos",
                           deselectAllText = "Ninguno",
                           noneSelectedText = "Seleccione"
                         )
                       ),
                       
                       # Filtro Edad Categorica
                       checkboxGroupInput(
                         "edad_filter_2",
                         label = tags$span("Categoría de edad:", style = "font-size:15px;"),
                         choices = c("0 a 12", "13 a 17", "18 a 29", "30 a 60"),
                         selected = c("0 a 12", "13 a 17", "18 a 29", "30 a 60")),
                       
                       # Filtro año
                       pickerInput(
                         "nivel_educativo_filter",
                         label = tags$span("Nivel educativo alcanzado:", style = "font-size:15px;"),
                         choices = c("Sin instrucción formal", 
                                     "Primario incompleto", 
                                     "Primario en curso", 
                                     "Primario completo", 
                                     "Secundario incompleto", 
                                     "Secundario en curso", 
                                     "Secundario completo", 
                                     "Nivel superior incompleto", 
                                     "Nivel superior en curso", 
                                     "Nivel superior completo"),  # Los años se actualizarán en el server
                         selected = NULL,
                         multiple = TRUE,
                         options = pickerOptions(
                           actionsBox = TRUE,   # Para seleccionar/deseleccionar todo
                           #live-search = TRUE,    # Permite buscar dentro del selector
                           selectAllText = "Todos",
                           deselectAllText = "Ninguno",
                           noneSelectedText = "Seleccione"
                         )
                       ),
                       
                       # Filtro año
                       pickerInput(
                         "sit_laboral_filter",
                         label = tags$span("Situación laboral actual:", style = "font-size:15px;"),
                         choices = c("Estable", 
                                     "Esporádico", 
                                     "No tiene"),  # Los años se actualizarán en el server
                         selected = NULL,
                         multiple = TRUE,
                         options = pickerOptions(
                           actionsBox = TRUE,   # Para seleccionar/deseleccionar todo
                           #live-search = TRUE,    # Permite buscar dentro del selector
                           selectAllText = "Todos",
                           deselectAllText = "Ninguno",
                           noneSelectedText = "Seleccione"
                         )
                       )
                     )
                   )
                   )
                 ),
               
               column(
                 width = 9,
                 
                 fluidRow(
                   
                   column(
                     width = 8,
                     plotlyOutput("barras.nivel.educativo",
                                    height = "380px")
                     ),
                   
                   column(
                     width = 4,
                     plotlyOutput("donut.laboral",
                                  height = "380px")
                     )
                   ),
                 
                 fluidRow(
                   
                   style = "margin-top: 10px",
                   
                   plotlyOutput("matriz.colores",
                                height = "280px")
                   ),
                 
                 fluidRow(
                   
                   style = "margin-top: 10px",
                   
                   column(
                     width = 6,
                     plotlyOutput("barras_ingreso",
                                  height = "280px")
                   ),
                   
                   column(
                     width = 6,
                     plotlyOutput("barras_judicial",
                                  height = "280px")
                   )
                 ),
                 
                 fluidRow(
                   
                   style = "margin-top: 10px",
                   
                   column(
                     width = 6,
                     plotlyOutput("barras_habitacional",
                                  height = "280px")
                   ),
                   
                   column(
                     width = 6,
                     plotlyOutput("donut.cud",
                                  height = "280px")
                   )
                 )
                 )
               )
             )
    ),
  
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  )
)

server <- function(input, output, session) {
  
  # Cargar datos sacando los registros personales repetidos 
  data <- base() %>%
    group_by(`ID de la persona`) %>%
    filter(row_number() == n()) %>% 
    ungroup() %>%
    mutate(
      EdadCategorica = factor(
        ifelse(`Edad del registro` >= 0 & `Edad del registro` <= 12, "0 a 12",
               ifelse(`Edad del registro` >= 13 & `Edad del registro` <= 17, "13 a 17",
                      ifelse(`Edad del registro` >= 18 & `Edad del registro` <= 29, "18 a 29",
                             ifelse(`Edad del registro` >= 30 & `Edad del registro` <= 60, "30 a 60", NA)))),
        levels = c("0 a 12", "13 a 17", "18 a 29", "30 a 60"),
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
                   ordered = TRUE)
    )
  
  # Extraer los años únicos de la base y actualizar el filtro de años
  observe({
    years <- unique(year(data$`Fecha de registro`))
    years <- years[!is.na(years)]
    updatePickerInput(session, "year_filter", choices = rev(years))
  })
  
  # Crear el data frame reactivo filtrado
  filtered_data <- reactive({
    data %>%
      filter(is.null(input$year_filter) | year(`Fecha de registro`) %in% input$year_filter,
             is.null(input$edad_filter) | EdadCategorica %in% c(input$edad_filter,NA), # agrego NA para que funcione el conteo de vacíos
             is.null(input$sexo_filter) | `Sexo biológico` %in% c(input$sexo_filter,NA))
  })
  # Extraer los años únicos de la base y actualizar el filtro de años
  observe({
    years <- unique(year(data$`Fecha de registro`))
    years <- years[!is.na(years)]
    updatePickerInput(session, "year_filter_2", choices = rev(years))
  })
  
  # Crear el data frame reactivo filtrado
  filtered_data_2 <- reactive({
    data %>%
      filter(is.null(input$year_filter_2) | year(`Fecha de registro`) %in% input$year_filter_2,
             is.null(input$edad_filter_2) | EdadCategorica %in% c(input$edad_filter_2,NA),
             is.null(input$nivel_educativo_filter) | `Nivel Máximo Educativo Alcanzado` %in% c(input$nivel_educativo_filter,NA),
             is.null(input$sit_laboral_filter) | `Situación Laboral Actual` %in% c(input$sit_laboral_filter,NA)
             )
  })
  
  # Boxplot + histograma + tabla
  output$histbox.edad <- renderPlotly({
    # Cargar base reactiva
    df <- filtered_data()
    
    # Conteo de registros sin información de edad
    conteo_na <- paste("Hay",conteo_na,"de",nrow(df),"registros\npersonales sin información")
    
    # Filtrar edades no nulas y preparar los datos para el histograma
    edad_data <- df %>%
      filter(!is.na(`Edad del registro`))
    
    hist <- ggplot(edad_data) +
      geom_histogram(aes(x = `Edad del registro`, 
                         y = (..count..),
                         text = paste(
                           "Edad:", ..x..,
                           "\nFrecuencia:", ..count..)
      ),
      position = "identity", binwidth = 2,
      fill = "#ff8800", color = "grey1") +
      labs(x = "Edad de registro", 
           y = "Frecuencia", 
           title = "Distribución de la edad de registro",
           subtitle = conteo_na) +
      scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 10)) +
      theme_fivethirtyeight() + 
      theme(axis.title = element_text())
    
    hist_plotly <- ggplotly(hist, tooltip = "text") %>%
      layout(title = list(y = 0.95,
                          text = "Distribución de la edad de registro",
                          font = list(family = "Montserrat", size = 15, color = "grey1")),
             xaxis = list(title = list(text = "Edad de registro",
                                       font = list(family = "Montserrat", size = 12, color = "grey1")),
                          tickvals = seq(0, 60, 10),
                          tickfont = list(family = "Montserrat", size = 10, color = "grey")),
             yaxis = list(title = list(text = "Frecuencia",
                                       font = list(family = "Montserrat", size = 12, color = "grey1")),
                          tickfont = list(family = "Montserrat", size = 10, color = "grey")),
             hoverlabel = list(font = list(family = "Montserrat", size = 10, color = "white",
                                           style = "italic", textcase = "word caps"))
                               ) %>%
      add_annotations(
        text = conteo_na,
        x = 0.05, y = 0.95,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(family = "Montserrat", size = 12, color = "white"),
        bgcolor = "grey",
        bordercolor = "grey",
        borderwidth = 2,
        borderpad = 10,
        align = "center"
      )
    
    # Generar boxplot
    
    box_plotly <- plot_ly(edad_data, x = ~`Edad del registro`, type = "box",
                          jitter = 0.1,
                          marker = list(color = "grey10"),
                          line = list(color = "grey10"),
                          fillcolor = "#ff8800") %>%
      add_boxplot(hoverinfo = "x") %>%
      layout(title = list(y = 0.95,
                          text = "Distribución de la edad de registro",
                          font = list(family = "Montserrat", size = 15, color = "grey1")),
             xaxis = list(title = list(text = "Edad de registro",
                                       font = list(family = "Montserrat", size = 12, color = "grey1")),
                          tickvals = seq(0, 60, 10),
                          tickfont = list(family = "Montserrat", size = 10, color = "grey"),
                          hoverformat = ".2f"),
             yaxis = list(showticklabels = FALSE),
             hoverlabel = list(font = list(family = "Montserrat", size = 10, color = "white",
                                           style = "italic")))
    
    # tabla
    edades <- edad_data %>%
      group_by(EdadCategorica) %>%
      summarise(conteo = n()) %>%
      complete(EdadCategorica)%>%
      mutate(conteo = ifelse(is.na(conteo),0,conteo),
             pos = c(12/2,12+(17-12)/2,17+(29-17)/2,29+(60-29)/2),
             cantmax = c(12,17,29,60),
             cantmin = c(0,12,17,29))
    
    tabla_edades <- ggplot(edades, aes(text = paste("Categoría de SEDONAR:", EdadCategorica, "\nCantidad:", conteo))) +
      geom_rect(aes(xmin = cantmin, xmax = cantmax, ymin = 0, ymax = 2, 
                    fill = EdadCategorica, alpha = 0.2)) +
      geom_text(aes(x = pos, y = 1, label = paste(EdadCategorica,paste("n:",conteo),sep = "\n")), color = "grey1", size = 3) +
      scale_fill_manual(values = c("#FBC91C", "#EC7E14", "#4C443C","#F9EDCC")) +
      scale_color_manual(values = c("#FBC91C", "#EC7E14", "#4C443C","#F9EDCC")) +
      labs(title = "Distribución de la edad de registro") +
      scale_y_continuous(breaks = c(0,60)) +
      theme_fivethirtyeight() + 
      theme(legend.position = "none",
            axis.title = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            text = element_text(family = "Montserrat"),
            panel.background = element_blank(),
            panel.grid = element_blank())
    
    tabla_edades_plotly <- ggplotly(tabla_edades, tooltip = "text") %>%
      layout(title = list(y = 0.95,
                          text = "Distribución de la edad de registro",
                          font = list(family = "Montserrat", size = 15, color = "grey1")),
             xaxis = list(title = list(text = "Edad de registro",
                                       font = list(family = "Montserrat", size = 12, color = "grey1")),
                          tickvals = seq(0, 60, 10),
                          tickfont = list(family = "Montserrat", size = 10, color = "grey"),
                          hoverformat = ".2f"),
             yaxis = list(showticklabels = FALSE),
             hoverlabel = list(font = list(family = "Montserrat", size = 10, color = "white",
                                           style = "italic")))
    
    # Combinar histogram y boxplot
    subplot(box_plotly, hist_plotly, tabla_edades_plotly, nrows = 3, heights = c(0.2, 0.65, 0.15), 
            shareX = TRUE, titleX = TRUE, titleY = TRUE, margin = 0)
  })
  
  output$box_sexo_masc <- renderPlot({
    
    df <- filtered_data() %>%
      filter(!is.na(`Sexo biológico`))
    
    # Calcular el conteo de "Femenino" y el porcentaje
    total_count <- nrow(df)
    masc_count <- ifelse(is.na(sum(df$`Sexo biológico` == "Masculino")), 
                         0, 
                         sum(df$`Sexo biológico` == "Masculino"))
    masc_percentage <- round((masc_count / total_count) * 100, 1)
    
    ggplot() + 
      geom_rect(aes(xmin = 0, xmax=2, ymin=0, ymax=1), fill = "#F9EDCC") +
      geom_fontawesome(x = 0.3, y = 0.5,"fa-male", color='#EC7E14',size = 14,
                       vjust = 0.45) +
      geom_text(aes(x=0.60,y=0.625,label = "Sexo biológico:"),
                hjust=0,vjust=0,family = "Montserrat", size = 3.5, color = "grey1")+
      geom_text(aes(x=0.6,y=0.40,label = "Masculino"),
                hjust=0,vjust=0, fontface = "bold",family = "Montserrat", size = 5, color = "grey1")+
      geom_text(aes(x=0.6,y=0.305,
                    label = paste("Cantidad:",masc_count,paste("(",masc_percentage,"%)",sep = ""))),
                hjust = 0, family = "Montserrat",size = 3.5, color = "#EC7E14") +
      scale_y_continuous(breaks = c(0,1)) +
      theme(text = element_text(family = "Montserrat"),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.line = element_blank(),
            plot.background = element_rect(fill = "#e1e1e1", color = "#e1e1e1"),
            panel.background = element_rect(fill = "#e1e1e1",color = "#e1e1e1"))
  })
  
  output$box_sexo_fem <- renderPlot({
    
    df <- filtered_data() %>%
      filter(!is.na(`Sexo biológico`))
    
    # Calcular el conteo de "Femenino" y el porcentaje
    total_count <- nrow(df)
    female_count <- ifelse(is.na(sum(df$`Sexo biológico` == "Femenino")), 
                         0, 
                         sum(df$`Sexo biológico` == "Femenino"))
    female_percentage <- round((female_count / total_count) * 100, 1)
    
    ggplot() + 
      geom_rect(aes(xmin = 0, xmax=2, ymin=0, ymax=1), fill = "#F9EDCC") +
      geom_fontawesome(x = 0.3, y = 0.5,"fa-female", color='#EC7E14',size = 14,
                       vjust = 0.45) +
      geom_text(aes(x=0.60,y=0.625,label = "Sexo biológico:"),
                hjust=0,vjust=0,family = "Montserrat", size = 3.5, color = "grey1")+
      geom_text(aes(x=0.6,y=0.40,label = "Femenino"),
                hjust=0,vjust=0, fontface = "bold",family = "Montserrat", size = 5, color = "grey1")+
      geom_text(aes(x=0.6,y=0.305,
                    label = paste("Cantidad:",female_count,paste("(",female_percentage,"%)",sep = ""))),
                hjust = 0, family = "Montserrat",size = 3.5, color = "#EC7E14") +
      scale_y_continuous(breaks = c(0,1)) +
      theme(text = element_text(family = "Montserrat"),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.line = element_blank(),
            plot.background = element_rect(fill = "#e1e1e1", color = "#e1e1e1"),
            panel.background = element_rect(fill = "#e1e1e1",color = "#e1e1e1"))
  })
  
  output$box_sexo_ni <- renderPlot({
    
    df <- filtered_data() %>%
      filter(!is.na(`Sexo biológico`))
    
    # Calcular el conteo de "Femenino" y el porcentaje
    total_count <- nrow(df)
    ni_count <- ifelse(is.na(sum(df$`Sexo biológico` == "No informado")), 
                           0, 
                           sum(df$`Sexo biológico` == "No informado"))
    ni_percentage <- round((ni_count / total_count) * 100, 1)
    
    ggplot() + 
      geom_rect(aes(xmin = 0, xmax=2, ymin=0, ymax=1), fill = "#F9EDCC") +
      geom_fontawesome(x = 0.3, y = 0.5,"fa-question", color='#EC7E14',size = 14,
                       vjust = 0.45) +
      geom_text(aes(x=0.60,y=0.625,label = "Sexo biológico:"),
                hjust=0,vjust=0,family = "Montserrat", size = 3.5, color = "grey1")+
      geom_text(aes(x=0.6,y=0.40,label = "No informado"),
                hjust=0,vjust=0, fontface = "bold",family = "Montserrat", size = 5, color = "grey1")+
      geom_text(aes(x=0.6,y=0.305,
                    label = paste("Cantidad:",ni_count,paste("(",ni_percentage,"%)",sep = ""))),
                hjust = 0, family = "Montserrat",size = 3.5, color = "#EC7E14") +
      scale_y_continuous(breaks = c(0,1)) +
      theme(text = element_text(family = "Montserrat"),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.line = element_blank(),
            plot.background = element_rect(fill = "#e1e1e1", color = "#e1e1e1"),
            panel.background = element_rect(fill = "#e1e1e1",color = "#e1e1e1"))
  })
  
  output$map <- renderLeaflet({
    
    df <- filtered_data()
    provincias_alerta <- subset(provincias_df,
                                provincias_df$Provincia != "Santa Fe" & provincias_df$Provincia %in% df$Provincia)
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = -60, lat = -40, zoom = 2) %>%
      addCircleMarkers(
        data = provincias_alerta,
        lng = provincias_alerta$Longitud,
        lat = provincias_alerta$Latitud,
        color = "orange"
      )
  })
  
  output$map.table <- renderTable({
    # Calcular el conteo por provincia en el dataframe `filtered_data()`
    df <- filtered_data() %>%
      group_by(Provincia) %>%
      summarise(Conteo = n()) %>%
      filter(!is.na(Provincia))
    
    # Seleccionar las provincias de `provincias_df` que están en `df` y no son "Santa Fe"
    provincias_alerta <- subset(provincias_df,
                                provincias_df$Provincia != "Santa Fe" & provincias_df$Provincia %in% df$Provincia)
    
    # Combinar los datos de `provincias_alerta` y `df` para la tabla final
    df <- merge(provincias_alerta, df, by = "Provincia") %>%
      select(Provincia, Conteo)
    
    if(nrow(df) == 0) {
      df <- data.frame(
        Provincia = c(""),
        Conteo = c("")
      )
    }
    
    df  # Mostrar la tabla
  },
  colnames = TRUE)
  
  output$barras.nivel.educativo <- renderPlotly({
    
    df <- filtered_data_2() %>%
      group_by(`ID de la persona`) %>%
      filter(row_number() == n()) %>% 
      ungroup()
    
    conteo_na <- sum(is.na(df$`Nivel Máximo Educativo Alcanzado`) | df$`Nivel Máximo Educativo Alcanzado` == "No informado")
    conteo_na <- paste("Hay",conteo_na,"de",nrow(df),"registros personales sin información")
    
    df <- df %>%
      group_by(`Nivel Máximo Educativo Alcanzado`) %>%
      summarize(conteo = n()) %>%
      complete(`Nivel Máximo Educativo Alcanzado`) %>%
      mutate(conteo = ifelse(is.na(conteo),0,conteo)) %>%
      filter(!is.na(`Nivel Máximo Educativo Alcanzado`))
    
    g <- ggplot(df, aes(x = conteo, y = `Nivel Máximo Educativo Alcanzado`)) +
      geom_bar(stat = "identity", fill = "#ec7e14", 
               aes(text = paste("Nivel Máximo Educativo Alcanzado:", `Nivel Máximo Educativo Alcanzado`, "<br>Conteo:", conteo))) +
      labs(x = "Conteo", 
           y = "Nivel Máximo Educativo Alcanzado", 
           title = "Conteo por nivel máximo educativo alcanzado") +
      scale_x_continuous(breaks = seq(0,max(df$conteo)+100,by = 50),
                         limits = c(0,max(df$conteo) + 100)) +
      theme_fivethirtyeight() +
      theme(legend.position = 'none',
            plot.background = element_rect("#f0f0f0"))
    
    ggplotly(g, tooltip = 'text')  %>%
      layout(title = list(y = 0.95, title_x=0.5,
                          text = "Conteo por nivel máximo educativo alcanzado",
                          font = list(family = "Montserrat", size = 15, color = "grey1"),
                          pad = list(l=-80)),
             xaxis = list(title = list(text = "Frecuencia",
                                       font = list(family = "Montserrat", size = 12, color = "grey1")),
                          #tickvals = seq(0, max(df$conteo)+50, 50),
                          tickfont = list(family = "Montserrat", size = 10, color = "grey")),
             yaxis = list(title = list(text = "",
                                       font = list(family = "Montserrat", size = 12, color = "grey1")),
                          tickfont = list(family = "Montserrat", size = 12, color = "grey")),
             hoverlabel = list(font = list(family = "Montserrat", size = 10, color = "white",
                                           style = "italic", textcase = "word caps"))
      ) %>%
      add_annotations(
        text = conteo_na,
        x = 0.95, y = 0.95,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(family = "Montserrat", size = 10, color = "white"),
        bgcolor = "grey",
        bordercolor = "grey",
        borderwidth = 2,
        borderpad = 10,
        align = "center"
      )
  })
  
  output$matriz.colores <- renderPlotly({
    
    df <- filtered_data_2() %>%
      group_by(EdadCategorica, `Nivel Máximo Educativo Alcanzado`, .add=TRUE) %>%
      summarise(conteo = ifelse(is.na(n()),0,n()), .groups = "drop") %>%
      filter(!is.na(EdadCategorica),!is.na(`Nivel Máximo Educativo Alcanzado`))
    
    g <- ggplot(df, aes(x = EdadCategorica, y = `Nivel Máximo Educativo Alcanzado`, fill = conteo)) +
      geom_tile(aes(text = paste(
        "Nivel Máximo Educativo Alcanzado:", `Nivel Máximo Educativo Alcanzado`,
        "<br>Edad (SEDRONAR):", EdadCategorica, 
        "<br>Conteo:", conteo))) +
      scale_fill_gradient(low = "#FFDC2E", high = "#ec7e14") + # probar min y max
      labs(title = "Máximo nivel educativo alcanzado según grupo de edad",
           fill = "Conteo") +
      theme_fivethirtyeight() +
      theme(legend.text = element_text(size = 5),
            legend.title = element_text(size = 8))
    
    ggplotly(g, tooltip = 'text')  %>%
      layout(title = list(y = 0.95, title_x=0.5,
                          automargin = list(yref='container'),
                          text = paste("Máximo nivel educativo alcanzado según grupo de edad"),
                          font = list(family = "Montserrat", size = 15, color = "grey1"),
                          pad = list(l=-80)),
             xaxis = list(title = list(text = paste0("Conteo por ingreso económico"),
                                       font = list(family = "Montserrat", size = 12, color = "grey1")),
                          #tickvals = seq(0, max(df$conteo)+50, 50),
                          tickfont = list(family = "Montserrat", size = 10, color = "grey")),
             yaxis = list(title = list(text = "",
                                       font = list(family = "Montserrat", size = 12, color = "grey1")),
                          tickfont = list(family = "Montserrat", size = 12, color = "grey")),
             hoverlabel = list(font = list(family = "Montserrat", size = 10, color = "white",
                                           style = "italic", textcase = "word caps"))
      )
    
  })
  
  output$donut.laboral <- renderPlotly({
    df <- filtered_data_2()
    
    conteo_na <- sum(is.na(df$`Situación Laboral Actual`) | df$`Situación Laboral Actual` == "No informado")
    conteo_na <- paste(conteo_na,"de",nrow(df),"\nno informado")
    
    df <- df %>%
      group_by(`Situación Laboral Actual`,.add = TRUE) %>%
      summarise(conteo = n(), .groups = "drop") %>%
      complete(`Situación Laboral Actual`) %>%
      filter(!is.na(`Situación Laboral Actual`),
             `Situación Laboral Actual` != "No informado") %>%
      mutate(conteo = ifelse(is.na(conteo),0,conteo)) 
    
    df %>% plot_ly(labels = ~`Situación Laboral Actual`, values = ~conteo,
                   marker = list(colors = c("#FBC91C", "#EC7E14", "#4C443C","#F9EDCC"),
                                 line = list(color = '#e1e1e1', width = 2))) %>% 
      add_pie(hole = 0.6) %>% 
      
      layout(title = list(y = 0.98, x = 0.55,
                          text = "Distribución de la situación laboral",
                          font = list(family = "Montserrat", size = 15, color = "grey1")),
             showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             paper_bgcolor = '#e1e1e1',
             plot_bgcolor = '#e1e1e1',
             showlegend = TRUE,
             legend = list(
               title = list(text = "Situación laboral actual",
                            font = list(family = "Montserrat", size = 12, color = "grey1")),
               font = list(family = "Montserrat", size = 10, color = "grey1"),
               orientation = "v",
               x = 0.18,
               y = -0.3,
               bordercolor = "grey10",
               borderwidth = 1,
               borderpad = 5
             )
      ) %>%
      add_annotations(
        text = conteo_na,
        x = 0.5, y = 0.5,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(family = "Montserrat", size = 10, color = "white"),
        bgcolor = "grey",
        bordercolor = "grey",
        borderwidth = 2,
        borderpad = 5,
        align = "center"
      )
  })
  
  output$barras_ingreso <- renderPlotly({
    df <- filtered_data_2()
    
    conteo_na <- sum(is.na(df$`Ingresos Económicos`) | df$`Ingresos Económicos` == "No informado")
    conteo_na <- paste("Hay",conteo_na,"de",nrow(df),"registros personales sin información")
    
    df <- df %>%
      group_by(`Ingreso Económico`, .add = TRUE) %>%
      summarize(conteo = n(), .groups = "drop") %>%
      complete(`Ingreso Económico`) %>%
      filter(!is.na(`Ingreso Económico`),
             `Ingreso Económico`!= "No informado") %>%
      mutate(conteo = ifelse(is.na(conteo),0,conteo))
    
    g <- ggplot(df, aes(x = conteo, y = `Ingreso Económico`)) +
      geom_bar(stat = "identity", fill = "#ec7e14", 
               aes(text = paste("Ingreso económico:", `Ingreso Económico`, "<br>Conteo:", conteo))) +
      labs(x = "Conteo", y = "Ingreso económico", title = "Conteo por ingreso económico`",
           subtitle = conteo_na) +
      scale_x_continuous(limits = c(min(df$conteo),max(df$conteo))) +
      theme_fivethirtyeight() +
      theme(legend.position = 'none')
    
    ggplotly(g, tooltip = 'text') %>%
      layout(title = list(y = 0.93, title_x=0.2,
                          text = paste0("Conteo por ingreso económico",
                                        '<br>',
                                        '<sup>',
                                        conteo_na,
                                        '</sup>'),
                          font = list(family = "Montserrat", size = 15, color = "grey1"),
                          pad = list(l=-80)),
             xaxis = list(title = list(text = "Frecuencia",
                                       font = list(family = "Montserrat", size = 12, color = "grey1")),
                          #tickvals = seq(0, max(df$conteo)+50, 50),
                          tickfont = list(family = "Montserrat", size = 10, color = "grey")),
             yaxis = list(title = list(text = "",
                                       font = list(family = "Montserrat", size = 12, color = "grey1")),
                          tickfont = list(family = "Montserrat", size = 12, color = "grey")),
             hoverlabel = list(font = list(family = "Montserrat", size = 10, color = "white",
                                           style = "italic", textcase = "word caps"))
      ) 
  })
  
  output$barras_judicial <- renderPlotly({
    df <- filtered_data_2()
    
    conteo_na <- sum(is.na(df$`Situación Judicial`) | df$`Situación Judicial` == "No informada")
    conteo_na <- paste("Hay",conteo_na,"de",nrow(df),"registros personales sin información")
    
    df <- df %>%
      group_by(`Situación Judicial`, .add = TRUE) %>%
      summarize(conteo = n(), .groups = "drop") %>%
      complete(`Situación Judicial`) %>%
      filter(!is.na(`Situación Judicial`),
             `Situación Judicial`!= "No informada") %>%
      mutate(conteo = ifelse(is.na(conteo),0,conteo))
    
    g <- ggplot(df, aes(x = conteo, y = `Situación Judicial`)) +
      geom_bar(stat = "identity", fill = "#ec7e14", 
               aes(text = paste("Situación Judicial:", `Situación Judicial`, "<br>Conteo:", conteo))) +
      labs(x = "Conteo", y = "Situación Judicial", title = "Conteo por ingreso económico`",
           subtitle = conteo_na) +
      scale_x_continuous(limits = c(0,max(df$conteo))) +
      theme_fivethirtyeight() +
      theme(legend.position = 'none')
    
    ggplotly(g, tooltip = 'text') %>%
      layout(title = list(y = 0.93, title_x=0.2,
                          text = paste0("Conteo por situación judicial",
                                        '<br>',
                                        '<sup>',
                                        conteo_na,
                                        '</sup>'),
                          font = list(family = "Montserrat", size = 15, color = "grey1"),
                          pad = list(l=-80)),
             xaxis = list(title = list(text = "Frecuencia",
                                       font = list(family = "Montserrat", size = 12, color = "grey1")),
                          #tickvals = seq(0, max(df$conteo)+50, 50),
                          tickfont = list(family = "Montserrat", size = 10, color = "grey")),
             yaxis = list(title = list(text = "",
                                       font = list(family = "Montserrat", size = 12, color = "grey1")),
                          tickfont = list(family = "Montserrat", size = 12, color = "grey")),
             hoverlabel = list(font = list(family = "Montserrat", size = 10, color = "white",
                                           style = "italic", textcase = "word caps"))
      ) 
  })
  output$barras_habitacional <- renderPlotly({
    df <- filtered_data_2()
    
    conteo_na <- sum(is.na(df$`Situación Habitacional Actual`) | df$`Situación Habitacional Actual` == "No informada")
    conteo_na <- paste("Hay",conteo_na,"de",nrow(df),"registros personales sin información")
    
    df <- df %>%
      group_by(`Situación Habitacional Actual`, .add = TRUE) %>%
      summarize(conteo = n(), .groups = "drop") %>%
      complete(`Situación Habitacional Actual`) %>%
      filter(!is.na(`Situación Habitacional Actual`),
             `Situación Habitacional Actual`!= "No informada") %>%
      mutate(conteo = ifelse(is.na(conteo),0,conteo))
    
    g <- ggplot(df, aes(x = conteo, y = `Situación Habitacional Actual`)) +
      geom_bar(stat = "identity", fill = "#ec7e14", 
               aes(text = paste("Situación Habitacional Actual:", `Situación Habitacional Actual`, "<br>Conteo:", conteo))) +
      labs(x = "Conteo", y = "Situación Habitacional Actual", title = "Conteo por ingreso económico`",
           subtitle = conteo_na) +
      scale_x_continuous(limits = c(0,max(df$conteo))) +
      theme_fivethirtyeight() +
      theme(legend.position = 'none')
    
    ggplotly(g, tooltip = 'text') %>%
      layout(title = list(y = 0.93, title_x=0.2,
                          text = paste0("Conteo por Situación Habitacional Actual",
                                        '<br>',
                                        '<sup>',
                                        conteo_na,
                                        '</sup>'),
                          font = list(family = "Montserrat", size = 15, color = "grey1"),
                          pad = list(l=-80)),
             xaxis = list(title = list(text = "Frecuencia",
                                       font = list(family = "Montserrat", size = 12, color = "grey1")),
                          #tickvals = seq(0, max(df$conteo)+50, 50),
                          tickfont = list(family = "Montserrat", size = 10, color = "grey")),
             yaxis = list(title = list(text = "",
                                       font = list(family = "Montserrat", size = 12, color = "grey1")),
                          tickfont = list(family = "Montserrat", size = 12, color = "grey")),
             hoverlabel = list(font = list(family = "Montserrat", size = 10, color = "white",
                                           style = "italic", textcase = "word caps"))
      ) 
  })
  
  output$donut.cud <- renderPlotly({
    df <- filtered_data_2()
    
    conteo_na <- sum(is.na(df$CUD) | df$CUD == "No informado")
    conteo_na <- paste(conteo_na,"de",nrow(df),"\nno informado")
    
    df <- df %>%
      group_by(CUD,.add = TRUE) %>%
      summarise(conteo = n(), .groups = "drop") %>%
      complete(CUD) %>%
      filter(!is.na(CUD),
             CUD != "No informado") %>%
      mutate(conteo = ifelse(is.na(conteo),0,conteo)) 
    
    df %>% plot_ly(labels = ~CUD, values = ~conteo,
                   marker = list(colors = c("#FBC91C", "#EC7E14", "#4C443C","#F9EDCC"),
                                 line = list(color = '#e1e1e1', width = 2))) %>% 
      add_pie(hole = 0.6) %>% 
      
      layout(title = list(y = 0.98, x = 0.55,
                          text = "Distribución de la tenencia de CUD",
                          font = list(family = "Montserrat", size = 15, color = "grey1")),
             showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             paper_bgcolor = '#e1e1e1',
             plot_bgcolor = '#e1e1e1',
             showlegend = TRUE,
             legend = list(
               title = list(text = "Tiene CUD",
                            font = list(family = "Montserrat", size = 12, color = "grey1")),
               font = list(family = "Montserrat", size = 10, color = "grey1"),
               orientation = "v",
               x = 1,
               y = 0.5,
               bordercolor = "grey10",
               borderwidth = 1,
               borderpad = 5
             )
      ) %>%
      add_annotations(
        text = conteo_na,
        x = 0.5, y = 0.5,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(family = "Montserrat", size = 10, color = "white"),
        bgcolor = "grey",
        bordercolor = "grey",
        borderwidth = 2,
        borderpad = 5,
        align = "center"
      )
  })
  
}

# Ejecutar la aplicación
shinyApp(ui, server)