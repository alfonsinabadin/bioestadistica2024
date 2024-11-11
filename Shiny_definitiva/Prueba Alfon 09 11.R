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
library(kableExtra)
library(tidyr)
library(shinyWidgets)
library(ggplot2)
library(ggthemes)

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
  
  navbarMenu(
    tags$span("Tablero de visualización", style = "font-size: 14px;"),
    icon = icon("chart-simple"),
    
    tabPanel("Análisis demográfico",
             
             fluidRow(
               # Datos e historial de registro ------------------------------------------
               column(
                 width = 2,
                 wellPanel(
                   
                   style = "min-height: 400px;",
                   
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
                         selected = c("0 a 12", "13 a 17", "18 a 29", "30 a 60"))
                       )
                     )
                   )
                 ),
                 
                 column(
                   width = 10,
                   
                   fluidRow(
                     
                     column(
                       width = 8,
                       plotlyOutput("histbox.edad")
                       ),
                     
                     column(
                       width = 4,
                       plotlyOutput("dona.edad")  # Gráfico de dona
                       )
                     
                     )
                   
                   )
               
             )
             ),
    
    tabPanel("Análisis socioeconómico")
  ),
  
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  )
)

server <- function(input, output, session) {
  
  # Cargar datos
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
      )
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
             is.null(input$edad_filter) | EdadCategorica %in% input$edad_filter)
  })
  
  # Histograma con boxplot
  output$histbox.edad <- renderPlotly({
    # Cargar base reactiva
    df <- filtered_data()
    
    # Conteo de registros sin información de edad
    conteo_na <- paste("Hay", sum(is.na(df$`Edad del registro`)), "registros personales\nsin información")
    
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
                          font = list(family = "Montserrat", size = 20, color = "grey1")),
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
                          font = list(family = "Montserrat", size = 20, color = "grey1")),
             xaxis = list(title = list(text = "Edad de registro",
                                       font = list(family = "Montserrat", size = 15, color = "grey1")),
                          tickvals = seq(0, 60, 10),
                          tickfont = list(family = "Montserrat", size = 10, color = "grey"),
                          hoverformat = ".2f"),
             yaxis = list(showticklabels = FALSE),
             hoverlabel = list(font = list(family = "Montserrat", size = 10, color = "white",
                                           style = "italic")))
    
    # Combinar histogram y boxplot
    subplot(box_plotly, hist_plotly, nrows = 2, heights = c(0.2, 0.8), 
            shareX = TRUE, titleX = TRUE, titleY = TRUE, margin = 0)
  })
  
  # Gráfico de dona para distribución de edad
  output$dona.edad <- renderPlotly({
    df <- filtered_data() %>%
      group_by(EdadCategorica) %>%
      summarise(conteo = n()) %>%
      filter(!is.na(EdadCategorica))
    
    plot_ly(
      df,
      type = 'pie',
      labels = ~EdadCategorica,
      values = ~conteo,
      textinfo = 'none',
      hole = 0.5,  # Dona
      marker = list(colors = c("#FBC91C", "#EC7E14", "#4C443C","#F9EDCC")),
      showlegend = TRUE,
      source = "edad_dona_plot"
    ) %>%
      layout(
        paper_bgcolor = '#f0f0f0',
        plot_bgcolor = '#f0f0f0',
        showlegend = TRUE,
        legend = list(
          title = list(text = "Categorías de Edad",
                       font = list(family = "Montserrat", size = 12, color = "grey1")),
          orientation = "v",
          x = 1.2,
          y = 0.5,
          bordercolor = "grey10",
          borderwidth = 1
        ),
        title = list(y = 0.95,
                     text = "Según categorías del SEDRONAR",
                     font = list(family = "Montserrat", size = 20, color = "grey1")),
        hoverlabel = list(font = list(family = "Montserrat", size = 10, color = "white",
                                      style = "italic", textcase = "word caps"))
      )
  })
  
}

# Ejecutar la aplicación
shinyApp(ui, server)