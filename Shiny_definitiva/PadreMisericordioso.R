# Librerías
library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(plotly)
library(rlang)
library(readxl)
library(shinyvalidate)

# Data prep
# Importar base
base <- function(){
  data <- read_excel("Base completa.xlsx")
}

data <- base()

## Layout formulario

#ID_max <- function(base) {
#  return(last(base$ID))
#}

ui <- page_navbar(
  
  theme = bs_theme(
    bg = "#e1e1e1",
    fg = "black",
    primary = "#ec7e14",
    secondary = "#fbc91c",
    success = "#009E73",
    base_font = font_google("Montserrat")
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
  
  nav_panel(
    tags$span("Nuevo registro", style = "font-size: 14px;"),
    class = "bslib-page-dashboard",
    icon = icon("user"),
    
    fluidRow(
      
      wellPanel(
          h4(tags$span("Registro", style = "font-size: 14px;")),
          
          column(
            div(
              style = "margin-bottom: 10px;",
              tags$label(tags$span("ID de registro", style = "font-size: 12px;")),
              tags$input(
                id = "id_registro",
                type = "text",
                value = "",  # Este valor será actualizado desde el servidor
                readonly = TRUE,  # Hacer el campo no editable
                class = "form-control"
                )
              ),
            
            dateInput(
              "fecha_registro",
              tags$span(
                tagList(
                  tags$span("Fecha de registro", style = "font-size: 12px;"),
                  tags$span("*", style = "font-size: 12px;color:#ec7e14; font-weight:bold;")
                  )
                ),
              value = Sys.Date(),
              format = "dd/mm/yyyy"
              ),
            width = 2
            )
      )
    )
    ),
  
  nav_panel(
    tags$span("Consulta y modificación de registros", style = "font-size: 14px;"),
    class = "bslib-page-dashboard"
  ),
  nav_panel(
    tags$span("Tablero de visualización", style = "font-size: 14px;"),
    class = "bslib-page-dashboard"
  ),
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  )
)

server <- function(input, output, session) {
  
  # ACTUALIZACIÓN DEL ID REGISTRO EN TIEMPO REAL ---------------------------------------------------------------------
  
  # Cargar la base de datos y obtener el ID máximo
  data <- base()
  id_max <- reactiveVal(max(data$`ID de registro`, na.rm = TRUE))  # Suponiendo que la columna de IDs se llama "ID"
  
  # Actualizar el campo de "ID de registro" con el ID máximo + 1
  observe({
    updateTextInput(session, "id_registro", value = as.character(id_max() + 1))
  })
  
  # Validación del campo Fecha de registro (obligatoria)
  iv <- InputValidator$new()
  iv$add_rule("fecha_registro", sv_required(""))
  iv$enable()
}

shinyApp(ui, server)