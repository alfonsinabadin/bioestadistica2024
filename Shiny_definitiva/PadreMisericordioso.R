# Librerías
library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(plotly)
library(rlang)
library(readxl)
library(shinyjs)
library(shinyvalidate)

# Data prep
# Importar base
base <- function(){
  data <- read_excel("Base completa.xlsx")
}

data <- base()

## Layout formulario

ui <- page_navbar(
  
  useShinyjs(),
  
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
    
    # Layout con un fluidRow y columnas
    fluidRow(
      fluidRow(
        column(
          width = 12,  # Ajustar el ancho para que ocupe toda la fila
          wellPanel(  # Simular el estilo de un 'box' usando wellPanel
            
            h4("Datos del Registro", style = "font-size: 16px; font-weight: bold;"),
            
            fluidRow(
              
              # Campo ID de registro (readonly)
              column(
                width = 1,
                div(
                  tags$label(
                    tags$span("ID de registro", style = "font-size: 12px; margin-bottom: 10px; display: inline-block;")
                  ),
                  tags$input(
                    id = "id_registro",
                    type = "text",
                    value = "",  # Este valor será actualizado desde el servidor
                    readonly = TRUE,  # Hacer el campo no editable
                    class = "form-control"
                  )
                )
              ),
              
              # Campo Fecha de registro
              column(
                width = 2,  # Ajustar el ancho de cada columna para que ocupen una parte de la fila
                dateInput(
                  "fecha_registro",
                  tags$span(
                    tagList(
                      tags$span("Fecha de registro", style = "font-size: 12px;"),
                      tags$span("*", style = "font-size: 12px;color:#ec7e14; font-weight:bold;")
                    )
                  ),
                  value = Sys.Date(),
                  format = "dd/mm/yyyy"  # Corregir el formato de la fecha
                )
              )
            ),
            
            fluidRow(
              
              # Campo ID de la persona (readonly)
              column(
                width = 1,
                div(
                  tags$label(
                    tags$span("ID de la persona", style = "font-size: 12px; margin-bottom: 10px; display: inline-block;")
                  ),
                  tags$input(
                    id = "id_persona",
                    type = "text",
                    value = "",  # Este valor será actualizado dinámicamente
                    readonly = TRUE,  # Campo no editable
                    class = "form-control"
                  )
                )
              ),
              
              # Campo Fecha de primer registro
              column(
                width = 2,  # Ajustar el ancho de cada columna para que ocupen una parte de la fila
                dateInput(
                  "fecha_primer_registro",
                  tags$span(
                    tagList(
                      tags$span("Fecha del primer registro", style = "font-size: 12px;"),
                      tags$span("*", style = "font-size: 12px;color:#ec7e14; font-weight:bold;")
                    )
                  ),
                  value = Sys.Date(),
                  format = "dd/mm/yyyy"  # Corregir el formato de la fecha
                )
                ),
                
              # Campo Edad de primer registro
              column(
                width = 2,
                numericInput(
                  "edad_primer_registro",
                  tags$span(
                    tagList(
                      tags$span("Edad del primer registro", style = "font-size: 12px;"),
                      tags$span("*", style = "font-size: 12px;color:#ec7e14; font-weight:bold;")
                    )
                  ),
                  value = ""
                )
              )
            )
          )
        )
      )
      ,
      
      column(
        width = 6,  # Ajustar ancho
        wellPanel(  # Simular el estilo de un 'box' usando wellPanel
          
          style = "min-height: 320px;",  # Aplicar la misma altura mínima aquí
          
          h4("Datos de la persona", style = "font-size: 16px; font-weight: bold;"),
          fluidRow(
            column(
              width = 3,
              
              # Campo recuerda DNI
              selectInput(
                inputId = "recuerda_dni",
                tags$span("¿Recuerda el DNI?", style = "font-size: 12px;"),
                choices = c("Sí", "No", "No tiene" = "S/D"),  # Definir las opciones
                selected = "S/D"
              ),
              
              # Campo DNI
              numericInput(
                "dni",
                tags$span("DNI", style = "font-size: 12px;"),
                value = NULL
              )
            ),
            
            column(
              width = 5,
              
              # Apellido y Nombre
              div(
                textInput(
                  inputId = "apellido_nombre",
                  label = tags$span("Apellido, Nombre (Apodo)", style = "font-size: 12px;"),
                  placeholder = "Perez, Juan (Juanchi)"
                )
              ),
              
              # Fecha de nacimiento
              div(
                dateInput(
                  inputId = "fecha_nacimiento",
                  label = tags$span("Fecha de nacimiento", style = "font-size: 12px;"),
                  value = "",
                  format = "dd/mm/yyyy"  # Corregir formato de la fecha
                )
              )
            )
          )
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
  
  # Cargar la base de datoS
  data <- base()
  
  # Reglas ID registro
  
  ## Obtener el ID máximo
  id_max <- reactiveVal(max(data$`ID de registro`, na.rm = TRUE))
  
  ## Actualizar el campo con el ID máximo + 1
  observe({
    updateTextInput(session, "id_registro", value = as.character(id_max() + 1))
  })
  
  # Reglas Fecha de registro
  
  ## Obligatorio
  iv <- InputValidator$new()
  iv$add_rule("fecha_registro", sv_required("Campo obligatorio"))
  iv$enable()
  
  # Nota: como "Requiere DNI" está en un desplegable, no puede estar vacío así que evitamos esa regla 
  
  # Reglas para DNI
  
  ## Obligatorio
  iv_dni <- InputValidator$new()
  iv_dni$add_rule("dni", sv_required("Campo obligatorio"))
  iv_dni$enable()
  
  ## Manejar el campo DNI según la selección de "recuerda_dni"
  observeEvent(input$recuerda_dni, {
    # Si la opción es No o S/D, deshabilitar el campo y eliminar la validación
    if (input$recuerda_dni %in% c("No", "S/D")) {
      updateNumericInput(session, "dni", value = NULL)
      shinyjs::disable("dni")  # Deshabilitar el campo
      iv_dni$disable()  # Desactivar la regla de obligatoriedad
    } else {
      # Si selecciona Sí, habilitar el campo y agregar la validación
      shinyjs::enable("dni")  # Habilitar el campo
      iv_dni$enable()  # Activar la validación
    }
  })
  
  # Reglas para ID Persona
  observeEvent(input$dni, {
    # Si el campo "recuerda_dni" es "No" o "S/D", asignamos el nuevo ID con max() + 1
    if (input$recuerda_dni %in% c("No", "S/D")) {
      id_persona <- max(data$`ID de la persona`, na.rm = TRUE) + 1
      observeEvent(input$fecha_primer_registro, {
        observeEvent(input$fecha_nacimiento, {
          edad_primer_registro <- input$fecha_primer_registro - input$fecha_nacimiento
          updateNumericInput(session, "edad_primer_registro", value = edad_primer_registro)
        })
      })
      
       
    } else {
      req(input$dni)  # Asegurarse de que el DNI no esté vacío
      
      # Comprobar si el DNI ya existe en la base
      dni_existente <- data[which(data$DNI == input$dni), ]
      
      # Si el DNI ya existe, traer el ID de la persona de la última fila que tiene ese DNI
      if (nrow(dni_existente) > 0) {
        id_persona <- last(dni_existente$`ID de la persona`)
        apellido_nombre <- last(dni_existente$`Apellido y Nombre`)  # Obtener el último apellido y nombre para ese DNI
        updateTextInput(session, "apellido_nombre", value = apellido_nombre)  # Actualizar con el valor de la base
        fecha_nacimiento <- last(dni_existente$`Fecha de Nacimiento`)
        updateDateInput(session, "fecha_nacimiento", value = fecha_nacimiento)
        fecha_primer_registro <- first(dni_existente$`Fecha de registro`)
        updateDateInput(session, "fecha_primer_registro", value = fecha_primer_registro)
        edad_primer_registro <- first(dni_existente$`Edad del primer registro`)
        updateNumericInput(session, "edad_primer_registro", value = edad_primer_registro)
      } else {
        # Si el DNI no está en la base, asignar un nuevo ID de persona (máximo + 1)
        id_persona <- max(data$`ID de la persona`, na.rm = TRUE) + 1
        observeEvent(input$fecha_primer_registro, {
          observeEvent(input$fecha_nacimiento, {
            edad_primer_registro <- input$fecha_primer_registro - input$fecha_nacimiento
            updateNumericInput(session, "edad_primer_registro", value = edad_primer_registro)
          })
        })
      }
    }
    
    # Actualizar el campo ID de la persona
    updateTextInput(session, "id_persona", value = as.numeric(id_persona))
  })
  
  # APELLIDO Y NOMBRE OBLIGATORIO
  # Inicializamos otra validación para el DNI
  iv_apellido_nombre <- InputValidator$new()
  iv_apellido_nombre$add_rule("apellido_nombre", sv_required("Campo obligatorio"))
  iv_apellido_nombre$enable()
  
}

shinyApp(ui, server)
