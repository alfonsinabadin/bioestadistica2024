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
  "))
  )
  ,
  
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
      column(
        width = 2,  # Ajustar el ancho para que ocupe toda la fila
        wellPanel(  # Simular el estilo de un 'box' usando wellPanel
          
          h4("Datos del Registro", style = "font-size: 16px; font-weight: bold;"), 
          
          # Campo ID de registro (readonly)
          fluidRow(
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
          fluidRow(
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
          ),
          
          h4("Historial de Registro", style = "font-size: 16px; font-weight: bold;"), 
          
          # Campo ID de persona (readonly)
          div(
            tags$label(
              tags$span("ID de la persona", style = "font-size: 12px; margin-bottom: 10px; display: inline-block;")
            ),
            tags$input(
              id = "id_persona",
              type = "text",
              value = "",  # Este valor será actualizado desde el servidor
              readonly = TRUE,  # Hacer el campo no editable
              class = "form-control"
            )
          ),
          
          # Campo Fecha del primer registro
          dateInput(
            "fecha_primer_registro",
            tags$span(
              tagList(
                tags$span("Fecha del primer registro", style = "font-size: 12px;")
              )
            ),
            value = Sys.Date(),
            format = "dd/mm/yyyy"  # Corregir el formato de la fecha
          )
        )
      ),
      
      column(
        width = 10,  # Ajustar ancho
        wellPanel(  # Simular el estilo de un 'box' usando wellPanel
          
          style = "min-height: 320px;",  # Aplicar la misma altura mínima aquí
          
          h4("Datos de la persona", style = "font-size: 16px; font-weight: bold;"),
          
          fluidRow(
            
            # Campo recuerda DNI
            column(
              width = 2,
              selectInput(
                inputId = "recuerda_dni",
                tags$span("¿Recuerda el DNI?", style = "font-size: 12px;"),
                choices = c("Sí", "No", "No tiene" = "S/D"),  # Definir las opciones
                selected = "S/D"
              )
            ),
            
            # Campo DNI
            column(
              width = 2,
              numericInput(
                "dni",
                tags$span("DNI", style = "font-size: 12px;"),
                value = NULL
              )
            ),
            
            # Apellido y Nombre
            column(
              width = 3,
              div(
                textInput(
                  inputId = "apellido_nombre",
                  label = tags$span("Apellido, Nombre (Apodo)", style = "font-size: 12px;"),
                  placeholder = "Ejemplo: Perez, Juan (Juanchi)"
                )
              )
            ),
            
            # Fecha de nacimiento
            column(
              width = 2,
              div(
                dateInput(
                  inputId = "fecha_nacimiento",
                  label = tags$span("Fecha de nacimiento", style = "font-size: 12px;"),
                  value = Sys.Date(),
                  format = "dd/mm/yyyy"  # Corregir formato de la fecha
                )
              )
            )
          ),
          
          fluidRow(
            
            # Campo sexo biológico
            column(
              width = 2,
              selectInput(
                "sexo_biologico",
                label = tags$span("Sexo biológico", style = "font-size: 12px;"),
                choices = c("Femenino", "Masculino", "No contesta"),  # Definir las opciones
                selected = "No contesta"
              )
            ),
            
            # Campo género
            column(
              width = 2,
              selectInput(
                "genero",
                label = tags$span("Género", style = "font-size: 12px;"),
                choices = c("Hombre", "Hombre trans", 
                            "Mujer", "Mujer trans",
                            "No binario", "Otro"),  # Definir las opciones
              )
            ),
            
          )
        )
      ),
      
      column(
        width = 10,  # Ajustar ancho
        wellPanel(  # Simular el estilo de un 'box' usando wellPanel
          
          style = "min-height: 320px;",  # Aplicar la misma altura mínima aquí
          
          h4("Datos de contacto de referencia", style = "font-size: 16px; font-weight: bold;"),
          
          # CONTACTO 1
          
          fluidRow(
            
            # Campo telefono de contacto 1
            column(
            width = 2,
            numericInput(
              "telefono_contacto_1",  # inputId debe coincidir con el servidor
              label = tags$span("Teléfono de contacto 1", style = "font-size: 12px;"),
              value = NULL
            ),
            textOutput("error_msg"),  # Mensaje de error
          
          # Campo tipo de vinculo
            textInput(
              "tipo_vinculo_contacto_1",  # inputId para el campo
              label = tags$span("Tipo de Vínculo con el Contacto 1", style = "font-size: 12px;"),
              value = ""
            ),
            textOutput("error_vinculo"),  # Mensaje de error para la validación,
          
          # Campo nombre del contacto 1
          textInput(
            "nombre_contacto_1",  # inputId para el campo
            label = tags$span("Nombre del Contacto 1", style = "font-size: 12px;"),
            value = ""
          ),
          textOutput("error_nombre")  # Mensaje de error para la validación
          ),
          
          # CONTACTO 2
            
            # Campo telefono de contacto 2
            column(
              width = 2,
              numericInput(
                "telefono_contacto_2",  # inputId debe coincidir con el servidor
                label = tags$span("Teléfono de contacto 2", style = "font-size: 12px;"),
                value = NULL
              ),
              textOutput("error_msg"),  # Mensaje de error
            
            
            # Campo tipo de vínculo 2
              textInput(
                "tipo_vinculo_contacto_2",  # inputId para el campo
                label = tags$span("Tipo de Vínculo con el Contacto 2", style = "font-size: 12px;"),
                value = ""
              ),
              textOutput("error_vinculo_2"),  # Mensaje de error para la validación
            
            # Campo nombre del contacto 2
              textInput(
                "nombre_contacto_2",  # inputId para el campo
                label = tags$span("Nombre del Contacto 2", style = "font-size: 12px;"),
                value = ""
              ),
              textOutput("error_nombre_2")  # Mensaje de error para la validación
            ),
          
          # CONTACTO 3
            
            # Campo telefono de contacto 3
            column(
              width = 2,
              numericInput(
                "telefono_contacto_3",  # inputId debe coincidir con el servidor
                label = tags$span("Teléfono de contacto 3", style = "font-size: 12px;"),
                value = NULL
              ),
              textOutput("error_msg"),  # Mensaje de error

            # Campo tipo de vínculo 3
              textInput(
                "tipo_vinculo_contacto_3",  # inputId para el campo
                label = tags$span("Tipo de Vínculo con el Contacto 3", style = "font-size: 12px;"),
                value = ""
              ),
              textOutput("error_vinculo_3"),  # Mensaje de error para la validación
            
            # Campo nombre del contacto 3
              textInput(
                "nombre_contacto_3",  # inputId para el campo
                label = tags$span("Nombre del Contacto 3", style = "font-size: 12px;"),
                value = ""
              ),
              textOutput("error_nombre_3")  # Mensaje de error para la validación
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
  
  # Cargar la base de datos
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
      updateNumericInput(session, "dni", value = "")
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

  # DATOS DE REFERENCIA
  
  # Reglas para el contacto 1
  
  # Telefono
  iv_telefono_1 <- InputValidator$new()
  iv_telefono_1$add_rule("telefono_contacto_1", sv_required("Campo obligatorio."))
  iv_telefono_1$add_rule("telefono_contacto_1", function(value) {
    if (nchar(as.character(value)) < 7) {
      return("El teléfono debe tener al menos 7 dígitos.")
    } else if (nchar(as.character(value)) > 10) {
      return("El teléfono no puede tener más de 10 dígitos.")
    }
    return(NULL)  # Sin errores
  })
  iv_telefono_1$enable()
  
  # Vinculo
  iv_vinculo_1 <- InputValidator$new()
  iv_vinculo_1$add_rule("tipo_vinculo_contacto_1", sv_required("Campo obligatorio."))
  iv_vinculo_1$add_rule("tipo_vinculo_contacto_1", function(value) {
    if (nchar(value) < 2) {
      return("El campo debe tener al menos 2 caracteres.")
    }
    if (!grepl("^[a-zA-Z ]+$", value)) {
      return("El campo solo puede contener letras y espacios.")
    }
    return(NULL)
  })
  iv_vinculo_1$enable()
  
  # Nombre
  iv_nombre_1 <- InputValidator$new()
  iv_nombre_1$add_rule("nombre_contacto_1", function(value) {
    if (is.null(value) || nchar(value) == 0) {
      return(NULL)  # No hay error si está vacío
    }
    if (nchar(value) < 2) {
      return("El campo debe tener al menos 2 caracteres.")
    }
    if (!grepl("^[a-zA-Z ]+$", value)) {
      return("El campo solo puede contener letras y espacios.")
    }
    if (grepl("[0-9]", value)) {
      return("El campo no puede contener números.")
    }
    return(NULL)  # No hay error si todo es válido
  })
  iv_nombre_1$enable()
  
  # Reglas para el contacto 2
  
  # Teléfono
  iv_telefono_2 <- InputValidator$new()
  iv_telefono_2$add_rule("telefono_contacto_2", function(value) {
    # Si el valor está vacío o es NULL, no se realiza ninguna validación
    if (is.null(value) || (is.character(value) && nchar(trimws(value)) == 0)) {
      return(NULL)  # Sin errores
    }
    
    # Verifica si el valor es NA
    if (is.na(value)) {
      return(NULL)  # También se considera sin errores si es NA
    }
    
    # Si hay un valor, validamos la longitud
    if (nchar(as.character(value)) < 7) {
      return("El teléfono debe tener al menos 7 dígitos.")
    } else if (nchar(as.character(value)) > 10) {
      return("El teléfono no puede tener más de 10 dígitos.")
    }
    
    return(NULL)  # Sin errores
  })
  
  iv_telefono_2$enable()
  
  # Vínculo
  iv_vinculo_2 <- InputValidator$new()
  iv_vinculo_2$add_rule("tipo_vinculo_contacto_2", function(value) {
    if (!is.null(value) && nchar(value) > 0) {  # Solo validar si hay un valor
      if (nchar(value) < 2) {
        return("El campo debe tener al menos 2 caracteres.")
      }
      if (!grepl("^[a-zA-Z ]+$", value)) {
        return("El campo solo puede contener letras y espacios.")
      }
    }
    return(NULL)  # Sin errores
  })
  iv_vinculo_2$enable()
  
  # Nombre
  iv_nombre_2 <- InputValidator$new()
  iv_nombre_2$add_rule("nombre_contacto_2", function(value) {
    if (is.null(value) || nchar(value) == 0) {
      return(NULL)  # No hay error si está vacío
    }
    if (nchar(value) < 2) {
      return("El campo debe tener al menos 2 caracteres.")
    }
    if (!grepl("^[a-zA-Z ]+$", value)) {
      return("El campo solo puede contener letras y espacios.")
    }
    if (grepl("[0-9]", value)) {
      return("El campo no puede contener números.")
    }
    return(NULL)  # No hay error si todo es válido
  })
  iv_nombre_2$enable()
  
  # Reglas para el contacto 3
  
  # Teléfono
  iv_telefono_3 <- InputValidator$new()
  iv_telefono_3$add_rule("telefono_contacto_3", function(value) {
    # Si el valor está vacío o es NULL, no se realiza ninguna validación
    if (is.null(value) || (is.character(value) && nchar(trimws(value)) == 0)) {
      return(NULL)  # Sin errores
    }
    
    # Verifica si el valor es NA
    if (is.na(value)) {
      return(NULL)  # También se considera sin errores si es NA
    }
    
    # Si hay un valor, validamos la longitud
    if (nchar(as.character(value)) < 7) {
      return("El teléfono debe tener al menos 7 dígitos.")
    } else if (nchar(as.character(value)) > 10) {
      return("El teléfono no puede tener más de 10 dígitos.")
    }
    
    return(NULL)  # Sin errores
  })
  
  iv_telefono_3$enable()
  
  # Vínculo
  iv_vinculo_3 <- InputValidator$new()
  iv_vinculo_3$add_rule("tipo_vinculo_contacto_3", function(value) {
    if (!is.null(value) && nchar(value) > 0) {  # Solo validar si hay un valor
      if (nchar(value) < 2) {
        return("El campo debe tener al menos 2 caracteres.")
      }
      if (!grepl("^[a-zA-Z ]+$", value)) {
        return("El campo solo puede contener letras y espacios.")
      }
    }
    return(NULL)  # Sin errores
  })
  iv_vinculo_3$enable()
  
  # Nombre
  iv_nombre_3 <- InputValidator$new()
  iv_nombre_3$add_rule("nombre_contacto_3", function(value) {
    if (is.null(value) || nchar(value) == 0) {
      return(NULL)  # No hay error si está vacío
    }
    if (nchar(value) < 2) {
      return("El campo debe tener al menos 2 caracteres.")
    }
    if (!grepl("^[a-zA-Z ]+$", value)) {
      return("El campo solo puede contener letras y espacios.")
    }
    if (grepl("[0-9]", value)) {
      return("El campo no puede contener números.")
    }
    return(NULL)  # No hay error si todo es válido
  })
  iv_nombre_3$enable()
  
}

shinyApp(ui, server)