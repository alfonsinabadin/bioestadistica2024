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

# Importar base
base <- function(){
  data <- read_excel("Base completa.xlsx")
}
data <- base()

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
    icon = icon("user"),
    
    fluidRow(
      # Datos e historial de registro ------------------------------------------
      column(
        width = 2,
        wellPanel(
          
          h4("Datos del Registro", style = "font-size: 16px; font-weight: bold;"), 
          
          # Campo ID de registro (readonly)
          fluidRow(
            div(
              style = "margin-bottom: 8px;", 
              tags$label(tags$span("ID de registro", style = "font-size: 12px; margin-bottom: 10px; display: inline-block;")),
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
            div(
              style = "width: 100%;margin-bottom: 8px;",
              dateInput(
                "fecha_registro",
                tags$span(
                  tagList(
                    tags$span("Fecha de registro", style = "font-size: 12px;"),
                    tags$span("*", style = "font-size: 12px;color:#ec7e14; font-weight:bold;")
                  )
                ),
                value = Sys.Date(),
                format = "dd/mm/yyyy",
                min = Sys.Date() - years(1),
                max = Sys.Date()
              )
            )
          ),
          
          h4("Historial de Registro", style = "font-size: 16px; font-weight: bold;"), 
          
          # Campo ID de persona (readonly)
          div(
            style = "width: 100%;margin-bottom: 8px;", 
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
          div(
            style = "width: 100%;margin-bottom: 20px;",
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
          )
        )
      ),
      
      # Datos personales -------------------------------------------------------
      column(
        width = 6,
        wellPanel(
          
          style = "min-height: 320px;",  # Aplicar la misma altura mínima aquí
          
          h4("Datos de la persona", style = "font-size: 16px; font-weight: bold;"),
          
          fluidRow(
            
            # Campo recuerda DNI
            column(
              width = 3,
              selectInput(
                inputId = "recuerda_dni",
                tags$span("¿Recuerda el DNI?", style = "font-size: 12px;"),
                choices = c("Sí", "No", "No tiene" = "S/D"),  # Definir las opciones
                selected = "S/D"
              )
            ),
            
            # Campo DNI
            column(
              width = 3,
              numericInput(
                "dni",
                tags$span("DNI", style = "font-size: 12px;"),
                value = NULL
              ),
              # Mensaje sobre el DNI
              div(
                textOutput("dni_message"),
                style = "color: green; font-size: 12px; margin-top: 5px;"
              )
            ),
            
            # Apellido y Nombre
            column(
              width = 6,
              div(
                textInput(
                  inputId = "apellido_nombre",
                  label = tags$span("Apellido, Nombre (Apodo)", style = "font-size: 12px;"),
                  placeholder = "Ejemplo: Perez, Juan (Juanchi)"
                )
              )
            )
          ),
          
          fluidRow(
            
            # Campo sexo biológico
            column(
              width = 4,
              selectInput(
                "sexo_biologico",
                label = tags$span("Sexo biológico", style = "font-size: 12px;"),
                choices = c("Femenino", "Masculino", "No contesta"),  # Definir las opciones
                selected = "No contesta"
              )
            ),
            
            # Campo género
            column(
              width = 4,
              selectInput(
                "genero",
                label = tags$span("Género", style = "font-size: 12px;"),
                choices = c("Hombre", "Hombre trans", 
                            "Mujer", "Mujer trans",
                            "No binario", "Otro"),  # Definir las opciones
              )
            ),
            
            # Fecha de nacimiento
            column(
              width = 4,
              div(
                dateInput(
                  inputId = "fecha_nacimiento",
                  label = tags$span("Fecha de nacimiento", style = "font-size: 12px;"),
                  value = Sys.Date(),
                  format = "dd/mm/yyyy",  # Corregir formato de la fecha
                  min = Sys.Date() - years(100),  # Limitar a 110 años atrás
                  max = Sys.Date()  # Limitar a la fecha de hoy
                )
              )
            )
          ),
          
          fluidRow(
            
            # Campo Provincia
            column(
              width = 4,
              div(
                #style = "height: 38px;",  # Aplicar estilo de altura
                selectInput(
                  "provincia",
                  label = tags$span("Provincia de residencia", style = "font-size: 12px;"),
                  choices = provincias,
                  selected = "Santa Fe"
                )
              )
            ),
            
            # Campo localidad
            column(
              width = 4,
              div(
                #style = "height: 38px;",  # Aplicar estilo de altura
                uiOutput("localidad_ui")
              )
            ),
            
            # Campo barrio
            column(
              width = 4,
              div(
                #style = "height: 38px;",  # Aplicar estilo de altura
                textInput(
                  "barrio",
                  label = tags$span("Barrio", style = "font-size: 12px;")
                )
              )
            )
          )
        )
      ),

      # Datos de contacto -------------------------------------------------------
            
      column(
        width = 4,  # Ajustar ancho
        wellPanel(  # Simular el estilo de un 'box' usando wellPanel
          
          style = "min-height: 320px;",  # Aplicar la misma altura mínima aquí
          
          h4("Contacto 1", style = "font-size: 16px; font-weight: bold;"),
          
          fluidRow(
            
            # Campo teléfono de contacto 1
            column(
              width = 4,
              textInput(
                "telefono_contacto_1",  # inputId debe coincidir con el servidor
                label = tags$span("Teléfono", style = "font-size: 12px;"),
                value = ""
              )
            ),
            
            # Campo tipo de vinculo 1
            column(
              width = 4,
              textInput(
                "tipo_vinculo_contacto_1",  # inputId para el campo
                label = tags$span("Tipo de vínculo", style = "font-size: 12px;"),
                value = ""
              )
            ),
            
            # Campo nombre del contacto 1
            column(
              width = 4,
              textInput(
                "nombre_contacto_1",  # inputId para el campo
                label = tags$span("Nombre", style = "font-size: 12px;"),
                value = ""
              )
            )
          ),
          
          h4("Contacto 2", style = "font-size: 16px; font-weight: bold;"),
          
          fluidRow(
            
            # Campo telefono de contacto 2
            column(
              width = 4,
              textInput(
                "telefono_contacto_2",  # inputId debe coincidir con el servidor
                label = tags$span("Teléfono", style = "font-size: 12px;"),
                value = ""
              )
            ),
            
            # Campo tipo de vínculo 2
            column(
              width = 4,
              textInput(
                "tipo_vinculo_contacto_2",  # inputId para el campo
                label = tags$span("Tipo de vínculo", style = "font-size: 12px;"),
                value = ""
              )
            ),
            
            # Campo nombre del contacto 2
            column(
              width = 4,
              textInput(
                "nombre_contacto_2",  # inputId para el campo
                label = tags$span("Nombre", style = "font-size: 12px;"),
                value = ""
              )
            )
          ),
          
          h4("Contacto 3", style = "font-size: 16px; font-weight: bold;"),
          
          fluidRow(
            
            # Campo telefono de contacto 3
            column(
              width = 4,
              textInput(
                "telefono_contacto_3",  # inputId debe coincidir con el servidor
                label = tags$span("Teléfono", style = "font-size: 12px;"),
                value = ""
              )
            ),
            
            # Campo tipo de vínculo 3
            column(
              width = 4,
              textInput(
                "tipo_vinculo_contacto_3",  # inputId para el campo
                label = tags$span("Tipo de vínculo", style = "font-size: 12px;"),
                value = ""
              )
            ),
            
            # Campo nombre del contacto 3
            column(
              width = 4,
              textInput(
                "nombre_contacto_3",  # inputId para el campo
                label = tags$span("Nombre", style = "font-size: 12px;"),
                value = ""
              )
            )
          )
        )
      ),
      
      # Entrevistas -------------------------------------------------------
      
      column(
        width = 4,  # Ajustar ancho
        wellPanel(  # Simular el estilo de un 'box' usando wellPanel
          
          style = "min-height: 320px;",  # Aplicar la misma altura mínima aquí
          
          h4("Entrevista con Psicólogo", style = "font-size: 16px; font-weight: bold;"),
          
          fluidRow(
            
            # Campo estado - entrevista Psicologo
            column(
              width = 4,
              selectInput(
              inputId = "estado_psicologo",
              tags$span("Estado", style = "font-size: 12px;"),
                          choices = list(
                            "Presente" = "Presente",
                            "Ausente" = "Ausente",
                            "Pendiente" = "Pendiente",
                            "No necesaria" = "No necesaria",
                            "No asignada" = "No asignada"
                          ),
                          selected = NULL)
              ),  # Ninguna opción seleccionada por defecto

            # Campo fecha - entrevista Psicolgo
            column(
              width = 4,
              dateInput(
                "fecha_entrevista_psicologo",
                tags$span(
                  tagList(
                    tags$span("Fecha de la entrevista", style = "font-size: 12px;"),
                  )
                ),
                value = Sys.Date(),
                format = "dd/mm/yyyy"  # Corregir el formato de la fecha
              )
            )
          ),
          
          h4("Entrevista con Psiquiatra", style = "font-size: 16px; font-weight: bold;"),
          
          fluidRow(
            
            # Campo estado - entrevista psiquiatra
            column(
              width = 4,
              selectInput(
                inputId = "estado_psiquiatra",
                tags$span("Estado", style = "font-size: 12px;"),
                choices = list(
                  "Presente" = "Presente",
                  "Ausente" = "Ausente",
                  "Pendiente" = "Pendiente",
                  "No necesaria" = "No necesaria",
                  "No asignada" = "No asignada"
                ),
                selected = NULL)
            ),  # Ninguna opción seleccionada por defecto
            
            # Campo fecha - entrevista Psicolgo
            column(
              width = 4,
              dateInput(
                "fecha_entrevista_psiquiatra",
                tags$span(
                  tagList(
                    tags$span("Fecha de la entrevista", style = "font-size: 12px;"),
                  )
                ),
                value = Sys.Date(),
                format = "dd/mm/yyyy"  # Corregir el formato de la fecha
              )
        )
      ),
          
          h4("Entrevista con Trabajador Social", style = "font-size: 16px; font-weight: bold;"),
          
          fluidRow(
            
            # Campo estado - entrevista trabajador social
            column(
              width = 4,
              selectInput(
                inputId = "estado_ts",
                tags$span("Estado", style = "font-size: 12px;"),
                choices = list(
                  "Presente" = "Presente",
                  "Ausente" = "Ausente",
                  "Pendiente" = "Pendiente",
                  "No necesaria" = "No necesaria",
                  "No asignada" = "No asignada"
                ),
                selected = NULL)
            ),  # Ninguna opción seleccionada por defecto
            
            # Campo fecha - entrevista trabajador social
            column(
              width = 4,
              dateInput(
                "fecha_entrevista_ts",
                tags$span(
                  tagList(
                    tags$span("Fecha de la entrevista", style = "font-size: 12px;"),
                  )
                ),
                value = Sys.Date(),
                format = "dd/mm/yyyy"  # Corregir el formato de la fecha
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
  iv_fecha_registro <- InputValidator$new()
  iv_fecha_registro$add_rule("fecha_registro", sv_required("Campo obligatorio"))
  iv_fecha_registro$enable()
  
  # Reglas Fecha primer registro
  
  ## Obligatorio
  iv_fecha_primer_registro <- InputValidator$new()
  iv_fecha_primer_registro$add_rule("fecha_primer_registro", sv_required("Campo obligatorio"))
  iv_fecha_primer_registro$enable()
  
  # Nota: como "Requiere DNI" está en un desplegable, no puede estar vacío así que evitamos esa regla 
  
  # Reglas para DNI
  
  ## Obligatorio
  iv_dni <- InputValidator$new()
  iv_dni$add_rule("dni", sv_required("Campo obligatorio. Sin puntos ni espacios."))
  iv_dni$add_rule("dni", function(value) {
    if (nchar(as.character(value)) != 8) {
      return("El DNI no puede contener caracteres especiales y debe tener 8 dígitos.")
    }
  })
  iv_dni$enable()
  
  # APELLIDO Y NOMBRE 
  # Inicializamos otra validación para el DNI
  iv_apellido_nombre <- InputValidator$new()
  iv_apellido_nombre$add_rule("apellido_nombre", sv_required("Campo obligatorio"))
  iv_apellido_nombre$add_rule("apellido_nombre", function(value) {
    if(grepl("[^a-zA-ZáéíóúÁÉÍÓÚñÑ,() ]", value)) {
      return("No se admiten caracteres especiales.")
    }
  })
  iv_apellido_nombre$add_rule("apellido_nombre", function(value) {
    if(nchar(as.character(value)) <= 4) {
      return("El campo debe tener más de 4 caracteres.")
    }
  })
  iv_apellido_nombre$enable()
  
  # Barrio 
  # Inicializamos otra validación para el Barrio
  iv_barrio <- InputValidator$new()
  iv_barrio$add_rule("barrio", function(value) {
    if(grepl("[^a-zA-ZáéíóúÁÉÍÓÚñÑ,() ]", value)) {
      return("No se admiten caracteres especiales.")
    }
  })
  iv_barrio$add_rule("barrio", function(value) {
    if(nchar(as.character(value)) <= 2 & nchar(as.character(value)) >0) {
      return("El campo debe tener más de 2 caracteres.")
    }
    if(nchar(as.character(value)) > 100) {
      return("El campo debe tener menos de 100 caracteres.")
    }
  })
  iv_barrio$enable()
  
  # Reglas para Fecha de nacimiento
  iv_fecha_nacimiento <- InputValidator$new()
  iv_fecha_nacimiento$add_rule("fecha_nacimiento", sv_required("Campo Obligatorio"))
  iv_fecha_nacimiento$enable()
  
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
        iv_dni$disable()
        
        id_persona <- last(dni_existente$`ID de la persona`)
        
        fecha_primer_registro <- first(dni_existente$`Fecha de registro`)
        updateDateInput(session, "fecha_primer_registro", value = fecha_primer_registro)
        
        apellido_nombre <- last(dni_existente$`Apellido y Nombre`)  # Obtener el último apellido y nombre para ese DNI
        updateTextInput(session, "apellido_nombre", value = apellido_nombre)  # Actualizar con el valor de la base
        
        fecha_nacimiento <- last(dni_existente$`Fecha de Nacimiento`)
        updateDateInput(session, "fecha_nacimiento", value = fecha_nacimiento)
        
        sexo_biologico <- last(dni_existente$`Sexo biológico`)
        updateSelectInput(session, "sexo_biologico", selected = sexo_biologico)
        
        genero <- last(dni_existente$Género)
        updateSelectInput(session, "genero", selected = genero)
        
        provincia <- last(dni_existente$`Provincia`)
        localidad <- last(dni_existente$`Localidad`)
        
        # Si la provincia no está en las opciones del selectInput, la añadimos temporalmente
        if (!is.na(provincia) && !(provincia %in% provincias)) {
          provincias <- c(provincias, provincia)
        }
        
        # Actualizamos el campo de provincias con la provincia del DNI
        updateSelectInput(session, "provincia", choices = provincias, selected = provincia)
        
        # Actualizamos las localidades dependiendo de la provincia seleccionada
        localidades <- localidades_por_provincia[[provincia]]
        
        # Si la localidad no está en las opciones, la añadimos temporalmente
        if (!is.na(localidad) && !(localidad %in% localidades)) {
          localidades <- c(localidades, localidad)
        }
        
        # Actualizamos el campo de localidades
        updateSelectInput(session, "localidad", choices = localidades, selected = localidad)
        
        # Mostrar mensaje en verde
        output$dni_message <- renderText({
          "El DNI ya figura en la base, los campos han sido completados"
        })
        
        telefono_contacto_1 <- last(dni_existente$`Teléfono de Contacto 1`)
        updateSelectInput(session, "telefono_contacto_1", selected = telefono_contacto_1)
        
        telefono_contacto_2 <- last(dni_existente$`Teléfono de Contacto 2`)
        updateSelectInput(session, "telefono_contacto_2", selected = telefono_contacto_2)
        
        telefono_contacto_3 <- last(dni_existente$`Teléfono de Contacto 3`)
        updateSelectInput(session, "telefono_contacto_3", selected = telefono_contacto_3)
        
        tipo_vinculo_contacto_1 <- last(dni_existente$`Tipo de Vínculo con el Contacto 1`)
        updateSelectInput(session, "tipo_vinculo_contacto_1", selected = tipo_vinculo_contacto_1)
        
        tipo_vinculo_contacto_2 <- last(dni_existente$`Tipo de Vínculo con el Contacto 2`)
        updateSelectInput(session, "tipo_vinculo_contacto_2", selected = tipo_vinculo_contacto_2)
        
        tipo_vinculo_contacto_3 <- last(dni_existente$`Tipo de Vínculo con el Contacto 3`)
        updateSelectInput(session, "tipo_vinculo_contacto_3", selected = tipo_vinculo_contacto_3)
        
        nombre_contacto_1 <- last(dni_existente$`Nombre del Contacto 1`)
        updateSelectInput(session, "nombre_contacto_1", selected = nombre_contacto_1)
        
        nombre_contacto_2 <- last(dni_existente$`Nombre del Contacto 2`)
        updateSelectInput(session, "nombre_contacto_2", selected = nombre_contacto_2)
        
        nombre_contacto_3 <- last(dni_existente$`Nombre del Contacto 3`)
        updateSelectInput(session, "nombre_contacto_3", selected = nombre_contacto_3)
        
      } else {
        
        iv_dni$enable()
        # Si el DNI no está en la base, asignar un nuevo ID de persona (máximo + 1)
        id_persona <- max(data$`ID de la persona`, na.rm = TRUE) + 1
        
        # Eliminar el mensaje si no se encuentra el DNI
        output$dni_message <- renderText({ "" })
        
        # Restablecer los campos a sus valores iniciales si el DNI no está en la base
        updateTextInput(session, "apellido_nombre", value = "")
        updateDateInput(session, "fecha_nacimiento", value = Sys.Date())  # O puedes usar `NULL` si deseas que el campo quede vacío
        updateSelectInput(session, "sexo_biologico", selected = "Masculino")  # Seleccionar una opción predeterminada
        updateSelectInput(session, "genero", selected = "Hombre")  # Seleccionar una opción predeterminada
        updateSelectInput(session, "provincia", selected = NULL)  # Dejar la provincia en blanco
        updateSelectInput(session, "localidad", choices = NULL, selected = NULL)  # Dejar la localidad en blanco
        updateTextInput(session, "barrio", value = "")
        updateTextInput(session, "telefono_contacto_1", value = "")
        updateTextInput(session, "telefono_contacto_2", value = "")
        updateTextInput(session, "telefono_contacto_3", value = "")
        updateTextInput(session, "tipo_vinculo_contacto_1", value = "")
        updateTextInput(session, "tipo_vinculo_contacto_2", value = "")
        updateTextInput(session, "tipo_vinculo_contacto_3", value = "")
        updateTextInput(session, "nombre_contacto_1", value = "")
        updateTextInput(session, "nombre_contacto_2", value = "")
        updateTextInput(session, "nombre_contacto_3", value = "")
      }
    }
    
    # Actualizar el campo ID de la persona
    updateTextInput(session, "id_persona", value = as.numeric(id_persona))
  })
  
  # Crear el uiOutput para las localidades
  output$localidad_ui <- renderUI({
    selectInput("localidad",
                label = tags$span("Localidad de residencia", style = "font-size: 12px;"), 
                choices = "Rosario")
  })
  
  # Actualizar las localidades en función de la provincia seleccionada
  observeEvent(input$provincia, {
    localidades <- sort(localidades_por_provincia[[input$provincia]])
    updateSelectInput(session, "localidad", choices = c(localidades,NULL), selected = NULL)
  })
  
  # Reglas datos de contacto 
  
  # Validación para asegurar que solo se ingresen números en el campo de teléfono
  iv_telefono_1 <- InputValidator$new()
  iv_telefono_1$add_rule("telefono_contacto_1", sv_required("Campo obligatorio."))
  iv_telefono_1$add_rule("telefono_contacto_1", function(value) {
    if (nchar(value) < 7) {
      return("El teléfono debe tener al menos 7 dígitos.")
    }
    if (nchar(value) > 10) {
      return("El teléfono no puede tener más de 10 dígitos.")
    }
    if (!grepl("^[0-9]+$", value)) {
      return("Solo se admiten números.")
    }
    return(NULL)
  })
  iv_telefono_1$enable()
  
  # Repetir para los otros campos de teléfono
  iv_telefono_2 <- InputValidator$new()
  iv_telefono_2$add_rule("telefono_contacto_2", function(value) {
    if (value != "") {
      if (nchar(value) < 7) {
        return("El teléfono debe tener al menos 7 dígitos.")
      }
      if (nchar(value) > 10) {
        return("El teléfono no puede tener más de 10 dígitos.")
      }
      if (!grepl("^[0-9]+$", value)) {
        return("Solo se admiten números.")
      }
    }
    return(NULL)
  })
  iv_telefono_2$enable()
  
  iv_telefono_3 <- InputValidator$new()
  iv_telefono_3$add_rule("telefono_contacto_3", function(value) {
    if (value != "") {
      if (nchar(value) < 7) {
        return("El teléfono debe tener al menos 7 dígitos.")
      }
      if (nchar(value) > 10) {
        return("El teléfono no puede tener más de 10 dígitos.")
      }
      if (!grepl("^[0-9]+$", value)) {
        return("Solo se admiten números.")
      }
    }
    return(NULL)
  })
  iv_telefono_3$enable()
  
  # Vinculo
  iv_vinculo_1 <- InputValidator$new()
  iv_vinculo_1$add_rule("tipo_vinculo_contacto_1", sv_required("Campo obligatorio."))
  iv_vinculo_1$add_rule("tipo_vinculo_contacto_1", function(value) {
    if (value != "") {
      if (nchar(value) < 2) {
        return("El campo debe tener al menos 2 caracteres.")
      }
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {  # Esta expresión regular es correcta
        return("El campo solo puede contener letras y espacios.")
      }
    }
    return(NULL)
  })
  iv_vinculo_1$enable()
  
  # Repitiendo para los otros campos
  iv_vinculo_2 <- InputValidator$new()
  iv_vinculo_2$add_rule("tipo_vinculo_contacto_2", function(value) {
    if (value != "") {
      if (nchar(value) < 2) {
        return("El campo debe tener al menos 2 caracteres.")
      }
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {  # Expresión corregida
        return("El campo solo puede contener letras y espacios.")
      }
    }
    return(NULL)
  })
  iv_vinculo_2$enable()
  
  iv_vinculo_3 <- InputValidator$new()
  iv_vinculo_3$add_rule("tipo_vinculo_contacto_3", function(value) {
    if (value != "") {
      if (nchar(value) < 2) {
        return("El campo debe tener al menos 2 caracteres.")
      }
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {  # Expresión corregida
        return("El campo solo puede contener letras y espacios.")
      }
    }
    return(NULL)
  })
  iv_vinculo_3$enable()
  
  # Nombre
  iv_nombre_1 <- InputValidator$new()
  iv_nombre_1$add_rule("nombre_contacto_1", sv_required("El campo es obligatorio."))
  iv_nombre_1$add_rule("nombre_contacto_1", function(value) {
    if (value != "") {
      if (nchar(value) < 2) {
        return("El campo debe tener al menos 2 caracteres.")
      }
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {  # Expresión corregida
        return("El campo solo puede contener letras y espacios.")
      }
      if (grepl("[0-9]", value)) {
        return("El campo no puede contener números.")
      }
    }
    return(NULL)
  })
  iv_nombre_1$enable()
  
  # Repetir para los otros campos de nombre
  iv_nombre_2 <- InputValidator$new()
  iv_nombre_2$add_rule("nombre_contacto_2", function(value) {
    if (value != "") {
      if (nchar(value) < 2) {
        return("El campo debe tener al menos 2 caracteres.")
      }
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {  # Expresión corregida
        return("El campo solo puede contener letras y espacios.")
      }
      if (grepl("[0-9]", value)) {
        return("El campo no puede contener números.")
      }
    }
    return(NULL)
  })
  iv_nombre_2$enable()
  
  iv_nombre_3 <- InputValidator$new()
  iv_nombre_3$add_rule("nombre_contacto_3", function(value) {
    if (value != "") {
      if (nchar(value) < 2) {
        return("El campo debe tener al menos 2 caracteres.")
      }
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {  # Expresión corregida
        return("El campo solo puede contener letras y espacios.")
      }
      if (grepl("[0-9]", value)) {
        return("El campo no puede contener números.")
      }
    }
    return(NULL)
  })
  iv_nombre_3$enable()
  
  # Reglas para entrevistas
  
  # Estado psicologo
  iv_estado_psicologo <- InputValidator$new()
  iv_estado_psicologo$add_rule("estado_psicologo", sv_required(message = "Campo obligatorio"))
  iv_estado_psicologo$add_rule("estado_psicologo", function(value) {
    categorias_validas <- c("Presente", "Ausente", "Pendiente", "No necesaria", "No asignada")
    if (!value %in% categorias_validas) {
      return("Debe seleccionar una de las categorías predefinidas.")
    }
    return(NULL)
  })
  iv_estado_psicologo$enable()
  
  # Estado psiquiatra
  iv_estado_psiquiatra <- InputValidator$new()
  iv_estado_psiquiatra$add_rule("estado_psiquiatra", sv_required(message = "Campo obligatorio"))
  iv_estado_psiquiatra$add_rule("estado_psiquiatra", function(value) {
    categorias_validas <- c("Presente", "Ausente", "Pendiente", "No necesaria", "No asignada")
    if (!value %in% categorias_validas) {
      return("Debe seleccionar una de las categorías predefinidas.")
    }
    return(NULL)
  })
  iv_estado_psiquiatra$enable()
  
  # Estado trabajador social
  iv_estado_ts <- InputValidator$new()
  iv_estado_ts$add_rule("estado_ts", sv_required(message = "Campo obligatorio"))
  iv_estado_ts$add_rule("estado_ts", function(value) {
    categorias_validas <- c("Presente", "Ausente", "Pendiente", "No necesaria", "No asignada")
    if (!value %in% categorias_validas) {
      return("Debe seleccionar una de las categorías predefinidas.")
    }
    return(NULL)
  })
  iv_estado_ts$enable()
  
  # Fecha psicologo
  iv_fecha_psicologo <- InputValidator$new()
  iv_fecha_psicologo$add_rule("fecha_entrevista_psicologo", function(value) {
    estado <- input$estado_psicologo  # Tomar el valor del campo estado
    if (estado %in% c("Presente", "Ausente", "Pendiente")) {
      if (is.null(value)) {
        return("La fecha es obligatoria.")
      }
      fecha_seleccionada <- as.Date(value)
      if (estado %in% c("Presente", "Ausente") && fecha_seleccionada > Sys.Date()) {
        return("La fecha no puede ser futura.")
      }
      if (estado == "Pendiente" && fecha_seleccionada <= Sys.Date()) {
        return("La fecha debe ser futura.")
      }
    }
    if (estado %in% c("No necesaria", "No asignada") && !is.null(value)) {
      return("El campo de fecha debe estar vacío.")
    }
    return(NULL)  # No hay error
  })
  
  iv_fecha_psicologo$enable()
  
  # Fecha psiquiatra
  iv_fecha_psiquiatra <- InputValidator$new()
  iv_fecha_psiquiatra$add_rule("fecha_entrevista_psiquiatra", function(value) {
    estado <- input$estado_psiquiatra  # Tomar el valor del campo estado
    if (estado %in% c("Presente", "Ausente", "Pendiente")) {
      if (is.null(value)) {
        return("La fecha es obligatoria.")
      }
      fecha_seleccionada <- as.Date(value)
      if (estado %in% c("Presente", "Ausente") && fecha_seleccionada > Sys.Date()) {
        return("La fecha no puede ser futura.")
      }
      if (estado == "Pendiente" && fecha_seleccionada <= Sys.Date()) {
        return("La fecha debe ser futura.")
      }
    }
    if (estado %in% c("No necesaria", "No asignada") && !is.null(value)) {
      return("El campo de fecha debe estar vacío.")
    }
    return(NULL)  # No hay error
  })
  
  iv_fecha_psiquiatra$enable()
  
  # Fecha Trabajador Social
  iv_fecha_ts <- InputValidator$new()
  iv_fecha_ts$add_rule("fecha_entrevista_ts", function(value) {
    estado <- input$estado_ts  # Tomar el valor del campo estado
    if (estado %in% c("Presente", "Ausente", "Pendiente")) {
      if (is.null(value)) {
        return("La fecha es obligatoria.")
      }
      fecha_seleccionada <- as.Date(value)
      if (estado %in% c("Presente", "Ausente") && fecha_seleccionada > Sys.Date()) {
        return("La fecha no puede ser futura.")
      }
      if (estado == "Pendiente" && fecha_seleccionada <= Sys.Date()) {
        return("La fecha debe ser futura.")
      }
    }
    if (estado %in% c("No necesaria", "No asignada") && !is.null(value)) {
      return("El campo de fecha debe estar vacío.")
    }
    return(NULL)  # No hay error
  })
  
  iv_fecha_ts$enable()
  
  
}

shinyApp(ui, server)