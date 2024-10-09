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
library(tibble)

# Importar base
base <- function(){
  data <- read_excel("Base completa.xlsx")
}
data <- base()

# Base de provincias y localidades
provincias <- show_arg_codes()[2:25, 5]
provincias[[1]][1] <- "CABA"
provincia_vacia <- tibble(name_iso = " ")
provincias <- bind_rows(provincia_vacia, provincias)
colnames(provincias) <- "Provincias"

localidades_por_provincia <- readRDS("localidades.rds")
for (provincia in names(localidades_por_provincia)) {
  localidades_por_provincia[[provincia]] <- c("", localidades_por_provincia[[provincia]])
}

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
            
            style = "min-height: 410px;",
            
            h4("Datos del Registro", style = "font-size: 15px; font-weight: bold;"), 
            
            # Campo ID de registro (readonly)
            fluidRow(
              div(
                style = "margin-bottom: 8px;", 
                tags$label(
                  tags$span(
                    "ID de registro", 
                    style = "font-size: 12px; margin-bottom: 10px; display: inline-block;")),
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
                  tags$span("Fecha de registro", style = "font-size: 12px;"),
                  value = Sys.Date(),
                  format = "dd/mm/yyyy",
                  min = Sys.Date() - years(1),
                  max = Sys.Date()
                )
              )
            ),
            
            h4("Historial de Registro", style = "font-size: 15px; font-weight: bold;"), 
            
            # Campo ID de persona (readonly)
            div(
              style = "width: 100%; margin-bottom: 8px;", 
              tags$label(
                tags$span("ID de la persona", 
                          style = "font-size: 12px; margin-bottom: 10px; display: inline-block;")
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
                tags$span("Fecha del primer registro", style = "font-size: 12px;"),
                value = Sys.Date(),
                format = "dd/mm/yyyy"
                )
              )
            )
          ),
        
      # Datos personales -------------------------------------------------------
        column(
          width = 6,
          wellPanel(
            
            style = "min-height: 410px;", 
            
            h4("Datos de la persona", style = "font-size: 15px; font-weight: bold;"),
            
            fluidRow(
              
              # Campo recuerda DNI
              column(
                width = 3,
                selectInput(
                  inputId = "recuerda_dni",
                  tags$span("¿Recuerda el DNI?", style = "font-size: 12px;"),
                  choices = c("","Si", "No", "No tiene" = "S/D"),
                  selected = ""
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
              
              # Fecha de nacimiento
              column(
                width = 4,
                div(
                  dateInput(
                    inputId = "fecha_nacimiento",
                    label = tags$span("Fecha de nacimiento", style = "font-size: 12px;"),
                    value = "",
                    format = "dd/mm/yyyy",  
                    min = Sys.Date() - years(100),  # Limitar a 110 años atrás
                    max = Sys.Date()  # Limitar a la fecha de hoy
                  )
                )
              ),
              
              # Campo sexo biológico
              column(
                width = 4,
                selectInput(
                  "sexo_biologico",
                  label = tags$span("Sexo biológico", style = "font-size: 12px;"),
                  choices = c("Femenino", "Masculino", "No informado",""), 
                  selected = ""
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
                              "No binario", "Otro",""),  
                  selected = ""
                )
              )
              ),
            
            fluidRow(
              
              # Campo Provincia
              column(
                width = 4,
                div(
                  style = "height: 38px;",
                  selectInput(
                    "provincia",
                    label = tags$span("Provincia de residencia", style = "font-size: 12px;"),
                    choices = provincias,
                    selected = provincias[[1]][1]
                  )
                )
              ),
              
              # Campo localidad
              column(
                width = 4,
                div(
                  style = "height: 38px;",
                  uiOutput("localidad_ui")
                )
              ),
              
              # Campo barrio
              column(
                width = 4,
                div(
                  style = "height: 38px;",
                  textInput(
                    "barrio",
                    label = tags$span("Barrio", style = "font-size: 12px;")
                  )
                )
              )
            )
            )
          ),
        
      # Datos de contacto ------------------------------------------------------
        column(
          width = 4, 
          wellPanel( 
            
            style = "min-height: 410px;",
            
            h4("Contacto 1", style = "font-size: 15px; font-weight: bold;"),
            
            fluidRow(
            
              # Campo teléfono de contacto 1
              column(
                width = 4,
                numericInput(
                  "telefono_contacto_1", 
                  label = tags$span("Teléfono", style = "font-size: 12px;"),
                  value = ""
                )
              ),
              
              # Campo tipo de vinculo 1
              column(
                width = 4,
                selectizeInput(
                  "tipo_vinculo_contacto_1",
                  label = tags$span("Tipo de vínculo", style = "font-size: 12px;"),
                  choices = c("","Propio","Papá/Mamá", "Hermano/a", "Hijo/a", "Amigo/a"),
                  multiple = FALSE,
                  options = list(create = TRUE),
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
            
            h4("Contacto 2", style = "font-size: 15px; font-weight: bold;"),
            
            fluidRow(
              
              # Campo telefono de contacto 2
              column(
                width = 4,
                numericInput(
                  "telefono_contacto_2",  # inputId debe coincidir con el servidor
                  label = tags$span("Teléfono", style = "font-size: 12px;"),
                  value = ""
                  )
                ),
              
              # Campo tipo de vínculo 2
              column(
                width = 4,
                selectizeInput(
                  "tipo_vinculo_contacto_2",
                  label = tags$span("Tipo de vínculo", style = "font-size: 12px;"),
                  choices = c("","Propio","Papá/Mamá", "Hermano/a", "Hijo/a", "Amigo/a"),
                  multiple = FALSE,
                  options = list(create = TRUE),
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
            
            h4("Contacto 3", style = "font-size: 15px; font-weight: bold;"),
            
            fluidRow(
              
              # Campo telefono de contacto 3
              column(
                width = 4,
                numericInput(
                  "telefono_contacto_3",  # inputId debe coincidir con el servidor
                  label = tags$span("Teléfono", style = "font-size: 12px;"),
                  value = ""
                )
              ),
              
              # Campo tipo de vínculo 3
              column(
                width = 4,
                selectizeInput(
                  "tipo_vinculo_contacto_3",
                  label = tags$span("Tipo de vínculo", style = "font-size: 12px;"),
                  choices = c("","Propio","Papá/Mamá", "Hermano/a", "Hijo/a", "Amigo/a"),
                  multiple = FALSE,
                  options = list(create = TRUE),
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
  
  # Cargar la base de datos
  data <- base()
  
  # ID registro ----------------------------------------------------------------
  
  ## Obtener el ID máximo
  id_max <- reactiveVal(max(data$`ID de registro`, na.rm = TRUE))
  
  ## Actualizar el campo con el ID máximo + 1
  observe({
    updateTextInput(session, "id_registro", value = as.character(id_max() + 1))
  })
  
  # Fecha de registro ----------------------------------------------------------
  
  iv_fecha_registro <- InputValidator$new()
  
  ## Campo obligatorio
  iv_fecha_registro$add_rule("fecha_registro", 
                             sv_required(
                               tags$span("Campo obligatorio.", style = "font-size: 10px;")
                               )
                             )
  
  iv_fecha_registro$enable()
  
  # Fecha primer registro ------------------------------------------------------
  
  iv_fecha_primer_registro <- InputValidator$new()
  
  ## Campo obligatorio
  iv_fecha_primer_registro$add_rule("fecha_primer_registro",
                                    sv_required(
                                      tags$span("Campo obligatorio.", style = "font-size: 10px;")
                                      )
                                    )
  
  iv_fecha_primer_registro$enable()
  
  # Recuerda DNI ---------------------------------------------------------------
  
  iv_recuerda_dni <- InputValidator$new()
  
  ## Campo obligatorio
  iv_recuerda_dni$add_rule("recuerda_dni",
                           sv_required(
                             tags$span("Campo obligatorio.", style = "font-size: 10px;")
                             )
                           )
  
  iv_recuerda_dni$enable()
  
  ## Acciones según la selección de recuerda_dni
  observeEvent(input$recuerda_dni, {
    # Si la opción es "", No o S/D, deshabilitar el campo y eliminar la validación
    if (input$recuerda_dni %in% c("","No", "S/D")) {
      updateNumericInput(session, "dni", value = "")
      shinyjs::disable("dni")
      iv_dni$disable()
      } else {
      # Si selecciona Sí, habilitar el campo y agregar la validación
      shinyjs::enable("dni")  # Habilitar el campo
      iv_dni$enable()  # Activar la validación
    }
  })
  
  # Validación para DNI ---------------------------------------------------------
  iv_dni <- InputValidator$new()
  
  # Agregar la regla de campo obligatorio
  iv_dni$add_rule("dni", sv_required(
    tags$span("Campo obligatorio.",
              style = "font-size: 10px;")
    )
  )
  
  # Agregar la regla para que solo contenga números (sin puntos, espacios, etc.)
  iv_dni$add_rule("dni", function(value) {
    if (!grepl("^[0-9]+$", value)) {
      return("El DNI solo puede contener números, sin puntos ni caracteres especiales.")
    }
    return(NULL)  # Si pasa la validación, no devuelve errores
  })
  
  # Agregar la regla para asegurarse de que tenga exactamente 8 dígitos
  iv_dni$add_rule("dni", function(value) {
    if (nchar(value) != 8) {
      return("El DNI debe tener exactamente 8 dígitos.")
    }
    return(NULL)
  })
  
  # Habilitar el validador
  iv_dni$enable()
  
  # Apellido, Nombre (apodo) ---------------------------------------------------
  iv_apellido_nombre <- InputValidator$new()
  
  ## Obligatorio
  iv_apellido_nombre$add_rule("apellido_nombre", 
                              sv_required(tags$span("Campo obligatorio.", 
                                                    style = "font-size: 10px;")
                                          )
                              )
  
  ## Caracteres especiales (excepto tildes, coma y paréntesis)
  iv_apellido_nombre$add_rule("apellido_nombre", function(value) {
    if(grepl("[^a-zA-ZáéíóúÁÉÍÓÚñÑ,() ]", value)) {
      return(tags$span("No se admiten caracteres especiales.",
                       style = "font-size: 10px;")
             )
    }
  })
  
  ## Más de 4 caracteres
  iv_apellido_nombre$add_rule("apellido_nombre", function(value) {
    if(nchar(as.character(value)) <= 4) {
      return(tags$span("El campo debe tener más de 4 caracteres.",
                       style = "font-size: 10px;")
      )
    }
  })
  
  iv_apellido_nombre$enable()
  
  # Fecha de nacimiento --------------------------------------------------------
  
  # Sexo biológico -------------------------------------------------------------
  
  iv_sexo_biologico <- InputValidator$new()
  
  ## Obligatorio
  iv_sexo_biologico$add_rule("sexo_biologico",
                             sv_required(tags$span("Campo obligatorio.",
                                                   style = "font-size: 10px;")
                                         )
                             )
  
  iv_sexo_biologico$enable()
  
  # Género ---------------------------------------------------------------------
  
  iv_genero <- InputValidator$new()
  
  ## Obligatorio
  iv_genero$add_rule("genero",
                     sv_required(tags$span("Campo obligatorio.",
                                           style = "font-size: 10px;")
                                 )
                     )
  
  iv_genero$enable()
  
  # Provincia ------------------------------------------------------------------
  
  iv_provincia <- InputValidator$new()
  
  ## Obligatorio
  iv_provincia$add_rule("provincia", function(value) {
    if(value == provincias[[1]][1]) {
      return(tags$span("Campo obligatorio.",
                       style = "font-size: 10px;"))
    }
  })
  
  iv_provincia$enable()
  
  # Localidad ------------------------------------------------------------------
  
  # Crear el uiOutput para las localidades
  output$localidad_ui <- renderUI({
    selectInput("localidad",
                label = tags$span("Localidad de residencia", style = "font-size: 12px;"), 
                choices = " ")
  })
  
  # Actualizar las localidades en función de la provincia seleccionada
  observeEvent(input$provincia, {
    localidades <- sort(localidades_por_provincia[[input$provincia]])
    updateSelectInput(session, "localidad", choices = localidades, selected = NULL)
  })
  
  # Barrio ---------------------------------------------------------------------
  
  iv_barrio <- InputValidator$new()
  
  ## Nada de caracteres especiales
  iv_barrio$add_rule("barrio", function(value) {
    if(grepl("[^a-zA-ZáéíóúÁÉÍÓÚñÑ,() ]", value)) {
      return("No se admiten caracteres especiales.")
    }
  })
  
  ## El campo debe tener entre 2 y 100 caracteres
  iv_barrio$add_rule("barrio", function(value) {
    if(nchar(as.character(value)) <= 2 & nchar(as.character(value)) >0) {
      return("El campo debe tener más de 2 caracteres.")
    }
    if(nchar(as.character(value)) > 100) {
      return("El campo debe tener menos de 100 caracteres.")
    }
  })
  
  iv_barrio$enable()
  
  # Contacto 1 - Teléfono ------------------------------------------------------
  
  iv_telefono_1 <- InputValidator$new()
  
  ## Obligatorio
  iv_telefono_1$add_rule("telefono_contacto_1",
                         sv_required(tags$span("Campo obligatorio.",
                                               style = "font-size: 10px;")
                                     )
                         )
  
  ## Entre 7 y 10 caracteres
  iv_telefono_1$add_rule("telefono_contacto_1", function(value) {
    if (nchar(as.character(value)) < 7) {
      return(tags$span("El teléfono debe tener al menos 7 dígitos.", style = "font-size: 10px;"))
    }
    if (nchar(as.character(value)) > 10) {
      return(tags$span("El teléfono debe tener menor de 10 dígitos.", style = "font-size: 10px;"))
    }
    if (!grepl("^[0-9]+$", as.character(value))) {
      return(return(tags$span("Sólo se admiten números.", style = "font-size: 10px;")))
    }
    return(NULL)
  })
  
  iv_telefono_1$enable()
  
  # Función auxiliar para verificar si el valor no es NULL o NA
  validar_telefono <- function(value) {
    if (is.null(value) || is.na(value)) {
      return(NULL)  # Si es NULL o NA, no hay error
    }
    
    # Convertimos a cadena de texto por si el valor no lo es
    value <- as.character(value)
    
    if (nchar(value) < 7) {
      return(tags$span("El teléfono debe tener al menos 7 dígitos.", style = "font-size: 10px;"))
    }
    if (nchar(value) > 10) {
      return(tags$span("El teléfono debe tener menos de 10 dígitos.", style = "font-size: 10px;"))
    }
    if (!grepl("^[0-9]+$", value)) {
      return(tags$span("Solo se admiten números.", style = "font-size: 10px;"))
    }
    
    return(NULL)  # Si todo es correcto, no hay error
  }
  
  # Contacto 2 - Teléfono ------------------------------------------------------
  iv_telefono_2 <- InputValidator$new()
  
  # Añadimos la regla utilizando la función auxiliar
  iv_telefono_2$add_rule("telefono_contacto_2", function(value) {
    validar_telefono(value)
  })
  
  iv_telefono_2$enable()
  
  # Contacto 3 - Teléfono ------------------------------------------------------
  iv_telefono_3 <- InputValidator$new()
  
  # Añadimos la regla utilizando la función auxiliar
  iv_telefono_3$add_rule("telefono_contacto_3", function(value) {
    validar_telefono(value)
  })
  
  iv_telefono_3$enable()
  
  # Contacto 1 - Vinculo -------------------------------------------------------
  
  iv_vinculo_1 <- InputValidator$new()
  
  ## Obligatorio
  iv_vinculo_1$add_rule("tipo_vinculo_contacto_1",
                        sv_required(tags$span("Campo obligatorio.",
                                              style = "font-size: 10px;")
                                    )
                        )
  
  ## Más de 2 caracteres y sin caracteres especiales
  iv_vinculo_1$add_rule("tipo_vinculo_contacto_1", function(value) {
    if (value != "") {
      if (nchar(value) < 2) {
        return("El campo debe tener al menos 2 caracteres.")
      }
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {  # Esta expresión regular es correcta
        return("No se admiten caracteres especiales.")
      }
    }
    return(NULL)
  })
  
  iv_vinculo_1$enable()
  
  # Contacto 2 - Vinculo -------------------------------------------------------
  
  iv_vinculo_2 <- InputValidator$new()
  
  ## Más de 2 caracteres y sin caracteres especiales
  iv_vinculo_2$add_rule("tipo_vinculo_contacto_2", function(value) {
    if (value != "") {
      if (nchar(value) < 2) {
        return("El campo debe tener al menos 2 caracteres.")
      }
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {  # Esta expresión regular es correcta
        return("No se admiten caracteres especiales.")
      }
    }
    return(NULL)
  })
  
  iv_vinculo_2$enable()
  
  # Contacto 3 - Vinculo -------------------------------------------------------
  
  iv_vinculo_3 <- InputValidator$new()
  
  ## Más de 2 caracteres y sin caracteres especiales
  iv_vinculo_3$add_rule("tipo_vinculo_contacto_3", function(value) {
    if (value != "") {
      if (nchar(value) < 2) {
        return("El campo debe tener al menos 2 caracteres.")
      }
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {  # Esta expresión regular es correcta
        return("No se admiten caracteres especiales.")
      }
    }
    return(NULL)
  })
  
  iv_vinculo_3$enable()
  
  # Contacto 1 - Nombre --------------------------------------------------------
  
  iv_nombre_1 <- InputValidator$new()
  
  ## Obligatorio
  iv_nombre_1$add_rule("nombre_contacto_1", function(value) {
    if (input$tipo_vinculo_contacto_1 != "Propio") {
      if (value == "") {
        return(tags$span("Campo obligatorio.", style = "font-size: 10px;"))
      }
    }
    return(NULL)
  })

  ## Al menos 2 caracteres, sin caracteres especiales
  
  iv_nombre_1$add_rule("nombre_contacto_1", function(value) {
    if (value != "") {
      if (nchar(value) < 2) {
        return(tags$span("El campo debe tener al menos 2 caracteres.", style = "font-size: 10px;"))
      }
    }
    return(NULL)
  })
  
  iv_nombre_1$add_rule("nombre_contacto_1",function(value) {
    esta <- input$tipo_vinculo_contacto_1 %in% c("","Propio","Papá/Mamá", "Hermano/a", "Hijo/a", "Amigo/a")
    if (!esta) {
      if(!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ /]+$", value)) {  # Incluye el símbolo '/'
        return(tags$span("No se admiten caracteres especiales excepto '/', letras y espacios.", style = "font-size: 10px;"))
      }
    }
  })
  
  
  iv_nombre_1$enable()
  
  # Contacto 2 - Nombre --------------------------------------------------------
  
  iv_nombre_2 <- InputValidator$new()
  
  ## Al menos 2 caracteres, sin caracteres especiales
  iv_nombre_2$add_rule("nombre_contacto_2", function(value) {
    if (value != "") {
      if (nchar(value) < 2) {
        return(tags$span("El campo debe tener al menos 2 caracteres.", style = "font-size: 10px;"))
      }
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {  # Expresión corregida
        return(tags$span("No se admiten caracteres especiales.", style = "font-size: 10px;"))
      }
    }
    return(NULL)
  })
  iv_nombre_2$enable()
  
  # Contacto 3 - Nombre --------------------------------------------------------
  
  iv_nombre_3 <- InputValidator$new()
  
  ## Al menos 2 caracteres, sin caracteres especiales
  iv_nombre_3$add_rule("nombre_contacto_3", function(value) {
    if (value != "") {
      if (nchar(value) < 2) {
        return(tags$span("El campo debe tener al menos 2 caracteres.", style = "font-size: 10px;"))
      }
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {  # Expresión corregida
        return(tags$span("No se admiten caracteres especiales.", style = "font-size: 10px;"))
      }
    }
    return(NULL)
  })
  iv_nombre_3$enable()
  
  # Actualizaciones con el dni -------------------------------------------------
  
  observeEvent(input$dni, {
    
    # Si el campo "recuerda_dni" es Vacío, "No" o "S/D", asignamos el nuevo ID con max() + 1
    if (input$recuerda_dni %in% c("","No", "S/D")) {
      id_persona <- max(data$`ID de la persona`, na.rm = TRUE) + 1
    } else {
      req(input$dni)  # Asegurarse de que el DNI no esté vacío
      
      # Comprobar si el DNI ya existe en la base
      dni_existente <- data[which(data$DNI == input$dni), ]
      iv_dni$enable()
      
      # Si el DNI ya existe, traer la información de esa persona
      if (nrow(dni_existente) > 0) {
        
        # Mostrar mensaje en verde
        output$dni_message <- renderText({
          "El DNI ya figura en la base, los campos han sido completados"
        })
        
        id_persona <- last(dni_existente$`ID de la persona`)
        # no va update date input porque es de texto, no input
        
        fecha_primer_registro <- first(dni_existente$`Fecha de registro`)
        updateDateInput(session, "fecha_primer_registro", value = fecha_primer_registro)
        
        apellido_nombre <- last(dni_existente$`Apellido y Nombre`)  # Obtener el último apellido y nombre para ese DNI
        updateTextInput(session, "apellido_nombre", value = apellido_nombre)  # Actualizar con el valor de la base
        
        sexo_biologico <- last(dni_existente$`Sexo biológico`)
        updateSelectInput(session, "sexo_biologico", selected = sexo_biologico)

        genero <- last(dni_existente$Género)
        updateSelectInput(session, "genero", selected = genero)
        
        fecha_nacimiento <- last(dni_existente$`Fecha de Nacimiento`)
        updateDateInput(session, "fecha_nacimiento", value = fecha_nacimiento)
        
        provincia <- last(dni_existente$`Provincia`)
        localidad <- last(dni_existente$`Localidad`)
        
        # Si la provincia no está en las opciones del selectInput, la añadimos temporalmente
        # (por ejemplo, un DNI tiene provincia vacío)
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
        
        barrio <- last(dni_existente$Barrio)
        updateSelectInput(session, "barrio", selected = barrio)
        
        telefono_contacto_1 <- last(dni_existente$`Teléfono de Contacto 1`)
        updateNumericInput(session, "telefono_contacto_1", value = telefono_contacto_1)
        
        telefono_contacto_2 <- last(dni_existente$`Teléfono de Contacto 2`)
        updateNumericInput(session, "telefono_contacto_2", value = telefono_contacto_2)
        
        telefono_contacto_3 <- last(dni_existente$`Teléfono de Contacto 3`)
        updateNumericInput(session, "telefono_contacto_3", value = telefono_contacto_3)
        
        tipo_vinculo_contacto_1 <- last(dni_existente$`Tipo de Vínculo con el Contacto 1`)
        updateSelectizeInput(session, "tipo_vinculo_contacto_1", selected = tipo_vinculo_contacto_1)
        
        tipo_vinculo_contacto_2 <- last(dni_existente$`Tipo de Vínculo con el Contacto 2`)
        updateSelectizeInput(session, "tipo_vinculo_contacto_2", selected = tipo_vinculo_contacto_2)
        
        tipo_vinculo_contacto_3 <- last(dni_existente$`Tipo de Vínculo con el Contacto 3`)
        updateSelectizeInput(session, "tipo_vinculo_contacto_3", selected = tipo_vinculo_contacto_3)
        
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
        updateSelectInput(session, "sexo_biologico", selected = "")
        updateSelectInput(session, "genero", selected = "")
        updateDateInput(session, "fecha_nacimiento", value = NULL)
        updateSelectInput(session, "provincia", selected = "")
        updateSelectInput(session, "localidad", choices = NULL, selected = NULL)
        updateTextInput(session, "barrio", value = "")
        updateNumericInput(session, "telefono_contacto_1", value = "")
        updateNumericInput(session, "telefono_contacto_2", value = "")
        updateNumericInput(session, "telefono_contacto_3", value = "")
        updateSelectizeInput(session, "tipo_vinculo_contacto_1", selected = "")
        updateSelectizeInput(session, "tipo_vinculo_contacto_2", selected = "")
        updateSelectizeInput(session, "tipo_vinculo_contacto_3", selected = "")
        updateTextInput(session, "nombre_contacto_1", value = "")
        updateTextInput(session, "nombre_contacto_2", value = "")
        updateTextInput(session, "nombre_contacto_3", value = "")
      }
    }
    
    # Actualizar el campo ID de la persona
    updateTextInput(session, "id_persona", value = as.numeric(id_persona))
  })
  
}

shinyApp(ui, server)
