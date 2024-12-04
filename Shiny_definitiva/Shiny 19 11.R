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
library(writexl)
library(kableExtra)
library(tidyr)
library(openxlsx)
library(ggplot2)
library(ggthemes)
library(shinydashboard)
library(showtext)
library(leaflet) # para gráfico de mapa
library(emojifont) # para icono de mujer y hombre
font_add_google("Montserrat")
library(shinymanager) #para usuario y contraseña
library(toastui)
library(DT)
library(shinyWidgets)
library(forcats)

# Usuario y contraseña
set_labels(
  language = "en",
  "Please authenticate" = "Gestión de registros - Padre misericordioso",
  "Username:" = "Nombre de usuario:",
  "Password:" = "Contraseña:"
)

credentials <- data.frame(
  user = c("registroPM", "adminPM"), # mandatory
  password = c("Rosario24", "Estadistica24"), # mandatory
  admin = c(FALSE, TRUE),
  comment = "Simple and secure authentification mechanism 
  for single ‘Shiny’ applications.",
  stringsAsFactors = FALSE
)

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

## User Interface --------------------------------------------------------------
ui <- page_navbar(
  
  verbatimTextOutput("auth_output"),
  
  useShinyjs(),
  
  # Tema con función bs_theme()
  theme = bs_theme(
    bg = "#e1e1e1",
    fg = "black",
    primary = "#ec7e14",
    secondary = "#fbc91c",
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
    .checkbox-inline, .checkbox-label {
      font-size: 12px;
    }
    .shiny-input-container {
      font-size: 12px;
    }
    .checkbox-group-input {
      margin-top: 10px;
    }
      /* Color naranja para la fila seleccionada y hover */
    #search_results .dataTable tbody tr.selected {
      background-color: #FFA500 !important;
      color: white;
    }
    #search_results .dataTable tbody tr:hover,
    #search_results .dataTable tbody tr:focus,
    #search_results .dataTable tbody tr.active {
      background-color: #FFA500 !important;
      color: white !important;
    }
    /* Eliminar cualquier borde o fondo azul de la tabla */
    #search_results .dataTable tbody tr.selected td,
    #search_results .dataTable tbody tr.selected {
      box-shadow: none !important;
      outline: none !important;
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
          
          style = "min-height: 400px;",
          
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
                inputId = "fecha_registro",
                label = tags$span("Fecha de registro", style = "font-size: 12px;"),
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
              inputId = "fecha_primer_registro",
              label =  tags$span("Fecha del primer registro", style = "font-size: 12px;"),
              value = Sys.Date(),
              format = "dd/mm/yyyy",
              max = Sys.Date()
            )
          )
        )
      ),
      
      # Datos personales -------------------------------------------------------
      column(
        width = 6,
        wellPanel(
          
          style = "min-height: 400px;", 
          
          h4("Datos de la persona", style = "font-size: 15px; font-weight: bold;"),
          
          fluidRow(
            
            # Campo recuerda DNI
            column(
              width = 3,
              selectInput(
                inputId = "recuerda_dni",
                label = tags$span("¿Recuerda el DNI?", style = "font-size: 12px;"),
                choices = c("","Si", "No", "No tiene" = "S/D"),
                selected = ""
              )
            ),
            
            # Campo DNI
            column(
              width = 3,
              numericInput(
                inputId = "dni",
                label = tags$span("DNI", style = "font-size: 12px;"),
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
              width = 3,
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
            
            # Edad
            column(
              width = 3,
              numericInput(
                "edad",
                tags$span("Edad", style = "font-size:10px;"),
                value = NULL
              )
            ),
            
            # Campo sexo biológico
            column(
              width = 3,
              selectInput(
                "sexo_biologico",
                label = tags$span("Sexo biológico", style = "font-size: 12px;"),
                choices = c("No informado","Femenino", "Masculino", ""), 
                selected = ""
              )
            ),
            
            # Campo género
            column(
              width = 3,
              selectInput(
                "genero",
                label = tags$span("Género", style = "font-size: 12px;"),
                choices = c("No informado","Mujer", "Hombre", "Trans (feminidades)", "Trans (masculinidades)", "Otro", ""),  
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
          
          style = "min-height: 400px;",
          
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
                choices = c("","Propio","Papá","Mamá", "Hermano","Hermana", "Hijo","Hija", "Amigo", "Amiga"),
                multiple = FALSE,
                options = list(create = TRUE)
              )
            ),
            
            # Campo nombre del contacto 1
            column(
              width = 4,
              textInput(
                "nombre_contacto_1",  
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
                "telefono_contacto_2",  
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
                choices = c("","Propio","Papá","Mamá", "Hermano","Hermana", "Hijo","Hija", "Amigo", "Amiga"),
                multiple = FALSE,
                options = list(create = TRUE)
              )
            ),
            
            # Campo nombre del contacto 2
            column(
              width = 4,
              textInput(
                "nombre_contacto_2",  
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
                "telefono_contacto_3", 
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
                choices = c("","Propio","Papá","Mamá", "Hermano","Hermana", "Hijo","Hija", "Amigo", "Amiga"),
                multiple = FALSE,
                options = list(create = TRUE)
              )
            ),
            
            # Campo nombre del contacto 3
            column(
              width = 4,
              textInput(
                "nombre_contacto_3",  
                label = tags$span("Nombre", style = "font-size: 12px;"),
                value = ""
              )
            )
          )
        )
      ),
      
      # Información del Consumo y Tratamiento ----------------------------------
      
      column(
        width = 8, 
        wellPanel(  
          
          style = "min-height: 460px; margin-top: 20px;", 
          
          h4("Inicio del consumo", style = "font-size: 15px; font-weight: bold;"),
          
          fluidRow(
            
            # Campo edad de inicio de consumo
            column(
              style = "margin-top:10px;",
              width = 4,
              textInput(
                "edad_inicio_consumo",
                label = tags$span("Edad de inicio de consumo", style = "font-size: 12px;"),
                value = ""
              )
            ),
            
            # Campo sustancia de inicio
            column(
              width = 4,
              style = "margin-top:10px;",
              selectInput(
                inputId = "sustancia_inicio_consumo",
                label = tags$span("Sustancia de Inicio de Consumo", style = "font-size: 12px; white-space: nowrap;"),
                choices = c("No informado","Alcohol", "Crack", "Cocaína", "Marihuana", "Nafta aspirada","Pegamento", "Psicofármacos", "Otra", ""),
                selected = ""  # No selecciona ninguna opción por defecto
              )
            ),
            
            # Campo emergente de texto para "Otra" opción al lado del selectInput
            column(
              style = "margin-top:10px;",
              width = 4,  
              conditionalPanel(
                condition = "input.sustancia_inicio_consumo == 'Otra'",
                textInput(
                  inputId = "otra_sustancia",
                  label = tags$span("Especifique la sustancia", style="font-size: 12px;"),
                  value = ""
                )
              )
            )
          ),
          
          h4("Consumo actual", style = "font-size: 15px; font-weight: bold;"),
          
          fluidRow(
            selectInput(
              "persona_consume",
              tags$span("¿Consume actualmente?", style = "font-size:12px;"),
              choices = c("No informado","","Si","No"),
              selected = ""
            )
          ),
          
          fluidRow(
            
            
            # Campo de selección múltiple de sustancias
            column(
              width = 8,
              tags$div(
                style = "margin-bottom: 10px;",
                tags$span("Sustancia/s de Consumo Actual", style = "font-size: 12px; white-space: nowrap;")
              ),
              tags$div(
                style = "column-count: 3; column-gap: 50px; margin-top: 10px;",  # Espacio entre columnas y margen superior
                checkboxGroupInput(
                  inputId = "sustancias_consumo_actual",
                  label = NULL,  # No mostramos la etiqueta aquí porque ya está arriba
                  choices = c("Alcohol","Crack","Cocaína","Marihuana","Nafta",
                              "Pegamento","Psicofármacos","Otra"),
                  selected = NULL
                )
              )
            ),
            
            # Campo emergente de texto para "Otra" opción
            column(
              width = 4,
              conditionalPanel(
                condition = "input.sustancias_consumo_actual.includes('Otra')",
                textInput(
                  inputId = "otra_sustancia_actual",
                  label = tags$span("Especifique la sustancia", style="font-size: 12px;"),
                  value = ""
                )
              )
            )
          ),
          
          h4("Tratamiento", style = "font-size: 15px; font-weight: bold; margin-top: 20px;"),
          
          fluidRow(
            
            # Campo derivación
            column(
              
              width = 3,
              
              selectInput(
                inputId = "derivacion",
                label = tags$span("Derivación", style = "font-size: 12px;"),
                choices = c("No informado","","Si", "No"),
                selected = ""
              )
            ),
            
            # Campo derivado de
            column(
              width = 3,
              
              textInput(
                inputId = "derivado_de",
                label = tags$span("Derivado de", style = "font-size: 12px;"),
                placeholder = "",
                value = ""
              )
            ),
            
            # Campo número de tratamientos previos
            column(
              width = 3,
              
              tags$div(
                tags$span("Nº de Tratamientos Previos", style = "font-size: 12px; white-space: nowrap;")
              ),
              numericInput(
                inputId = "num_tratamientos_previos",
                label = NULL,
                value = NA,
                min = 0,
                max = 99
              )
            ),
            
            # Campo emergente de texto para Número de trat > 0
            column(
              width = 3,
              style = "margin-bottom: 10px;", 
              conditionalPanel(
                condition = "input.num_tratamientos_previos > '0'",
                textInput(
                  inputId = "lugar_ultimo_tratamiento",
                  label = tags$span("Lugar de Último Tratamiento", style="font-size: 12px;"),
                  value = ""
                )
              )
            )
          )
        )
      ),
      
      # Entrevistas ------------------------------------------------------------
      
      column(
        width = 4,
        wellPanel( 
          
          style = "min-height: 460px; margin-top: 20px;",
          
          h4("Entrevista con Psicólogo", style = "font-size: 15px; font-weight: bold;"),
          
          fluidRow(
            
            # Campo estado - entrevista Psicologo
            column(
              width = 6,
              selectInput(
                inputId = "estado_psicologo",
                tags$span("Estado", style = "font-size: 12px;"),
                choices = list(
                  "Presente",
                  "Ausente",
                  "Pendiente",
                  "No necesaria",
                  "No asignada",
                  ""
                ),
                selected = "")
            ),  # Ninguna opción seleccionada por defecto
            
            # Campo fecha - entrevista Psicolgo
            column(
              width = 6,
              tags$div(
                style = "margin-bottom: 10px;",  # Espaciado inferior
                tags$span("Fecha de la entrevista", style = "font-size: 12px; white-space: nowrap;")
              ),
              dateInput(
                inputId = "fecha_entrevista_psicologo",
                label = NULL,  # Sin etiqueta adicional
                value = "",
                format = "dd/mm/yyyy"  # Formato de la fecha
              )
            )
          ),
          
          h4("Entrevista con Psiquiatra", style = "font-size: 15px; font-weight: bold;"),
          
          fluidRow(
            
            # Campo estado - entrevista psiquiatra
            column(
              width = 6,
              selectInput(
                inputId = "estado_psiquiatra",
                tags$span("Estado", style = "font-size: 12px;"),
                choices = list(
                  "Presente",
                  "Ausente",
                  "Pendiente",
                  "No necesaria",
                  "No asignada",
                  ""
                ),
                selected = "")
            ),  # Ninguna opción seleccionada por defecto
            
            # Campo fecha - entrevista Psiquiatra
            column(
              width = 6,
              tags$div(
                style = "margin-bottom: 10px;",  # Espaciado inferior
                tags$span("Fecha de la entrevista", style = "font-size: 12px; white-space: nowrap;")
              ),
              dateInput(
                inputId = "fecha_entrevista_psiquiatra",
                label = NULL,  # Sin etiqueta adicional
                value = "",
                format = "dd/mm/yyyy"  # Formato de la fecha
              )
            )
          ),
          
          h4("Entrevista con Trabajador Social", style = "font-size: 15px; font-weight: bold;"),
          
          fluidRow(
            
            # Campo estado - entrevista trabajador social
            column(
              width = 6,
              selectInput(
                inputId = "estado_ts",
                tags$span("Estado", style = "font-size: 12px;"),
                choices = list(
                  "Presente",
                  "Ausente",
                  "Pendiente",
                  "No necesaria",
                  "No asignada",
                  ""
                ),
                selected = "")
            ),  # Ninguna opción seleccionada por defecto
            
            # Campo fecha - entrevista trabajador social
            column(
              width = 6,
              tags$div(
                style = "margin-bottom: 10px;",  # Espaciado inferior
                tags$span("Fecha de la entrevista", style = "font-size: 12px; white-space: nowrap;")
              ),
              dateInput(
                inputId = "fecha_entrevista_ts",
                label = NULL,  # Sin etiqueta adicional
                value = "",
                format = "dd/mm/yyyy"  # Formato de la fecha
              )
            )
          ),
          
          h4("Tratamiento elegido", style = "font-size: 15px; font-weight: bold;"),
          
          fluidRow(
            column(
              width = 12,
              selectInput(
                inputId = "tratamiento_elegido",
                label = "",
                choices = c(
                  "Seguimiento","Cdd Baigorria",
                  "Cdd Buen Pastor",
                  "Centro de día Zeballos",
                  "Derivado",
                  "Internación B.P.",
                  "Internación Baig.",
                  "Internación Cristalería",
                  "No finalizó admisión",
                  "Rechaza tratamiento",
                  
                  ""
                ),
                selected = ""
              )
            )
          )
        )
      ),
      
      fluidRow(
        
        # Situación Socioeconómica, Jurídica y de Salud ------------------------------------------------------------
        column(
          width = 7,
          wellPanel( 
            style = "min-height: 460px; margin-top: 20px;",
            
            h4("Situación Socioeconómica, Jurídica y de Salud", style = "font-size: 15px; font-weight: bold;"),
            
            fluidRow(
              # Campo nivel educativo
              column(
                width = 6,
                selectInput(
                  inputId = "nivel_educativo_max",
                  tags$span("Máximo Nivel educativo alcanzado", style = "font-size: 12px;"),
                  choices = list( 
                    "No informado",
                    "Sin instrucción formal", 
                    "Primario incompleto", 
                    "Primario en curso", 
                    "Primario completo", 
                    "Secundario incompleto", 
                    "Secundario en curso", 
                    "Secundario completo", 
                    "Nivel superior incompleto", 
                    "Nivel superior en curso", 
                    "Nivel superior completo",
                    ""
                  ),
                  selected = "")
              ),
              
              # CUD
              column(
                width = 6,
                selectInput(
                  inputId = "cud",
                  tags$span("CUD", style = "font-size: 12px;"),
                  choices = list(
                    "No informado",
                    "Si", 
                    "No", 
                    ""
                  ),
                  selected = "")
              ),
              
              # Situación Habitacional Actual
              column(
                width = 6,
                selectInput(
                  inputId = "situacion_habitacional_actual",
                  tags$span("Situación Habitacional Actual", style = "font-size: 12px;"),
                  choices = list(
                    "No informada",
                    "Casa/Departamento", 
                    "Casa/Departamento alquilado", 
                    "Casa/Departamento cedido", 
                    "Casa/Departamento propio", 
                    "Institución de salud mental", 
                    "Institución penal", 
                    "Institución terapéutica", 
                    "Pensión", 
                    "Refugio", 
                    "Situación de calle", 
                    "Otra", 
                    ""
                  ),
                  selected = "")
              ),
              
              # Campo emergente de texto para "Otra" opción
              column(
                width = 6,
                conditionalPanel(
                  condition = "input.situacion_habitacional_actual.includes('Otra')",
                  textInput(
                    inputId = "otra_situacion_habitacional_actual",
                    label = tags$span("Especifique la situación habitacional", style="font-size: 12px;"),
                    value = ""
                  ) 
                )
              ),
              
              # Situación Laboral Actual
              column(
                width = 6,
                selectInput(
                  inputId = "situacion_laboral_actual",
                  tags$span("Situación Laboral Actual", style = "font-size: 12px;"),
                  choices = list( 
                    "No informado",
                    "Estable", 
                    "Esporádico", 
                    "No tiene",
                    "Otra",
                    ""
                  ),
                  selected = "")
              ),
              
              # Campo emergente de texto para "Otra" opción
              column(
                width = 6,
                conditionalPanel(
                  condition = "input.situacion_laboral_actual.includes('Otra')",
                  textInput(
                    inputId = "otra_situacion_laboral_actual",
                    label = tags$span("Especifique la situación laboral", style="font-size: 12px;"),
                    value = ""
                  ) 
                )
              ),
              
              # Campo de selección múltiple de ingreso económico
              column(
                width = 12,
                tags$div(
                  style = "margin-bottom: 5px;",
                  tags$span("Ingreso Económico", style = "font-size: 12px; white-space: nowrap;")
                ),
                tags$div(
                  style = "column-count: 3; column-gap: 50px; margin-top: 10px;",  # Espacio entre columnas y margen superior
                  checkboxGroupInput(
                    inputId = "ingreso_economico",
                    label = NULL,  # No mostramos la etiqueta aquí porque ya está arriba
                    choices = c(
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
                    selected = NULL
                  )
                )
              ),
              # Campo emergente para texto si selecciona alguna de las opciones "Otro"
              conditionalPanel(
                condition = "input.ingreso_economico.includes('Otro subsidio/plan social') || 
                input.ingreso_economico.includes('Otro tipo de pensión') || 
                input.ingreso_economico.includes('Otro tipo de ingreso')",
                tags$div(
                  style = "margin-top: 20px; margin-left: 10px",
                  textInput(
                    inputId = "otro_ingreso",
                    label = tags$span("Especifique el otro tipo de ingreso", style = "font-size: 12px;"),
                    value = ""
                  )
                )
              ),
              
              # Situación Judicial
              column(
                width = 6,
                tags$div(
                  style = "margin-top: 20px;",  # Ajusta el valor según el espacio que desees
                  selectInput(
                    inputId = "situacion_judicial",
                    tags$span("Situación Judicial", style = "font-size: 12px;"),
                    choices = list(
                      "No informada",
                      "Sin causas", 
                      "Con causa cerrada", 
                      "Con causa abierta", 
                      "Desconoce", 
                       
                      "Otra",
                      ""
                    ),
                    selected = "")
                )
              ),
              
              # Campo emergente de texto para "Otra" opción
              column(
                width = 6,
                tags$div(
                  style = "margin-top: 20px;",
                  conditionalPanel(
                    condition = "input.situacion_judicial.includes('Otra')",
                    textInput(
                      inputId = "otra_situacion_judicial",
                      label = tags$span("Especifique la situación judicial", style="font-size: 12px;"),
                      value = ""
                    )
                  ) 
                )
              )
            )
          )
        ),
        
        # Red de Apoyo y Referencias ------------------------------------------------------------
        column(
          width = 5,
          wellPanel( 
            style = "min-height: 300px; margin-top: 20px;",
            
            h4("Red de Apoyo y Referencias", style = "font-size: 15px; font-weight: bold;"),
            
            fluidRow(
              
              # Campo redes de apoyo
              column(
                width = 12,
                tags$div(
                  style = "margin-bottom: 5px;",
                  tags$span("Redes de Apoyo", style = "font-size: 12px; white-space: nowrap;")
                ),
                tags$div(
                  style = "column-count: 2; column-gap: 50px; margin-top: 10px;",  # Espacio entre columnas y margen superior
                  checkboxGroupInput(
                    inputId = "redes_apoyo",
                    label = NULL,  # No mostramos la etiqueta aquí porque ya está arriba
                    choices = c("No informado",
                                "Familiares", 
                                "Amistades", 
                                "Institucionalidades",
                                "Sin vínculos actualmente"),
                    selected = NULL
                  )
                )
              )
            ),
            
            fluidRow(
              # Campo referencias APS
              column(
                width = 12,  # Cambiado a 12 para que ocupe toda la fila
                style = "margin-top: 10px;",  # Ajusta el valor según el espacio que desees
                selectInput(
                  inputId = "referencia_aps",
                  tags$span("Referencia APS", style = "font-size: 12px;"),
                  choices = list(
                    "No informada","Referencia con seguimiento", 
                    "Referencia sin seguimiento", 
                    "No está referenciado", 
                    
                    ""
                  ),
                  selected = ""
                )
              )
            ),
            
            fluidRow(
              # Campo equipo de referencia
              column(
                width = 12,  # Cambiado a 12 para que ocupe toda la fila
                textInput(
                  inputId = "equipo_referencia",
                  label = tags$span("Equipo de Referencia", style = "font-size: 12px;"),
                  value = ""
                )
              )
            )
          ),
          
          wellPanel( 
            style = "min-height: 160px; margin-top: 20px;",
            
            h4("Información Adicional", style = "font-size: 15px; font-weight: bold;"),
            
            fluidRow(
              # Campo de Observaciones
              column(
                width = 12,  # Cambia el ancho según sea necesario
                textAreaInput(
                  inputId = "observaciones",
                  label = tags$span("Observaciones", style = "font-size: 12px;"),
                  value = "",
                  width = "100%",
                  height = "80px"  # Ajusta la altura según sea necesario
                )
              )
            )
          ),
          tags$div(
            style = "margin-top:10px; margin-bottom:10px;",
            actionButton(
              inputId = "guardar_registro",
              label = "Guardar registro",
              icon = icon("save"),
              class = "btn-primary"
            )
          )
        )
      )
    )
  ),
  
  nav_panel(
    tags$span("Modificación de registros", style = "font-size: 14px;"),
    class = "bslib-page-dashboard",
    icon = icon("pen-to-square"),
      
    fluidPage(
      useShinyjs(),
    
      # Buscador de DNI o Nombre
      fluidRow(
      textInput("search_input", 
                tags$span("Buscar por DNI, Nombre o Apellido",style = "fontsize: 12px;"), 
                width = 300),
      actionButton("search_button", 
                   tags$span("Buscar",style = "fontsize: 12px;"),
                   icon = icon("search"),
                   style = "height: 36px; line-height: 20px; font-size: 12px; margin-top: 24px;", 
                   width = 120,
                   class = "btn-primary")
      ),
      
      conditionalPanel(
        condition = "output.showTable == true",
        
        # Tabla de resultados de búsqueda
        div(dataTableOutput("search_results"),
            style = "font-size:12px"),
        
        # Botones en la esquina inferior derecha de la tabla
        tags$div(
          style = "margin-top: 15px; display: flex; gap: 10px; justify-content: flex-end;",
          actionButton("delete_button", tags$span("Eliminar registro",style = "font-size: 12px;"), 
                       icon = icon("trash"), width = 180,
                       class = "btn-primary"),
          actionButton("modify_button", tags$span("Consultar o modificar registro",style = "font-size: 12px;"), 
                       width = '250px',
                       class = "btn-primary"),
          actionButton("cancel_button", tags$span("Cancelar búsqueda",style = "font-size: 12px;"), 
                       width = '180px',
                       class = "btn-primary")
        )
      ),  
    fluidRow(
      uiOutput("admin_button")
    )
    )
  ),
  
  nav_menu(
    tags$span("Tablero de visualización", style = "font-size: 14px;"),
    icon = icon("chart-simple"),
    
    tabPanel(HTML("<span style='font-size:14px'>Análisis demográfico</span>"),
             
             fluidRow(
               column(
                 width = 2,
                 wellPanel(
                   
                   style = "min-height: 390px;",
                   
                   h4("Filtros", style = "font-size: 15px; font-weight: bold;"), 
                   
                   fluidRow(
                     div(
                       
                       selectInput(
                         "year_filter",
                         label = tags$span("Año del registro:", style = "font-size:15px;"),
                         choices = c("Seleccione el año" = ""), # Texto indicativo inicial
                         selected = "", # Inicia con el texto indicativo seleccionado
                         multiple = TRUE
                       ),
                       
                       # Filtro Edad Categorica
                       checkboxGroupInput(
                         "edad_filter",
                         label = tags$span("Categoría de edad:", style = "font-size:15px;"),
                         choices = c("0 a 12", "13 a 17", "18 a 29", "30 a 60", "+ 60"),
                         selected = c("0 a 12", "13 a 17", "18 a 29", "30 a 60", "+ 60")),
                       
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
                 width = 10,
                 
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
                 width = 2,
                 wellPanel(
                   
                   style = "min-height: 390px;",
                   
                   h4("Filtros", style = "font-size: 15px; font-weight: bold;"),
                   
                   fluidRow(
                     div(
                       
                       # Filtro año
                       selectInput(
                         "year_filter_2",
                         label = tags$span("Año del registro:", style = "font-size:15px;"),
                         choices = c("Seleccione el año" = ""), # Texto indicativo inicial
                         selected = "", # Inicia con el texto indicativo seleccionado
                         multiple = TRUE
                       ),
                       
                       # Filtro Edad Categorica
                       checkboxGroupInput(
                         "edad_filter_2",
                         label = tags$span("Categoría de edad:", style = "font-size:15px;"),
                         choices = c("0 a 12", "13 a 17", "18 a 29", "30 a 60", "+ 60"),
                         selected = c("0 a 12", "13 a 17", "18 a 29", "30 a 60", "+ 60")),
                       
                       # Filtro año
                       selectInput(
                         "nivel_educativo_filter",
                         label = tags$span("Nivel educativo alcanzado:", style = "font-size:15px;"),
                         choices = c("Sin instrucción formal",
                                     "Primario incompleto", "Primario en curso",
                                     "Primario completo", "Secundario incompleto",
                                     "Secundario en curso", "Secundario completo",
                                     "Nivel superior incompleto", "Nivel superior en curso",
                                     "Nivel superior completo"), # Texto indicativo inicial
                         selected = c("Sin instrucción formal",
                                      "Primario incompleto", "Primario en curso",
                                      "Primario completo", "Secundario incompleto",
                                      "Secundario en curso", "Secundario completo",
                                      "Nivel superior incompleto", "Nivel superior en curso",
                                      "Nivel superior completo"), # Inicia con el texto indicativo seleccionado
                         multiple = TRUE # Permite seleccionar múltiples años
                       ),
                       
                     selectInput(
                       "sit_laboral_filter",
                       label = tags$span("Situación laboral actual:", style = "font-size:15px;"),
                       choices = c("Estable", "Esporádico", "No tiene"), # Texto indicativo inicial
                     selected = c("Estable", "Esporádico", "No tiene"), # Inicia con el texto indicativo seleccionado
                     multiple = TRUE # Permite seleccionar múltiples años
                     )
                     )
                     )
                   )
                 ),
               
               column(
                 width = 10,
                 
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
    ),
    
    tabPanel(HTML("<span style='font-size:14px'>Análisis de consumo</span>"),
             
             fluidRow(
               column(
                 width = 2,
                 wellPanel(
                   
                   style = "min-height: 390px;",
                   
                   h4("Filtros", style = "font-size: 15px; font-weight: bold;"), 
                   
                   fluidRow(
                     div(
                       
                       # Filtro año
                       selectInput(
                         "year_filter_3",
                         label = tags$span("Año del registro:", style = "font-size:15px;"),
                         choices = c("Seleccione el año" = ""), # Texto indicativo inicial
                         selected = "", # Inicia con el texto indicativo seleccionado
                         multiple = TRUE
                       )
                   )
                 )
               )
               ),
               
               column(
                 width = 10,
                 
                 h2("Análisis de consumo", style = "font-size: 20px; font-weight: bold;"),
                 fluidRow(
                   column(
                     width = 7,
                       plotlyOutput("histbox.edadinicio", height = "260px")
                   ),
                   column(
                     width = 4,
                     uiOutput("sustancias", height = "260px")
                   )
                 ),
                 
                 fluidRow(
                   style = "margin-top:10px;",
                   uiOutput("tabla_inicio_reg")
                   ),
                 
                 fluidRow(
                   column(
                     width = 6,
                     plotlyOutput("barras_sustancias",
                                  height = "300px")
                   ),
                   column(
                     width = 6,
                     plotlyOutput("barras_edad_sustancias",
                                  height = "300px")
                   )
                 )
                 )
               )
             )
    ),
  nav_panel(
    "",
    class = "bslib-page-dashboard",
    icon = icon("calendar-days"),
    
    fluidRow(
      calendarOutput("calendar")
    )
  ),
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  )
)

ui <- secure_app(ui,
                 timeout = 30,
                 theme = shinythemes::shinytheme("flatly"),
                 background  = "linear-gradient(rgba(225,225,225, 0.5),
                 rgba(225,225,225,0.5)));")

server <- function(input, output, session) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # Cargar la base de datos
  data <- base()
  
  # Definir una variable reactiva para los datos de la base
  base_reactiva <- reactiveVal()
  
  # Función para cargar los datos del Excel
  cargar_base <- function() {
    read_excel("Base completa.xlsx")
  }
  
  # Inicializar la base reactiva al iniciar la app
  observe({
    base_reactiva(cargar_base())
  })
  
  # ----------------------------------------------------------------------------
  # nuevo registro
  # ----------------------------------------------------------------------------
  
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
  
  # DNI ------------------------------------------------------------------------
  iv_dni <- InputValidator$new()
  
  ## Campo obligatorio
  iv_dni$add_rule("dni", sv_required(
    tags$span("Campo obligatorio.",
              style = "font-size: 10px;")
  )
  )
  
  ## Solo números (sin puntos, espacios, etc.)
  iv_dni$add_rule("dni", function(value) {
    if (!grepl("^[0-9]+$", value)) {
      return("El DNI solo puede contener números, sin puntos ni caracteres especiales.")
    }
    return(NULL)  # Si pasa la validación, no devuelve errores
  })
  
  ## 8 dígitos
  iv_dni$add_rule("dni", function(value) {
    if (nchar(value) != 8) {
      return("El DNI debe tener exactamente 8 dígitos.")
    }
    return(NULL)
  })
  
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
  
  # Edad -----------------------------------------------------------------------
  
  iv_edad <- InputValidator$new()
  
  iv_edad$add_rule("edad", function(value) {
    if(!isTruthy(input$fecha_nacimiento)) {
      if(length(as.numeric(input$edad)) == 0 | is.na(input$edad)) {
        return(tags$span("Campo obligatorio.", style = "font-size: 10px;"))
      }
    }
  })
  
  iv_edad$add_rule("edad", function(value) {
    if(!is.na(value)) {
      if (value < 1) {
        return("La edad debe tener 1 o 2 dígitos.")
      }
      if (value > 99) {
        return("La edad debe tener 1 o 2 dígitos.")
      }
      return(NULL)
    }
  })
  
  ## 8 dígitos
  iv_edad$add_rule("edad", function(value) {
    if(!is.na(value)) {
      if (!grepl("^[0-9]+$", value)) {
        return("No se admiten caracteres especiales.")
      }
      return(NULL)
    }
  })
  
  iv_edad$enable()
  
  observe({
    # Verificar que ambas fechas estén registradas
    if (isTruthy(input$fecha_nacimiento) && isTruthy(input$fecha_registro)) {
      
      # Calcular la edad en años usando fecha de registro como referencia
      edad <- trunc(as.numeric(difftime(as.Date(input$fecha_registro), as.Date(input$fecha_nacimiento), units = "days")) %/% 365)
      
    } else {
      # Si falta alguna de las fechas, dejar el campo en NA
      edad <- NA
    }
    
    # Actualizar el campo de `edad` en el formulario
    updateNumericInput(session, "edad", value = edad)
  })
  
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
    if (nchar(value) > 11) {
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
    # Opciones predefinidas
    opciones_validas <- c("", "Propio", "Papá/Mamá", "Hermano/a", "Hijo/a", "Amigo/a")
    
    # Si el valor está en las opciones válidas, no se necesita validación adicional
    if (value %in% opciones_validas) {
      return(NULL)
    }
    
    # Si no está vacío y se intenta crear una nueva opción
    if (value != "") {
      if (nchar(value) < 2) {
        return("El campo debe tener al menos 2 caracteres.")
      }
      # Expresión regular para permitir solo letras y espacios
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {
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
    # Opciones predefinidas
    opciones_validas <- c("", "Propio", "Papá/Mamá", "Hermano/a", "Hijo/a", "Amigo/a")
    
    # Si el valor está en las opciones válidas, no se necesita validación adicional
    if (value %in% opciones_validas) {
      return(NULL)
    }
    
    # Si no está vacío y se intenta crear una nueva opción
    if (value != "") {
      if (nchar(value) < 2) {
        return("El campo debe tener al menos 2 caracteres.")
      }
      # Expresión regular para permitir solo letras y espacios
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {
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
    # Opciones predefinidas
    opciones_validas <- c("", "Propio", "Papá/Mamá", "Hermano/a", "Hijo/a", "Amigo/a")
    
    # Si el valor está en las opciones válidas, no se necesita validación adicional
    if (value %in% opciones_validas) {
      return(NULL)
    }
    
    # Si no está vacío y se intenta crear una nueva opción
    if (value != "") {
      if (nchar(value) < 2) {
        return("El campo debe tener al menos 2 caracteres.")
      }
      # Expresión regular para permitir solo letras y espacios
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {
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
        return(tags$span("No se admiten caracteres especiales.", style = "font-size: 10px;"))
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
  
  # Entrevista psicologo - Estado ----------------------------------------------
  iv_estado_psicologo <- InputValidator$new()
  
  ## Obligatorio
  iv_estado_psicologo$add_rule("estado_psicologo", sv_required(message = tags$span("Campo obligatorio", style = "font-size: 10px;")))
  
  iv_estado_psicologo$enable()
  
  # Entrevista psiquiatra - Estado ---------------------------------------------
  
  iv_estado_psiquiatra <- InputValidator$new()
  
  ## Obligatorio
  iv_estado_psiquiatra$add_rule("estado_psiquiatra", sv_required(message = tags$span("Campo obligatorio", style = "font-size: 10px;")))
  
  iv_estado_psiquiatra$enable()
  
  # Entrevista ts - Estado -----------------------------------------------------
  
  iv_estado_ts <- InputValidator$new()
  
  ## Obligatorio
  iv_estado_ts$add_rule("estado_ts", sv_required(message = tags$span("Campo obligatorio", style = "font-size: 10px;")))
  
  iv_estado_ts$enable()
  
  # Entrevista psicologo - Fecha -----------------------------------------------
  
  # Validaciones dinámicas para el campo de fecha de la entrevista
  iv_fecha_psicologo <- InputValidator$new()
  
  ## Regla para "Presente" o "Ausente" -> la fecha no puede ser futura
  iv_fecha_psicologo$add_rule("fecha_entrevista_psicologo", function(value) {
    estado <- input$estado_psicologo
    
    # Verificamos si el campo es obligatorio para este estado
    if (estado %in% c("Presente", "Ausente")) {
      
      # Convertimos el valor a Date si no es NULL o vacío
      fecha <- as.Date(value, format = "%Y-%m-%d")
      
      if (length(fecha) == 0) {
        return(tags$span("La fecha es obligatoria para 'Presente' o 'Ausente'.", style = "font-size:10px;"))
      }
      
      # Validamos que la fecha no sea futura
      if (!is.na(fecha) && fecha > Sys.Date()) {
        return(tags$span("La fecha no puede ser futura seleccionando 'Presente' o 'Ausente'.", style = "font-size:10px;"))
      }
    }
  })
  
  ## Regla para "Pendiente" -> la fecha debe ser futura
  iv_fecha_psicologo$add_rule("fecha_entrevista_psicologo", function(value) {
    estado <- input$estado_psicologo
    
    # Si está pendiente, verificamos si la fecha es futura
    if (estado == "Pendiente") {
      
      # Convertimos el valor a Date si no es NULL o vacío
      fecha <- as.Date(value, format = "%Y-%m-%d")
      
      if (length(fecha) == 0) {
        return(tags$span("La fecha es obligatoria para 'Pendiente'.", style = "font-size:10px;"))
      }
      
      # Validamos que la fecha sea futura
      if (!is.na(fecha) && fecha <= Sys.Date()) {
        return(tags$span("La fecha debe ser futura seleccionando 'Pendiente'.", style = "font-size:10px;"))
      }
    }
  })
  
  ## Regla para "No necesaria" o "No asignada" -> la fecha no es obligatoria
  observeEvent(input$estado_psicologo, {
    # Si la opción es "", No o S/D, deshabilitar el campo y eliminar la validación
    if (input$estado_psicologo %in% c("","No asignada", "No necesaria")) {
      updateDateInput(session, "fecha_entrevista_psicologo", value = NA)
      shinyjs::disable("fecha_entrevista_psicologo")
    } else {
      shinyjs::enable("fecha_entrevista_psicologo")
    }
  })
  
  # Activar validaciones
  iv_fecha_psicologo$enable()
  
  # Entrevista psiquiatra - Fecha ----------------------------------------------
  
  iv_fecha_psiquiatra <- InputValidator$new()
  
  # Validaciones dinámicas para el campo de fecha de la entrevista
  iv_fecha_psiquiatra <- InputValidator$new()
  
  ## Regla para "Presente" o "Ausente" -> la fecha no puede ser futura
  iv_fecha_psiquiatra$add_rule("fecha_entrevista_psiquiatra", function(value) {
    estado <- input$estado_psiquiatra
    
    # Verificamos si el campo es obligatorio para este estado
    if (estado %in% c("Presente", "Ausente")) {
      
      # Convertimos el valor a Date si no es NULL o vacío
      fecha <- as.Date(value, format = "%Y-%m-%d")
      
      if (length(fecha) == 0) {
        return(tags$span("La fecha es obligatoria para 'Presente' o 'Ausente'.", style = "font-size:10px;"))
      }
      
      # Validamos que la fecha no sea futura
      if (!is.na(fecha) && fecha > Sys.Date()) {
        return(tags$span("La fecha no puede ser futura seleccionando 'Presente' o 'Ausente'.", style = "font-size:10px;"))
      }
    }
  })
  
  ## Regla para "Pendiente" -> la fecha debe ser futura
  iv_fecha_psiquiatra$add_rule("fecha_entrevista_psiquiatra", function(value) {
    estado <- input$estado_psiquiatra
    
    # Si está pendiente, verificamos si la fecha es futura
    if (estado == "Pendiente") {
      
      # Convertimos el valor a Date si no es NULL o vacío
      fecha <- as.Date(value, format = "%Y-%m-%d")
      
      if (length(fecha) == 0) {
        return(tags$span("La fecha es obligatoria para 'Pendiente'.", style = "font-size:10px;"))
      }
      
      # Validamos que la fecha sea futura
      if (!is.na(fecha) && fecha <= Sys.Date()) {
        return(tags$span("La fecha debe ser futura seleccionando 'Pendiente'.", style = "font-size:10px;"))
      }
    }
  })
  
  ## Regla para "No necesaria" o "No asignada" -> la fecha no es obligatoria
  observeEvent(input$estado_psiquiatra, {
    # Si la opción es "", No o S/D, deshabilitar el campo y eliminar la validación
    if (input$estado_psiquiatra %in% c("","No asignada", "No necesaria")) {
      updateDateInput(session, "fecha_entrevista_psiquiatra", value = NA)
      shinyjs::disable("fecha_entrevista_psiquiatra")
    } else {
      shinyjs::enable("fecha_entrevista_psiquiatra")
    }
  })
  
  # Activar validaciones
  iv_fecha_psiquiatra$enable()
  
  # Entrevista ts - Fecha ------------------------------------------------------
  
  iv_fecha_ts <- InputValidator$new()
  
  # Validaciones dinámicas para el campo de fecha de la entrevista
  iv_fecha_ts <- InputValidator$new()
  
  ## Regla para "Presente" o "Ausente" -> la fecha no puede ser futura
  iv_fecha_ts$add_rule("fecha_entrevista_ts", function(value) {
    estado <- input$estado_ts
    
    # Verificamos si el campo es obligatorio para este estado
    if (estado %in% c("Presente", "Ausente")) {
      
      # Convertimos el valor a Date si no es NULL o vacío
      fecha <- as.Date(value, format = "%Y-%m-%d")
      
      if (length(fecha) == 0) {
        return(tags$span("La fecha es obligatoria para 'Presente' o 'Ausente'.", style = "font-size:10px;"))
      }
      
      # Validamos que la fecha no sea futura
      if (!is.na(fecha) && fecha > Sys.Date()) {
        return(tags$span("La fecha no puede ser futura seleccionando 'Presente' o 'Ausente'.", style = "font-size:10px;"))
      }
    }
  })
  
  ## Regla para "Pendiente" -> la fecha debe ser futura
  iv_fecha_ts$add_rule("fecha_entrevista_ts", function(value) {
    estado <- input$estado_ts
    
    # Si está pendiente, verificamos si la fecha es futura
    if (estado == "Pendiente") {
      
      # Convertimos el valor a Date si no es NULL o vacío
      fecha <- as.Date(value, format = "%Y-%m-%d")
      
      if (length(fecha) == 0) {
        return(tags$span("La fecha es obligatoria para 'Pendiente'.", style = "font-size:10px;"))
      }
      
      # Validamos que la fecha sea futura
      if (!is.na(fecha) && fecha <= Sys.Date()) {
        return(tags$span("La fecha debe ser futura seleccionando 'Pendiente'.", style = "font-size:10px;"))
      }
    }
  })
  
  ## Regla para "No necesaria" o "No asignada" -> la fecha no es obligatoria
  observeEvent(input$estado_ts, {
    # Si la opción es "", No o S/D, deshabilitar el campo y eliminar la validación
    if (input$estado_ts %in% c("","No asignada", "No necesaria")) {
      updateDateInput(session, "fecha_entrevista_ts", value = NA)
      shinyjs::disable("fecha_entrevista_ts")
    } else {
      shinyjs::enable("fecha_entrevista_ts")
    }
  })
  
  # Activar validaciones
  iv_fecha_ts$enable()
  
  # Información consumo - Edad de inicio ---------------------------------------
  
  iv_edad_inicio <- InputValidator$new()
  iv_edad_inicio$add_rule("edad_inicio_consumo", function(value) {
    if (value != "") {
      if(nchar(value) > 2) {
        return(tags$span("La edad no puede tener más de 2 dígitos.", style = "font-size:10px;"))
      }
      if(!grepl("^[0-9]+$", value)) {
        return(tags$span("Solo se admiten números.", style = "font-size:10px;"))
      }
      if(!is.na(value) & !is.na(input$edad) & input$edad < value) {
        return(tags$span("La edad de inicio no puede ser mayor a la actual.", style = "font-size:10px;"))
      }
    }
    return(NULL)
  })
  iv_edad_inicio$enable()
  
  # Información consumo - Sustancia de inicio ----------------------------------
  
  iv_sustancia_inicio <- InputValidator$new()
  
  iv_sustancia_inicio$add_rule("sustancia_inicio_consumo", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))
  
  iv_sustancia_inicio$add_rule("otra_sustancia", function(value) {
    # Verificar si 'Otra' está seleccionada en el selectInput
    if ("Otra" %in% input$sustancia_inicio_consumo) {
      
      # Validar si el campo está vacío o contiene caracteres especiales
      if (is.null(value) || value == "") {
        return(tags$span("Debe completar el campo si selecciona 'Otra'.", style = "font-size:10px;"))
      } else if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {
        return(tags$span("No se admiten caracteres especiales.", style = "font-size:10px;"))
      }
      
    } else {
      # Si no está seleccionada la opción "Otra", no hacer ninguna validación
      return(NULL)
    }
  })
  
  iv_sustancia_inicio$enable()
  
  # Información consumo - Consumo actual ---------------------------------------
  
  iv_persona_consume <- InputValidator$new()
  
  iv_persona_consume$add_rule("persona_consume", sv_required(tags$span("Campo obligatorio.", style = "font-size: 10px;")))
  
  iv_persona_consume$enable()
  
  # Información consumo - Sustancia de consumo actual --------------------------
  
  iv_sustancias_actual <- InputValidator$new()
  
  ## Obligatorio
  
  iv_sustancias_actual$add_rule("sustancias_consumo_actual", function(value) {
    if(input$persona_consume == "Si") {
      if(is.null(value) || length(value) == 0) {
        return(tags$span("Campo obligatorio.", style = "font-size: 10px;"))
      }
    }
  })
  
  iv_sustancias_actual$add_rule("otra_sustancia_actual", function(value) {
    # Verificar si 'Otra' está seleccionada en el selectInput
    if ("Otra" %in% input$sustancias_consumo_actual) {
      
      # Validar si el campo está vacío o contiene caracteres especiales
      if (is.null(value) || value == "") {
        return(tags$span("Debe completar el campo si selecciona 'Otra'.", style = "font-size:10px;"))
      } else if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {
        return(tags$span("No se admiten caracteres especiales.", style = "font-size:10px;"))
      }
      
    } else {
      # Si no está seleccionada la opción "Otra", no hacer ninguna validación
      return(NULL)
    }
  })
  
  iv_sustancias_actual$enable()
  
  
  # Información tratamiento - Derivación ---------------------------------------
  
  iv_derivacion <- InputValidator$new()
  
  ## Obligatorio
  iv_derivacion$add_rule("derivacion", sv_required(tags$span("Campo obligatorio.",style = "font-size:10px;")))
  
  iv_derivacion$enable()
  
  # Información tratamiento - Derivado de --------------------------------------
  
  iv_derivado_de <- InputValidator$new()
  
  ## Obligatorio
  iv_derivado_de$add_rule("derivado_de", function(value) {
    if(input$derivacion == "Si" & nchar(value) == 0) {
      return(tags$span("Campo obligatorio.",style = "font-size:10px;"))
      if (!is.null(input$derivacion) && input$derivacion == "Si") {
        if (nchar(value) < 2 & nchar(value) > 0) {
          return(tags$span("El campo debe tener al menos 2 caracteres.",style = "font-size:10px;"))
        }
        if (grepl("[^a-zA-Z0-9 ]", value)) {
          return(tags$span("No se admiten caracteres especiales.",style = "font-size:10px;"))
        }
      }
    } else if (input$derivacion %in% c("No","No informado")) {
      if(value != "") {
        return(tags$span("El campo debe estar vacío.",style = "font-size:10px;"))
      }
    }
  })
  
  iv_derivado_de$enable()
  
  # Información tratamiento - Nº de tratameintos previos -----------------------
  
  iv_tratamientos_previos <- InputValidator$new()
  
  ## Obligatorio
  iv_tratamientos_previos$add_rule("num_tratamientos_previos", function(value) {
    if(is.na(value)) {
      return() }
    else {
      if (value < 0 || value > 99) {
        return(tags$span("El número debe estar entre 0 y 99.",style = "font-size:10px;"))
        }
    }
  })

iv_tratamientos_previos$enable()

# Información tratamiento - Lugar de último tratameinto ----------------------

iv_lugar_ultimo_tratamiento <- InputValidator$new()
iv_lugar_ultimo_tratamiento$add_rule("lugar_ultimo_tratamiento", function(value) {
  # Validar solo si "Número de Tratamientos previos" tiene un valor y es mayor que 0
  if (!is.null(input$num_tratamientos_previos) && !is.na(input$num_tratamientos_previos) && input$num_tratamientos_previos > 0) {
    if (nchar(value) < 2) {
      return(tags$span("El campo debe tener al menos 2 caracteres.", style = "font-size:10px;"))
    }
    if (grepl("[^a-zA-Z0-9 ]", value)) {
      return(tags$span("No se admiten caracteres especiales.",style = "font-size:10px;"))
    }
  }
  return(NULL)  # Sin errores
})

iv_lugar_ultimo_tratamiento$enable()

# Información tratamiento - Tratameinto elegido ------------------------------

iv_tratamiento_elegido <- InputValidator$new()
iv_tratamiento_elegido$add_rule("tratamiento_elegido", sv_required("Campo obligatorio"))
iv_tratamiento_elegido$add_rule("tratamiento_elegido", function(value) {
  if (is.null(value) || value == "") {
    return(tags$span("Campo obligatorio.",style = "font-size:10px;"))
  }
  return(NULL)  # Sin errores
})

iv_tratamiento_elegido$enable()

# Situación Socioeconómica, Jurídica y de Salud - Educación ------------------

iv_nivel_educativo_max <- InputValidator$new()
iv_nivel_educativo_max$add_rule("nivel_educativo_max", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))

iv_nivel_educativo_max$enable()

# Situación Socioeconómica, Jurídica y de Salud - CUD ------------------------

iv_cud <- InputValidator$new()
iv_cud$add_rule("cud", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))

iv_cud$enable()

# Situación Socioeconómica, Jurídica y de Salud - Situción habitacional ------

iv_situacion_habitacional <- InputValidator$new()
iv_situacion_habitacional$add_rule("situacion_habitacional_actual", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))

iv_situacion_habitacional$add_rule("otra_situacion_habitacional_actual", function(value) {
  if (input$situacion_habitacional_actual == "Otra") {
    if (is.null(value) || value == "") {
      return(tags$span("Debe completar el campo si selecciona 'Otra'.",style = "font-size:10px;"))
    }
    if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {
      return(tags$span("No se admiten caracteres especiales.",style="font-size:10px;"))
    }
  }
  return(NULL)
})

iv_situacion_habitacional$enable()  

# Situación Socioeconómica, Jurídica y de Salud - Situación laboral --------------------------------------------------------

iv_situacion_laboral_actual <- InputValidator$new()
iv_situacion_laboral_actual$add_rule("situacion_laboral_actual", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))

iv_situacion_laboral_actual$add_rule("otra_situacion_laboral_actual", function(value) {
  if (input$situacion_laboral_actual == "Otra") {
    if (is.null(value) || value == "") {
      return(tags$span("Debe completar el campo si selecciona 'Otra'.",style = "font-size:10px;"))
    }
    if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {
      return(tags$span("No se admiten caracteres especiales.",style="font-size:10px;"))
    }
  }
  return(NULL)
})

iv_situacion_laboral_actual$enable()

# Situación Socioeconómica, Jurídica y de Salud - Ingreso económico --------------------------------------------------------

iv_ingreso_economico <- InputValidator$new()
iv_ingreso_economico$add_rule("ingreso_economico", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))

iv_ingreso_economico$add_rule("otro_ingreso", function(value) {
  if (any(c("Otro subsidio/plan social", "Otro tipo de pensión", "Otro tipo de ingreso") %in% input$ingreso_economico)) {
    if (value == "") {
      return(tags$span("Debe completar el campo si seleccionó 'Otro subsidio/plan social', 'Otro tipo de pensión' o 'Otro tipo de ingreso'.",style = "font-size: 10px;"))
    }
    if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {
      return(tags$span("No se admiten caracteres especiales.",style="font-size:10px;"))
    }
  }
  return(NULL)
})

iv_ingreso_economico$enable()

# Situación Socioeconómica, Jurídica y de Salud - Situción judicial ----------

iv_situacion_judicial <- InputValidator$new()
iv_situacion_judicial$add_rule("situacion_judicial", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))

iv_situacion_judicial$add_rule("otra_situacion_judicial", function(value) {
  if (input$situacion_judicial == "Otra" && (is.null(value) || value == "")) {
    return(tags$span("Debe completar el campo si selecciona 'Otra'.",style = "font-size:10px;"))
  }
  return(NULL)
})

iv_situacion_judicial$enable() 

# Redes de Apoyo y Referencias - Redes de apoyo ------------------------------

iv_redes_apoyo <- InputValidator$new()
iv_redes_apoyo$add_rule("redes_apoyo", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))

iv_redes_apoyo$enable() 

# Redes de Apoyo y Referencias - Referencias APS -----------------------------

iv_referencia_aps <- InputValidator$new()
iv_referencia_aps$add_rule("referencia_aps", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))

iv_referencia_aps$enable() 

# Redes de Apoyo y Referencias - Equipo de referencia ------------------------

iv_equipo_referencia <- InputValidator$new()
iv_equipo_referencia$add_rule("equipo_referencia", function(value) {
  # Verificar si el campo debe ser obligatorio
  if (input$referencia_aps %in% c("Referencia con seguimiento", "Referencia sin seguimiento")) {
    if (is.null(value) || value == "") {
      return("El campo es obligatorio.")
    }
  }
  if (nchar(value) > 0) {
    if (!grepl("^[a-zA-Z0-9 ]+$", value)) {
      return("No se admiten caracteres especiales.")
    }
    if (nchar(value) < 3) {
      return("El campo debe tener al menos 3 caracteres.")
    }
  }
  if (input$referencia_aps %in% c("No está referenciado", "No informada") && value != "") {
    return("El campo debe estar vacío si la referencia APS es 'No está referenciado' o 'No informada'.")
  }
  
  return(NULL)
}) 

iv_equipo_referencia$enable() 

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
      
      apellido_nombre <- last(dni_existente$`Apellido, Nombre`)  # Obtener el último apellido y nombre para ese DNI
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
      
      edad_inicio_consumo <- last(dni_existente$`Edad de Inicio de Consumo`)
      updateTextInput(session, "edad_inicio_consumo", value = edad_inicio_consumo)
      
      sustancia_inicio_consumo <- ifelse(is.na(last(dni_existente$`Sustancia de inicio`)), "No informado", last(dni_existente$`Sustancia de inicio`))
      updateSelectInput(session, "sustancia_inicio_consumo", selected = sustancia_inicio_consumo)
      
      if (sustancia_inicio_consumo == "Otra") {
        seleccion_otra = last(dni_existente$`Inicio con Otras - Descripción`)
        updateTextInput(session, "otra_sustancia", value = seleccion_otra)
      }
      
      derivacion <- last(dni_existente$Derivación)
      updateTextInput(session, "derivacion", value = derivacion)
      
      derivado_de <- last(dni_existente$`Derivado de`)
      updateTextInput(session, "derivado_de", value = derivado_de)
      
      num_tratamientos_previos <- last(dni_existente$`Número de Tratamientos Previos`)
      updateNumericInput(session, "num_tratamientos_previos", value = num_tratamientos_previos)
      
      lugar_ultimo_tratamiento <- last(dni_existente$`Lugar de Último Tratamiento`)
      updateTextInput(session, "lugar_ultimo_tratamiento", value = lugar_ultimo_tratamiento)
      
      tratamiento_elegido <- last(dni_existente$`Tratamiento Elegido`)
      updateSelectInput(session, "tratamiento_elegido", selected = tratamiento_elegido)
      
    } else if (nrow(dni_existente) == 0) {
      
      iv_dni$enable()
      
      # Si el DNI no está en la base, asignar un nuevo ID de persona (máximo + 1)
      id_persona <- max(data$`ID de la persona`, na.rm = TRUE) + 1
      
      # Eliminar el mensaje si no se encuentra el DNI
      output$dni_message <- renderText({ "" })
      
      # Restablecer los campos a sus valores iniciales si el DNI no está en la base
      updateDateInput(session, "fecha_primer_registro", value = Sys.Date())
      updateTextInput(session, "apellido_nombre", value = "")
      updateSelectInput(session, "sexo_biologico", selected = "")
      updateSelectInput(session, "genero", selected = "")
      updateDateInput(session, "fecha_nacimiento", value = NA)
      updateSelectInput(session, "provincia", selected = "")
      updateSelectInput(session, "localidad", choices = NULL, selected = NULL)
      updateTextInput(session, "barrio", value = "")
      updateTextInput(session, "edad_inicio_consumo", value = "")
      updateSelectInput(session, "sustancia_inicio_consumo", selected = "")
      updateTextInput(session, "otra_sustancia", value = "")
      updateTextInput(session, "derivacion", value = "")
      updateTextInput(session, "derivado_de", value = "")
      updateNumericInput(session, "num_tratamientos_previos", value = "")
    }
  }
  
  # Actualizar el campo ID de la persona
  updateTextInput(session, "id_persona", value = as.numeric(id_persona))
})

# GUARDAR NUEVO REGISTRO

validadores <- list(iv_fecha_registro, iv_fecha_primer_registro, iv_recuerda_dni,
                    iv_dni, iv_apellido_nombre, iv_edad, iv_sexo_biologico,
                    iv_genero, iv_provincia, iv_barrio, iv_telefono_1,
                    iv_telefono_2, iv_telefono_3, iv_vinculo_1, iv_vinculo_2,
                    iv_vinculo_3, iv_nombre_1, iv_nombre_2, iv_nombre_3,
                    iv_estado_ts, iv_estado_psiquiatra, iv_estado_psicologo,
                    iv_fecha_ts, iv_fecha_psiquiatra, iv_fecha_psicologo,
                    iv_edad_inicio, iv_sustancia_inicio, iv_persona_consume,
                    iv_sustancias_actual, iv_derivacion, iv_derivado_de,
                    iv_tratamientos_previos, iv_lugar_ultimo_tratamiento,
                    iv_tratamiento_elegido, iv_nivel_educativo_max,
                    iv_cud, iv_situacion_habitacional, iv_situacion_laboral_actual,
                    iv_situacion_judicial, iv_ingreso_economico, iv_redes_apoyo,
                    iv_referencia_aps, iv_equipo_referencia)

observeEvent(input$guardar_registro, {
  
  nuevo_registro <- list(
    
    # Datos de registro --------------------------------------------------
    `ID de registro` = ifelse(isTruthy(input$id_registro), as.numeric(input$id_registro), NA),
    `Fecha de registro` = ifelse(isTruthy(input$fecha_registro), as.character(input$fecha_registro), NA),
    `ID de la persona` = as.numeric(input$id_persona),
    
    # Datos de la persona ------------------------------------------------
    `Recuerda DNI` = ifelse(isTruthy(input$recuerda_dni), input$recuerda_dni, NA),
    DNI = ifelse(isTruthy(input$dni), input$dni, NA),
    `Apellido, Nombre` = ifelse(isTruthy(input$apellido_nombre), input$apellido_nombre, NA),
    `Fecha de Nacimiento` = ifelse(isTruthy(input$fecha_nacimiento), as.character(input$fecha_nacimiento), NA),
    `Edad del registro` = ifelse(isTruthy(input$edad), as.numeric(input$edad), NA),
    `Sexo biológico` = ifelse(isTruthy(input$sexo_biologico), input$sexo_biologico, NA),
    `Género` = ifelse(isTruthy(input$genero), input$genero, NA),
    Provincia = ifelse(isTruthy(input$provincia), input$provincia, NA),
    Localidad = ifelse(isTruthy(input$localidad), input$localidad, NA),
    Barrio = ifelse(isTruthy(input$barrio), input$barrio, NA),
    
    # Contacto 1 ---------------------------------------------------------
    `Teléfono de Contacto 1` = ifelse(isTruthy(input$telefono_contacto_1), as.numeric(input$telefono_contacto_1), NA),
    `Tipo de Vínculo con el Contacto 1` = ifelse(isTruthy(input$tipo_vinculo_contacto_1), input$tipo_vinculo_contacto_1, NA),
    `Nombre del Contacto 1` = ifelse(isTruthy(input$nombre_contacto_1), input$nombre_contacto_1, NA),
    
    # Contacto 2 ---------------------------------------------------------
    `Teléfono de Contacto 2` = ifelse(isTruthy(input$telefono_contacto_2), as.numeric(input$telefono_contacto_2), NA),
    `Tipo de Vínculo con el Contacto 2` = ifelse(isTruthy(input$tipo_vinculo_contacto_2), input$tipo_vinculo_contacto_2, NA),
    `Nombre del Contacto 2` = ifelse(isTruthy(input$nombre_contacto_2), input$nombre_contacto_2, NA),
    
    # Contacto 3 ---------------------------------------------------------
    `Teléfono de Contacto 3` = ifelse(isTruthy(input$telefono_contacto_3), as.numeric(input$telefono_contacto_3), NA),
    `Tipo de Vínculo con el Contacto 3` = ifelse(isTruthy(input$tipo_vinculo_contacto_3), input$tipo_vinculo_contacto_3, NA),
    `Nombre del Contacto 3` = ifelse(isTruthy(input$nombre_contacto_3), input$nombre_contacto_3, NA),
    
    # Entrevista Psicólogo -----------------------------------------------
    `Estado de la Entrevista con Psicólogo` = ifelse(isTruthy(input$estado_psicologo), input$estado_psicologo, NA),
    `Fecha de la Entrevista con Psicológo` = input$fecha_entrevista_psicologo,
    
    # Entrevista Psiquiatra -----------------------------------------------
    `Estado de la Entrevista con Psiquiátra` = ifelse(isTruthy(input$estado_psiquiatra), input$estado_psiquiatra, NA),
    `Fecha de la Entrevista con Psiquiátra` = input$fecha_entrevista_psiquiatra,
    
    # Entrevista trabajador social ----------------------------------------
    `Estado de la Entrevista con Trabajador Social` = ifelse(isTruthy(input$estado_ts), input$estado_ts, NA),
    `Fecha de la Entrevista con Trabajador Social` = input$fecha_entrevista_ts,
    
    # Inicio del consumo -------------------------------------------------
    `Edad de Inicio de Consumo` = ifelse(isTruthy(input$edad_inicio_consumo), as.numeric(input$edad_inicio_consumo), NA),
    `Sustancia de inicio` = ifelse(isTruthy(input$sustancia_inicio_consumo), input$sustancia_inicio_consumo, NA),
    `Inicio con Otras - Descripción` = ifelse(isTruthy(input$otra_sustancia), input$otra_sustancia, NA),
    
    # Consumo actual -----------------------------------------------------
    `¿Consume actualmente?` = ifelse(isTruthy(input$persona_consume), input$persona_consume, NA),
    `Consumo actual con Alcohol` = ifelse(isTruthy(input$sustancias_consumo_actual) && "Alcohol" %in% input$sustancias_consumo_actual, "Si", NA),
    `Consumo actual con Cocaína` = ifelse(isTruthy(input$sustancias_consumo_actual) && "Cocaína" %in% input$sustancias_consumo_actual, "Si", NA),
    `Consumo actual con Crack` = ifelse(isTruthy(input$sustancias_consumo_actual) && "Crack" %in% input$sustancias_consumo_actual, "Si", NA),
    `Consumo actual con Marihuana` = ifelse(isTruthy(input$sustancias_consumo_actual) && "Marihuana" %in% input$sustancias_consumo_actual, "Si", NA),
    `Consumo actual con Nafta Aspirada` = ifelse(isTruthy(input$sustancias_consumo_actual) && "Nafta" %in% input$sustancias_consumo_actual, "Si", NA),
    `Consumo actual con Pegamento` = ifelse(isTruthy(input$sustancias_consumo_actual) && "Pegamento" %in% input$sustancias_consumo_actual, "Si", NA),
    `Consumo actual con Psicofármacos` = ifelse(isTruthy(input$sustancias_consumo_actual) && "Psicofármacos" %in% input$sustancias_consumo_actual, "Si", NA),
    `Consumo actual con Otras` = ifelse(isTruthy(input$sustancias_consumo_actual) && "Otra" %in% input$sustancias_consumo_actual, "Si", NA),
    `Consumo actual con Otras - Descripción` = ifelse(isTruthy(input$otra_sustancia_actual), input$otra_sustancia_actual, NA),
    `Consumo actual con Policonsumo` = ifelse(length(input$sustancias_consumo_actual) > 1, "Policonsumo", NA),
    
    # Tratamiento --------------------------------------------------------
    Derivación = ifelse(isTruthy(input$derivacion), input$derivacion, NA),
    `Derivado de`= ifelse(isTruthy(input$derivado_de), input$derivado_de, NA),
    `Número de Tratamientos Previos` = ifelse(isTruthy(input$num_tratamientos_previos), as.numeric(input$num_tratamientos_previos), NA),
    `Lugar de Último Tratamiento` = ifelse(isTruthy(input$lugar_ultimo_tratamiento), input$lugar_ultimo_tratamiento, NA),
    `Tratamiento Elegido` = ifelse(isTruthy(input$tratamiento_elegido), input$tratamiento_elegido, NA),
    
    # Situación Socioeconómica, Jurídica y de Salud -----------------------
    `Nivel Máximo Educativo Alcanzado` = ifelse(isTruthy(input$nivel_educativo_max), input$nivel_educativo_max, NA),
    CUD = ifelse(isTruthy(input$cud), input$cud, NA),
    `Situación Habitacional Actual` = ifelse(isTruthy(input$situacion_habitacional_actual), input$situacion_habitacional_actual, NA),
    `Situación Habitacional Actual - Otra` = ifelse(isTruthy(input$otra_situacion_habitacional_actual), input$otra_situacion_habitacional_actual, NA),
    `Situación Laboral Actual`  = ifelse(isTruthy(input$situacion_laboral_actual), input$situacion_laboral_actual, NA),
    `Situación Laboral Actual - Otra` = ifelse(isTruthy(input$otra_situacion_laboral_actual), input$otra_situacion_laboral_actual, NA),
    `Ingresos Económicos` = ifelse(!is.null(input$ingreso_economico), paste(input$ingreso_economico, collapse = ", "), NA),
    `Ingresos Económicos - Otros` = ifelse(!is.null(input$otro_ingreso), input$otro_ingreso, NA),
    `Situación Judicial` = ifelse(isTruthy(input$situacion_judicial), input$situacion_judicial, NA),
    `Situación Judicial - Otro` = ifelse(isTruthy(input$otra_situacion_judicial), input$otra_situacion_judicial, NA),
    
    # Red de Apoyo y Referencias ------------------------------------------
    `Redes de Apoyo` = ifelse(!is.null(input$redes_apoyo), paste(input$redes_apoyo, collapse = ", "), NA),
    `Referencia a APS` = ifelse(isTruthy(input$referencia_aps), input$referencia_aps, NA),
    `Equipo de Referencia` = ifelse(isTruthy(input$equipo_referencia), input$equipo_referencia, NA),
    
    # Información Adicional -----------------------------------------------
    Observaciones = ifelse(isTruthy(input$observaciones), input$observaciones, NA)
    
  )
  
  nuevo_registro <- as.data.frame(lapply(nuevo_registro, function(col) {
    if (length(col) == 0) {
      return(NA)  # Si la columna está vacía, asigna NA
    } else {
      return(col)  # Mantén el valor original si tiene longitud válida
    }
  }), stringsAsFactors = FALSE)
  colnames(nuevo_registro) <- colnames(data)
  
  todos_validos <- all(sapply(validadores, function(iv) iv$is_valid()))
  
  # Guardar el nuevo registro directamente en el archivo "Base completa.xlsx" ----
  if (todos_validos) {
    data <- base()
    
    # Ahora puedes hacer el rbind sin problemas
    datos_actualizados <- rbind(data, nuevo_registro)
    
    # Guardar el archivo actualizado
    wb <- createWorkbook()
    addWorksheet(wb,"Registros")
    writeData(wb, "Registros", datos_actualizados)
    saveWorkbook(wb, "Base completa.xlsx", overwrite = TRUE)
    
    showModal(modalDialog(title = "Registro exitoso",
    "El registro ingresado ha sido guardado con éxito en la base.", 
    footer = modalButton("Cerrar")))
  } else {
    showModal(modalDialog(title = "Error",
    "Por favor, complete todos los campos obligatorios.", 
    footer = modalButton("Entendido")))
  }
  
})



  output$admin_button <- renderUI({
    if (res_auth$admin) { # Verificar si el usuario es administrador
      actionButton("descarga", 
                   tags$span("Descargar base de datos",style = "font-size: 12px"),
                   icon = icon("download"))
    }
})
  
  result <- reactive({
    
    df <- base() %>%
      select(`ID de registro`, `Apellido, Nombre`,
             `Estado de la Entrevista con Psicólogo`, `Fecha de la Entrevista con Psicólogo`,
             `Estado de la Entrevista con Psiquiátra`, `Fecha de la Entrevista con Psiquiátra`,
             `Estado de la Entrevista con Trabajador Social`, `Fecha de la Entrevista con Trabajador Social`)
    
    result <- df %>%
      pivot_longer(
        cols = c("Fecha de la Entrevista con Psicólogo", 
                 "Fecha de la Entrevista con Psiquiátra", 
                 "Fecha de la Entrevista con Trabajador Social"),
        names_to = "Entrevista",
        values_to = "Fecha"
      ) %>%
      filter(!is.na(Fecha)) %>% # Filtrar solo las filas con fecha
      mutate(
        Entrevista = case_when(
          Entrevista == "Fecha de la Entrevista con Psicólogo" ~ "Psicólogo",
          Entrevista == "Fecha de la Entrevista con Psiquiátra" ~ "Psiquiátra",
          Entrevista == "Fecha de la Entrevista con Trabajador Social" ~ "Trabajador Social"
        )
      ) %>%
      select(calendarId = `ID de registro`, title = `Apellido, Nombre`, Entrevista, start = Fecha) %>%
      mutate(
        short_entrev = case_when(
          Entrevista == "Psicólogo" ~ "Psic",
          Entrevista == "Psiquiátra" ~ "Psiq",
          Entrevista == "Trabajador Social" ~ "TS"),
        title = paste(short_entrev, "|",title),
        end = start, # Para que el evento sea de un solo día
        body = NA,
        recurrenceRule = NA,
        start = as.Date(start), # Asegurarnos de que la fecha esté en formato Date
        end = as.Date(end), # Asegurarnos de que la fecha esté en formato Date
        category = "allday",
        location = NA,
        backgroundColor = case_when(
          Entrevista == "Psicólogo" ~ "#FBC91C",
          Entrevista == "Psiquiátra" ~ "#EC7E14",
          Entrevista == "Trabajador Social" ~ "#4C443C"
        ),
        color = case_when(
          Entrevista == "Psicólogo" ~ "black",
          Entrevista == "Psiquiátra" ~ "white",
          Entrevista == "Trabajador Social" ~ "white"
        ),
        borderColor = case_when(
          Entrevista == "Psicólogo" ~ "#FBC91C",
          Entrevista == "Psiquiátra" ~ "#EC7E14",
          Entrevista == "Trabajador Social" ~ "#4C443C"
        )
      ) %>%
      select(-Entrevista, short_entrev)
      
    
    
    result
  })
  
  # Renderizar el calendario
  output$calendar <- renderCalendar({
    calendar(result(), navigation = TRUE, defaultDate = Sys.Date()) %>%
      cal_month_options(
        startDayOfWeek  = 0, 
        daynames = c("Dom","Lun","Mar","Mié","Jue","Vie","Sáb"),
        narrowWeekend = FALSE
      ) %>% 
      cal_props(cal_demo_props())
  })
  
  # ----------------------------------------------------------------------------
  # modificación
  # ----------------------------------------------------------------------------
  
  # Reactivo para almacenar los resultados de la búsqueda
  search_results <- reactiveVal(NULL)
  registro_seleccionado <- reactiveVal(NULL)
  registro_reactivo <- reactiveVal()
  sustancias_reactivo <- reactiveVal()
  
  # Reglas --------------------------------------------
  # Recuerda DNI ----------------------------------------------
  iv_recuerda_dni1 <- InputValidator$new()
  
  ## Campo obligatorio
  iv_recuerda_dni1$add_rule("recuerda_dni1",
                            sv_required(
                              tags$span("Campo obligatorio.", style = "font-size: 10px;")
                            )
  )
  iv_recuerda_dni1$enable()
  
  # Validación de DNI -----------------------------------------
  iv_dni1 <- InputValidator$new()
  
  ## Regla para validar que el DNI no esté vacío
  iv_dni1$add_rule("dni1",
                   sv_required(
                     tags$span("DNI es obligatorio si se recuerda.", style = "font-size: 10px;")
                   )
  )
  iv_dni1$disable()  # Desactivado por defecto
  
  # Observador para manejar la lógica según "Recuerda DNI"
  observeEvent(input$recuerda_dni1, {
    # Si la opción es "", No o S/D, deshabilitar el campo y eliminar la validación
    if (input$recuerda_dni1 %in% c("", "No", "S/D")) {
      updateNumericInput(session, "dni1", value = "")  # Reiniciar el campo
      shinyjs::disable("dni1")  # Deshabilitar el campo
      iv_dni1$disable()  # Desactivar validación
    } else {
      # Si selecciona Sí, habilitar el campo y agregar la validación
      shinyjs::enable("dni1")  # Habilitar el campo
      iv_dni1$enable()  # Activar validación
    }
  })
  # DNI ------------------------------------------------------------------------
  iv_dni1 <- InputValidator$new()
  
  ## Regla 1: Campo obligatorio
  iv_dni1$add_rule("dni1", sv_required(
    tags$span("Campo obligatorio.",
              style = "font-size: 10px;")
  )
  )
  
  ## Regla 2: Solo números (sin puntos, espacios, etc.)
  iv_dni1$add_rule("dni1", function(value) {
    if (nzchar(value) && !grepl("^[0-9]+$", value)) {  # Verifica solo si no está vacío
      return("El DNI solo puede contener números, sin puntos ni caracteres especiales.")
    }
    return(NULL)  # Si pasa la validación, no devuelve errores
  })
  
  ## Regla 3: Longitud de 8 dígitos
  iv_dni1$add_rule("dni1", function(value) {
    if (nzchar(value) && nchar(value) != 8) {  # Verifica solo si no está vacío
      return("El DNI debe tener exactamente 8 dígitos.")
    }
    return(NULL)
  })
  iv_dni1$enable()
  # Apellido, Nombre (apodo) ---------------------------------------------------
  iv_apellido_nombre1 <- InputValidator$new()
  
  ## Obligatorio
  iv_apellido_nombre1$add_rule("apellido_nombre1", 
                               sv_required(tags$span("Campo obligatorio.", 
                                                     style = "font-size: 10px;")
                               )
  )
  
  ## Caracteres especiales (excepto tildes, coma y paréntesis)
  iv_apellido_nombre1$add_rule("apellido_nombre1", function(value) {
    if(grepl("[^a-zA-ZáéíóúÁÉÍÓÚñÑ,() ]", value)) {
      return(tags$span("No se admiten caracteres especiales.",
                       style = "font-size: 10px;")
      )
    }
  })
  
  ## Más de 4 caracteres
  iv_apellido_nombre1$add_rule("apellido_nombre1", function(value) {
    if(nchar(as.character(value)) <= 4) {
      return(tags$span("El campo debe tener más de 4 caracteres.",
                       style = "font-size: 10px;")
      )
    }
  })
  
  iv_apellido_nombre1$enable()
  # Validación de Edad --------------------------------------------------------
  
  # Crear validador para el campo edad
  iv_edad1 <- InputValidator$new()
  
  # Regla 1: Campo obligatorio si fecha de nacimiento está vacía
  iv_edad1$add_rule("edad1", function(value) {
    if (!isTruthy(input$fecha_nacimiento1)) {
      if (is.null(value) || is.na(value) || value == "") {
        return(tags$span("Campo obligatorio.", style = "font-size: 10px;"))
      }
    }
    return(NULL)
  })
  
  # Regla 2: Rango válido (1-99 años)
  iv_edad1$add_rule("edad1", function(value) {
    if (!is.null(value) && !is.na(value)) {
      if (!grepl("^[0-9]+$", value)) {
        return("El campo solo debe contener números.")
      }
      if (value < 1 || value > 99) {
        return("La edad debe estar entre 1 y 99 años.")
      }
    }
    return(NULL)
  })
  
  # Activar validaciones
  iv_edad1$enable()
  
  # Cálculo automático de la edad --------------------------------------------
  
  observe({
    # Verificar que ambas fechas estén registradas
    if (isTruthy(input$fecha_nacimiento1) && isTruthy(input$fecha_registro1)) {
      
      # Calcular la edad en años usando fecha de registro como referencia
      edad1 <- trunc(as.numeric(difftime(as.Date(input$fecha_registro1), as.Date(input$fecha_nacimiento1), units = "days")) %/% 365)
      
    } else {
      edad1 <- input$edad1
    }
    
    # Actualizar el campo de `edad` en el formulario
    updateNumericInput(session, "edad1", value = edad1)
  })
  
  # Sexo biológico -------------------------------------------------------------
  
  iv_sexo_biologico1 <- InputValidator$new()
  
  ## Obligatorio
  iv_sexo_biologico1$add_rule("sexo_biologico1",
                              sv_required(tags$span("Campo obligatorio.",
                                                    style = "font-size: 10px;")
                              )
  )
  
  iv_sexo_biologico1$enable()
  
  # Género ---------------------------------------------------------------------
  
  iv_genero1 <- InputValidator$new()
  
  ## Obligatorio
  iv_genero1$add_rule("genero1",
                      sv_required(tags$span("Campo obligatorio.",
                                            style = "font-size: 10px;")
                      )
  )
  
  iv_genero1$enable()
  # Provincia ------------------------------------------------------------------
  
  iv_provincia1 <- InputValidator$new()
  
  ## Obligatorio
  iv_provincia1$add_rule("provincia1", function(value) {
    if(value == provincias[[1]][1]) {
      return(tags$span("Campo obligatorio.",
                       style = "font-size: 10px;"))
    }
  })
  
  iv_provincia1$enable()
  
  # Localidad ------------------------------------------------------------------
  ## esta dentro del modal
  # Barrio ---------------------------------------------------------------------
  
  iv_barrio1 <- InputValidator$new()
  
  ## Nada de caracteres especiales
  iv_barrio1$add_rule("barrio1", function(value) {
    if(grepl("[^a-zA-ZáéíóúÁÉÍÓÚñÑ,() ]", value)) {
      return("No se admiten caracteres especiales.")
    }
  })
  
  ## El campo debe tener entre 2 y 100 caracteres
  iv_barrio1$add_rule("barrio1", function(value) {
    if(nchar(as.character(value)) <= 2 & nchar(as.character(value)) >0) {
      return("El campo debe tener más de 2 caracteres.")
    }
    if(nchar(as.character(value)) > 100) {
      return("El campo debe tener menos de 100 caracteres.")
    }
  })
  
  iv_barrio1$enable()
  # Contacto 1 - Teléfono ------------------------------------------------------
  
  iv_telefono_11 <- InputValidator$new()
  
  ## Obligatorio
  iv_telefono_11$add_rule("telefono_contacto_11",
                          sv_required(tags$span("Campo obligatorio.",
                                                style = "font-size: 10px;")
                          )
  )
  
  ## Entre 7 y 10 caracteres
  iv_telefono_11$add_rule("telefono_contacto_11", function(value) {
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
  
  iv_telefono_11$enable()
  
  # Función auxiliar para verificar si el valor no es NULL o NA
  validar_telefono1 <- function(value) {
    if (is.null(value) || is.na(value)) {
      return(NULL)  # Si es NULL o NA, no hay error
    }
    
    # Convertimos a cadena de texto por si el valor no lo es
    value <- as.character(value)
    
    if (nchar(value) < 7) {
      return(tags$span("El teléfono debe tener al menos 7 dígitos.", style = "font-size: 10px;"))
    }
    if (nchar(value) > 11) {
      return(tags$span("El teléfono debe tener menos de 10 dígitos.", style = "font-size: 10px;"))
    }
    if (!grepl("^[0-9]+$", value)) {
      return(tags$span("Solo se admiten números.", style = "font-size: 10px;"))
    }
    
    return(NULL)  # Si todo es correcto, no hay error
  }
  # Contacto 2 - Teléfono ------------------------------------------------------
  iv_telefono_21 <- InputValidator$new()
  
  # Añadimos la regla utilizando la función auxiliar
  iv_telefono_21$add_rule("telefono_contacto_21", function(value) {
    validar_telefono1(value)
  })
  
  iv_telefono_21$enable()
  
  # Contacto 3 - Teléfono ------------------------------------------------------
  iv_telefono_31 <- InputValidator$new()
  
  # Añadimos la regla utilizando la función auxiliar
  iv_telefono_31$add_rule("telefono_contacto_31", function(value) {
    validar_telefono1(value)
  })
  
  iv_telefono_31$enable()
  # Contacto 1 - Vinculo -------------------------------------------------------
  
  iv_vinculo_11 <- InputValidator$new()
  
  ## Obligatorio
  iv_vinculo_11$add_rule("tipo_vinculo_contacto_11",
                         sv_required(tags$span("Campo obligatorio.",
                                               style = "font-size: 10px;")
                         )
  )
  
  ## Más de 2 caracteres y sin caracteres especiales
  iv_vinculo_11$add_rule("tipo_vinculo_contacto_11", function(value) {
    # Opciones predefinidas
    opciones_validas1 <- c("", "Propio", "Papá/Mamá", "Hermano/a", "Hijo/a", "Amigo/a")
    
    # Si el valor está en las opciones válidas, no se necesita validación adicional
    if (value %in% opciones_validas1) {
      return(NULL)
    }
    
    # Verificar si el valor no es nulo ni vacío (después de eliminar espacios)
    if (!is.null(value) && trimws(value) != "") {
      # Verificar longitud mínima
      if (nchar(trimws(value)) < 2) {
        return("El campo debe tener al menos 2 caracteres.")
      }
      # Verificar caracteres válidos
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", trimws(value))) {
        return("No se admiten caracteres especiales.")
      }
    }
    
    return(NULL)
  })
  
  iv_vinculo_11$enable()
  # Contacto 2 - Vinculo -------------------------------------------------------
  
  iv_vinculo_21 <- InputValidator$new()
  
  ## Más de 2 caracteres y sin caracteres especiales
  iv_vinculo_21$add_rule("tipo_vinculo_contacto_21", function(value) {
    # Opciones predefinidas
    opciones_validas1 <- c("", "Propio", "Papá/Mamá", "Hermano/a", "Hijo/a", "Amigo/a")
    
    # Si el valor está en las opciones válidas, no se necesita validación adicional
    if (value %in% opciones_validas1) {
      return(NULL)
    }
    
    # Si no está vacío y se intenta crear una nueva opción
    if (value != "") {
      if (nchar(value) < 2) {
        return("El campo debe tener al menos 2 caracteres.")
      }
      # Expresión regular para permitir solo letras y espacios
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {
        return("No se admiten caracteres especiales.")
      }
    }
    
    return(NULL)
  })
  
  iv_vinculo_21$enable()
  # Contacto 3 - Vinculo -------------------------------------------------------
  
  iv_vinculo_31 <- InputValidator$new()
  
  ## Más de 2 caracteres y sin caracteres especiales
  iv_vinculo_31$add_rule("tipo_vinculo_contacto_31", function(value) {
    # Opciones predefinidas
    opciones_validas1 <- c("", "Propio", "Papá/Mamá", "Hermano/a", "Hijo/a", "Amigo/a")
    
    # Si el valor está en las opciones válidas, no se necesita validación adicional
    if (value %in% opciones_validas1) {
      return(NULL)
    }
    
    # Si no está vacío y se intenta crear una nueva opción
    if (value != "") {
      if (nchar(value) < 2) {
        return("El campo debe tener al menos 2 caracteres.")
      }
      # Expresión regular para permitir solo letras y espacios
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {
        return("No se admiten caracteres especiales.")
      }
    }
    
    return(NULL)
  })
  
  iv_vinculo_31$enable()
  iv_nombre_11 <- InputValidator$new()
  
  ## Obligatorio
  iv_nombre_11$add_rule("nombre_contacto_11", function(value) {
    if (input$tipo_vinculo_contacto_11 != "Propio") {
      if (value == "") {
        return(tags$span("Campo obligatorio.", style = "font-size: 10px;"))
      }
    }
    return(NULL)
  })
  
  ## Al menos 2 caracteres, sin caracteres especiales
  iv_nombre_11$add_rule("nombre_contacto_11", function(value) {
    if (value != "") {
      if (nchar(value) < 2) {
        return(tags$span("El campo debe tener al menos 2 caracteres.", style = "font-size: 10px;"))
      }
    }
    return(NULL)
  })
  
  ## Validación de caracteres especiales
  iv_nombre_11$add_rule("nombre_contacto_11", function(value) {
    # Verificar si el vínculo es diferente de las opciones predefinidas
    esta <- input$tipo_vinculo_contacto_11 %in% c("", "Propio", "Papá/Mamá", "Hermano/a", "Hijo/a", "Amigo/a")
    
    if (!esta) {
      # Expresión regular que permite letras, tildes, ñ, espacios y no permite caracteres especiales
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {  
        return(tags$span("No se admiten caracteres especiales.", style = "font-size: 10px;"))
      }
    }
    return(NULL)
  })
  
  iv_nombre_11$enable()
  # Contacto 2 - Nombre --------------------------------------------------------
  
  iv_nombre_21 <- InputValidator$new()
  
  ## Al menos 2 caracteres, sin caracteres especiales
  iv_nombre_21$add_rule("nombre_contacto_21", function(value) {
    if (value != "") {
      if (nchar(value) < 2) {
        return(tags$span("El campo debe tener al menos 2 caracteres.", style = "font-size: 10px;"))
      }
      # Validación de caracteres permitidos: solo letras, tildes, ñ y espacios
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {
        return(tags$span("No se admiten caracteres especiales.", style = "font-size: 10px;"))
      }
    }
    return(NULL)
  })
  iv_nombre_21$enable()
  
  # Contacto 3 - Nombre --------------------------------------------------------
  
  iv_nombre_31 <- InputValidator$new()
  
  ## Al menos 2 caracteres, sin caracteres especiales
  iv_nombre_31$add_rule("nombre_contacto_31", function(value) {
    if (value != "") {
      if (nchar(value) < 2) {
        return(tags$span("El campo debe tener al menos 2 caracteres.", style = "font-size: 10px;"))
      }
      # Validación de caracteres permitidos: solo letras, tildes, ñ y espacios
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {
        return(tags$span("No se admiten caracteres especiales.", style = "font-size: 10px;"))
      }
    }
    return(NULL)
  })
  iv_nombre_31$enable()
  
  # Entrevista psicologo - Estado ----------------------------------------------
  iv_estado_psicologo1 <- InputValidator$new()
  
  ## Obligatorio
  iv_estado_psicologo1$add_rule("estado_psicologo1", sv_required(message = tags$span("Campo obligatorio", style = "font-size: 10px;")))
  
  iv_estado_psicologo1$enable()
  
  # Entrevista psiquiatra - Estado ---------------------------------------------
  
  iv_estado_psiquiatra1 <- InputValidator$new()
  
  ## Obligatorio
  iv_estado_psiquiatra1$add_rule("estado_psiquiatra1", sv_required(message = tags$span("Campo obligatorio", style = "font-size: 10px;")))
  
  iv_estado_psiquiatra1$enable()
  
  # Entrevista ts - Estado -----------------------------------------------------
  
  iv_estado_ts1 <- InputValidator$new()
  
  ## Obligatorio
  iv_estado_ts1$add_rule("estado_ts1", sv_required(message = tags$span("Campo obligatorio", style = "font-size: 10px;")))
  
  iv_estado_ts1$enable()
  
  # Entrevista psicologo - Fecha -----------------------------------------------
  
  # Validaciones dinámicas para el campo de fecha de la entrevista
  iv_fecha_psicologo1 <- InputValidator$new()
  
  ## Regla para "Presente" o "Ausente" -> la fecha no puede ser futura
  iv_fecha_psicologo1$add_rule("fecha_entrevista_psicologo1", function(value) {
    estado <- input$estado_psicologo1
    
    # Verificamos si el campo es obligatorio para este estado
    if (estado %in% c("Presente", "Ausente")) {
      
      # Convertimos el valor a Date si no es NULL o vacío
      fecha <- as.Date(value, format = "%Y-%m-%d")
      
      if (length(fecha) == 0) {
        return(tags$span("La fecha es obligatoria para 'Presente' o 'Ausente'.", style = "font-size:10px;"))
      }
      
      # Validamos que la fecha no sea futura
      if (!is.na(fecha) && fecha > Sys.Date()) {
        return(tags$span("La fecha no puede ser futura seleccionando 'Presente' o 'Ausente'.", style = "font-size:10px;"))
      }
    }
  })
  
  ## Regla para "Pendiente" -> la fecha debe ser futura
  iv_fecha_psicologo1$add_rule("fecha_entrevista_psicologo1", function(value) {
    estado <- input$estado_psicologo1
    
    # Si está pendiente, verificamos si la fecha es futura
    if (estado == "Pendiente") {
      
      # Convertimos el valor a Date si no es NULL o vacío
      fecha <- as.Date(value, format = "%Y-%m-%d")
      
      if (length(fecha) == 0) {
        return(tags$span("La fecha es obligatoria para 'Pendiente'.", style = "font-size:10px;"))
      }
      
      # Validamos que la fecha sea futura
      if (!is.na(fecha) && fecha <= Sys.Date()) {
        return(tags$span("La fecha debe ser futura seleccionando 'Pendiente'.", style = "font-size:10px;"))
      }
    }
  })
  
  ## Regla para "No necesaria" o "No asignada" -> la fecha no es obligatoria
  observeEvent(input$estado_psicologo1, {
    # Si la opción es "", No o S/D, deshabilitar el campo y eliminar la validación
    if (input$estado_psicologo1 %in% c("","No asignada", "No necesaria")) {
      updateDateInput(session, "fecha_entrevista_psicologo1", value = NA)
      shinyjs::disable("fecha_entrevista_psicologo1")
    } else {
      shinyjs::enable("fecha_entrevista_psicologo1")
    }
  })
  
  # Activar validaciones
  iv_fecha_psicologo1$enable()
  
  # Entrevista psiquiatra - Fecha ----------------------------------------------
  
  iv_fecha_psiquiatra1 <- InputValidator$new()
  
  # Validaciones dinámicas para el campo de fecha de la entrevista
  iv_fecha_psiquiatra1 <- InputValidator$new()
  
  ## Regla para "Presente" o "Ausente" -> la fecha no puede ser futura
  iv_fecha_psiquiatra1$add_rule("fecha_entrevista_psiquiatra1", function(value) {
    estado <- input$estado_psiquiatra1
    
    # Verificamos si el campo es obligatorio para este estado
    if (estado %in% c("Presente", "Ausente")) {
      
      # Convertimos el valor a Date si no es NULL o vacío
      fecha <- as.Date(value, format = "%Y-%m-%d")
      
      if (length(fecha) == 0) {
        return(tags$span("La fecha es obligatoria para 'Presente' o 'Ausente'.", style = "font-size:10px;"))
      }
      
      # Validamos que la fecha no sea futura
      if (!is.na(fecha) && fecha > Sys.Date()) {
        return(tags$span("La fecha no puede ser futura seleccionando 'Presente' o 'Ausente'.", style = "font-size:10px;"))
      }
    }
  })
  
  ## Regla para "Pendiente" -> la fecha debe ser futura
  iv_fecha_psiquiatra1$add_rule("fecha_entrevista_psiquiatra1", function(value) {
    estado <- input$estado_psiquiatra1
    
    # Si está pendiente, verificamos si la fecha es futura
    if (estado == "Pendiente") {
      
      # Convertimos el valor a Date si no es NULL o vacío
      fecha <- as.Date(value, format = "%Y-%m-%d")
      
      if (length(fecha) == 0) {
        return(tags$span("La fecha es obligatoria para 'Pendiente'.", style = "font-size:10px;"))
      }
      
      # Validamos que la fecha sea futura
      if (!is.na(fecha) && fecha <= Sys.Date()) {
        return(tags$span("La fecha debe ser futura seleccionando 'Pendiente'.", style = "font-size:10px;"))
      }
    }
  })
  
  ## Regla para "No necesaria" o "No asignada" -> la fecha no es obligatoria
  observeEvent(input$estado_psiquiatra1, {
    # Si la opción es "", No o S/D, deshabilitar el campo y eliminar la validación
    if (input$estado_psiquiatra1 %in% c("","No asignada", "No necesaria")) {
      updateDateInput(session, "fecha_entrevista_psiquiatra1", value = NA)
      shinyjs::disable("fecha_entrevista_psiquiatra1")
    } else {
      shinyjs::enable("fecha_entrevista_psiquiatra1")
    }
  })
  
  # Activar validaciones
  iv_fecha_psiquiatra1$enable()
  
  # Entrevista ts - Fecha ------------------------------------------------------
  
  iv_fecha_ts1 <- InputValidator$new()
  
  # Validaciones dinámicas para el campo de fecha de la entrevista
  iv_fecha_ts1 <- InputValidator$new()
  
  ## Regla para "Presente" o "Ausente" -> la fecha no puede ser futura
  iv_fecha_ts1$add_rule("fecha_entrevista_ts1", function(value) {
    estado <- input$estado_ts1
    
    # Verificamos si el campo es obligatorio para este estado
    if (estado %in% c("Presente", "Ausente")) {
      
      # Convertimos el valor a Date si no es NULL o vacío
      fecha <- as.Date(value, format = "%Y-%m-%d")
      
      if (length(fecha) == 0) {
        return(tags$span("La fecha es obligatoria para 'Presente' o 'Ausente'.", style = "font-size:10px;"))
      }
      
      # Validamos que la fecha no sea futura
      if (!is.na(fecha) && fecha > Sys.Date()) {
        return(tags$span("La fecha no puede ser futura seleccionando 'Presente' o 'Ausente'.", style = "font-size:10px;"))
      }
    }
  })
  
  ## Regla para "Pendiente" -> la fecha debe ser futura
  iv_fecha_ts1$add_rule("fecha_entrevista_ts", function(value) {
    estado <- input$estado_ts1
    
    # Si está pendiente, verificamos si la fecha es futura
    if (estado == "Pendiente") {
      
      # Convertimos el valor a Date si no es NULL o vacío
      fecha <- as.Date(value, format = "%Y-%m-%d")
      
      if (length(fecha) == 0) {
        return(tags$span("La fecha es obligatoria para 'Pendiente'.", style = "font-size:10px;"))
      }
      
      # Validamos que la fecha sea futura
      if (!is.na(fecha) && fecha <= Sys.Date()) {
        return(tags$span("La fecha debe ser futura seleccionando 'Pendiente'.", style = "font-size:10px;"))
      }
    }
  })
  
  ## Regla para "No necesaria" o "No asignada" -> la fecha no es obligatoria
  observeEvent(input$estado_ts1, {
    # Si la opción es "", No o S/D, deshabilitar el campo y eliminar la validación
    if (input$estado_ts1 %in% c("","No asignada", "No necesaria")) {
      updateDateInput(session, "fecha_entrevista_ts1", value = NA)
      shinyjs::disable("fecha_entrevista_ts1")
    } else {
      shinyjs::enable("fecha_entrevista_ts1")
    }
  })
  
  # Activar validaciones
  iv_fecha_ts1$enable()
  
  # Información consumo - Edad de inicio ---------------------------------------
  
  iv_edad_inicio1 <- InputValidator$new()
  iv_edad_inicio1$add_rule("edad_inicio_consumo1", function(value) {
    
    if (!is.na(value)) {
      if(nchar(value) > 2) {
        return(tags$span("La edad no puede tener más de 2 dígitos.", style = "font-size:10px;"))
      }
      if(!grepl("^[0-9]+$", value)) {
        return(tags$span("Solo se admiten números.", style = "font-size:10px;"))
      }
      if(!is.na(value) & !is.na(input$edad1) & input$edad1 < value) {
        return(tags$span("La edad de inicio no puede ser mayor a la actual.", style = "font-size:10px;"))
      }
    }
    # Si todas las condiciones están bien, no hay error
    return(NULL)
  })
  iv_edad_inicio1$enable()
  
  # Información consumo - Sustancia de inicio ----------------------------------
  
  iv_sustancia_inicio1 <- InputValidator$new()
  
  iv_sustancia_inicio1$add_rule("sustancia_inicio_consumo1", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))
  
  iv_sustancia_inicio1$add_rule("otra_sustancia1", function(value) {
    # Verificar si 'Otra' está seleccionada en el selectInput
    if ("Otra" %in% input$sustancia_inicio_consumo1) {
      
      # Validar si el campo está vacío o contiene caracteres especiales
      if (is.null(value) || value == "") {
        return(tags$span("Debe completar el campo si selecciona 'Otra'.", style = "font-size:10px;"))
      } else if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {
        return(tags$span("No se admiten caracteres especiales.", style = "font-size:10px;"))
      }
      
    } else {
      # Si no está seleccionada la opción "Otra", no hacer ninguna validación
      return(NULL)
    }
  })
  
  iv_sustancia_inicio1$enable()
  
  # Información consumo - Consumo actual ---------------------------------------
  
  iv_persona_consume1 <- InputValidator$new()
  
  iv_persona_consume1$add_rule("persona_consume1", sv_required(tags$span("Campo obligatorio.", style = "font-size: 10px;")))
  
  iv_persona_consume1$enable()
  
  # Información consumo - Sustancia de consumo actual --------------------------
  
  iv_sustancias_actual1 <- InputValidator$new()
  
  ## Obligatorio
  
  iv_sustancias_actual1$add_rule("sustancias_consumo_actual1", function(value) {
    if(input$persona_consume1 == "Si") {
      if(is.null(value) || length(value) == 0) {
        return(tags$span("Campo obligatorio.", style = "font-size: 10px;"))
      }
    }
  })
  
  iv_sustancias_actual1$add_rule("otra_sustancia_actual1", function(value) {
    # Verificar si 'Otra' está seleccionada en el selectInput
    if ("Otra" %in% input$sustancias_consumo_actual1) {
      
      # Validar si el campo está vacío o contiene caracteres especiales
      if (is.null(value) || value == "") {
        return(tags$span("Debe completar el campo si selecciona 'Otra'.", style = "font-size:10px;"))
      } else if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {
        return(tags$span("No se admiten caracteres especiales.", style = "font-size:10px;"))
      }
      
    } else {
      # Si no está seleccionada la opción "Otra", no hacer ninguna validación
      return(NULL)
    }
  })
  
  iv_sustancias_actual1$enable()
  
  # Información tratamiento - Derivación ---------------------------------------
  
  iv_derivacion1 <- InputValidator$new()
  
  ## Obligatorio
  iv_derivacion1$add_rule("derivacion1", sv_required(tags$span("Campo obligatorio.",style = "font-size:10px;")))
  
  iv_derivacion1$enable()
  
  # Información tratamiento - Derivado de --------------------------------------
  
  iv_derivado_de1 <- InputValidator$new()
  
  ## Obligatorio
  iv_derivado_de1$add_rule("derivado_de1", function(value) {
    if(input$derivacion1 == "Si" & nchar(value) == 0) {
      return(tags$span("Campo obligatorio.",style = "font-size:10px;"))
      if (!is.null(input$derivacion1) && input$derivacion1 == "Si") {
        if (nchar(value) < 2 & nchar(value) > 0) {
          return(tags$span("El campo debe tener al menos 2 caracteres.",style = "font-size:10px;"))
        }
        if (grepl("[^a-zA-Z0-9 ]", value)) {
          return(tags$span("No se admiten caracteres especiales.",style = "font-size:10px;"))
        }
      }
    } else if (input$derivacion1 %in% c("No","No informado")) {
      if(value != "") {
        return(tags$span("El campo debe estar vacío.",style = "font-size:10px;"))
      }
    }
  })
  
  iv_derivado_de1$enable()
  
  # Información tratamiento - Nº de tratameintos previos -----------------------
  
  iv_tratamientos_previos1 <- InputValidator$new()
  
  ## Obligatorio
  iv_tratamientos_previos1$add_rule("num_tratamientos_previos1", function(value) {
    if(is.na(value)) {
      return() }
    else {
      if (value < 0 || value > 99) {
        return(tags$span("El número debe estar entre 0 y 99.",style = "font-size:10px;"))
      }
    }
  })
  
  iv_tratamientos_previos1$enable()
  
  # Información tratamiento - Lugar de último tratameinto ----------------------
  
  iv_lugar_ultimo_tratamiento1 <- InputValidator$new()
  iv_lugar_ultimo_tratamiento1$add_rule("lugar_ultimo_tratamiento1", function(value) {
    # Validar solo si "Número de Tratamientos previos" tiene un valor válido y es mayor que 0
    if (!is.null(input$num_tratamientos_previos1) && 
        !is.na(input$num_tratamientos_previos1) && 
        input$num_tratamientos_previos1 > 0) {
      
      # Si el campo está vacío, no mostrar error
      if (is.null(value) || value == "") {
        return(NULL)
      }
      
      # Validar longitud mínima
      if (nchar(value) < 2) {
        return(tags$span("El campo debe tener al menos 2 caracteres.", style = "font-size:10px;"))
      }
      
      # Validar que no contenga caracteres especiales
      if (grepl("[^a-zA-Z0-9 ´/*()\\[\\]]", value)) {
        return(tags$span("No se admiten caracteres especiales.", style = "font-size:10px;"))
      }
    }
    
    return(NULL)  # Sin errores
  })
  
  iv_lugar_ultimo_tratamiento1$enable()
  
  # Información tratamiento - Tratameinto elegido ------------------------------
  
  iv_tratamiento_elegido1 <- InputValidator$new()
  iv_apellido_nombre1$add_rule("tratamiento_elegido1", sv_required("Campo obligatorio"))
  iv_tratamiento_elegido1$add_rule("tratamiento_elegido1", function(value) {
    if (is.null(value) || value == "") {
      return(tags$span("Campo obligatorio.",style = "font-size:10px;"))
    }
    return(NULL)  # Sin errores
  })
  
  iv_tratamiento_elegido1$enable()
  
  # Situación Socioeconómica, Jurídica y de Salud - Educación ------------------
  
  iv_nivel_educativo_max1 <- InputValidator$new()
  iv_nivel_educativo_max1$add_rule("nivel_educativo_max1", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))
  
  iv_nivel_educativo_max1$enable()
  
  # Situación Socioeconómica, Jurídica y de Salud - CUD ------------------------
  
  iv_cud1 <- InputValidator$new()
  iv_cud1$add_rule("cud1", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))
  
  iv_cud1$enable()
  
  # Situación Socioeconómica, Jurídica y de Salud - Situción habitacional ------
  
  iv_situacion_habitacional1 <- InputValidator$new()
  iv_situacion_habitacional1$add_rule("situacion_habitacional_actual1", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))
  
  iv_situacion_habitacional1$add_rule("otra_situacion_habitacional_actual1", function(value) {
    if (input$situacion_habitacional_actual1 == "Otra") {
      if (is.null(value) || value == "") {
        return(tags$span("Debe completar el campo si selecciona 'Otra'.",style = "font-size:10px;"))
      }
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {
        return(tags$span("No se admiten caracteres especiales.",style="font-size:10px;"))
      }
    }
    return(NULL)
  })
  
  iv_situacion_habitacional1$enable()  
  
  # Situación Socioeconómica, Jurídica y de Salud - Situación laboral --------------------------------------------------------
  
  iv_situacion_laboral_actual1 <- InputValidator$new()
  iv_situacion_laboral_actual1$add_rule("situacion_laboral_actual1", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))
  
  iv_situacion_laboral_actual1$add_rule("otra_situacion_laboral_actual1", function(value) {
    if (input$situacion_laboral_actual1 == "Otra") {
      if (is.null(value) || value == "") {
        return(tags$span("Debe completar el campo si selecciona 'Otra'.",style = "font-size:10px;"))
      }
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {
        return(tags$span("No se admiten caracteres especiales.",style="font-size:10px;"))
      }
    }
    return(NULL)
  })
  
  iv_situacion_laboral_actual1$enable()
  
  # Situación Socioeconómica, Jurídica y de Salud - Ingreso económico --------------------------------------------------------
  
  iv_ingreso_economico1 <- InputValidator$new()
  iv_ingreso_economico1$add_rule("ingreso_economico1", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))
  
  iv_ingreso_economico1$add_rule("otro_ingreso1", function(value) {
    if (any(c("Otro subsidio/plan social", "Otro tipo de pensión", "Otro tipo de ingreso") %in% input$ingreso_economico1)) {
      if (value == "") {
        return(tags$span("Debe completar el campo si seleccionó 'Otro subsidio/plan social', 'Otro tipo de pensión' o 'Otro tipo de ingreso'.",style = "font-size: 10px;"))
      }
      if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {
        return(tags$span("No se admiten caracteres especiales.",style="font-size:10px;"))
      }
    }
    return(NULL)
  })
  
  iv_ingreso_economico1$enable()
  # Situación Socioeconómica, Jurídica y de Salud - Situción judicial ----------
  
  iv_situacion_judicial1 <- InputValidator$new()
  iv_situacion_judicial1$add_rule("situacion_judicial1", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))
  
  iv_situacion_judicial1$add_rule("otra_situacion_judicial1", function(value) {
    if (input$situacion_judicial1 == "Otra" && (is.null(value) || value == "")) {
      return(tags$span("Debe completar el campo si selecciona 'Otra'.",style = "font-size:10px;"))
    }
    return(NULL)
  })
  
  iv_situacion_judicial1$enable() 
  
  # Redes de Apoyo y Referencias - Redes de apoyo ------------------------------
  
  iv_redes_apoyo1 <- InputValidator$new()
  iv_redes_apoyo1$add_rule("redes_apoyo1", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))
  
  iv_redes_apoyo1$enable() 
  
  # Redes de Apoyo y Referencias - Referencias APS -----------------------------
  
  iv_referencia_aps1 <- InputValidator$new()
  iv_referencia_aps1$add_rule("referencia_aps1", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))
  
  iv_referencia_aps1$enable() 
  
  # Redes de Apoyo y Referencias - Equipo de referencia ------------------------
  
  iv_equipo_referencia1 <- InputValidator$new()
  iv_equipo_referencia1$add_rule("equipo_referencia1", function(value) {
    # Verificar si el campo debe ser obligatorio
    if (input$referencia_aps1 %in% c("Referencia con seguimiento", "Referencia sin seguimiento")) {
      if (is.null(value) || value == "") {
        return("El campo es obligatorio.")
      }
    }
    if (nchar(value) > 0) {
      if (grepl("[^a-zA-ZáéíóúÁÉÍÓÚñÑ,()// ]", "Hospital Samco PErez")) {
        return("No se admiten caracteres especiales.")
      }
      if (nchar(value) < 3) {
        return("El campo debe tener al menos 3 caracteres.")
      }
    }
    if (input$referencia_aps1 %in% c("No está referenciado", "No informada") && value != "") {
      return("El campo debe estar vacío si la referencia APS es 'No está referenciado' o 'No informada'.")
    }
    
    return(NULL)
  })
  iv_equipo_referencia1$enable() 
  
  # Función para limpiar solo "No informado" y "No informada"
  limpiar_no_informado <- function(valor) {
    if (is.null(valor) || valor %in% c("No informado", "No informada")) {
      return("")  # Limpiar si es "No informado" o "No informada"
    }
    return(valor)  # Mantener el valor original si no es "No informado"
  }
  
  observe({
    # Asegúrate de que el registro esté cargado correctamente
    registro <- registro_reactivo()
    
    if (!is.null(registro)) {
      # Actualizar campos con la lógica de limpieza de "No informado"
      updateTextInput(session, "sexo_biologico1", value = limpiar_no_informado(registro$`Sexo biológico`))
      updateTextInput(session, "genero1", value = limpiar_no_informado(registro$`Género`))
      updateTextInput(session, "sustancia_inicio_consumo1", value = limpiar_no_informado(registro$`Sustancia de inicio`))
      updateTextInput(session, "persona_consume1", value = limpiar_no_informado(registro$`¿Consume actualmente?`))
      updateTextInput(session, "derivacion1", value = limpiar_no_informado(registro$`Derivación`))
      updateTextInput(session, "nivel_educativo_max1", value = limpiar_no_informado(registro$`Nivel Máximo Educativo Alcanzado`))
      updateTextInput(session, "situacion_habitacional_actual1", value = limpiar_no_informado(registro$`Situación Habitacional Actual`))
      updateTextInput(session, "cud1", value = limpiar_no_informado(registro$`CUD`))
      updateTextInput(session, "situacion_laboral_actual1", value = limpiar_no_informado(registro$`Situación Laboral Actual`))
      updateTextInput(session, "referencia_aps1", value = limpiar_no_informado(registro$`Referencia a APS`))
      updateTextInput(session, "situacion_judicial1", value = limpiar_no_informado(registro$`Situación Judicial`))
      
      # Actualizar checkboxes de ingreso económico (solo eliminar "No informado" si está seleccionado)
      ingreso_limpio <- registro$`Ingresos Económicos`
      ingreso_limpio <- ingreso_limpio[!ingreso_limpio %in% c("No informado", "No informada")]
      updateCheckboxGroupInput(session, "ingreso_economico1", selected = ingreso_limpio)
      
      # Actualizar checkboxes de redes de apoyo (solo eliminar "No informado" si está seleccionado)
      redes_limpias <- registro$`Redes de Apoyo`
      redes_limpias <- redes_limpias[!redes_limpias %in% c("No informado", "No informada")]
      updateCheckboxGroupInput(session, "redes_apoyo1", selected = redes_limpias)
    }
  })
  
  # Realizar la búsqueda cuando se presiona el botón "Buscar"
  observeEvent(input$search_button, {
    if (input$search_input != "") {
      resultados <- data %>%
        filter(grepl(input$search_input, as.character(DNI)) |
                 grepl(input$search_input, `Apellido, Nombre`, ignore.case = TRUE))
      
      if (nrow(resultados) == 0) {
        # Mostrar cartel emergente si no hay resultados
        showNotification("No hay resultados para tu búsqueda.", type = "warning")
        search_results(data.frame()) # Borra resultados previos
      } else {
        search_results(resultados) # Actualiza los resultados
      }
    }
  })
  
  output$search_results_ui <- renderUI({
    req(search_results())  # Muestra solo si hay resultados en la tabla
    
    tags$div(
      style = "position: relative;", # Posiciona los botones de manera relativa al contenedor
      DTOutput("search_results"),
      
      # Botones de acciones en la esquina inferior derecha de la tabla
      tags$div(
        style = "position: absolute; bottom: 10px; right: 10px; display: flex; flex-direction: column; gap: 10px;",
        actionButton("cancel_button", "Cancelar búsqueda", width = '15px'),
        actionButton("modify_button", "Modificar registro", width = '15px')
      )
    )
  })
  
  output$search_results <- renderDataTable({
    req(search_results()) 
    
    # Agregar una columna de índice temporal para referencia
    resultados_tabla <- search_results() %>%
      mutate(`Temp_ID` = row_number()) %>%  # Crear un identificador temporal
      arrange(desc(`Fecha de registro`)) %>%  # Ordenar por fecha
      mutate(`Fecha de registro` = format(`Fecha de registro`, "%d/%m/%Y"),
             `Fecha de Nacimiento` = format(`Fecha de Nacimiento`, "%d/%m/%Y")) %>%
      select(`ID de registro`,	`Fecha de registro`,	`ID de la persona`, 
             `DNI`,	`Apellido, Nombre`,	`Fecha de Nacimiento`, `Provincia`,
             `Localidad`,`¿Consume actualmente?`)
    
    
    
    datatable(
      resultados_tabla,
      selection = "single",  # Permite seleccionar una fila
      rownames = FALSE,
      options = list(
        paging = FALSE,
        scrollX = TRUE,
        dom = 'ft',  # Elimina la barra de paginación (solo muestra el texto)
        searching = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      class = "compact stripe hover",
      caption = htmltools::tags$caption(
        id = "table_caption",  # Asignar un ID para actualizarlo dinámicamente
        style = 'caption-side: bottom; text-align: left; color: #ffb600;',
        'Registros encontrados'  # Este será el texto inicial, que se actualizará
      ),
      callback = JS(
        "
  table.on('draw', function() {
    var rowCount = table.rows({ filter: 'applied' }).count();  // Obtener la cantidad de filas visibles
    $('#table_caption').text('Registros encontrados: ' + rowCount);  // Actualizar el texto del caption
  });

  table.on('click', 'tr', function() {
    // Eliminar la clase 'selected' de todas las filas
    table.$('tr').removeClass('selected');
    
    // Agregar la clase 'selected' a la fila clickeada
    $(this).addClass('selected');
  });

  "
      ) 
    )
  })
  # Indicador para mostrar la tabla y los botones solo si hay resultados
  output$showTable <- reactive({
    nrow(search_results()) > 0
  })
  outputOptions(output, "showTable", suspendWhenHidden = FALSE)
  # Botón cancelar búsqueda
  observeEvent(input$cancel_button, {
    updateTextInput(session, "search_input", value = "")
    search_results(NULL)
  })
  
  # Botón modificar registro
  observeEvent(input$modify_button, {
    
    selected <- input$search_results_rows_selected  # Índice visual
    
    if (length(selected) > 0) {
      # Extraer la tabla renderizada con Temp_ID
      resultados_tabla <- search_results() %>%
        mutate(`Temp_ID` = row_number()) %>%
        arrange(desc(`Fecha de registro`))
      
      # Identificar el Temp_ID de la fila seleccionada
      temp_id <- resultados_tabla$Temp_ID[selected]
      
      # Extraer el registro correspondiente del dataset original
      registro <- search_results() %>% filter(row_number() == temp_id)
      
      # Guardar el registro en la variable reactiva
      registro_reactivo(registro)
      
      sustancias_seleccionadas <- c(
        ifelse(registro$`Consumo actual con Alcohol` == "Si", "Alcohol",NA),
        ifelse(registro$`Consumo actual con Cocaína` == "Si", "Cocaína",NA),
        ifelse(registro$`Consumo actual con Crack` == "Si", "Crack",NA),
        ifelse(registro$`Consumo actual con Marihuana` == "Si", "Marihuana",NA),
        ifelse(registro$`Consumo actual con Pegamento` == "Si", "Pegamento",NA),
        ifelse(registro$`Consumo actual con Nafta Aspirada` == "Si", "Nafta",NA),
        ifelse(registro$`Consumo actual con Psicofármacos` == "Si", "Psicofármacos",NA),
        ifelse(registro$`Consumo actual con Otras` == "Si", "Otra",NA)
      )
      sustancias_seleccionadas <- subset(sustancias_seleccionadas,!is.na(sustancias_seleccionadas))
      
      sustancias_reactivo(sustancias_seleccionadas)
      
      # Mostrar el modal con los datos
      showModal(modalDialog(
        title = "Modificar Registro",
        tags$style(HTML("
         .modal-dialog {
          width: 95% !important;
          max-width: 95% !important;
        }
        
         .modal-content {
          height: 90vh;
          overflow: auto;
        }
      ")),
        div(
          style = "height: 100%; display: flex; flex-direction: column; padding: 20px; overflow-y: auto;", # Permite scroll si el contenido es largo
          
          # Recuadro único para entrevistas ----------------------------------------
          wellPanel(
            style = "width: 100%; padding: 20px; margin-bottom: 20px;",  
            div(
              style = "display: flex; gap: 20px; flex-wrap: wrap;",  # Flexbox para columnas
              # Columna para Entrevista con Psicólogo
              div(
                style = "flex: 1; min-width: 300px;",
                h4("Entrevista con Psicólogo", style = "font-size: 15px; font-weight: bold; margin-bottom: 10px;"),
                selectInput(
                  inputId = "estado_psicologo1",
                  label = tags$span("Estado", style = "font-size: 12px;"),
                  choices = list("Presente", "Ausente", "Pendiente", "No necesaria", "No asignada", ""),
                  selected = ifelse(!is.na(registro$`Estado de la Entrevista con Psicólogo`),registro$`Estado de la Entrevista con Psicólogo`,""),
                ),
                dateInput(
                  inputId = "fecha_entrevista_psicologo1",
                  label = tags$span("Fecha de la entrevista", style = "font-size: 12px; white-space: nowrap;"),
                  value = as.Date(registro$`Fecha de la Entrevista con Psicólogo`, format = "%Y-%m-%d"),
                  format = "dd/mm/yyyy"
                )
              ),
              
              # Columna para Entrevista con Psiquiatra
              div(
                style = "flex: 1; min-width: 300px;",
                h4("Entrevista con Psiquiatra", style = "font-size: 15px; font-weight: bold; margin-bottom: 10px;"),
                selectInput(
                  inputId = "estado_psiquiatra1",
                  label = tags$span("Estado", style = "font-size: 12px;"),
                  choices = list("Presente", "Ausente", "Pendiente", "No necesaria", "No asignada", ""),
                  selected = ifelse(!is.na(registro$`Estado de la Entrevista con Psiquiátra`),
                                    registro$`Estado de la Entrevista con Psiquiátra`,
                                    "")
                ),
                dateInput(
                  inputId = "fecha_entrevista_psiquiatra1",
                  label = tags$span("Fecha de la entrevista", style = "font-size: 12px; white-space: nowrap;"),
                  value = as.Date(registro$`Fecha de la Entrevista con Psiquiátra`, format = "%Y-%m-%d"),
                  format = "dd/mm/yyyy"
                )
              ),
              
              # Columna para Entrevista con Trabajador Social
              div(
                style = "flex: 1; min-width: 300px;",
                h4("Entrevista con Trabajador Social", style = "font-size: 15px; font-weight: bold; margin-bottom: 10px;"),
                selectInput(
                  inputId = "estado_ts1",
                  label = tags$span("Estado", style = "font-size: 12px;"),
                  choices = list("Presente", "Ausente", "Pendiente", "No necesaria", "No asignada", ""),
                  selected = ifelse(!is.na(registro$`Estado de la Entrevista con Trabajador Social`),
                                    registro$`Estado de la Entrevista con Trabajador Social`,
                                    "")
                ),
                dateInput(
                  inputId = "fecha_entrevista_ts1",
                  label = tags$span("Fecha de la entrevista", style = "font-size: 12px; white-space: nowrap;"),
                  value = as.Date(registro$`Fecha de la Entrevista con Trabajador Social`, format = "%Y-%m-%d"),
                  format = "dd/mm/yyyy"
                )
              ),
              
              # Columna para Tratamiento Elegido
              div(
                style = "flex: 1; min-width: 300px;",
                h4("Tratamiento Elegido", style = "font-size: 15px; font-weight: bold; margin-bottom: 10px;"),
                selectInput(
                  inputId = "tratamiento_elegido1",
                  label = tags$span("Tratamiento", style = "font-size: 12px;"),
                  choices = c(
                    "Seguimiento", "Cdd Baigorria", "Cdd Buen Pastor", "Centro de día Zeballos", "Derivado",
                    "Internación B.P.", "Internación Baig.", "Internación Cristalería",
                    "No finalizó admisión", "Rechaza tratamiento", ""
                  ),
                  selected = ifelse(!is.na(registro$`Tratamiento Elegido`),
                                    registro$`Tratamiento Elegido`,
                                    "")
                )
              )
            )
          ),
          
          # Datos del Registro, Datos Personales y Contactos ---------------------------------
          fluidRow(
            # Columna para Datos del Registro
            column(
              width = 2,
              wellPanel(
                style = "min-height: 400px; padding-right: 5px;", 
                h4("Datos del Registro", style = "font-size: 15px; font-weight: bold; margin-bottom: 10px;"),
                # Campo no editable: ID de Registro
                textInput(
                  inputId = "id_registro1", 
                  label = "ID de Registro", 
                  value = registro$`ID de registro`
                ),
                tags$script('$("#id_registro1").prop("readonly", true);'), # Hacerlo no editable
                
                # Campo no editable: Fecha de Registro
                dateInput(
                  inputId = "fecha_registro1",
                  label = tags$span("Fecha de Registro", style = "font-size: 12px; white-space: nowrap;"),
                  value = as.Date(registro$`Fecha de registro`, format = "%Y-%m-%d"),
                  format = "dd/mm/yyyy"
                ),
                tags$script('$("#fecha_registro1").parent().find("input").prop("readonly", true);'), # Hacerlo no editable
                
                h4("Historial de Registro", style = "font-size: 15px; font-weight: bold; margin-bottom: 10px;"),
                
                # Campo no editable: ID de la persona
                textInput(
                  inputId = "id_persona1", 
                  label = "ID de la persona", 
                  value = registro$`ID de la persona`
                ),
                tags$script('$("#id_persona1").prop("readonly", true);') # Hacerlo no editable
              )
            ),
            
            # Columna para Datos Personales
            column(
              width = 6,
              wellPanel(
                style = "min-height: 400px;", 
                h4("Datos de la persona", style = "font-size: 15px; font-weight: bold;"),
                fluidRow(
                  # Campo recuerda DNI
                  column(
                    width = 3,
                    selectInput(
                      inputId = "recuerda_dni1",
                      label = tags$span("¿Recuerda el DNI?", style = "font-size: 12px;"),
                      choices = c("","Si", "No", "No tiene" = "S/D"),
                      selected = registro$`Recuerda DNI`
                    )
                  ),
                  
                  # Campo DNI
                  column(
                    width = 3,
                    numericInput(
                      inputId = "dni1",
                      label = tags$span("DNI", style = "font-size: 12px;"),
                      value = registro$DNI
                    )
                  ),
                  
                  # Apellido y Nombre
                  column(
                    width = 6,
                    textInput(
                      inputId = "apellido_nombre1",
                      label = tags$span("Apellido, Nombre (Apodo)", style = "font-size: 12px;"),
                      value = registro$`Apellido, Nombre`
                    )
                  )
                ),
                
                fluidRow(
                  # Fecha de nacimiento
                  column(
                    width = 3,
                    dateInput(
                      inputId = "fecha_nacimiento1",
                      label = tags$span("Fecha de nacimiento", style = "font-size: 12px;"),
                      value = as.Date(registro$`Fecha de Nacimiento`, format = "%Y-%m-%d"),
                      format = "dd/mm/yyyy",  
                      min = Sys.Date() - years(100),  # Limitar a 110 años atrás
                      max = Sys.Date()  # Limitar a la fecha de hoy
                    )
                  ),
                  
                  # Edad
                  column(
                    width = 3,
                    numericInput(
                      "edad1",
                      tags$span("Edad", style = "font-size:10px;"),
                      value = registro$`Edad del registro`
                    )
                  ),
                  
                  # Campo sexo biológico
                  column(
                    width = 3,
                    selectInput(
                      "sexo_biologico1",
                      label = tags$span("Sexo biológico", style = "font-size: 12px;"),
                      choices = c("No informado","Femenino", "Masculino", ""), 
                      selected = registro$`Sexo biológico`
                    )
                  ),
                  
                  # Campo género
                  column(
                    width = 3,
                    selectInput(
                      "genero1",
                      label = tags$span("Género", style = "font-size: 12px;"),
                      choices = c("No informado","Mujer", "Hombre", "Trans (feminidades)", "Trans (masculinidades)", "Otro", ""),  
                      selected = registro$Género
                    )
                  )
                ),
                
                fluidRow(
                  column(
                    width = 4,
                    selectInput(
                      "provincia1",
                      label = tags$span("Provincia de residencia", style = "font-size: 12px;"),
                      choices = provincias,  # Lista de provincias inicial
                      selected = registro$Provincia  # Valor seleccionado por defecto
                    )
                  ),
                  column(
                    width = 4,
                    selectInput(
                      inputId = "localidad1",
                      label = tags$span("Localidad", style = "font-size: 12px;"),
                      choices = NULL,  # Inicialmente vacío
                      selected = registro$Localidad  # Valor seleccionado por defecto
                    )
                  ),
                  
                  # Campo barrio
                  column(
                    width = 4,
                    textInput(
                      inputId = "barrio1",
                      label = tags$span("Barrio", style = "font-size: 12px;"),
                      value = registro$Barrio
                    )
                  )
                )
              )
            ),
            # Columna para Datos de contacto
            column(
              width = 4,
              wellPanel(
                style = "min-height: 400px;", 
                # Contacto 1
                fluidRow(
                  h4("Contacto 1", style = "font-size: 15px; font-weight: bold;"),
                  
                  # Contacto 1 - Teléfono
                  column(
                    width = 4,
                    numericInput(
                      inputId = "telefono_contacto_11",
                      label = tags$span("Teléfono", style = "font-size: 12px;"),
                      value = registro$`Teléfono de Contacto 1`
                    )
                  ),
                  
                  # Contacto 1 - Tipo de vínculo
                  column(
                    width = 4,
                    selectizeInput(
                      "tipo_vinculo_contacto_11",
                      label = tags$span("Vínculo", style = "font-size: 12px;"),
                      choices = c(unique(registro$`Tipo de Vínculo con el Contacto 1`),""),
                      selected = ifelse(!is.na(registro$`Tipo de Vínculo con el Contacto 1`),registro$`Tipo de Vínculo con el Contacto 1`,""),
                      options = list(create = TRUE)
                    )
                  ),
                  
                  # Contacto 1 - Nombre
                  column(
                    width = 4,
                    textInput(
                      inputId = "nombre_contacto_11",
                      label = tags$span("Nombre", style = "font-size: 12px;"),
                      value = registro$`Nombre del Contacto 1`
                    )
                  )
                )  ,              
                # Contacto 2
                fluidRow(
                  h4("Contacto 2", style = "font-size: 15px; font-weight: bold;"),
                  
                  # Contacto 2 - Teléfono
                  column(
                    width = 4,
                    numericInput(
                      inputId = "telefono_contacto_21",
                      label = tags$span("Teléfono", style = "font-size: 12px;"),
                      value = registro$`Teléfono de Contacto 2`
                    )
                  ),
                  
                  # Contacto 2 - Tipo de vínculo
                  column(
                    width = 4,
                    selectizeInput(
                      "tipo_vinculo_contacto_21",
                      label = tags$span("Vínculo", style = "font-size: 12px;"),
                      choices = c(unique(registro$`Tipo de Vínculo con el Contacto 2`),""),
                      selected = ifelse(!is.na(registro$`Tipo de Vínculo con el Contacto 2`),registro$`Tipo de Vínculo con el Contacto 2`,""),
                      options = list(create = TRUE)
                    )
                  ),
                  
                  # Contacto 2 - Nombre
                  column(
                    width = 4,
                    textInput(
                      inputId = "nombre_contacto_21",
                      label = tags$span("Nombre", style = "font-size: 12px;"),
                      value = registro$`Nombre del Contacto 2`
                    )
                  )
                ),                
                # Contacto 3
                fluidRow(
                  h4("Contacto 3", style = "font-size: 15px; font-weight: bold;"),
                  
                  # Contacto 3 - Teléfono
                  column(
                    width = 4,
                    numericInput(
                      inputId = "telefono_contacto_31",
                      label = tags$span("Teléfono", style = "font-size: 12px;"),
                      value = registro$`Teléfono de Contacto 3`
                    )
                  ),
                  
                  # Contacto 3 - Tipo de vínculo
                  column(
                    width = 4,
                    selectizeInput(
                      "tipo_vinculo_contacto_31",
                      label = tags$span("Vínculo", style = "font-size: 12px;"),
                      choices = c(unique(registro$`Tipo de Vínculo con el Contacto 3`),""),
                      selected = ifelse(!is.na(registro$`Tipo de Vínculo con el Contacto 3`),registro$`Tipo de Vínculo con el Contacto 3`,""),
                      options = list(create = TRUE)
                    )
                  ),
                  
                  # Contacto 3 - Nombre
                  column(
                    width = 4,
                    textInput(
                      inputId = "nombre_contacto_31",
                      label = tags$span("Nombre", style = "font-size: 12px;"),
                      value = registro$`Nombre del Contacto 3`
                    )
                  )
                )
              )
            )
          ),
          fluidRow(
          column(
            width = 5,
          wellPanel(
            style = "margin-top: 20px;", 
            
            # Inicio del consumo
            fluidRow(
              h4("Inicio del consumo", style = "font-size: 15px; font-weight: bold;"),
              column(
                width = 5,
                numericInput(
                  inputId = "edad_inicio_consumo1",
                  label = tags$span("Edad de inicio de consumo", style = "font-size: 12px;"),
                  value = registro$`Edad de Inicio de Consumo`
                )
              ),
              # Campo sustancia de inicio
              column(
                width = 5,
                style = "",
                selectInput(
                  inputId = "sustancia_inicio_consumo1",
                  label = tags$span("Sustancia de Inicio de Consumo", style = "font-size: 12px; white-space: nowrap;"),
                  choices = c("No informado", "Alcohol", "Crack", "Cocaína", "Marihuana", 
                              "Nafta aspirada", "Pegamento", "Psicofármacos", "Otra", ""),
                  selected = ifelse(registro$`Sustancia de inicio` == "Otra", "Otra", registro$`Sustancia de inicio`)
                )
              ),
              
              # Campo emergente de texto para "Otra" opción
              column(
                style = "",
                width = 2,  
                conditionalPanel(
                  condition = "input.sustancia_inicio_consumo1 == 'Otra'", # Se activa si se selecciona 'Otra'
                  textInput(
                    inputId = "otra_sustancia1",
                    label = tags$span("Especifique", style = "font-size: 12px;"),
                    value = ifelse(registro$`Sustancia de inicio` == "Otra", registro$`Inicio con Otras - Descripción`, "")
                  )
                )
              )
            ),
            
            # Consumo actual
            fluidRow(
              style = "margin-top: 20px;", # Espacio entre filas
              h4("Consumo actual", style = "font-size: 15px; font-weight: bold;"),
              column(
                width = 12,
                selectInput(
                  "persona_consume1",
                  label = tags$span("¿Consume actualmente?", style = "font-size: 12px;"),
                  choices = c("No informado", "", "Si", "No"),
                  selected = registro$`¿Consume actualmente?`
                )
              )
            ),
            
            # Sustancias de consumo actual
            fluidRow(
              column(
                width = 12,
                tags$div(
                  style = "margin-bottom: 10px;",
                  tags$span("Sustancia/s de Consumo Actual", style = "font-size: 12px; white-space: nowrap;")
                ),
                tags$div(
                  style = "column-count: 3; column-gap: 50px; margin-top: 10px;",
                  checkboxGroupInput(
                    inputId = "sustancias_consumo_actual1",
                    label = NULL,
                    choices = c("Alcohol", "Crack", "Cocaína", "Marihuana", "Nafta",
                                "Pegamento", "Psicofármacos", "Otra"),
                    selected = sustancias_seleccionadas
                  )
                )
              )
              ),
              
              # Campo emergente para "Otra"
              fluidRow(
                column(
                width = 12,
                style = "margin-top: 10px;",
                conditionalPanel(
                  condition = "input.sustancias_consumo_actual1.includes('Otra')", # Mostrar el campo si se selecciona "Otra"
                  textInput(
                    inputId = "otra_sustancia_actual1",
                    label = tags$span("Especifique la sustancia", style = "font-size: 12px;"),
                    value = ifelse(!is.null(registro$`Consumo actual con Otras - Descripción`) && 
                                     !is.na(registro$`Consumo actual con Otras - Descripción`), 
                                   registro$`Consumo actual con Otras - Descripción`, 
                                   "") # Precarga el valor si está disponible
                  )
                )
              )
            ),
            
            # Tratamientos previos
            fluidRow(
              style = "margin-top: 20px;",
              h4("Tratamiento", style = "font-size: 15px; font-weight: bold;"),
              
              # Campo de derivación
              column(
                width = 6,
                selectInput(
                  "derivacion1",
                  label = tags$span("Derivación", style = "font-size: 12px;"),
                  choices = c("No informado", "", "Si", "No"),
                  selected = registro$Derivación
                )
              ),
              
              # Campo de derivado de
              column(
                width = 6,
                textInput(
                  inputId = "derivado_de1",
                  label = tags$span("Derivado de", style = "font-size: 12px;"),
                  placeholder = "",
                  value = registro$`Derivado de`
                )
              ),
              
              # Campo de número de tratamientos previos
              column(
                width = 6,
                numericInput(
                  inputId = "num_tratamientos_previos1",
                  label = tags$span("Nº de Tratamientos Previos", style = "font-size: 12px;"),
                  value = registro$`Número de Tratamientos Previos`,
                  min = 0,
                  max = 99
                )
              ),
              
              # Campo emergente de texto para Número de tratamientos > 0
              column(
                width = 6,
                style = "margin-bottom: 10px;", 
                conditionalPanel(
                  condition = "input.num_tratamientos_previos1 > 0",  # Activo si el número de tratamientos es mayor que 0
                  textInput(
                    inputId = "lugar_ultimo_tratamiento1",
                    label = tags$span("Lugar de Último Tratamiento", style="font-size: 12px;"),
                    value = ifelse(!is.null(registro$`Lugar de Último Tratamiento`) && 
                                     !is.na(registro$`Lugar de Último Tratamiento`), 
                                   registro$`Lugar de Último Tratamiento`, 
                                   "")  # Precarga el valor si está disponible
                  )
                )
              )
            )
          )
          ),
          # Situación Socioeconómica, Jurídica y de Salud ------------------------------------------------------------
          column(
            width = 7,
          wellPanel(
            style = "margin-top: 20px;",
            
            fluidRow(
              h4("Situación Socioeconómica, Jurídica y de Salud", style = "font-size: 15px; font-weight: bold;"),
              
              column(
                width = 6,
                selectInput(
                  inputId = "nivel_educativo_max1",
                  tags$span("Máximo Nivel educativo alcanzado", style = "font-size: 12px;"),
                  choices = list( 
                    "No informado",
                    "Sin instrucción formal", 
                    "Primario incompleto", 
                    "Primario en curso", 
                    "Primario completo", 
                    "Secundario incompleto", 
                    "Secundario en curso", 
                    "Secundario completo", 
                    "Nivel superior incompleto", 
                    "Nivel superior en curso", 
                    "Nivel superior completo",
                    ""
                  ),
                  selected = registro$`Nivel Máximo Educativo Alcanzado`)
              ),
              column(
                width = 6,
                selectInput(
                  inputId = "cud1",
                  tags$span("CUD", style = "font-size: 12px;"),
                  choices = list(
                    "No informado",
                    "Si", 
                    "No", 
                    ""
                  ),
                  selected = registro$CUD)
              )
            ),
            fluidRow(
              # Campo principal: Situación Habitacional Actual
              column(
                width = 6,
                selectInput(
                  inputId = "situacion_habitacional_actual1",
                  label = tags$span("Situación Habitacional Actual", style = "font-size: 12px;"),
                  choices = list(
                    "No informada",
                    "Casa/Departamento", 
                    "Casa/Departamento alquilado", 
                    "Casa/Departamento cedido", 
                    "Casa/Departamento propio", 
                    "Institución de salud mental", 
                    "Institución penal", 
                    "Institución terapéutica", 
                    "Pensión", 
                    "Refugio", 
                    "Situación de calle", 
                    "Otra", 
                    ""
                  ),
                  selected = registro$`Situación Habitacional Actual` # Precarga el valor registrado
                )
              ),
              
              # Campo emergente: Especificar si selecciona "Otra"
              column(
                width = 6,
                conditionalPanel(
                  condition = "input.situacion_habitacional_actual1 == 'Otra'",  # Verifica si selecciona "Otra"
                  textInput(
                    inputId = "otra_situacion_habitacional_actual1",
                    label = tags$span("Especifique la situación habitacional", style = "font-size: 12px;"),
                    value = ifelse(
                      !is.null(registro$`Situación Habitacional Actual - Otra`) && !is.na(registro$`Situación Habitacional Actual - Otra`),
                      registro$`Situación Habitacional Actual - Otra`,  
                      ""  
                    )
                  ) 
                )
              )
            ),
            fluidRow(
              column(
                width = 6,
                selectInput(
                  inputId = "situacion_laboral_actual1",
                  tags$span("Situación Laboral Actual", style = "font-size: 12px;"),
                  choices = list( 
                    "No informado",
                    "Estable", 
                    "Esporádico", 
                    "No tiene",
                    "Otra",
                    ""
                  ),
                  selected = registro$`Situación Laboral Actual`)
              ),
              
              
              # Campo emergente de texto para "Otra" opción
              column(
                width = 6,
                conditionalPanel(
                  condition = "input.situacion_laboral_actual1.includes('Otra')",
                  textInput(
                    inputId = "otra_situacion_laboral_actual1",
                    label = tags$span("Especifique la situación laboral", style="font-size: 12px;"),
                    value = ""
                  ) 
                )
              )
            ),
            fluidRow(
              column(
                width = 12,
                tags$div(
                  style = "margin-bottom: 5px;",
                  tags$span("Ingreso Económico", style = "font-size: 12px; white-space: nowrap;")
                ),
                tags$div(
                  style = "column-count: 3; column-gap: 50px; margin-top: 10px;",  # Espacio entre columnas y margen superior
                  checkboxGroupInput(
                    inputId = "ingreso_economico1",
                    label = NULL,  # No mostramos la etiqueta aquí porque ya está arriba
                    choices = c(
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
                    selected = registro$`Ingresos Económicos`)
                  
                )
              ),
              column(
                width = 12,
                conditionalPanel(
                  condition = "input.ingreso_economico1.includes('Otro subsidio/plan social') || 
                     input.ingreso_economico1.includes('Otro tipo de pensión') || 
                     input.ingreso_economico1.includes('Otro tipo de ingreso')",
                  textInput(
                    inputId = "otro_ingreso1",
                    label = tags$span("Especifique el otro tipo de ingreso", style = "font-size: 12px;"),
                    value = ifelse(
                      !is.null(registro$`Ingresos Económicos - Otros`) && !is.na(registro$`Ingresos Económicos - Otros`),
                      registro$`Ingresos Económicos - Otros`,
                      ""  # Valor vacío si no existe información previa
                    )
                  )
                )
              )
            ),
            fluidRow(
              style = "margin-top: 20px;",
              column(
                width = 6,
                selectInput(
                  "situacion_judicial1",
                  label = tags$span("Situación Judicial", style = "font-size: 12px;"),
                  choices = c(
                    "No informada", "Sin causas", 
                    "Con causa cerrada", 
                    "Con causa abierta", 
                    "Desconoce", 
                    
                    "Otra",
                    ""
                  ),
                  selected = registro$`Situación Judicial`
                )
              ),
              column(
                width = 6,
                conditionalPanel(
                  condition = "input.situacion_judicial1.includes('Otra')",  # Verifica si selecciona "Otra"
                  textInput(
                    inputId = "otra_situacion_judicial1",
                    label = tags$span("Especifique la situación judicial", style = "font-size: 12px;"),
                    value = ifelse(
                      !is.null(registro$`Situación Judicial - Otro`) && !is.na(registro$`Situación Judicial - Otro`),
                      registro$`Situación Judicial - Otro`,
                      ""  # Valor vacío si no hay información previa
                    )
                  )
                )
              )
            )
          )
          )
          ),
          # Red de apoyo y referencia
          fluidRow(
            column(
              width = 6,
          wellPanel(
            style = "margin-top: 20px;",
            h4("Red de Apoyo y Referencias", style = "font-size: 15px; font-weight: bold;"),
            fluidRow(
              
              # Campo redes de apoyo
              column(
                width = 12,
                tags$div(
                  style = "margin-bottom: 5px;",
                  tags$span("Redes de Apoyo", style = "font-size: 12px; white-space: nowrap;")
                ),
                tags$div(
                  style = "column-count: 2; column-gap: 50px; margin-top: 10px;",  # Espacio entre columnas y margen superior
                  checkboxGroupInput(
                    inputId = "redes_apoyo1",
                    label = NULL,  # No mostramos la etiqueta aquí porque ya está arriba
                    choices = c("No informado",
                                "Familiares", 
                                "Amistades", 
                                "Institucionalidades",
                                "Sin vínculos actualmente"),
                    selected = registro$`Redes de Apoyo`
                  )
                )
              )
            ),
            fluidRow(
              # Campo referencias APS
              column(
                width = 12,  # Cambiado a 12 para que ocupe toda la fila
                style = "margin-top: 10px;",  # Ajusta el valor según el espacio que desees
                selectInput(
                  inputId = "referencia_aps1",
                  tags$span("Referencia APS", style = "font-size: 12px;"),
                  choices = list(
                    "No informada","Referencia con seguimiento", 
                    "Referencia sin seguimiento", 
                    "No está referenciado", 
                    
                    ""
                  ),
                  selected = registro$`Referencia a APS`
                )
              )
            ),
            fluidRow(
              # Campo equipo de referencia
              column(
                width = 12,  # Cambiado a 12 para que ocupe toda la fila
                textInput(
                  inputId = "equipo_referencia1",
                  label = tags$span("Equipo de Referencia", style = "font-size: 12px;"),
                  value = registro$`Equipo de Referencia`
                )
              )
            )
          )
          ),
          # Informacion Adicional
          column(
            width = 6,
          wellPanel(
            style = "margin-top: 20px;",
            
            h4("Información Adicional", style = "font-size: 15px; font-weight: bold;"),
            
            fluidRow(
              # Campo de Observaciones
              column(
                width = 12,  # Cambia el ancho según sea necesario
                textAreaInput(
                  inputId = "observaciones1",
                  label = tags$span("Observaciones", style = "font-size: 12px;"),
                  value = ifelse(!is.na(registro$Observaciones),
                                 registro$Observaciones,
                                 ""),
                  width = "100%",
                  height = "80px"  # Ajusta la altura según sea necesario
                )
              )
            )
          )
          )
          )
        ),
        
        footer = tagList(
          actionButton("save_button", tags$span("Guardar cambios",style = "font-size:12px"),
                       icon = icon("save"), width = 200,
                       class = "btn-primary"),
          modalButton(tags$span("Cancelar",style = "font-size:12px",
                                class = "btn-primary"))
        ),
        easyClose = TRUE
      ))
      
      
      observeEvent(input$provincia1, {
        localidades <- localidades_por_provincia[[input$provincia1]]
        updateSelectInput(
          session,
          inputId = "localidad1",
          choices = localidades,
          selected = iconv("Pérez", to = "ASCII//TRANSLIT")  # Mantiene el valor seleccionado por defecto
        )
      })
      
      
    } else {
      showNotification("Por favor, seleccione un registro para modificar.", type = "warning")
    }
  })
  
  # GUARDAR NUEVO REGISTRO
  
  validadores1 <- list(iv_recuerda_dni1,
                      iv_dni1, iv_apellido_nombre1, iv_edad1, iv_sexo_biologico1,
                      iv_genero1, iv_provincia1, iv_barrio1, iv_telefono_11,
                      iv_telefono_21, iv_telefono_31, iv_vinculo_11, iv_vinculo_21,
                      iv_vinculo_31, iv_nombre_11, iv_nombre_21, iv_nombre_31,
                      iv_estado_ts1, iv_estado_psiquiatra1, iv_estado_psicologo1,
                      iv_fecha_ts1, iv_fecha_psiquiatra1, iv_fecha_psicologo1,
                      iv_edad_inicio1, iv_sustancia_inicio1, iv_persona_consume1,
                      iv_sustancias_actual1, iv_derivacion1, iv_derivado_de1,
                      iv_tratamientos_previos1, iv_lugar_ultimo_tratamiento1,
                      iv_tratamiento_elegido1, iv_nivel_educativo_max1,
                      iv_cud1, iv_situacion_habitacional1, iv_situacion_laboral_actual1,
                      iv_situacion_judicial1, iv_ingreso_economico1, iv_redes_apoyo1,
                      iv_referencia_aps1, iv_equipo_referencia1)

observeEvent(input$save_button, {
  
  cambios <- list(
    
    # Datos de registro --------------------------------------------------
    `ID de registro` = ifelse(isTruthy(input$id_registro1), as.numeric(input$id_registro1), NA),
    `Fecha de registro` = as.POSIXct(ifelse(isTruthy(input$fecha_registro1), as.character(input$fecha_registro1), NA), format = "%Y-%m-%d"),
    `ID de la persona` = as.numeric(input$id_persona1),
    
    # Datos de la persona ------------------------------------------------
    `Recuerda DNI` = ifelse(isTruthy(input$recuerda_dni1), input$recuerda_dni1, NA),
    DNI = ifelse(isTruthy(input$dni1), input$dni1, NA),
    `Apellido, Nombre` = ifelse(isTruthy(input$apellido_nombre1), input$apellido_nombre1, NA),
    `Fecha de Nacimiento` = as.POSIXct(ifelse(isTruthy(input$fecha_nacimiento1), as.character(input$fecha_nacimiento1), NA),format = "%Y-%m-%d"),
    `Edad del registro` = ifelse(isTruthy(input$edad1), as.numeric(input$edad1), NA),
    `Sexo biológico` = ifelse(isTruthy(input$sexo_biologico1), input$sexo_biologico1, NA),
    `Género` = ifelse(isTruthy(input$genero1), input$genero1, NA),
    Provincia = ifelse(isTruthy(input$provincia1), input$provincia1, NA),
    Localidad = ifelse(isTruthy(input$localidad1), input$localidad1, NA),
    Barrio = ifelse(isTruthy(input$barrio1), input$barrio1, NA),
    
    # Contacto 1 ---------------------------------------------------------
    `Teléfono de Contacto 1` = ifelse(isTruthy(input$telefono_contacto_11), as.numeric(input$telefono_contacto_11), NA),
    `Tipo de Vínculo con el Contacto 1` = ifelse(isTruthy(input$tipo_vinculo_contacto_11), input$tipo_vinculo_contacto_11, NA),
    `Nombre del Contacto 1` = ifelse(isTruthy(input$nombre_contacto_11), input$nombre_contacto_11, NA),
    
    # Contacto 2 ---------------------------------------------------------
    `Teléfono de Contacto 2` = ifelse(isTruthy(input$telefono_contacto_21), as.numeric(input$telefono_contacto_21), NA),
    `Tipo de Vínculo con el Contacto 2` = ifelse(isTruthy(input$tipo_vinculo_contacto_21), input$tipo_vinculo_contacto_21, NA),
    `Nombre del Contacto 2` = ifelse(isTruthy(input$nombre_contacto_21), input$nombre_contacto_21, NA),
    
    # Contacto 3 ---------------------------------------------------------
    `Teléfono de Contacto 3` = ifelse(isTruthy(input$telefono_contacto_31), as.numeric(input$telefono_contacto_31), NA),
    `Tipo de Vínculo con el Contacto 3` = ifelse(isTruthy(input$tipo_vinculo_contacto_31), input$tipo_vinculo_contacto_31, NA),
    `Nombre del Contacto 3` = ifelse(isTruthy(input$nombre_contacto_31), input$nombre_contacto_31, NA),
    
    # Entrevista Psicólogo -----------------------------------------------
    `Estado de la Entrevista con Psicólogo` = ifelse(isTruthy(input$estado_psicologo1), input$estado_psicologo1, NA),
    `Fecha de la Entrevista con Psicológo` = input$fecha_entrevista_psicologo1,
    
    # Entrevista Psiquiatra -----------------------------------------------
    `Estado de la Entrevista con Psiquiátra` = ifelse(isTruthy(input$estado_psiquiatra1), input$estado_psiquiatra1, NA),
    `Fecha de la Entrevista con Psiquiátra` = input$fecha_entrevista_psiquiatra1,
    
    # Entrevista trabajador social ----------------------------------------
    `Estado de la Entrevista con Trabajador Social` = ifelse(isTruthy(input$estado_ts1), input$estado_ts1, NA),
    `Fecha de la Entrevista con Trabajador Social` = input$fecha_entrevista_ts1,
    
    # Inicio del consumo -------------------------------------------------
    `Edad de Inicio de Consumo` = ifelse(isTruthy(input$edad_inicio_consumo1), as.numeric(input$edad_inicio_consumo1), NA),
    `Sustancia de inicio` = ifelse(isTruthy(input$sustancia_inicio_consumo1), input$sustancia_inicio_consumo1, NA),
    `Inicio con Otras - Descripción` = ifelse(isTruthy(input$otra_sustancia1), input$otra_sustancia1, NA),
    
    # Consumo actual -----------------------------------------------------
    `¿Consume actualmente?` = ifelse(isTruthy(input$persona_consume1), input$persona_consume1, NA),
    `Consumo actual con Alcohol` = ifelse(isTruthy(input$sustancias_consumo_actual1) && "Alcohol" %in% input$sustancias_consumo_actual1, "Si", NA),
    `Consumo actual con Cocaína` = ifelse(isTruthy(input$sustancias_consumo_actual1) && "Cocaína" %in% input$sustancias_consumo_actual1, "Si", NA),
    `Consumo actual con Crack` = ifelse(isTruthy(input$sustancias_consumo_actual1) && "Crack" %in% input$sustancias_consumo_actual1, "Si", NA),
    `Consumo actual con Marihuana` = ifelse(isTruthy(input$sustancias_consumo_actual1) && "Marihuana" %in% input$sustancias_consumo_actual1, "Si", NA),
    `Consumo actual con Nafta Aspirada` = ifelse(isTruthy(input$sustancias_consumo_actual1) && "Nafta" %in% input$sustancias_consumo_actual1, "Si", NA),
    `Consumo actual con Pegamento` = ifelse(isTruthy(input$sustancias_consumo_actual1) && "Pegamento" %in% input$sustancias_consumo_actual1, "Si", NA),
    `Consumo actual con Psicofármacos` = ifelse(isTruthy(input$sustancias_consumo_actual1) && "Psicofármacos" %in% input$sustancias_consumo_actual1, "Si", NA),
    `Consumo actual con Otras` = ifelse(isTruthy(input$sustancias_consumo_actual1) && "Otra" %in% input$sustancias_consumo_actual1, "Si", NA),
    `Consumo actual con Otras - Descripción` = ifelse(isTruthy(input$otra_sustancia_actual1), input$otra_sustancia_actual1, NA),
    `Consumo actual con Policonsumo` = ifelse(length(input$sustancias_consumo_actual1) > 1, "Policonsumo", NA),
    
    # Tratamiento --------------------------------------------------------
    Derivación = ifelse(isTruthy(input$derivacion1), input$derivacion1, NA),
    `Derivado de`= ifelse(isTruthy(input$derivado_de1), input$derivado_de1, NA),
    `Número de Tratamientos Previos` = ifelse(isTruthy(input$num_tratamientos_previos1), as.numeric(input$num_tratamientos_previos1), NA),
    `Lugar de Último Tratamiento` = ifelse(isTruthy(input$lugar_ultimo_tratamiento1), input$lugar_ultimo_tratamiento1, NA),
    `Tratamiento Elegido` = ifelse(isTruthy(input$tratamiento_elegido1), input$tratamiento_elegido1, NA),
    
    # Situación Socioeconómica, Jurídica y de Salud -----------------------
    `Nivel Máximo Educativo Alcanzado` = ifelse(isTruthy(input$nivel_educativo_max1), input$nivel_educativo_max1, NA),
    CUD = ifelse(isTruthy(input$cud1), input$cud1, NA),
    `Situación Habitacional Actual` = ifelse(isTruthy(input$situacion_habitacional_actual1), input$situacion_habitacional_actual1, NA),
    `Situación Habitacional Actual - Otra` = ifelse(isTruthy(input$otra_situacion_habitacional_actual1), input$otra_situacion_habitacional_actual1, NA),
    `Situación Laboral Actual`  = ifelse(isTruthy(input$situacion_laboral_actual1), input$situacion_laboral_actual1, NA),
    `Situación Laboral Actual - Otra` = ifelse(isTruthy(input$otra_situacion_laboral_actual1), input$otra_situacion_laboral_actual1, NA),
    `Ingresos Económicos` = ifelse(!is.null(input$ingreso_economico1), paste(input$ingreso_economico1, collapse = ", "), NA),
    `Ingresos Económicos - Otros` = ifelse(!is.null(input$otro_ingreso1), input$otro_ingreso1, NA),
    `Situación Judicial` = ifelse(isTruthy(input$situacion_judicial1), input$situacion_judicial1, NA),
    `Situación Judicial - Otro` = ifelse(isTruthy(input$otra_situacion_judicial1), input$otra_situacion_judicial1, NA),
    
    # Red de Apoyo y Referencias ------------------------------------------
    `Redes de Apoyo` = ifelse(!is.null(input$redes_apoyo1), paste(input$redes_apoyo1, collapse = ", "), NA),
    `Referencia a APS` = ifelse(isTruthy(input$referencia_aps1), input$referencia_aps1, NA),
    `Equipo de Referencia` = ifelse(isTruthy(input$equipo_referencia), input$equipo_referencia1, NA),
    
    # Información Adicional -----------------------------------------------
    Observaciones = ifelse(isTruthy(input$observaciones1), input$observaciones1, NA)
    
  )
  
  cambios <- as.data.frame(lapply(cambios, function(col) {
    if (length(col) == 0) {
      return(NA)  # Si la columna está vacía, asigna NA
    } else {
      return(col)  # Mantén el valor original si tiene longitud válida
    }
  }), stringsAsFactors = FALSE)
  colnames(cambios) <- colnames(data)
  
  todos_validos1 <- all(sapply(validadores1, function(iv) iv$is_valid()))
  
  # Guardar el nuevo registro directamente en el archivo "Base completa.xlsx" ----
  if (todos_validos1) {
    
    data <- base()
    
    # Encontrar el índice de la fila con el mismo ID de registro
    id_to_replace <- as.numeric(input$id_registro1)
    
    row_index <- which(data$`ID de registro` == id_to_replace)
    
    if (length(row_index) == 1) {
      # Reemplazar la fila correspondiente con los nuevos datos
      data[row_index, ] <- cambios
    } else if (length(row_index) == 0) {
      # Si no se encuentra el ID, agregar el registro como uno nuevo
      data <- rbind(data, cambios)
    } else {
      showNotification("Error: Se encontraron múltiples registros con el mismo ID.", 
                       type = "error", 
                       duration = 5)
      return()  # Terminar la ejecución si hay múltiples IDs duplicados
    }
    
    # Guardar el archivo actualizado
    wb <- createWorkbook()
    addWorksheet(wb,"Registros")
    writeData(wb,"Registro",data)
    saveWorkbook(wb, "Registros", "Base completa.xlsx", overwrite = TRUE)
    
    showNotification("El registro ha sido actualizado con éxito.", 
                     type = "message", 
                     duration = 5)
  } else {
    showNotification("Por favor, complete todos los campos obligatorios.", 
                     type = "error", 
                     duration = 5)
  }
})

# Botón modificar registro
observeEvent(input$delete_button, {
  
  selected <- input$search_results_rows_selected  # Índice visual
  
  if (length(selected) > 0) {
    # Extraer la tabla renderizada con Temp_ID
    resultados_tabla <- search_results() %>%
      mutate(`Temp_ID` = row_number()) %>%
      arrange(desc(`Fecha de registro`))
    
    # Identificar el Temp_ID de la fila seleccionada
    temp_id <- resultados_tabla$Temp_ID[selected]
    
    # Extraer el registro correspondiente del dataset original
    registro <- search_results() %>% filter(row_number() == temp_id)
    
    # Guardar el registro en la variable reactiva
    registro_reactivo(registro)
  showModal(
    modalDialog(
      title = "Confirmar eliminación",
      HTML(
        paste0(
          "¿Estás seguro de que deseas eliminar este registro?",
          "<br><strong>ID de registro:</strong> ", registro$`ID de registro`,
          "<br><strong>Apellido, Nombre:</strong> ", registro$`Apellido, Nombre`
        )
      ),
      footer = tagList(
        modalButton(tags$span("No",size = "font-size:12px;")), # Cierra el modal
        actionButton("confirm_delete", tags$span("Si",size = "font-size:12px;"), class = "btn btn-danger") # Botón de confirmación
      )
    )
  )
  }
})

observeEvent(input$confirm_delete, {
  
  # Cargar la base de datos
  data <- base()
  selected <- input$search_results_rows_selected  # Índice visual
  
  if (length(selected) > 0) {
    # Extraer la tabla renderizada con Temp_ID
    resultados_tabla <- search_results() %>%
      mutate(`Temp_ID` = row_number()) %>%
      arrange(desc(`Fecha de registro`))
    
    # Identificar el Temp_ID de la fila seleccionada
    temp_id <- resultados_tabla$Temp_ID[selected]
    
    # Extraer el registro correspondiente del dataset original
    registro <- search_results() %>% filter(row_number() == temp_id)
  
  if (!is.null(registro)) {
    # Extraer el ID del registro a eliminar
    id_eliminar <- registro$`ID de registro`
    
    # Filtrar los datos para excluir el registro con el ID a eliminar
    datos_actualizados <- data %>%
      filter(`ID de registro` != id_eliminar)
    
    # Guardar los datos actualizados en el archivo Excel
    wb <- createWorkbook()
    addWorksheet(wb,"Registros")
    writeData(wb, "Registros", datos_actualizados)
    saveWorkbook(wb, "Base completa.xlsx", overwrite = TRUE)
    
    # Actualizar la base reactiva `search_results`
      search_results(resultados_tabla %>%
                       filter(`ID de registro` != id_eliminar))
    
    # Mostrar notificación de éxito
    showNotification("El registro ha sido eliminado correctamente.", 
                     type = "message", duration = 5)
  } else {
    # Mostrar notificación si no se encuentra el registro
    showNotification("Error: No se encontró el registro a eliminar.", 
                     type = "error", duration = 5)
  }
  }
  
  # Cerrar el modal
  removeModal()
})

# ------------------------------------------------------------------------------
# PESTAÑA VISUALIZACION
# ------------------------------------------------------------------------------

# Cargar datos sacando los registros personales repetidos 
datagraf <- data %>%
  group_by(`ID de la persona`) %>%
  filter(row_number() == n()) %>% 
  ungroup() %>%
  mutate(
    EdadCategorica = factor(
      ifelse(`Edad del registro` >= 0 & `Edad del registro` <= 12, "0 a 12",
             ifelse(`Edad del registro` >= 13 & `Edad del registro` <= 17, "13 a 17",
                    ifelse(`Edad del registro` >= 18 & `Edad del registro` <= 29, "18 a 29",
                           ifelse(`Edad del registro` >= 30 & `Edad del registro` <= 60, "30 a 60", 
                                  ifelse(`Edad del registro` >= 61, "+ 60", NA))))),
      levels = c("0 a 12", "13 a 17", "18 a 29", "30 a 60", "+ 60"),
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
                                  levels = c("No informada", "Sin causas", 
                                             "Con causa cerrada", 
                                             "Con causa abierta", 
                                             "Desconoce", 
                                             
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
                 ordered = TRUE),
    EdadInicioCategorica = factor(
      ifelse(`Edad de Inicio de Consumo` >= 0 & `Edad de Inicio de Consumo` <= 12, "0 a 12",
             ifelse(`Edad de Inicio de Consumo` >= 13 & `Edad de Inicio de Consumo` <= 17, "13 a 17",
                    ifelse(`Edad de Inicio de Consumo` >= 18 & `Edad de Inicio de Consumo` <= 29, "18 a 29",
                           ifelse(`Edad de Inicio de Consumo` >= 30 & `Edad de Inicio de Consumo` <= 60, "30 a 60", 
                                  ifelse(`Edad de Inicio de Consumo` >= 61, "+ 60", NA))))),
      levels = c("0 a 12", "13 a 17", "18 a 29", "30 a 60", "+ 60"),
      ordered = TRUE),
    `Sustancia de inicio`= factor(`Sustancia de inicio`,
                                  levels = c("Alcohol", "Crack", "Cocaína", "Marihuana",
                                             "Nafta aspirada", "Pegamento", "Psicofármacos", "Otra"),
                                  ordered = TRUE)
  )

# Extraer los años únicos de la base y actualizar el filtro de años
observe({
  years <- unique(year(datagraf$`Fecha de registro`)) # Extraer años únicos
  years <- sort(years[!is.na(years)]) # Ordenar y eliminar NAs
  choices <- c("Seleccione el año" = "", years) # Agregar texto indicativo
  updateSelectInput(session, "year_filter", choices = choices)
})

# Crear el data frame reactivo filtrado
filtered_data <- reactive({
  datagraf %>%
    filter(is.null(input$year_filter) | year(`Fecha de registro`) %in% input$year_filter,
           is.null(input$edad_filter) | EdadCategorica %in% c(input$edad_filter,NA), # agrego NA para que funcione el conteo de vacíos
           is.null(input$sexo_filter) | `Sexo biológico` %in% c(input$sexo_filter,NA))
})
# Extraer los años únicos de la base y actualizar el filtro de años
observe({
  years <- unique(year(datagraf$`Fecha de registro`)) # Extraer años únicos
  years <- sort(years[!is.na(years)]) # Ordenar y eliminar NAs
  choices <- c("Seleccione el año" = "", years) # Agregar texto indicativo
  updateSelectInput(session, "year_filter_2", choices = choices)
})

# Crear el data frame reactivo filtrado
filtered_data_2 <- reactive({
  datagraf %>%
    filter(is.null(input$year_filter_2) | year(`Fecha de registro`) %in% input$year_filter_2,
           is.null(input$edad_filter_2) | EdadCategorica %in% c(input$edad_filter_2,NA),
           is.null(input$nivel_educativo_filter) | `Nivel Máximo Educativo Alcanzado` %in% c(input$nivel_educativo_filter,NA),
           is.null(input$sit_laboral_filter) | `Situación Laboral Actual` %in% c(input$sit_laboral_filter,NA)
    )
})

# Extraer los años únicos de la base y actualizar el filtro de años
observe({
  years <- unique(year(datagraf$`Fecha de registro`)) # Extraer años únicos
  years <- sort(years[!is.na(years)]) # Ordenar y eliminar NAs
  choices <- c("Seleccione el año" = "", years) # Agregar texto indicativo
  updateSelectInput(session, "year_filter_3", choices = choices)
})

# Crear el data frame reactivo filtrado
filtered_data_3 <- reactive({
  datagraf %>%
    filter(is.null(input$year_filter_3) | year(`Fecha de registro`) %in% input$year_filter)
})

# Boxplot + histograma + tabla
output$histbox.edad <- renderPlotly({
  # Cargar base reactiva
  df <- filtered_data()
  
  # Conteo de registros sin información de edad
  conteo_na <- sum(is.na(df$`Edad del registro`))
  conteo_na <- paste("Hay",conteo_na,"de",nrow(df),"registros\npersonales sin información")
  
  # Filtrar edades no nulas y preparar los datos para el histograma
  edad_data <- df %>%
    filter(!is.na(`Edad del registro`))
  
  hist <- ggplot(edad_data) +
    geom_histogram(aes(x = `Edad del registro`, 
                       y = (..count..),
                       text = paste(
                         "Edad:", ..x.. -1, "a", ..x.. +1,
                         "\nFrecuencia:", ..count..)
    ),
    position = "identity", binwidth = 2,
    fill = "#ff8800", color = "grey1") +
    labs(x = "Edad de registro", 
         y = "Frecuencia", 
         title = "Distribución de la edad al momento de la admisión",
         subtitle = conteo_na) +
    scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 10)) +
    theme_fivethirtyeight() + 
    theme(axis.title = element_text())
  
  hist_plotly <- ggplotly(hist, tooltip = "text") %>%
    layout(title = list(y = 0.95,
                        text = "Distribución de la edad al momento de la admisión",
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
                        text = "Distribución de la edad al momento de la admisión",
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
    filter(!is.na(EdadCategorica),
           EdadCategorica != "+ 60") %>%
    mutate(conteo = ifelse(is.na(conteo),0,conteo),
           pos = c(12/2,13+(17-13)/2,18+(29-18)/2,30+(60-30)/2),
           cantmax = c(12,17,29,60),
           cantmin = c(0,13,18,30))
  
  tabla_edades <- ggplot(edades, aes(text = paste("Categoría de SEDONAR:", EdadCategorica, "\nCantidad:", conteo))) +
    geom_rect(aes(xmin = cantmin, xmax = cantmax, ymin = 0, ymax = 2, 
                  fill = EdadCategorica, alpha = 0.2)) +
    geom_text(aes(x = pos, y = 1, label = paste(EdadCategorica,paste("n:",conteo),sep = "\n")), color = "grey1", size = 3) +
    scale_fill_manual(values = c("#FBC91C", "#EC7E14", "#4C443C","#F9EDCC")) +
    scale_color_manual(values = c("#FBC91C", "#EC7E14", "#4C443C","#F9EDCC")) +
    labs(title = "Distribución de la edad al momento de la admisión") +
    scale_x_continuous(breaks = seq(0,60,10)) +
    scale_y_continuous(breaks = c(0,2)) +
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
                        text = "Distribución de la edad al momento de la admisión",
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

# Boxplot + histograma + tabla
output$histbox.edadinicio <- renderPlotly({
  # Cargar base reactiva
  df <- filtered_data_3()
  
  # Conteo de registros sin información de edad
  conteo_na <- sum(is.na(df$`Edad de Inicio de Consumo`))
  conteo_na <- paste("Hay",conteo_na,"de",nrow(df),"registros personales sin información")
  
  # Filtrar edades no nulas y preparar los datos para el histograma
  edad_data <- df %>%
    filter(!is.na(`Edad de Inicio de Consumo`))
  
  mean <- mean(edad_data$`Edad de Inicio de Consumo`)
  sd <- round(sd(edad_data$`Edad de Inicio de Consumo`),2)
  tooltip <- paste(conteo_na,"\nMedia:",mean,"\nDesvío:",sd)
  
  hist <- ggplot(edad_data) +
    geom_histogram(aes(x = `Edad de Inicio de Consumo`, 
                       y = (..count..),
                       text = paste(
                         "Edad:", ..x.. -1, "a", ..x.. +1,
                         "\nFrecuencia:", ..count..)
    ),
    position = "identity", binwidth = 2,
    fill = "#ff8800", color = "grey1") +
    labs(x = "Edad de inicio de consumo", 
         y = "Frecuencia", 
         title = "Distribución de la edad de inicio de consumo",
         subtitle = conteo_na) +
    scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 10)) +
    theme_fivethirtyeight() + 
    theme(axis.title = element_text())
  
  hist_plotly <- ggplotly(hist, tooltip = "text") %>%
    layout(title = list(y = 0.95,
                        text = "Distribución de la edad de inicio de consumo",
                        font = list(family = "Montserrat", size = 15, color = "grey1")),
           xaxis = list(title = list(text = "Edad de inicio de consumo",
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
      text = tooltip,
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
  
  # Generar boxplot
  
  box_plotly <- plot_ly(edad_data, x = ~`Edad de Inicio de Consumo`, type = "box",
                        jitter = 0.1,
                        marker = list(color = "grey10"),
                        line = list(color = "grey10"),
                        fillcolor = "#ff8800") %>%
    add_boxplot(hoverinfo = "x") %>%
    layout(title = list(y = 0.95,
                        text = "Distribución de la edad de inicio de consumo",
                        font = list(family = "Montserrat", size = 15, color = "grey1")),
           xaxis = list(title = list(text = "Edad de inicio de consumo",
                                     font = list(family = "Montserrat", size = 12, color = "grey1")),
                        tickvals = seq(0, 60, 10),
                        tickfont = list(family = "Montserrat", size = 10, color = "grey"),
                        hoverformat = ".2f"),
           yaxis = list(showticklabels = FALSE),
           hoverlabel = list(font = list(family = "Montserrat", size = 10, color = "white",
                                         style = "italic")))
  
  # Combinar histogram y boxplot
  subplot(box_plotly, hist_plotly, nrows = 2, heights = c(0.3, 0.7), 
          shareX = TRUE, titleX = TRUE, titleY = TRUE, margin = 0)
})

output$tabla_inicio_reg <- renderText({
  df <- filtered_data_3()
  
  tabla_cruzada <- df %>%
    filter(!is.na(EdadCategorica) & !is.na(EdadInicioCategorica)) %>%
    count(EdadCategorica, EdadInicioCategorica, .drop = FALSE) %>% 
    pivot_wider(
      names_from = EdadCategorica, 
      values_from = n, 
      values_fill = 0  
    ) %>%
    ungroup() %>%
    rename("Inicio \\ Actual" = EdadInicioCategorica) 
  
  tabla_cruzada <- tabla_cruzada %>%
    mutate(Total_Fila = rowSums(select(., -`Inicio \\ Actual`)))
  
  
  totales_columna <- tabla_cruzada %>%
    summarise(across(starts_with("0 a 12"):ends_with("+ 60"), sum, na.rm = TRUE))
  
  tabla_cruzada <- bind_rows(tabla_cruzada, c(`Inicio \\ Actual` = "Total_Columna", totales_columna))
  
  suma_total <- sum(tabla_cruzada$Total_Fila, na.rm = TRUE)
  
  tabla_cruzada <- tabla_cruzada %>%
    add_row(`Inicio \\ Actual` = "Total", !!!totales_columna, Total_Fila = suma_total)
  
  tabla_cruzada <- tabla_cruzada %>%
    filter(`Inicio \\ Actual` != "Total_Columna") %>%
    rename("Total" = Total_Fila) 
  
  kable(tabla_cruzada, format = "html", table.attr = "style='width:100%;'") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  font_size = 12) %>%
    add_header_above(c("Edad de inicio de consumo vs edad registrada" = ncol(tabla_cruzada))) %>%  
    column_spec(1, bold = TRUE, width = "150px", background = "#ffb600", color = "white") %>%  
    row_spec(0, background = "#ffb600", color = "white") %>%
    row_spec(nrow(tabla_cruzada), bold = TRUE)%>%  
    column_spec(7, bold = TRUE)
})

output$sustancias <- renderText({
  df <- filtered_data_3()
  
  tabla_s_inicio <- df %>%
    group_by(`ID de la persona`) %>%
    filter(row_number() == n()) %>%
    select(`Sustancia de inicio`) %>%
    filter(!is.na(`Sustancia de inicio`)) %>%  
    group_by(`Sustancia de inicio`) %>%
    summarize(conteo = n(), .groups = 'drop') %>%
    mutate(
      Porcentaje = round((conteo / sum(conteo)) * 100, 2)
    ) %>%
    ungroup() 
  
  tabla_s_inicio <- tabla_s_inicio %>%
    mutate(`Sustancia de inicio` = fct_reorder(`Sustancia de inicio`,
                                               as.numeric(`Sustancia de inicio` != "Otras")))
  
  kable(tabla_s_inicio, format = "html", table.attr = "style='width:100%;'") %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  font_size = 12) %>%
    row_spec(0, background = "#ffb600", color = "white") %>%
    add_header_above(if(ncol(tabla_s_inicio) == 6) c("Sustancia de inicio" = 5) else NULL) %>%
    column_spec(1, width = "25em", extra_css = "white-space: nowrap; overflow: hidden; text-overflow: ellipsis;")
})

output$barras_sustancias <- renderPlotly({
  
  df <- filtered_data_3()
  
  df <- df %>%
    pivot_longer(
      cols = c("Consumo actual con Alcohol",
               "Consumo actual con Crack",
               "Consumo actual con Cocaína",
               "Consumo actual con Marihuana",
               "Consumo actual con Nafta Aspirada",
               "Consumo actual con Pegamento",
               "Consumo actual con Psicofármacos",
               "Consumo actual con Otras"
      ),
      names_to = "Sustancia",
      values_to = "Consume"
    ) %>%
    filter(Consume == "Si") %>%
    mutate(
      Sustancia = factor(case_when(
        Sustancia == "Consumo actual con Alcohol" ~ "Alcohol",
        Sustancia == "Consumo actual con Crack" ~ "Crack",
        Sustancia == "Consumo actual con Cocaína" ~ "Cocaína",
        Sustancia == "Consumo actual con Marihuana" ~ "Marihuana",
        Sustancia == "Consumo actual con Nafta Aspirada" ~ "Nafta Aspirada",
        Sustancia == "Consumo actual con Pegamento" ~ "Pegamento",
        Sustancia == "Consumo actual con Psicofármacos" ~ "Psicofármacos",
        Sustancia == "Consumo actual con Otras" ~ "Otras"
      ), levels = c("Alcohol", "Crack", "Cocaína", "Marihuana", "Nafta aspirada","Pegamento", "Psicofármacos", "Otra"),
      ordered = TRUE),
      `Sustancia de inicio`= factor(`Sustancia de inicio`,
                                    levels = c("Alcohol", "Crack", "Cocaína", "Marihuana",
                                               "Nafta aspirada", "Pegamento", "Psicofármacos", "Otra"),
                                    ordered = TRUE)
    ) %>%
    select(`ID de registro`, Sustancia, `Sustancia de inicio`) %>%
    group_by(`Sustancia de inicio`,Sustancia)%>%
    summarise(conteo = n(), .groups = "drop") %>%
    filter(!is.na(Sustancia),!is.na(`Sustancia de inicio`)) %>%
    complete(Sustancia, `Sustancia de inicio`, fill = list(conteo = 0))
  
  g <- ggplot(df, aes(x = conteo, y = Sustancia, fill = `Sustancia de inicio`,
                      text = paste(
                        "\nSustancia de inicio:", ..fill..,
                        "\nFrecuencia:", x))) +
    geom_bar(stat = "identity", position = "stack") + # Usa "stack" para apilar, "dodge" para barras lado a lado
    labs(
      x = "Frecuencia",
      y = "Sustancia de consumo actual",
      fill = "Sustancia de Inicio",
      title = "Sustancia de inicio en cada sustancia de consumo actual"
    )+
    scale_fill_manual(values = c("#FBC91C", "#828a00", "#274001", "#EC7E14", "#4d8584", "#a62f03", "#400d01", "#4C443C")) +
    theme_fivethirtyeight() +
    theme(
      legend.position = "right",
      legend.title.position = "top",
      legend.title = element_text(hjust = 0.5),      # Centrar el título de la leyenda
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      plot.title = element_text(hjust = 0.5)
    )
  ggplotly(g, tooltip = "text") %>%
    layout(
      title = list(
        y = 0.93,
        title_x = 0.2,
        font = list(family = "Montserrat", size = 15, color = "grey1"),
        pad = list(l = -80)
      ),
      xaxis = list(
        title = list(
          text = "Frecuencia",
          font = list(family = "Montserrat", size = 12, color = "grey1")
        ),
        tickfont = list(family = "Montserrat", size = 10, color = "grey")
      ),
      yaxis = list(
        title = list(
          text = "",
          font = list(family = "Montserrat", size = 12, color = "grey1")
        ),
        tickfont = list(family = "Montserrat", size = 12, color = "grey")
      ),
      legend = list(
        title = list(
          text = "Sustancia de Inicio", # Texto del título de la leyenda
          font = list(family = "Montserrat", size = 12, color = "grey1") # Estilo del título
        ),
        font = list(family = "Montserrat", size = 10, color = "grey") # Estilo del texto de la leyenda
      ),
      hoverlabel = list(
        font = list(
          family = "Montserrat",
          size = 10,
          color = "white",
          style = "italic",
          textcase = "word caps"
        )
      )
    )
})

output$barras_edad_sustancias <- renderPlotly({
  df <- filtered_data_3()
  
  df <- df %>%
    select(EdadCategorica,`Sustancia de inicio`) %>%
    group_by(EdadCategorica,`Sustancia de inicio`) %>%
    summarise(conteo = n()) %>%
    ungroup() %>%
    filter(!is.na(EdadCategorica)) %>%
    complete(EdadCategorica, `Sustancia de inicio`, fill = list(conteo = 0))
  
  g <- ggplot(df, aes(x = conteo, y = EdadCategorica, fill = `Sustancia de inicio`,
                      text = paste(
                        "\nSustancia de inicio:", ..fill..,
                        "\nFrecuencia:", x))) +
    geom_bar(stat = "identity", position = "stack") + # Usa "stack" para apilar, "dodge" para barras lado a lado
    labs(
      x = "Frecuencia",
      y = "Grupo de Edad (SEDRONAR)",
      fill = "Sustancia de Inicio",
      title = "Sustancia de inicio en cada grupo de edad (SEDRONAR)"
    )+
    scale_fill_manual(values = c("#FBC91C", "#828a00", "#274001", "#EC7E14", "#4d8584", "#a62f03", "#400d01", "#4C443C")) +
    theme_fivethirtyeight() +
    theme(
      legend.position = "right",
      legend.title.position = "top",
      legend.title = element_text(hjust = 0.5),      # Centrar el título de la leyenda
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      plot.title = element_text(hjust = 0.5)
    )
  ggplotly(g, tooltip = "text") %>%
    layout(
      title = list(
        y = 0.93,
        title_x = 0.2,
        font = list(family = "Montserrat", size = 15, color = "grey1"),
        pad = list(l = -80)
      ),
      xaxis = list(
        title = list(
          text = "Frecuencia",
          font = list(family = "Montserrat", size = 12, color = "grey1")
        ),
        tickfont = list(family = "Montserrat", size = 10, color = "grey")
      ),
      yaxis = list(
        title = list(
          text = "",
          font = list(family = "Montserrat", size = 12, color = "grey1")
        ),
        tickfont = list(family = "Montserrat", size = 12, color = "grey")
      ),
      legend = list(
        title = list(
          text = "Sustancia de Inicio", # Texto del título de la leyenda
          font = list(family = "Montserrat", size = 12, color = "grey1") # Estilo del título
        ),
        font = list(family = "Montserrat", size = 10, color = "grey") # Estilo del texto de la leyenda
      ),
      hoverlabel = list(
        font = list(
          family = "Montserrat",
          size = 10,
          color = "white",
          style = "italic",
          textcase = "word caps"
        )
      )
    )
  
})

}

shinyApp(ui, server)
