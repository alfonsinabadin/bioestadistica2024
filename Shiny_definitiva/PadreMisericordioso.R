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

## User Interface --------------------------------------------------------------
ui <- page_navbar(
  
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
                  "Cdd Baigorria",
                  "Cdd Buen Pastor",
                  "Centro de día Zeballos",
                  "Derivado",
                  "Internación B.P.",
                  "Internación Baig.",
                  "Internación Cristalería",
                  "No finalizó admisión",
                  "Rechaza tratamiento",
                  "Seguimiento",
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
                      "Sin causas", 
                      "Con causa cerrada", 
                      "Con causa abierta", 
                      "Desconoce", 
                      "No informada", 
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
                    "Referencia con seguimiento", 
                    "Referencia sin seguimiento", 
                    "No está referenciado", 
                    "No informada",
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
    tags$span("Consulta y modificación de registros", style = "font-size: 14px;"),
    class = "bslib-page-dashboard"
  ),
  
  nav_panel(
    tags$span("Tablero de visualización", style = "font-size: 14px;"),
    class = "bslib-page-dashboard",
  sidebarLayout(
    sidebarPanel(
      h4("Menú de categorías"),
      selectInput("anio", "Seleccione un año:", 
                  choices = c("Todos los años", 
                              sort(unique(format(data$`Fecha de registro`, "%Y")))), 
                  selected = "Todos los años"),
      tabsetPanel(
        id = "menu_tabs",
        tabPanel("Características Demográficas"),
        tabPanel("Características Sociales y Económicas"),
        tabPanel("Características de Consumo"),
        tabPanel("Derivación y Tratamiento Asignado")
      )
    ),
    mainPanel(
      uiOutput("contenido_pestana"),
      conditionalPanel(
        condition = "input.menu_tabs == 'Características de Consumo'",
        tags$div(style = "margin-bottom: 20px;", plotlyOutput("hist_edad_inicio")),
        uiOutput("tabla_inicio_reg"),
        uiOutput("tabla_s_inicio"),
        uiOutput("tabla_s_actual"),
        tags$div(style = "margin-bottom: 20px;", plotlyOutput("acual_vs_inicio")),
        tags$div(style = "margin-bottom: 20px;", plotlyOutput("edad_sinicio")),
        tags$div(style = "margin-bottom: 20px;", plotlyOutput("educ_sustancia"))
      ),
      conditionalPanel(
        condition = "input.menu_tabs == 'Derivación y Tratamiento Asignado'",
        tags$div(style = "margin-bottom: 20px;", plotlyOutput("trat_previos")),
        tags$div(style = "margin-bottom: 20px;", plotlyOutput("trat_asignados")),
        tags$div(style = "margin-bottom: 20px;", plotlyOutput("cons_vs_trat"))
      )
  )
)
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
    if (value < 0 || value > 99) {
      return(tags$span("El número debe estar entre 0 y 99.",style = "font-size:10px;"))
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
  iv_apellido_nombre$add_rule("tratamiento_elegido", sv_required("Campo obligatorio"))
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
        
        edad_inicio_consumo <- last(dni_existente$`Edad de Inicio de Cosumo`)
        updateTextInput(session, "edad_inicio_consumo", value = edad_inicio_consumo)
        
        
        # Vector con los nombres de las drogas (para el selectInput)
        drogas <- c("NoAlcohol", "Cocaína", "Crack", "Marihuana", "Nafta aspirada", "Pegamento", "Psicofármacos", "Otra","")
        
        # Vector con los nombres de las columnas correspondientes en el data frame
        columnas <- c("Inicio con Alcohol", "Inicio con Cocaína", "Inicio con Crack", 
                      "Inicio con Marihuana", "Inicio con Nafta aspirada", 
                      "Inicio con Pegamento", "Inicio con Psicofármacos", "Inicio con Otras")
        
        # Vector observado
        ultima_fila  <- tail(dni_existente[,30:38],1)
        
        # Columna con si
        columna_con_si <- columnas[which(ultima_fila == "Si")]
        
        if (length(columna_con_si) == 1) {
          # Obtener el índice de la droga correspondiente
          indice <- match(columna_con_si, columnas)
          
          # Seleccionar la droga en el selectInput
          seleccionar <- drogas[indice]
          updateSelectInput(session, "sustancia_inicio_consumo", selected = seleccionar)
        } else {
          # Si no hay ninguna columna con "Sí", dejar el selectInput vacío
          updateSelectInput(session, "sustancia_inicio_consumo", selected = "")
        }
        
        if (seleccionar == "Otra") {
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
  
  observeEvent(input$guardar_registro, {
    
    nuevo_registro <- data.frame(
      
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
      `Estado de la Entrevista con Psicológo` = ifelse(isTruthy(input$estado_psicologo), input$estado_psicologo, NA),
      `Fecha de la Entrevista con Psicológo` = ifelse(isTruthy(input$fecha_entrevista_psiquiatra), as.character(input$fecha_entrevista_psiquiatra), NA),
      
      # Entrevista Psiquiatra -----------------------------------------------
      `Estado de entrevista con Psiquiátra` = ifelse(isTruthy(input$estado_psiquiatra), input$estado_psiquiatra, NA),
      `Fecha de la Entrevista con Psiquiátra` = ifelse(isTruthy(input$fecha_entrevista_psiquiatra), as.character(input$fecha_entrevista_psiquiatra), NA),
      
      # Entrevista trabajador social ----------------------------------------
      `Estado de entrevista con Trabajador Social` = ifelse(isTruthy(input$estado_ts), input$estado_ts, NA),
      `Fecha de la Entrevista con Trabajador Social` = ifelse(isTruthy(input$fecha_entrevista_ts), as.character(input$fecha_entrevista_ts), NA),
      
      # Inicio del consumo -------------------------------------------------
      `Edad de Inicio de Cosumo` = ifelse(isTruthy(input$edad_inicio_consumo), as.numeric(input$edad_inicio_consumo), NA),
      `Sustancia de inicio` = ifelse(isTruthy(input$sustancia_inicio_consumo), input$sustancia_inicio_consumo, NA),
      `Inicio con Otras - Descripción` = ifelse(isTruthy(input$otra_sustancia), input$otra_sustancia, NA),
     
      # Consumo actual -----------------------------------------------------
      `¿Consume actualmente?` = ifelse(isTruthy(input$persona_consume), input$persona_consume, NA),
      `Consumo actual con Alcohol` = ifelse(isTruthy(input$sustancias_consumo_actual) && "Alcohol" %in% input$sustancias_consumo_actual, "Alcohol", NA),
      `Consumo actual con Cocaína` = ifelse(isTruthy(input$sustancias_consumo_actual) && "Cocaína" %in% input$sustancias_consumo_actual, "Cocaína", NA),
      `Consumo actual con Crack` = ifelse(isTruthy(input$sustancias_consumo_actual) && "Crack" %in% input$sustancias_consumo_actual, "Crack", NA),
      `Consumo actual con Marihuana` = ifelse(isTruthy(input$sustancias_consumo_actual) && "Marihuana" %in% input$sustancias_consumo_actual, "Marihuana", NA),
      `Consumo actual con Nafta Aspirada` = ifelse(isTruthy(input$sustancias_consumo_actual) && "Nafta" %in% input$sustancias_consumo_actual, "Nafta", NA),
      `Consumo actual con Pegamento` = ifelse(isTruthy(input$sustancias_consumo_actual) && "Pegamento" %in% input$sustancias_consumo_actual, "Pegamento", NA),
      `Consumo actual con Psicofármacos` = ifelse(isTruthy(input$sustancias_consumo_actual) && "Psicofármacos" %in% input$sustancias_consumo_actual, "Psicofármacos", NA),
      `Consumo actual con Otras` = ifelse(isTruthy(input$sustancias_consumo_actual) && "Otra" %in% input$sustancias_consumo_actual, "Otra", NA),
      `Consumo actual con Otras - Descripción` = ifelse(isTruthy(input$otra_sustancia_actual), input$otra_sustancia_actual, NA),
      `Consumo actual con Policonsumo` = ifelse(isTruthy(input$sustancias_consumo_actual) && "Policonsumo" %in% input$sustancias_consumo_actual, "Policonsumo", NA),
      
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
      `Ingresos Económicos` = ifelse(isTruthy(input$ingreso_economico), paste(input$ingreso_economico, collapse = ", "), NA),
      `Situación Judicial` = ifelse(isTruthy(input$situacion_judicial), input$situacion_judicial, NA),
      `Situación Judicial - Otro` = ifelse(isTruthy(input$otra_situacion_judicial), input$otra_situacion_judicial, NA),
    
      # Red de Apoyo y Referencias ------------------------------------------
      `Redes de Apoyo` = ifelse(isTruthy(input$redes_apoyo), paste(input$redes_apoyo, collapse = ", "), NA),
      `Referencia a APS` = ifelse(isTruthy(input$referencia_aps), input$referencia_aps, NA),
      `Equipo de Referencia` = ifelse(isTruthy(input$equipo_referencia), input$equipo_referencia, NA),
      
      # Información Adicional -----------------------------------------------
      Observaciones = ifelse(isTruthy(input$observaciones), input$observaciones, NA),
      check.names = FALSE
      
    )
    
    # Validaciones globales
    iv <- InputValidator$new()
    # Datos de la persona ------------------------------------------------------
    
    # Recuerda DNI
    iv$add_rule("recuerda_dni",
                sv_required(
                  tags$span("Campo obligatorio.", style = "font-size: 10px;")
                ))
    
    # Apellido, Nombre
    
    iv$add_rule("apellido_nombre", 
                sv_required(tags$span("Campo obligatorio.", 
                                      style = "font-size: 10px;")
                )
    )
    
    # Edad 
    
    iv$add_rule("edad", function(value) {
      if(!isTruthy(input$fecha_nacimiento)) {
        if(length(as.numeric(input$edad)) == 0 | is.na(input$edad)) {
          return(tags$span("Campo obligatorio.", style = "font-size: 10px;"))
        }
      }
    })
  
    
    # Sexo biológico
    
    iv$add_rule("sexo_biologico",
                sv_required(tags$span("Campo obligatorio.",
                                      style = "font-size: 10px;")
                ))
    
    
    # Género
    
    iv$add_rule("genero",
                sv_required(tags$span("Campo obligatorio.",
                                      style = "font-size: 10px;")
                ))
    
    # Provincia
    
    iv$add_rule("provincia", function(value) {
      if(value == provincias[[1]][1]) {
        return(tags$span("Campo obligatorio.",
                         style = "font-size: 10px;"))
      }
    })
    
    # Contactos de referencia --------------------------------------------------
    
    # Contacto 1 - Teléfono
    
    iv$add_rule("telefono_contacto_1",
                sv_required(tags$span("Campo obligatorio.",
                                      style = "font-size: 10px;")
                )
    )
    
    iv$add_rule("telefono_contacto_1", function(value) {
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
    validar_telefono <- function(value) {
      if (is.null(value) || is.na(value)) {
        return(NULL) 
      }
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
      
      return(NULL)
    }
    
    # Contacto 1 - Vinculo
    
    iv$add_rule("tipo_vinculo_contacto_1",
                sv_required(tags$span("Campo obligatorio.",
                                      style = "font-size: 10px;")
                ))
    
    iv$add_rule("tipo_vinculo_contacto_1", function(value) {
      opciones_validas <- c("", "Propio", "Papá/Mamá", "Hermano/a", "Hijo/a", "Amigo/a")
      if (value %in% opciones_validas) {
        return(NULL)
      }
      
      if (value != "") {
        if (nchar(value) < 2) {
          return("El campo debe tener al menos 2 caracteres.")
        }
        if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {
          return("No se admiten caracteres especiales.")
        }
      }
      
      return(NULL)
    })
    
    # Contacto 1 - Nombre
    
    iv$add_rule("nombre_contacto_1", function(value) {
      if (input$tipo_vinculo_contacto_1 != "Propio") {
        if (value == "") {
          return(tags$span("Campo obligatorio.", style = "font-size: 10px;"))
        }
      }
      return(NULL)
    })
    
    iv$add_rule("nombre_contacto_1", function(value) {
      if (value != "") {
        if (nchar(value) < 2) {
          return(tags$span("El campo debe tener al menos 2 caracteres.", style = "font-size: 10px;"))
        }
      }
      return(NULL)
    })
    
    iv$add_rule("nombre_contacto_1",function(value) {
      esta <- input$tipo_vinculo_contacto_1 %in% c("","Propio","Papá/Mamá", "Hermano/a", "Hijo/a", "Amigo/a")
      if (!esta) {
        if(!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ /]+$", value)) {  # Incluye el símbolo '/'
          return(tags$span("No se admiten caracteres especiales.", style = "font-size: 10px;"))
        }
      }
    })
    
    # Entrevistas --------------------------------------------------------------
    
    # Entrevista psicologo - Estado
    
    iv$add_rule("estado_psicologo", sv_required(message = tags$span("Campo obligatorio", style = "font-size: 10px;")))
    
    # Entrevista psiquiatra - Estado
    
    iv$add_rule("estado_psiquiatra", sv_required(message = tags$span("Campo obligatorio", style = "font-size: 10px;")))
    
    # Entrevista ts - Estado
    
    iv$add_rule("estado_ts", sv_required(message = tags$span("Campo obligatorio", style = "font-size: 10px;")))
    
    # Inicio del consumo -------------------------------------------------------
    
    # Sustancia de inicio
    
    iv$add_rule("sustancia_inicio_consumo", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))
    
    iv$add_rule("otra_sustancia", function(value) {
      if ("Otra" %in% input$sustancia_inicio_consumo) {
        
        if (is.null(value) || value == "") {
          return(tags$span("Debe completar el campo si selecciona 'Otra'.", style = "font-size:10px;"))
        } else if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {
          return(tags$span("No se admiten caracteres especiales.", style = "font-size:10px;"))
        }
        
      } else {
        return(NULL)
      }
    })
    
    # Consumo actual -----------------------------------------------------------
    
    # Consumo actual
    
    iv$add_rule("persona_consume", sv_required(tags$span("Campo obligatorio.", style = "font-size: 10px;")))
    
    # Sustancia de consumo actual
    
    iv$add_rule("sustancias_consumo_actual", function(value) {
      if(input$persona_consume == "Si") {
        if(is.null(value) || length(value) == 0) {
          return(tags$span("Campo obligatorio.", style = "font-size: 10px;"))
        }
      }
    })
    
    iv$add_rule("otra_sustancia_actual", function(value) {
      if ("Otra" %in% input$sustancias_consumo_actual) {
        
        if (is.null(value) || value == "") {
          return(tags$span("Debe completar el campo si selecciona 'Otra'.", style = "font-size:10px;"))
        } else if (!grepl("^[a-zA-ZáéíóúÁÉÍÓÚñÑ ]+$", value)) {
          return(tags$span("No se admiten caracteres especiales.", style = "font-size:10px;"))
        }
        
      } else {
        return(NULL)
      }
    })
    
    # Tratamiento --------------------------------------------------------------
    
    # Derivación
    iv$add_rule("derivacion", sv_required(tags$span("Campo obligatorio.",style = "font-size:10px;")))
    
    # Derivado de
    
    iv$add_rule("derivado_de", function(value) {
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
    
    # Nº de tratameintos previos
    
    iv$add_rule("num_tratamientos_previos", function(value) {
      if (value < 0 || value > 99) {
        return(tags$span("El número debe estar entre 0 y 99.",style = "font-size:10px;"))
      }
    })
    
    # Lugar de último tratameinto
    
    iv$add_rule("lugar_ultimo_tratamiento", function(value) {
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
    
    # Tratameinto elegido
    
    iv$add_rule("tratamiento_elegido", sv_required("Campo obligatorio"))
    iv$add_rule("tratamiento_elegido", function(value) {
      if (is.null(value) || value == "") {
        return(tags$span("Campo obligatorio.",style = "font-size:10px;"))
      }
      return(NULL)
    })
    
    # Situación Socioeconómica, Jurídica y de Salud ----------------------------
    
    # Educación
    
    iv$add_rule("nivel_educativo_max", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))
    
    # CUD
    
    iv$add_rule("cud", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))
    
    # Situción habitacional
    
    iv$add_rule("situacion_habitacional_actual", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))
    
    iv$add_rule("otra_situacion_habitacional_actual", function(value) {
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
    
    # Situación laboral 
    
    iv$add_rule("situacion_laboral_actual", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))
    
    iv$add_rule("otra_situacion_laboral_actual", function(value) {
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
    
    # Ingreso económico
    
    iv$add_rule("ingreso_economico", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))
    
    iv$add_rule("otro_ingreso", function(value) {
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
    
    # Situción judicial
    
    iv$add_rule("situacion_judicial", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))
    
    iv$add_rule("otra_situacion_judicial", function(value) {
      if (input$situacion_judicial == "Otra" && (is.null(value) || value == "")) {
        return(tags$span("Debe completar el campo si selecciona 'Otra'.",style = "font-size:10px;"))
      }
      return(NULL)
    })
    
    # Red de Apoyo y Referencias -----------------------------------------------
    
    # Redes de apoyo
    
    iv$add_rule("redes_apoyo", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))
    
    # Referencias APS
    
    iv$add_rule("referencia_aps", sv_required(tags$span("Campo obligatorio.",style="font-size:10px;")))
    
    # Equipo de referencia
    
    iv$add_rule("equipo_referencia", function(value) {
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
    
    iv$enable()  # Habilitar validaciones
    
    
    # Guardar el nuevo registro directamente en el archivo "Base completa.xlsx" ----
    if (iv$is_valid()) {
      wb <- loadWorkbook("Base completa.xlsx")
      datos_existentes <- read.xlsx(wb, sheet = 1)
      
      # Asegurar que los nombres de las columnas sean los mismos
      colnames(datos_existentes) <- colnames(nuevo_registro)
      
      # Ahora puedes hacer el rbind sin problemas
      datos_actualizados <- rbind(datos_existentes, nuevo_registro)
      
      # Guardar el archivo actualizado
      writeData(wb, sheet = 1, datos_actualizados)
      saveWorkbook(wb, "Base completa.xlsx", overwrite = TRUE)
      
      showNotification("Registro guardado con éxito.", type = "message")
    } else {
      showNotification("Por favor, complete todos los campos obligatorios.", type = "error")
    }
    
  })
  
  
# PESTAÑA VISUALIZACION
  datos_filtrados <- reactive({
    if (input$anio == "Todos los años") {
      return(data)  # Devuelve todos los datos
    } else {
      anio_seleccionado <- as.numeric(input$anio)
      return(data[data$`Fecha de registro` >= as.Date(paste0(anio_seleccionado, "-01-01")) & 
                    data$`Fecha de registro` < as.Date(paste0(anio_seleccionado + 1, "-01-01")), ])
    }
  })
  # Características Demográficas -----------------------------------------------
  # Características Sociales y Económicas --------------------------------------
  # Caracteristicas de consumo -------------------------------------------------
  # Edad de inicio consumo
  output$hist_edad_inicio <- renderPlotly({
    data <- datos_filtrados()
    
    df <- data %>%
      filter(!is.na(`Edad de Inicio de Cosumo`)) %>%
      group_by(`Edad de Inicio de Cosumo`) %>%
      summarize(conteo = n(), .groups = 'drop') %>%
      ungroup()
    
    # Medidas descriptivas
    media_edad <- mean(data$`Edad de Inicio de Cosumo`, na.rm = TRUE)
    desviacion_edad <- sd(data$`Edad de Inicio de Cosumo`, na.rm = TRUE)
    n <- sum(!is.na(data$`Edad de Inicio de Cosumo`))
    
    
    g <- ggplot(df, aes(x = `Edad de Inicio de Cosumo`, y = conteo)) +
      geom_bar(stat = "identity", fill = "#ff8800") +
      labs(x = "Edad de Inicio", y = "Conteo", title = "Conteo por Edad de Inicio") +
      scale_x_continuous(breaks = seq(min(df$`Edad de Inicio de Cosumo`), max(df$`Edad de Inicio de Cosumo`), by = 1)) +
      theme_grey() +
      theme(legend.position = 'none')+
      annotate("text", 
               x = max(df$`Edad de Inicio de Cosumo`) * 0.95, 
               y = max(df$conteo) * 0.95,                   
               label = paste("Media:", round(media_edad, 1), 
                             "\nDesvío:", round(desviacion_edad, 1), 
                             "\nn:", n),
               color = "black", size = 4, hjust = 0)
    ggplotly(g, tooltip = 'text')
    
  })
  # Edad inicio y Edad de registro
  output$tabla_inicio_reg <- renderText({
    data <- datos_filtrados()
    
    data <- data %>%
      mutate(edad_inicio_cat = cut(
        `Edad de Inicio de Cosumo`,
        breaks = c(-Inf, 12, 17, 29, 60, Inf),  
        labels = c("0 a 12", "13 a 17", "18 a 29", "30 a 60", "Mayor de 60"),
        right = FALSE  
      )) %>%
      mutate(edad_registro_cat = cut(
        `Edad del registro`,
        breaks = c(-Inf, 12, 17, 29, 60, Inf),  
        labels = c("0 a 12", "13 a 17", "18 a 29", "30 a 60", "Mayor de 60"),
        right = FALSE  
      ))
    tabla_cruzada <- data %>%
      group_by(`ID de la persona`) %>%
      filter(row_number() == n()) %>%  
      ungroup() %>%
      filter(!is.na(edad_inicio_cat) & !is.na(edad_registro_cat)) %>%
      count(edad_inicio_cat, edad_registro_cat, .drop = FALSE) %>% 
      pivot_wider(
        names_from = edad_registro_cat, 
        values_from = n, 
        values_fill = 0  
      ) %>%
      ungroup() %>%
      rename("Edad de Inicio\\Edad de Registro" = edad_inicio_cat) 
    
    tabla_cruzada <- tabla_cruzada %>%
      mutate(Total_Fila = rowSums(select(., -`Edad de Inicio\\Edad de Registro`)))
    
    
    totales_columna <- tabla_cruzada %>%
      summarise(across(starts_with("0 a 12"):ends_with("Mayor de 60"), sum, na.rm = TRUE))
    
    tabla_cruzada <- bind_rows(tabla_cruzada, c(`Edad de Inicio\\Edad de Registro` = "Total_Columna", totales_columna))
    
    suma_total <- sum(tabla_cruzada$Total_Fila, na.rm = TRUE)
    
    tabla_cruzada <- tabla_cruzada %>%
      add_row(`Edad de Inicio\\Edad de Registro` = "Total", !!!totales_columna, Total_Fila = suma_total)
    
    tabla_cruzada <- tabla_cruzada %>%
      filter(`Edad de Inicio\\Edad de Registro` != "Total_Columna") %>%
      rename("Total" = Total_Fila) 
    
    kable(tabla_cruzada, format = "html", table.attr = "style='width:100%;'") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
      add_header_above(c("Edad de inicio vs Edad de Registro" = ncol(tabla_cruzada))) %>%  
      column_spec(1, bold = TRUE, width = "150px", background = "#ffb600", color = "white") %>%  
      row_spec(0, background = "#ffb600", color = "white") %>%
      row_spec(nrow(tabla_cruzada), bold = TRUE)%>%  
      column_spec(7, bold = TRUE)
  })
  # Tabla sustancia de inicio
  output$tabla_s_inicio <- renderText({
    data <- datos_filtrados()
    
    # Procesa la tabla si hay datos
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
    
    # Genera la tabla con kable
    kable(tabla_s_inicio, format = "html", table.attr = "style='width:100%;'") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
      row_spec(0, background = "#ffb600", color = "white") %>%
      add_header_above(c("Sustancia de inicio" = 5)) %>%
      column_spec(1, width = "25em", extra_css = "white-space: nowrap; overflow: hidden; text-overflow: ellipsis;") %>%
      as.character()  # Convertir la tabla a cadena de texto HTML
  })
  # Tabla sustancia actual
  output$tabla_s_actual <- renderText({
    data <- datos_filtrados()
    
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
    data <- datos_filtrados()
    
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
    data <- datos_filtrados()
    
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
    data <- datos_filtrados()
    
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
  
  # Derivacion y tratamiento asignado ------------------------------------------
  # Cantidad de tratamientos Previos
  output$trat_previos <- renderPlotly({
    data <- datos_filtrados()
    
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
      geom_bar(stat = "identity", fill = "#ff8800", 
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
    data <- datos_filtrados()
    
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
      geom_bar(stat = "identity", fill = "#ff8800", 
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
    data <- datos_filtrados()
    
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
      scale_fill_manual(values = c("#ff4800","#ff7b00", "#ff8800", "#ff9500", "#ffa200",
                                   "#ffaa00", "#ffb700", "#ffc300","#ffd000","#ffdd00",
                                   "#ffea00")) +
      labs(x = "Porcentaje", y = "Sustancia de Consumo Actual", 
           title = "Distribución de Tratamientos por Sustancia de Consumo Actual") +
      scale_x_continuous(breaks = seq(0, 100, by = 10)) +
      
      theme_grey() +
      theme(legend.position = "right")
    
    ggplotly(g, tooltip = 'text')
  })
}

shinyApp(ui, server)
