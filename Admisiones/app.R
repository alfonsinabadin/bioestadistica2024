# Cargar las bibliotecas necesarias
library(shiny)
library(shinyWidgets)
library(readxl)
library(writexl)
library(DT)
library(shinyvalidate)
library(shinydashboard)
library(geoAr)
library(dplyr)
library(ggplot2)
library(DT)
library(shinymanager)

# Definir la ruta del archivo Excel donde se guardarán los datos
file_path <- "ADMISIÓN.xlsx"

credentials <- data.frame(
  user = c("abadin", "vsalvarezza", "csebastiani","usuario"), # mandatory
  password = c("bio2024"), # mandatory
  admin = c(TRUE, TRUE, FALSE, TRUE),
  stringsAsFactors = FALSE
)

# Función para leer los datos del archivo Excel
read_data <- function() {
  if (file.exists(file_path)) {
    data <- read_excel(file_path)
    # Verificar las columnas esperadas
    expected_columns <- c("Apellido_nombres", "DNI", "Entrevista_psicológica_fecha", "Entrevista_psicológica_asistencia",
                          "Entrevista_psiquiátrica_fecha", "Entrevista_psiquiátrica_asistencia", "Entrevista_ts_fecha",
                          "Entrevista_ts_asistencia", "Tratamiento", "Contacto", "Fecha_de_nacimiento", "Edad",
                          "Sexo", "Nivel_educativo", "Situacion_habitacional", "Provincia", "Localidad", "Barrio",
                          "Redes_de_apoyo", "Tiene_CUD", "Trabajo", "Ingresos_económicos", "Situación_Judicial",
                          "Referencia_APS", "Derivado_de", "Policonsumo", "Sustancia_actual", "Edad_de_inicio",
                          "Sustancia_de_inicio", "Tratamientos_previos", "Observaciones")
    if (!all(expected_columns %in% colnames(data))) {
      stop("El archivo Excel no contiene las columnas esperadas.")
    }
    # Asegurar que las fechas se leen correctamente
    data$Fecha_de_nacimiento <- as.Date(data$Fecha_de_nacimiento, format = "%d/%m/%Y")
    data$Entrevista_psicológica_fecha <- as.Date(data$Entrevista_psicológica_fecha, format = "%d/%m/%Y")
    data$Entrevista_psiquiátrica_fecha <- as.Date(data$Entrevista_psiquiátrica_fecha, format = "%d/%m/%Y")
    data$Entrevista_ts_fecha <- as.Date(data$Entrevista_ts_fecha, format = "%d/%m/%Y")
    return(data)
  } else {
    data.frame(
      Apellido_nombres = character(),
      DNI = numeric(),
      Entrevista_psicológica_fecha = as.Date(character()),
      Entrevista_psicológica_asistencia = character(),
      Entrevista_psiquiátrica_fecha = as.Date(character()),
      Entrevista_psiquiátrica_asistencia = character(),
      Entrevista_ts_fecha = as.Date(character()),
      Entrevista_ts_asistencia = character(),
      Tratamiento = character(),
      Contacto = numeric(),
      Fecha_de_nacimiento = as.Date(character()),
      Edad = numeric(),
      Sexo = character(),
      Nivel_educativo = character(),
      Situacion_habitacional = character(),
      Provincia = character(),
      Localidad = character(),
      Barrio = character(),
      Redes_de_apoyo = character(),
      Tiene_CUD = character(),
      Trabajo = character(),
      Ingresos_económicos = character(),
      Situación_Judicial = character(),
      Referencia_APS = character(),
      Derivado_de = character(),
      Policonsumo = character(),
      Sustancia_actual = character(),
      Edad_de_inicio = character(),
      Sustancia_de_inicio = character(),
      Tratamientos_previos = character(),
      Observaciones = character(),
      stringsAsFactors = FALSE
    )
  }
}

provincias <- show_arg_codes()[2:25, 5]
provincias[[1]][1] <- "CABA"

localidades_por_provincia <- readRDS("localidades.rds")

# Función para escribir los datos en el archivo Excel
write_data <- function(data) {
  temp_file <- tempfile(fileext = ".xlsx")
  write_xlsx(data, temp_file)
  file.copy(temp_file, file_path, overwrite = TRUE)
}

# Definir la interfaz de usuario (UI) de la aplicación Shiny
ui <- secure_app(
  theme = shinythemes::shinytheme("superhero"),
  language = "es",
  ### EDIT: Add an image ### 
  tag_img = tags$img(
    src = "logo.png", width = 100
  ),
  fluidPage(
  dashboardPage(skin = "black",
                dashboardHeader(title = tags$div(
                  tags$img(src = "logo.png", height = "51px", style = "margin-right: 10px;"),
                  "Admisiones - Comunidad Padre Misericordioso"
                ), titleWidth = 550),
                dashboardSidebar(disable = TRUE),
                dashboardBody(
                  tabsetPanel(
                    # Pestaña "Nuevo registro" ------------------------------------------------------------------------------------------------------------------
                    tabPanel("Nuevo registro",
                             fluidRow(
                               box(
                                 title = "Identificación",
                                 status = "warning",
                                 solidHeader = TRUE,
                                 width = 3,
                                 fluidRow(
                                   column(12, textInput("apellido_nombres", 
                                                        "Apellido, Nombres", 
                                                        value = NULL,
                                                        placeholder = "Ejemplo: Perez, Juan")),
                                   column(12, numericInput("dni", 
                                                           "DNI", 
                                                           value = NULL)),
                                   column(12, dateInput("fecha_nacimiento", 
                                                        "Fecha de nacimiento",
                                                        value = "",
                                                        format = "dd/mm/yyyy")),
                                   column(12, numericInput("contacto", 
                                                           "Teléfono de contacto", 
                                                           value = NULL))
                                 )
                               ),
                               box(
                                 title = "Tratamiento actual",
                                 status = "warning",
                                 solidHeader = TRUE,
                                 width = 9,
                                 box(
                                   title = "Entrevista psicológica",
                                   status = "warning",
                                   solidHeader = FALSE,
                                   width = 4,
                                   fluidRow(
                                     column(12, dateInput("psico_fecha", 
                                                          "Fecha", 
                                                          value = "", 
                                                          format = "dd/mm/yyyy"))), 
                                   fluidRow(
                                     column(12, radioGroupButtons("psico", 
                                                                  "Estado",
                                                                  choices = c("No asignada", 
                                                                              "Presente", 
                                                                              "Ausente", 
                                                                              "Pendiente"), 
                                                                  selected = "No asignada",
                                                                  size = "xs",
                                                                  justified = TRUE,
                                                                  width = "100%"))
                                   )
                                 ),
                                 box(
                                   title = "Entrevista psiquiátrica",
                                   status = "warning",
                                   solidHeader = FALSE,
                                   width = 4,
                                   fluidRow(
                                     column(12, dateInput("psiqui_fecha", 
                                                          "Fecha", 
                                                          value = "", 
                                                          format = "dd/mm/yyyy"))
                                   ),
                                   fluidRow(
                                     column(12, radioGroupButtons("psiqui", 
                                                                  "Estado",
                                                                  choices = c("No asignada", 
                                                                              "Presente", 
                                                                              "Ausente", 
                                                                              "Pendiente"), 
                                                                  selected = "No asignada",
                                                                  size = "xs",
                                                                  justified = TRUE,
                                                                  width = "100%"))
                                   )
                                 ),
                                 box(
                                   title = "Entrevista T.S.",
                                   status = "warning",
                                   solidHeader = FALSE,
                                   width = 4,
                                   fluidRow(
                                     column(12, dateInput("ts_fecha", 
                                                          "Fecha", 
                                                          value = "", 
                                                          format = "dd/mm/yyyy"))
                                   ),
                                   fluidRow(
                                     column(12, radioGroupButtons("ts", 
                                                                  "Estado",
                                                                  choices = c("No asignada", 
                                                                              "Presente", 
                                                                              "Ausente", 
                                                                              "Pendiente"), 
                                                                  selected = "No asignada",
                                                                  size = "xs",
                                                                  justified = TRUE,
                                                                  width = "100%"))
                                   )
                                 ),
                                 fluidRow(
                                   column(1, ""),
                                   column(10, selectInput("tratamiento", 
                                                          "Tratamiento asignado", 
                                                          choices = c("Continúa en seguimiento", 
                                                                      "Internación Cristalería", 
                                                                      "Internación Baigorria",
                                                                      "Internación Buen Pastor", 
                                                                      "Centro de día Zeballos",
                                                                      "Centro de día Buen Pastor",
                                                                      "Centro de día Baigorria",
                                                                      "Derivado",
                                                                      "No finalizó el proceso",
                                                                      "Rechaza tratamiento"))),
                                   column(1, ""))
                               ),
                               # Espacio para q no se rompa todo
                               fluidRow(
                                 column(12, "")
                               ),
                               # Cajita información especial
                               box(
                                 title = "Información personal",
                                 status = "warning",
                                 solidHeader = TRUE,
                                 width = 6,
                                 fluidRow(
                                   column(4, selectInput("identidad_gen", 
                                                         "Identidad de género", 
                                                         choices = c("Mujer", 
                                                                     "Mujer Trans", 
                                                                     "Varón", 
                                                                     "Varón Trans", 
                                                                     "No binario", 
                                                                     "Otro",
                                                                     "No responde"),
                                                         selected = "No responde")),
                                   column(4, selectInput("nivel_educativo", 
                                                         "Nivel educativo", 
                                                         choices = c("Sin instrucción formal", 
                                                                     "Inicial", 
                                                                     "Primario en curso", 
                                                                     "Primario incompleto (incluye educación especial)",
                                                                     "Primario completo", 
                                                                     "Secundario en curso", 
                                                                     "Secundario incompleto", 
                                                                     "Secundario completo", 
                                                                     "Superior terciario o universitario en curso",
                                                                     "Superior terciario o universitario incompleto", 
                                                                     "Superior terciario o universitario completo",
                                                                     "No sabe / No responde"))),
                                   column(4, selectInput("tiene_cud", 
                                                         "Tiene CUD", 
                                                         choices = c("Sí", "No"),
                                                         selected = "No"))
                                 ),
                                 fluidRow(
                                   column(3, selectizeInput("situacion_habitacional", 
                                                            "Situación Habitacional", 
                                                            choices = c("Casa/depto propio", 
                                                                        "Casa/depto alquilado", 
                                                                        "Casa/depto cedido", 
                                                                        "Pensión",
                                                                        "Internado / Institucionalizado", 
                                                                        "Refugio", 
                                                                        "Situación de calle"),
                                                            options = list(create = TRUE, 
                                                                           placeholder = 'Escriba la opción'))),
                                   column(3, selectInput("provincia", 
                                                         "Provincia", 
                                                         choices = provincias,
                                                         selected = "Santa Fe")),
                                   column(3, uiOutput("localidad_ui")),
                                   column(3, textInput("barrio", 
                                                       "Barrio"))
                                 ),
                                 fluidRow(
                                   column(3, selectInput("redes_de_apoyo", 
                                                         "Redes de Apoyo", 
                                                         choices = c("Ninguna", 
                                                                     "Escasa", 
                                                                     "Buena", 
                                                                     "Familia/Amigos", 
                                                                     "Institución", 
                                                                     "Familia/Amigos e Institución"))),
                                   column(3, selectInput("trabajo", 
                                                         "Trabajo", 
                                                         choices = c("No tiene", 
                                                                     "Esporádico", 
                                                                     "Estable"))),
                                   column(3, selectInput("ingresos_economicos", 
                                                         "Ingresos Económicos", 
                                                         choices = c("Sin ingresos", 
                                                                     "Pensión no Contributiva (PNC)", 
                                                                     "Subsidio", 
                                                                     "Salario informal", 
                                                                     "Salario informal + subsidio", 
                                                                     "Salario formal"))),
                                   column(3, selectInput("situacion_judicial", 
                                                         "Situación Judicial", 
                                                         choices = c("Sin causas Judiciales", 
                                                                     "Con causa abierta", 
                                                                     "Con causa cerrada", 
                                                                     "Desconoce")))
                                 ),
                                 fluidRow(column(12,textInput("obs", "Observaciones")))
                               ),
                               box(
                                 title = "Consumo",
                                 status = "warning",
                                 solidHeader = TRUE,
                                 width = 6,
                                 fluidRow(
                                   column(6, 
                                          textInput("consumo",
                                                    "Consumo actual",
                                                    placeholder = "Droga más prevalente"), 
                                          selectInput("policonsumo",
                                                      "Policonsumo",
                                                      choices = c("No", "Sí")),
                                          selectInput("referencia_aps", 
                                                      "Referencia APS", 
                                                      choices = c("No está referenciado",
                                                                  "Sólo clínica médica", 
                                                                  "Referencia con seguimiento", 
                                                                  "Referencia sin seguimiento"
                                                      ))
                                   ),
                                   column(6, 
                                          textInput("sustancia", 
                                                    "Sustancia de inicio", 
                                                    placeholder = "Completar con una sola droga"),
                                          radioGroupButtons("edad_inicio",
                                                            "Edad de inicio",
                                                            choices = c("Niños/as de hasta 12 años",
                                                                        "Adolescentes entre 13 a 17 años",
                                                                        "Jóvenes de 18 a 29 años",
                                                                        "Personas adultas de 30 a 59 años",
                                                                        "Personas de 60 años o más"),
                                                            individual = TRUE,
                                                            size = "s"))
                                 ),
                                 fluidRow(column(6, 
                                                 selectInput("trat_prev", 
                                                             "Tratamientos previos", 
                                                             choices = c("No", 
                                                                         "Entre 1 y 2", 
                                                                         "3 o más"))),
                                          column(6,
                                                 textInput("derivado_de", 
                                                           "Derivado de:")))
                               ),
                               box(
                                 status = "warning",
                                 solidHeader = TRUE,
                                 width = 12,
                                 fluidRow(
                                   column(12, 
                                          actionButton("guardar", "Guardar Registro"),
                                          downloadButton("download_data", "Descargar Base de Datos"))
                                 )
                               )
                             )
                    ),
                    # Pestaña "Modificación de registros" ----------------------------------------------------------------------------------------------------------
                    tabPanel("Modificación de registros",
                             fluidRow(
                               box(
                                 title = "Buscar y Modificar Registro",
                                 status = "warning",
                                 solidHeader = TRUE,
                                 width = 12,
                                 fluidRow(
                                   column(4, numericInput("buscar_dni", "Buscar por DNI", 
                                                          value = NULL)),
                                   column(2, actionButton("buscar", "Buscar"))
                                 ),
                                 uiOutput("resultados_busqueda")
                               ),
                               box(
                                 title = "Identificación",
                                 status = "warning",
                                 solidHeader = TRUE,
                                 width = 3,
                                 fluidRow(
                                   column(12, textInput("apellido_nombres_modificar", 
                                                        "Apellido, Nombres", 
                                                        value = NULL,
                                                        placeholder = "Ejemplo: Perez, Juan")),
                                   column(12, numericInput("dni_modificar", 
                                                           "DNI", 
                                                           value = NULL)),
                                   column(12, dateInput("fecha_nacimiento_modificar", 
                                                        "Fecha de nacimiento",
                                                        value = "",
                                                        format = "dd/mm/yyyy")),
                                   column(12, numericInput("contacto_modificar", 
                                                           "Teléfono de contacto", 
                                                           value = NULL))
                                 )
                               ),
                               box(
                                 title = "Tratamiento actual",
                                 status = "warning",
                                 solidHeader = TRUE,
                                 width = 9,
                                 box(
                                   title = "Entrevista psicológica",
                                   status = "warning",
                                   solidHeader = FALSE,
                                   width = 4,
                                   fluidRow(
                                     column(12, dateInput("psico_fecha_modificar", 
                                                          "Fecha", 
                                                          value = "", 
                                                          format = "dd/mm/yyyy"))), 
                                   fluidRow(
                                     column(12, radioGroupButtons("psico_modificar", 
                                                                  "Estado",
                                                                  choices = c("No asignada", 
                                                                              "Presente", 
                                                                              "Ausente", 
                                                                              "Pendiente"), 
                                                                  selected = "No asignada",
                                                                  size = "xs",
                                                                  justified = TRUE,
                                                                  width = "100%"))
                                   )
                                 ),
                                 box(
                                   title = "Entrevista psiquiátrica",
                                   status = "warning",
                                   solidHeader = FALSE,
                                   width = 4,
                                   fluidRow(
                                     column(12, dateInput("psiqui_fecha_modificar", 
                                                          "Fecha", 
                                                          value = "", 
                                                          format = "dd/mm/yyyy"))
                                   ),
                                   fluidRow(
                                     column(12, radioGroupButtons("psiqui_modificar", 
                                                                  "Estado",
                                                                  choices = c("No asignada", 
                                                                              "Presente", 
                                                                              "Ausente", 
                                                                              "Pendiente"), 
                                                                  selected = "No asignada",
                                                                  size = "xs",
                                                                  justified = TRUE,
                                                                  width = "100%"))
                                   )
                                 ),
                                 box(
                                   title = "Entrevista T.S.",
                                   status = "warning",
                                   solidHeader = FALSE,
                                   width = 4,
                                   fluidRow(
                                     column(12, dateInput("ts_fecha_modificar", 
                                                          "Fecha", 
                                                          value = "", 
                                                          format = "dd/mm/yyyy"))
                                   ),
                                   fluidRow(
                                     column(12, radioGroupButtons("ts_modificar", 
                                                                  "Estado",
                                                                  choices = c("No asignada", 
                                                                              "Presente", 
                                                                              "Ausente", 
                                                                              "Pendiente"), 
                                                                  selected = "No asignada",
                                                                  size = "xs",
                                                                  justified = TRUE,
                                                                  width = "100%"))
                                   )
                                 ),
                                 fluidRow(
                                   column(1, ""),
                                   column(10, selectInput("tratamiento_modificar", 
                                                          "Tratamiento asignado", 
                                                          choices = c("Continúa en seguimiento", 
                                                                      "Internación Cristalería", 
                                                                      "Internación Baigorria",
                                                                      "Internación Buen Pastor", 
                                                                      "Centro de día Zeballos",
                                                                      "Centro de día Buen Pastor",
                                                                      "Centro de día Baigorria",
                                                                      "Derivado",
                                                                      "No finalizó el proceso",
                                                                      "Rechaza tratamiento"))),
                                   column(1, ""))
                               ),
                               # Espacio para q no se rompa todo
                               fluidRow(
                                 column(12, "")
                               ),
                               # Cajita información especial
                               box(
                                 title = "Información personal",
                                 status = "warning",
                                 solidHeader = TRUE,
                                 width = 6,
                                 fluidRow(
                                   column(4, selectInput("identidad_gen_modificar", 
                                                         "Identidad de género", 
                                                         choices = c("Mujer", 
                                                                     "Mujer Trans", 
                                                                     "Varón", 
                                                                     "Varón Trans", 
                                                                     "No binario", 
                                                                     "Otro",
                                                                     "No responde"),
                                                         selected = "No responde")),
                                   column(4, selectInput("nivel_educativo_modificar", 
                                                         "Nivel educativo", 
                                                         choices = c("Sin instrucción formal", 
                                                                     "Inicial", 
                                                                     "Primario en curso", 
                                                                     "Primario incompleto (incluye educación especial)",
                                                                     "Primario completo", 
                                                                     "Secundario en curso", 
                                                                     "Secundario incompleto", 
                                                                     "Secundario completo", 
                                                                     "Superior terciario o universitario en curso",
                                                                     "Superior terciario o universitario incompleto", 
                                                                     "Superior terciario o universitario completo",
                                                                     "No sabe / No responde"))),
                                   column(4, selectInput("tiene_cud_modificar", 
                                                         "Tiene CUD", 
                                                         choices = c("Sí", "No"),
                                                         selected = "No"))
                                 ),
                                 fluidRow(
                                   column(3, selectizeInput("situacion_habitacional_modificar", 
                                                            "Situación Habitacional", 
                                                            choices = c("Casa/depto propio", 
                                                                        "Casa/depto alquilado", 
                                                                        "Casa/depto cedido", 
                                                                        "Pensión",
                                                                        "Internado / Institucionalizado", 
                                                                        "Refugio", 
                                                                        "Situación de calle"),
                                                            options = list(create = TRUE, 
                                                                           placeholder = 'Escriba la opción'))),
                                   column(3, selectInput("provincia_modificar", 
                                                         "Provincia", 
                                                         choices = provincias,
                                                         selected = "Santa Fe")),
                                   column(3, uiOutput("localidad_ui_modificar")),
                                   column(3, textInput("barrio_modificar", 
                                                       "Barrio"))
                                 ),
                                 fluidRow(
                                   column(3, selectInput("redes_de_apoyo_modificar", 
                                                         "Redes de Apoyo", 
                                                         choices = c("Ninguna", 
                                                                     "Escasa", 
                                                                     "Buena", 
                                                                     "Familia/Amigos", 
                                                                     "Institución", 
                                                                     "Familia/Amigos e Institución"))),
                                   column(3, selectInput("trabajo_modificar", 
                                                         "Trabajo", 
                                                         choices = c("No tiene", 
                                                                     "Esporádico", 
                                                                     "Estable"))),
                                   column(3, selectInput("ingresos_economicos_modificar", 
                                                         "Ingresos Económicos", 
                                                         choices = c("Sin ingresos", 
                                                                     "Pensión no Contributiva (PNC)", 
                                                                     "Subsidio", 
                                                                     "Salario informal", 
                                                                     "Salario informal + subsidio", 
                                                                     "Salario formal"))),
                                   column(3, selectInput("situacion_judicial_modificar", 
                                                         "Situación Judicial", 
                                                         choices = c("Sin causas Judiciales", 
                                                                     "Con causa abierta", 
                                                                     "Con causa cerrada", 
                                                                     "Desconoce")))
                                 ),
                                 fluidRow(column(12,textInput("obs_modificar", "Observaciones")))
                               ),
                               box(
                                 title = "Consumo",
                                 status = "warning",
                                 solidHeader = TRUE,
                                 width = 6,
                                 fluidRow(
                                   column(6, 
                                          textInput("consumo",
                                                    "Consumo actual_modificar",
                                                    placeholder = "Droga más prevalente"), 
                                          selectInput("policonsumo_modificar",
                                                      "Policonsumo",
                                                      choices = c("No", "Sí")),
                                          selectInput("referencia_aps_modificar", 
                                                      "Referencia APS", 
                                                      choices = c("No está referenciado",
                                                                  "Sólo clínica médica", 
                                                                  "Referencia con seguimiento", 
                                                                  "Referencia sin seguimiento"
                                                      ))
                                   ),
                                   column(6, 
                                          textInput("sustancia_modificar", 
                                                    "Sustancia de inicio", 
                                                    placeholder = "Completar con una sola droga"),
                                          radioGroupButtons("edad_inicio_modificar",
                                                            "Edad de inicio",
                                                            choices = c("Niños/as de hasta 12 años",
                                                                        "Adolescentes entre 13 a 17 años",
                                                                        "Jóvenes de 18 a 29 años",
                                                                        "Personas adultas de 30 a 59 años",
                                                                        "Personas de 60 años o más"),
                                                            individual = TRUE,
                                                            size = "s"))
                                 ),
                                 fluidRow(column(6, 
                                                 selectInput("trat_prev_modificar", 
                                                             "Tratamientos previos", 
                                                             choices = c("No", 
                                                                         "Entre 1 y 2", 
                                                                         "3 o más"))),
                                          column(6,
                                                 textInput("derivado_de_modificar", 
                                                           "Derivado de:")))
                               ),
                               box(
                                 status = "warning",
                                 solidHeader = TRUE,
                                 width = 12,
                                 fluidRow(
                                   column(12, 
                                          actionButton("actualizar", "Actualizar Registro"),
                                          actionButton("eliminar", "Eliminar Registro"))
                                   )
                               )
                             )
                    ),
                    # Pestaña "Visualización" ----------------------------------------------------------------------------------------------------------
                    tabPanel("Visualización de datos",
                             fluidRow(
                               valueBoxOutput("int_cristaleria", width = 2),
                               valueBoxOutput("int_baigorria", width = 2),
                               valueBoxOutput("int_bp", width = 2),
                               valueBoxOutput("cdd_zeballos", width = 2),
                               valueBoxOutput("cdd_bp", width = 2),
                               valueBoxOutput("cdd_baigorria", width = 2)
                               ),
                             fluidRow(
                               column(12, plotOutput("barras_por_fecha")),
                               column(6, plotOutput("barras_porcentuales_sustancia")),
                               column(6, plotOutput("barras_redes_apoyo")),
                               column(6, plotOutput("barras_policonsumo")),
                               column(6, plotOutput("barras_edad_inicio_trat")),
                               dataTableOutput("tabla_resumen")
                             )
                    )
                  )
                )
  )
)
)

# Definir la lógica del servidor de la aplicación Shiny
server <- function(input, output, session) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # Validación del campo DNI (tiene que estar sí o sí)
  iv <- InputValidator$new()
  iv$add_rule("dni", sv_required(""))
  iv$enable()
  
  # Crear el uiOutput para las localidades
  output$localidad_ui <- renderUI({
    selectInput("localidad", "Localidad", choices = NULL)
  })
  
  output$localidad_ui_modificar <- renderUI({
    selectInput("localidad_modificar", "Localidad", choices = NULL)
  })
  
  # Actualizar las localidades en función de la provincia seleccionada
  observeEvent(input$provincia, {
    localidades <- localidades_por_provincia[[input$provincia]]
    updateSelectInput(session, "localidad", choices = c(localidades,NULL), selected = NULL)
  })
  
  observeEvent(input$provincia_modificar, {
    observeEvent(input$buscar, {
      data <- read_data()
      registro <- data[data$DNI == input$buscar_dni, ]
      localidad_a_cambiar <- last(registro$Localidad)
      localidades <- localidades_por_provincia[[input$provincia_modificar]]
      updateSelectInput(session, "localidad_modificar", choices = localidades, selected = localidad_a_cambiar)
    })
  })
  
  # Acción al presionar el botón "Buscar"
  observeEvent(input$buscar, {
    data <- read_data()
    registro <- data[which(data$DNI == input$buscar_dni), ]
    
    if (nrow(registro) > 0) {
      updateTextInput(session, "apellido_nombres_modificar", value = registro$Apellido_nombres[nrow(registro)])
      updateNumericInput(session, "dni_modificar", value = as.numeric(registro$DNI[nrow(registro)]))
      updateDateInput(session, "fecha_nacimiento_modificar", value = as.Date(registro$Fecha_de_nacimiento[nrow(registro)]))
      updateNumericInput(session, "contacto_modificar", value = as.numeric(registro$Contacto[nrow(registro)]))
      updateDateInput(session, "psico_fecha_modificar", value = as.Date(registro$Entrevista_psicológica_fecha[nrow(registro)]))
      updateSelectInput(session, "psico_modificar", selected = registro$Entrevista_psicológica_asistencia[nrow(registro)])
      updateDateInput(session, "psiqui_fecha_modificar", value = as.Date(registro$Entrevista_psiquiátrica_fecha[nrow(registro)]))
      updateSelectInput(session, "psiqui_modificar", selected = registro$Entrevista_psiquiátrica_asistencia[nrow(registro)])
      updateDateInput(session, "ts_fecha_modificar", value = as.Date(registro$Entrevista_ts_fecha[nrow(registro)]))
      updateSelectInput(session, "ts_modificar", selected = registro$Entrevista_ts_asistencia[nrow(registro)])
      updateSelectInput(session, "tratamiento_modificar", selected = registro$Tratamiento[nrow(registro)])
      updateSelectInput(session, "identidad_gen_modificar", selected = registro$Sexo[nrow(registro)])
      updateSelectInput(session, "nivel_educativo_modificar", selected = registro$Nivel_educativo[nrow(registro)])
      updateSelectInput(session, "tiene_cud_modificar", selected = registro$Tiene_CUD[nrow(registro)])
      updateSelectInput(session, "situacion_habitacional_modificar", selected = registro$Situacion_habitacional[nrow(registro)])
      updateSelectInput(session, "provincia_modificar", selected = registro$Provincia[nrow(registro)])
      updateSelectInput(session, "localidad_modificar", selected = registro$Localidad[nrow(registro)])
      updateTextInput(session, "barrio_modificar", value = registro$Barrio[nrow(registro)])
      updateSelectInput(session, "redes_de_apoyo_modificar", selected = registro$Redes_de_apoyo[nrow(registro)])
      updateSelectInput(session, "trabajo_modificar", selected = registro$Trabajo[nrow(registro)])
      updateSelectInput(session, "ingresos_economicos_modificar", selected = registro$Ingresos_económicos[nrow(registro)])
      updateSelectInput(session, "situacion_judicial_modificar", selected = registro$Situación_Judicial[nrow(registro)])
      updateSelectInput(session, "referencia_aps_modificar", selected = registro$Referencia_APS[nrow(registro)])
      updateTextInput(session, "derivado_de_modificar", value = registro$Derivado_de[nrow(registro)])
      updateSelectInput(session, "policonsumo_modificar", selected = registro$Policonsumo[nrow(registro)])
      updateTextInput(session, "sustancia_modificar", value = registro$Sustancia_actual[nrow(registro)])
      updateNumericInput(session, "edad_inicio_modificar", value = as.numeric(registro$Edad_de_inicio[nrow(registro)]))
      updateTextInput(session, "sustancia_modificar", value = registro$Sustancia_de_inicio[nrow(registro)])
      updateSelectInput(session, "trat_prev_modificar", selected = registro$Tratamientos_previos[nrow(registro)])
      updateTextInput(session, "obs_modificar", value = registro$Observaciones[nrow(registro)])
    } else {
      showModal(modalDialog(
        title = "Registro no encontrado",
        "No se encontró ningún registro con el DNI proporcionado.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Acción al presionar el botón "Actualizar Registro"
  observeEvent(input$actualizar, {
    # Leer los datos actuales
    data <- read_data()
    data$Edad <- round(as.numeric(difftime(Sys.Date(), data$Fecha_de_nacimiento, units = "days")) / 365, 0)
    # Encontrar el índice de la fila a actualizar
    idx <- which(data$DNI == input$buscar_dni)
    
    # Crear un nuevo registro con los datos de entrada
    act_entry <- data.frame(
      Apellido_nombres = ifelse(is.null(input$apellido_nombres_modificar), NA, input$apellido_nombres_modificar),
      DNI = ifelse(is.na(input$dni_modificar), NA, input$dni_modificar),
      Entrevista_psicológica_fecha = ifelse(is.null(input$psico_fecha_modificar),NA,input$psico_fecha_modificar),
      Entrevista_psicológica_asistencia = ifelse(is.null(input$psico_modificar), NA, input$psico_modificar),
      Entrevista_psiquiátrica_fecha = ifelse(is.null(input$psiqui_fecha_modificar), NA, input$psiqui_fecha_modificar),
      Entrevista_psiquiátrica_asistencia = ifelse(is.null(input$psiqui_modificar), NA, input$psiqui_modificar),
      Entrevista_ts_fecha = ifelse(is.null(input$ts_fecha_modificar), NA, input$ts_fecha_modificar),
      Entrevista_ts_asistencia = ifelse(is.null(input$ts_modificar), NA, input$ts_modificar),
      Tratamiento = ifelse(is.null(input$tratamiento_modificar), NA, input$tratamiento_modificar),
      Contacto = ifelse(is.null(input$contacto_modificar), NA, input$contacto_modificar),
      Fecha_de_nacimiento = ifelse(is.null(input$fecha_nacimiento_modificar), NA, input$fecha_nacimiento_modificar),
      Edad = ifelse(is.na(input$fecha_nacimiento_modificar) || input$fecha_nacimiento_modificar == "", NA, as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(as.Date(input$fecha_nacimiento_modificar, format = "%d/%m/%Y"), "%Y"))),
      Sexo = ifelse(is.null(input$identidad_gen_modificar), NA, input$identidad_gen_modificar),
      Nivel_educativo = ifelse(is.null(input$nivel_educativo_modificar), NA, input$nivel_educativo_modificar),
      Situacion_habitacional = ifelse(is.null(input$situacion_habitacional_modificar), NA, input$situacion_habitacional_modificar),
      Provincia = ifelse(is.null(input$provincia_modificar), NA, input$provincia_modificar),
      Localidad = ifelse(is.null(input$localidad_modificar), NA, input$localidad_modificar),
      Barrio = ifelse(is.null(input$barrio_modificar), NA, input$barrio_modificar),
      Redes_de_apoyo = ifelse(is.null(input$redes_de_apoyo_modificar), NA, input$redes_de_apoyo_modificar),
      Tiene_CUD = ifelse(is.null(input$tiene_cud_modificar), NA, input$tiene_cud_modificar),
      Trabajo = ifelse(is.null(input$trabajo_modificar), NA, input$trabajo_modificar),
      Ingresos_económicos = ifelse(is.null(input$ingresos_economicos_modificar), NA, input$ingresos_economicos_modificar),
      Situación_Judicial = ifelse(is.null(input$situacion_judicial_modificar), NA, input$situacion_judicial_modificar),
      Referencia_APS = ifelse(is.null(input$referencia_aps_modificar), NA, input$referencia_aps_modificar),
      Derivado_de = ifelse(is.null(input$derivado_de_modificar), NA, input$derivado_de_modificar),
      Policonsumo = ifelse(is.null(input$policonsumo_modificar), NA, input$policonsumo_modificar),
      Sustancia_actual = ifelse(is.null(input$consumo_modificar), NA, input$consumo_modificar),
      Edad_de_inicio = ifelse(is.na(input$edad_inicio_modificar), NA, input$edad_inicio_modificar),
      Sustancia_de_inicio = ifelse(is.null(input$sustancia_modificar), NA, input$sustancia_modificar),
      Tratamientos_previos = ifelse(is.null(input$trat_prev_modificar), NA, input$trat_prev_modificar),
      Observaciones = ifelse(is.null(input$obs_modificar), NA, input$obs_modificar),
      stringsAsFactors = FALSE
    )
    
    # Reemplazar el registro en la posición correcta
    if (length(idx) > 0) {
      data <- data[-last(idx), ]
      data <- rbind(data,act_entry)
      
      # Escribir los datos actualizados en el archivo Excel
      write_data(data)
      
      # Mostrar un mensaje de confirmación
      showModal(modalDialog(
        title = "Registro Actualizado",
        "El registro ha sido actualizado exitosamente.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      showModal(modalDialog(
        title = "Error",
        "No se pudo encontrar el registro para actualizar.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  
  # Acción al presionar el botón "Actualizar Registro"
  observeEvent(input$eliminar, {
    # Leer los datos actuales
    data <- read_data()
    # Encontrar el índice de la fila a actualizar
    idx <- which(data$DNI == input$buscar_dni)
    
    if (length(idx) > 0) {
      data <- data[-last(idx), ]
      
      # Escribir los datos actualizados en el archivo Excel
      write_data(data)
      
      # Mostrar un mensaje de confirmación
      showModal(modalDialog(
        title = "Registro Eliminado",
        "El registro ha sido eliminado exitosamente.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      showModal(modalDialog(
        title = "Error",
        "No se pudo encontrar el registro para eliminar",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Acción al presionar el botón "Guardar Registro"
  observeEvent(input$guardar, {
    # Leer los datos actuales
    data <- read_data()
    data$Edad <- round(as.numeric(difftime(Sys.Date(), data$Fecha_de_nacimiento, units = "days")) / 365, 0)
    
    # Crear un nuevo registro con los datos de entrada
    new_entry <- data.frame(
      Apellido_nombres = ifelse(is.null(input$apellido_nombres), NA, input$apellido_nombres),
      DNI = ifelse(is.na(input$dni), NA, input$dni),
      Entrevista_psicológica_fecha = ifelse(is.null(input$psico_fecha),NA,input$psico_fecha),
      Entrevista_psicológica_asistencia = ifelse(is.null(input$psico), NA, input$psico),
      Entrevista_psiquiátrica_fecha = ifelse(is.null(input$psiqui_fecha), NA, input$psiqui_fecha),
      Entrevista_psiquiátrica_asistencia = ifelse(is.null(input$psiqui), NA, input$psiqui),
      Entrevista_ts_fecha = ifelse(is.null(input$ts_fecha), NA, input$ts_fecha),
      Entrevista_ts_asistencia = ifelse(is.null(input$ts), NA, input$ts),
      Tratamiento = ifelse(is.null(input$tratamiento), NA, input$tratamiento),
      Contacto = ifelse(is.null(input$contacto), NA, input$contacto),
      Fecha_de_nacimiento = ifelse(is.null(input$fecha_nacimiento), NA, input$fecha_nacimiento),
      Edad = ifelse(is.na(input$fecha_nacimiento) || input$fecha_nacimiento == "", NA, as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(as.Date(input$fecha_nacimiento, format = "%d/%m/%Y"), "%Y"))),
      Sexo = ifelse(is.null(input$identidad_gen), NA, input$identidad_gen),
      Nivel_educativo = ifelse(is.null(input$nivel_educativo), NA, input$nivel_educativo),
      Situacion_habitacional = ifelse(is.null(input$situacion_habitacional), NA, input$situacion_habitacional),
      Provincia = ifelse(is.null(input$provincia), NA, input$provincia),
      Localidad = ifelse(is.null(input$localidad), NA, input$localidad),
      Barrio = ifelse(is.null(input$barrio), NA, input$barrio),
      Redes_de_apoyo = ifelse(is.null(input$redes_de_apoyo), NA, input$redes_de_apoyo),
      Tiene_CUD = ifelse(is.null(input$tiene_cud), NA, input$tiene_cud),
      Trabajo = ifelse(is.null(input$trabajo), NA, input$trabajo),
      Ingresos_económicos = ifelse(is.null(input$ingresos_economicos), NA, input$ingresos_economicos),
      Situación_Judicial = ifelse(is.null(input$situacion_judicial), NA, input$situacion_judicial),
      Referencia_APS = ifelse(is.null(input$referencia_aps), NA, input$referencia_aps),
      Derivado_de = ifelse(is.null(input$derivado_de), NA, input$derivado_de),
      Policonsumo = ifelse(is.null(input$policonsumo), NA, input$policonsumo),
      Sustancia_actual = ifelse(is.null(input$consumo), NA, input$consumo),
      Edad_de_inicio = ifelse(is.na(input$edad_inicio), NA, input$edad_inicio),
      Sustancia_de_inicio = ifelse(is.null(input$sustancia), NA, input$sustancia),
      Tratamientos_previos = ifelse(is.null(input$trat_prev), NA, input$trat_prev),
      Observaciones = ifelse(is.null(input$obs), NA, input$obs),
      stringsAsFactors = FALSE
    )
    
    # Agregar el nuevo registro a los datos actuales
    data <- rbind(data, new_entry)
    
    # Escribir los datos actualizados en el archivo Excel
    write_data(data)
    
    # Mostrar un mensaje de confirmación
    showModal(modalDialog(
      title = "Registro Guardado",
      "El registro ha sido guardado exitosamente.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  
  # Acción para descargar la base de datos completa
  output$download_data <- downloadHandler(
    filename = function() {
      paste("ADMISIÓN", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      data <- read_data()
      write_xlsx(data, file)
    }
  )
  
  # Tabla resumen de datos
  output$tabla_resumen <- renderDT({
    data <- read_data()      
    datatable(
      data,
      extensions = 'Scroller',
      options = list(
        lengthChange = TRUE,
        scrollX = TRUE,
        deferRender = TRUE,
        scrollY = 350,
        scroller = TRUE,
        order = list(list(3, 'desc')),
        columnDefs = list(list(width = '150px', targets = c(1:31))),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#EF8D16', 'color': 'white'});",
          "}"
        ),
        language = list(
          url = "//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json",
          infoFiltered = "Todos" # supuestamente cambia el All por todos pero no anda
        )
      ),
      style = "bootstrap",
      colnames = c('Apellido, Nombre', 'DNI', 'Fecha Entrevista Psicológica', 
                   'Asistencia Entrevista Psicológica', 'Fecha Entrevista Psiquiátrica',
                   "Asistencia Entrevista Psiquiátrica", "Fecha Entrevista TS",
                   "Asistencia Entrevista TS", "Tratamiento", 
                   "Contacto", "Fecha de Nacimiento", "Edad", 
                   "Sexo", "Nivel Educativo", "Situación Habitacional", 
                   "Provincia", "Localidad", "Barrio", "Redes de Apoyo", 
                   "Tiene CUD", "Trabajo", "Ingresos Económicos", 
                   "Situación Judicial", "Referencia APS", "Derivado de", 
                   "Policonsumo", "Sustancia Actual", "Edad de Inicio",
                   "Sustancia de Inicio", "Tratamientos Previos", "Observaciones"),
      filter = "top"
    )
  })
  
  # Alfon
  output$int_baigorria <- renderValueBox({
    data <- read_data()
    valueBox(
      nrow(data[data$Tratamiento == "Internación Baigorria",]), 
      "Internación Baigorria",
      icon = icon("hospital-user"), color = "yellow", width = 2
    )
  })
  
  output$int_bp <- renderValueBox({
    data <- read_data()
    valueBox(
      nrow(data[data$Tratamiento == "Internación Buen Pastor",])+nrow(data[data$Tratamiento == "Internación B.P",]), 
      "Internación Buen Pastor",
      icon = icon("hospital-user"), color = "yellow", width = 2
    )
  })
  
  output$int_cristaleria <- renderValueBox({
    data <- read_data()
    valueBox(
      nrow(data[data$Tratamiento == "Internación Cristalería",]), 
      "Internación Cristalería",
      icon = icon("hospital-user"), color = "yellow", width = 2
    )
  })
  
  output$cdd_zeballos <- renderValueBox({
    data <- read_data()
    valueBox(
      nrow(data[data$Tratamiento == "Centro de día Zeballos",]), 
      "Centro de día Zeballos",
      icon = icon("warehouse"), color = "yellow", width = 2
    )
  })
  
  output$cdd_bp <- renderValueBox({
    data <- read_data()
    valueBox(
      nrow(data[data$Tratamiento == "Centro de día Buen Pastor",]), 
      "Centro de día Buen Pastor",
      icon = icon("warehouse"), color = "yellow", width = 2
    )
  })
  
  output$cdd_baigorria <- renderValueBox({
    data <- read_data()
    valueBox(
      nrow(data[data$Tratamiento == "Centro de día Baigorria",]), 
      "Centro de día Baigorria",
      icon = icon("warehouse"), color = "yellow", width = 2
    )
  })
  
  output$barras_por_fecha <- renderPlot({
    data <- read_data()
    # Grafico fechas y tto ---------------------------------------------------------
    primera_entrevista <- numeric()
    for(i in 1:nrow(data)) {
      fechas <- t(data[i,c(3,5,7)])
      fechas <- subset(fechas,!is.na(fechas))
      primera_entrevista[i] <- ifelse(nrow(fechas)==0,NA,format(as.Date(fechas),"%d/%m/%Y"))
    }
    primera_entrevista <- data.frame(
      mes = format(as.Date(primera_entrevista,"%d/%m/%Y"),"%m"),
      anio = format(as.Date(primera_entrevista,"%d/%m/%Y"),"%Y"),
      mes_anio = format(as.Date(primera_entrevista,"%d/%m/%Y"),"%m/%Y"),
      Tratamiento = factor(ifelse(grepl("Internación",data$Tratamiento),"Internación",
                                  ifelse(grepl("Centro de día", data$Tratamiento), "Centro de día",
                                         ifelse(grepl("No finalizó el proceso", data$Tratamiento), "Abandona",
                                                ifelse(grepl("Rechaza", data$Tratamiento), "Rechaza",
                                                       ifelse(grepl("Derivado",data$Tratamiento), "Derivado",NA))))),
                           levels = c("Abandona","Rechaza","Internación","Centro de día","Derivado"),
                           ordered = TRUE)
    )
    
    primera_entrevista <- primera_entrevista %>%
      filter(!is.na(mes_anio)) %>%
      filter(anio!="2022") %>%
      filter(anio!="2021") %>%
      filter(!is.na(Tratamiento)) %>%
      ungroup() %>%
      group_by(anio,mes,Tratamiento) %>%
      summarize(frecuencia = n(),
                x = sum(frecuencia)) %>%
      ungroup() %>%
      arrange(anio,mes,desc(Tratamiento)) %>%
      group_by(anio,mes) %>%
      mutate(label = cumsum(frecuencia))
    
    ggplot(primera_entrevista) +
      geom_bar(aes(x = mes, y = frecuencia, fill = Tratamiento), 
               stat = "identity") +
      geom_text(aes(x = mes, y = label, label = frecuencia), 
                color = "white", size = 5, vjust = 1.2) +
      labs(title = "Cantidad de casos y sus derivaciones según fecha de la primera entrevista",
           x = "Mes de la primera entrevista", y = "Frecuencia",
           fill = " ") +
      facet_wrap(~anio) +
      scale_fill_manual(values = c("#C57412","#EF8D16","#FBC91C","#BCBF1A","#5C8001")) +
      theme_minimal() +
      theme(strip.background =element_rect(fill="#EC7E14", color = "white"),
            strip.text = element_text(colour = 'white', size = 15, face="bold"),
            legend.position = "top",
            legend.text = element_text(size = 15),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 10),
            title = element_text(size = 15, hjust = 0.5))
  })
  
  output$barras_porcentuales_sustancia <- renderPlot({
    data <- read_data()
    data_limpia <- data %>%
      filter(!is.na(Tratamiento) & !is.na(Sustancia_de_inicio))
    data_pje <- data_limpia %>%
      group_by(Tratamiento, Sustancia_de_inicio) %>%
      summarise(count = n()) %>%
      mutate(porcentaje = count / sum(count)) %>%
      ungroup()
    
    data_pje$Tratamiento <- ifelse(data_pje$Tratamiento=="Internación B.P", "Internación Buen Pastor",data_pje$Tratamiento)
    data_pje$Tratamiento <- factor(data_pje$Tratamiento,
                                   levels = c("Centro de día Baigorria",
                                              "Centro de día Buen Pastor",
                                              "Centro de día Zeballos",
                                              "Internación Buen Pastor",
                                              "Internación Baigorria",
                                              "Internación Cristalería",
                                              "No finalizó el proceso",
                                              "Rechaza tratamiento",
                                              "Derivado"
                                              ),
                                   ordered = TRUE)
    
    ggplot(data_pje, aes(x = Tratamiento, y = porcentaje, fill = Sustancia_de_inicio)) +
      geom_bar(stat = "identity", position = "fill") +
      labs(title = "Distribución de Tratamiento por Sustancia actual",
           x = "Tratamiento",
           y = "Porcentaje",
           fill = "Sustancia actual") +
      theme_minimal() +
      coord_flip() +
      scale_fill_manual(values = c("#C57412","#EF8D16","#FBC91C","#BCBF1A","#5C8001")) +
      scale_y_continuous(labels = scales::percent_format()) +
      geom_text(aes(label = scales::percent(porcentaje, accuracy = 1)),
                position = position_fill(vjust = 0.5), size = 4, color = "white") +
      theme(legend.text = element_text(size = 15),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 15),
            title = element_text(size = 15, hjust = 0.5))
  })
  
  output$barras_redes_apoyo <- renderPlot({
    data <- read_data()
    data_limpia <- data %>%
      filter(!is.na(Redes_de_apoyo) & !is.na(Edad_de_inicio))
    data_pje <- data_limpia %>%
      group_by(Redes_de_apoyo, Edad_de_inicio) %>%
      summarise(count = n()) %>%
      mutate(porcentaje = count / sum(count)) %>%
      ungroup()
    
    ggplot(data_pje, aes(x = Redes_de_apoyo, y = porcentaje, fill = Edad_de_inicio)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_fill_brewer(palette = "Accent") +
      labs(title = "Edad de inicio de consumo según red de apoyo",
           x = "Redes de apoyo",
           y = "Porcentaje",
           fill = "Edad de inicio") +
      theme_minimal() +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format()) +
      geom_text(aes(label = scales::percent(porcentaje, accuracy = 1)),
                position = position_fill(vjust = 0.5), color="white") +
      scale_fill_manual(values = c("#EF8D16","#FBC91C","#BCBF1A")) +
      theme(legend.text = element_text(size = 15),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 15),
            title = element_text(size = 15, hjust = 0.5))
    
  })
  
  output$barras_policonsumo <- renderPlot({
    # Sustancia de inicio y policonsumo
    data <- read_data()
    data_limpia <- data %>%
      filter(!is.na(Policonsumo) & !is.na(Sustancia_de_inicio))
    data_pje <- data_limpia %>%
      group_by(Sustancia_de_inicio, Policonsumo) %>%
      summarise(count = n()) %>%
      mutate(porcentaje = count / sum(count)) %>%
      ungroup()
    
    ggplot(data_pje, aes(x = Sustancia_de_inicio, y = porcentaje, fill = Policonsumo)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_fill_brewer(palette = "Accent") +
      labs(title = "Porcentaje de personas en policoncumo para cada sutancia de inicio",
           x = "Sustancia de inicio",
           y = "Porcentaje",
           fill = "Policonsumo") +
      theme_minimal() +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(values = c("#EF8D16","#FBC91C")) +
      geom_text(aes(label = scales::percent(porcentaje, accuracy = 1)),
                position = position_fill(vjust = 0.5),color="white") +
      theme(legend.text = element_text(size = 15),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 15),
            title = element_text(size = 15, hjust = 0.5))
  })
  
  output$barras_edad_inicio_trat <- renderPlot({
    data <- read_data()
    # Edad y tratamiento
    data_limpia <- data %>%
      filter(!is.na(Tratamiento) & !is.na(Edad_de_inicio))
    data_pje <- data_limpia %>%
      group_by(Tratamiento, Edad_de_inicio) %>%
      summarise(count = n()) %>%
      mutate(porcentaje = count / sum(count)) %>%
      ungroup()
    
    ggplot(data_pje, aes(x = Tratamiento, y = porcentaje, fill = Edad_de_inicio)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_fill_brewer(palette = "Accent") +
      labs(title = "Distribución de Tratamiento por edad de inicio",
           x = "Tratamiento",
           y = "Porcentaje",
           fill = "Edad de inicio") +
      theme_minimal() +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(values = c("#EF8D16","#FBC91C","#BCBF1A")) +
      geom_text(aes(label = scales::percent(porcentaje, accuracy = 1)),
                position = position_fill(vjust = 0.5), size = 3) +
      theme(legend.text = element_text(size = 15),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 15),
            title = element_text(size = 15, hjust = 0.5))
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)