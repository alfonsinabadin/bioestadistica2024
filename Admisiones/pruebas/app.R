# Cargar las bibliotecas necesarias
library(shiny)
library(shinyWidgets)
library(readxl)
library(writexl)
library(DT)
library(shinyvalidate)

# Definir la ruta del archivo Excel donde se guardarán los datos
file_path <- "ADMISIÓN.xlsx"

# Función para leer los datos del archivo Excel
read_data <- function() {
  if (file.exists(file_path)) {
    read_excel(file_path)
  } else {
    data.frame(
      Apellido_nombres = character(),
      DNI = character(),
      Entrevista_psicológica_fecha = character(),
      Entrevista_psicológica_asistencia = character(),
      Entrevista_psiquiátrica_fecha = character(),
      Entrevista_psiquiátrica_asistencia = character(),
      Entrevista_ts_fecha = character(),
      Entrevista_ts_asistencia = character(),
      Tratamiento = character(),
      Contacto = character(),
      Fecha_de_nacimiento = character(),
      Edad = numeric(),
      Nivel_educativo = character(),
      Situacion_habitacional = character(),
      Redes_de_apoyo = character(),
      Tiene_CUD = character(),
      Trabajo = character(),
      Ingresos_económicos = character(),
      Situación_Judicial = character(),
      Referencia_APS = character(),
      Equipo_referencia = character(),
      Consumo_actual = character(),
      Edad_de_inicio = numeric(),
      Sustancia_de_inicio = character(),
      Tratamientos_previos = character(),
      Observaciones = character(),
      stringsAsFactors = FALSE
    )
  }
}

# Función para escribir los datos en el archivo Excel
write_data <- function(data) {
  write_xlsx(data, file_path)
}

# Definir la interfaz de usuario (UI) de la aplicación Shiny
ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "Admisiones - Comunidad Padre Misericordioso", titleWidth = 500),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
      tabsetPanel(
        # Pestaña "Nuevo registro" ------------------------------------------------------------------------------------------------------------------
        tabPanel("Nuevo registro",
                 fluidRow(
                   box(
                     title = "Identificación",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     fluidRow(
                       column(6, textInput("apellido_nombres", "Apellido y nombres")),
                       column(6, textInput("dni", "DNI", placeholder = "Sin puntos ni comas"))
                     )
                   ),
                   box(
                     title = "Tratamiento",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     box(
                       title = "Entrevista psicológica",
                       status = "primary",
                       solidHeader = FALSE,
                       width = 4,
                       fluidRow(
                         column(6, dateInput("psico_fecha", "Fecha", value = NULL, format = "dd/mm/yyyy")),
                         column(6, selectInput("psico", "Estado",
                                               choices = c("No asignada", "Presente", "Ausente", "Pendiente de registración"), selected = "No asignada"))
                       )
                     ),
                     box(
                       title = "Entrevista psiquiátrica",
                       status = "primary",
                       solidHeader = FALSE,
                       width = 4,
                       fluidRow(
                         column(6, dateInput("psiqui_fecha", "Fecha", value = NULL, format = "dd/mm/yyyy")),
                         column(6, selectInput("psiqui", "Estado",
                                               choices = c("No asignada", "Presente", "Ausente", "Pendiente de registración"), selected = "No asignada"))
                       )
                     ),
                     box(
                       title = "Entrevista T.S.",
                       status = "primary",
                       solidHeader = FALSE,
                       width = 4,
                       fluidRow(
                         column(6, dateInput("ts_fecha", "Fecha", value = NULL, format = "dd/mm/yyyy")),
                         column(6, selectInput("ts", "Estado",
                                               choices = c("No asignada", "Presente", "Ausente", "Pendiente de registración"), selected = "No asignada"))
                       )
                     ),
                     fluidRow(
                       column(1,""),
                       column(10, selectInput("tratamiento", "Tratamiento asignado", choices = c(
                         "Continúa en seguimiento", "Internación Cristalería", "Internación Baigorria",
                         "Internación Buen Pastor", "Internación B.P", "Centro de día Zeballos",
                         "Centro de día Buen Pastor", "Centro de día Baigorria", "Derivado",
                         "No finalizó el proceso", "Rechaza tratamiento"))),
                       column(1,""))
                   ),
                   box(
                     title = "Información personal",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 6,
                     fluidRow(
                       column(4, textInput("contacto", "Contacto")),
                       column(4, dateInput("fecha_nacimiento", "Fecha de nacimiento", format = "dd/mm/yyyy", value = NULL)),
                       column(4, numericInput("edad", "Edad", value = NA)),
                       column(4, selectInput("nivel_educativo", "Nivel educativo", choices = c(
                         "Sin instrucción formal", "Inicial", "Primario incompleto (incluye educación especial)",
                         "Primario completo", "Secundario incompleto", "Secundario Completo",
                         "Superior terciario o universitario incompleto", "Superior terciario o universitario completo",
                         "No sabe / No responde")))
                     ),
                     fluidRow(
                       column(4, selectizeInput("situacion_habitacional", "Situación Habitacional", choices = c(
                         "Casa/depto propio", "Casa/depto alquilado", "Casa/depto cedido", "Internado en efector de salud",
                         "Refugio", "Situación de calle", "Pensión", "Institucionalizado", "En institución penal",
                         "En institución de SM", "En institución terapéutica"), selected = NULL,
                         options = list(create = TRUE, placeholder = 'Escriba la opción'))),
                       column(4, selectInput("redes_de_apoyo", "Redes de Apoyo", choices = c(
                         "Ninguna", "Escasa", "Buena", "Familia/Amigos + Institución", "Familia/Amigos", "Institución"))),
                       column(4, selectInput("tiene_cud", "Tiene CUD", choices = c("Sí", "No")))
                     ),
                     fluidRow(
                       column(4, selectInput("trabajo", "Trabajo", choices = c("No tiene", "Esporádico", "Estable"))),
                       column(4, selectInput("ingresos_economicos", "Ingresos Económicos", choices = c(
                         "Sin ingresos", "PNC nacional", "Salario informal + subsidio", "Salario informal"))),
                       column(4, selectInput("situacion_judicial", "Situación Judicial", choices = c(
                         "Con causa cerrada", "Sin causas Judiciales", "Desconoce")))
                     )
                   ),
                   box(
                     title = "Historial de consumo",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 6,
                     fluidRow(
                       column(4, selectInput("referencia_aps", "Referencia APS", choices = c(
                         "Sólo clínica médica", "Referencia con seguimiento", "Referencia sin seguimiento", "No está referenciado"))),
                       column(4, textInput("referencia", "Derivado de:")),
                       column(4, selectInput("trat_prev", "Tratamientos previos", choices = c("No", "Entre 1 y 2", "3 o más")))
                     ),
                     fluidRow(
                       column(4, numericInput("edad_inicio", "Edad de inicio", value = NA)),
                       column(4, textInput("sustancia", "Sustancia de inicio")),
                       column(4, textInput("consumo", "Consumo actual"))
                     ),
                     fluidRow(
                       column(1, ""),
                       column(10, textInput("obs", "Observaciones")),
                       column(1, "")
                     )
                   ),
                   fluidRow(
                     column(4, actionButton("guardar", "Guardar Registro"))
                   )
                 )
        ),
        # Pestaña "Modificación de registros" ----------------------------------------------------------------------------------------------------------
        tabPanel("Modificación de registros",
                 fluidRow(
                   box(
                     title = "Modificación de registros",
                     status = "primary",
                     solidHeader = TRUE,
                     width = 12,
                     "Aquí puedes modificar los registros existentes."
                   )
                 )
        )
      )
    )
  )
)

# Definir la lógica del servidor de la aplicación Shiny
server <- function(input, output, session) {
  
  # Validación del campo DNI (tiene que estar si o si)
  iv <- InputValidator$new()
  iv$add_rule("dni", sv_required("Campo requerido"))
  iv$enable()
  
  # Acción al presionar el botón "Guardar Registro"
  observeEvent(input$guardar, {
    # Leer los datos actuales
    data <- read_data()
    
    # Crear un nuevo registro con los datos de entrada
    new_entry <- data.frame(
      Apellido_nombres = ifelse(is.null(input$apellido_nombres), NA, input$apellido_nombres),
      DNI = ifelse(is.null(input$dni), NA, input$dni),
      Entrevista_psicológica_fecha = as.character(ifelse(is.null(input$psico_fecha), NA, input$psico_fecha)),
      Entrevista_psicológica_asistencia = ifelse(is.null(input$psico), NA, input$psico),
      Entrevista_psiquiátrica_fecha = as.character(ifelse(is.null(input$psiqui_fecha), NA, input$psiqui_fecha)),
      Entrevista_psiquiátrica_asistencia = ifelse(is.null(input$psiqui), NA, input$psiqui),
      Entrevista_ts_fecha = as.character(ifelse(is.null(input$ts_fecha), NA, input$ts_fecha)),
      Entrevista_ts_asistencia = ifelse(is.null(input$ts), NA, input$ts),
      Tratamiento = ifelse(is.null(input$tratamiento), NA, input$tratamiento),
      Contacto = ifelse(is.null(input$contacto), NA, input$contacto),
      Fecha_de_nacimiento = as.character(ifelse(is.null(input$fecha_nacimiento), NA, input$fecha_nacimiento)),
      Edad = ifelse(is.na(input$edad), NA, input$edad),
      Nivel_educativo = ifelse(is.null(input$nivel_educativo), NA, input$nivel_educativo),
      Situacion_habitacional = ifelse(is.null(input$situacion_habitacional), NA, input$situacion_habitacional),
      Redes_de_apoyo = ifelse(is.null(input$redes_de_apoyo), NA, input$redes_de_apoyo),
      Tiene_CUD = ifelse(is.null(input$tiene_cud), NA, input$tiene_cud),
      Trabajo = ifelse(is.null(input$trabajo), NA, input$trabajo),
      Ingresos_económicos = ifelse(is.null(input$ingresos_economicos), NA, input$ingresos_economicos),
      Situación_Judicial = ifelse(is.null(input$situacion_judicial), NA, input$situacion_judicial),
      Referencia_APS = ifelse(is.null(input$referencia_aps), NA, input$referencia_aps),
      Equipo_referencia = ifelse(is.null(input$referencia), NA, input$referencia),
      Consumo_actual = ifelse(is.null(input$consumo), NA, input$consumo),
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
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)
