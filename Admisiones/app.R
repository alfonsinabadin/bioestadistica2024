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

# Definir la ruta del archivo Excel donde se guardarán los datos
file_path <- "ADMISIÓN.xlsx"

# Función para leer los datos del archivo Excel
read_data <- function() {
  if (file.exists(file_path)) {
    read_excel(file_path)
  } else {
    data.frame(
      Apellido_nombres = character(),
      DNI = numeric(),
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
      Edad_de_inicio = numeric(),
      Sustancia_de_inicio = character(),
      Tratamientos_previos = character(),
      Observaciones = character(),
      stringsAsFactors = FALSE
    )
  }
}

provincias <- show_arg_codes()[2:25,5]
provincias[[1]][1] <- "CABA"

localidades_por_provincia <- readRDS("localidades.rds")

# Función para escribir los datos en el archivo Excel
write_data <- function(data) {
  temp_file <- tempfile(fileext = ".xlsx")
  write_xlsx(data, temp_file)
  file.copy(temp_file, file_path, overwrite = TRUE)
}

# Definir la interfaz de usuario (UI) de la aplicación Shiny
ui <- fluidPage(
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
                                   column(12, textInput("apellido_nombres", "Apellido, Nombres"),)
                                 ),
                                 fluidRow(
                                   column(6, numericInput("dni", "DNI", value = NULL)),
                                   column(6, dateInput("fecha_nacimiento", "Fecha de nacimiento", format = "dd/mm/yyyy", 
                                                       value = ""))
                                 ),
                                 fluidRow(
                                   column(12, numericInput("contacto", "Teléfono de contacto", value = NULL))
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
                                     column(6, dateInput("psico_fecha", "Fecha", value = "", format = "dd/mm/yyyy")),
                                     column(6, selectInput("psico", "Estado",
                                                           choices = c("No asignada", "Presente", "Ausente", "Pendiente"), selected = "No asignada"))
                                   )
                                 ),
                                 box(
                                   title = "Entrevista psiquiátrica",
                                   status = "warning",
                                   solidHeader = FALSE,
                                   width = 4,
                                   fluidRow(
                                     column(6, dateInput("psiqui_fecha", "Fecha", value = "", format = "dd/mm/yyyy")),
                                     column(6, selectInput("psiqui", "Estado",
                                                           choices = c("No asignada", "Presente", "Ausente", "Pendiente"), selected = "No asignada"))
                                   )
                                 ),
                                 box(
                                   title = "Entrevista T.S.",
                                   status = "warning",
                                   solidHeader = FALSE,
                                   width = 4,
                                   fluidRow(
                                     column(6, dateInput("ts_fecha", "Fecha", value = "", format = "dd/mm/yyyy")),
                                     column(6, selectInput("ts", "Estado",
                                                           choices = c("No asignada", "Presente", "Ausente", "Pendiente"), selected = "No asignada"))
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
                                 status = "warning",
                                 solidHeader = TRUE,
                                 width = 6,
                                 fluidRow(
                                   column(4, selectInput("identidad_gen", "Identidad de género", choices = c(
                                     "Mujer", "Varón", "Mujer Trans", "Varón Trans", "No binario", "Otro"
                                   ))),
                                   column(4, selectInput("nivel_educativo", "Nivel educativo", choices = c(
                                     "Sin instrucción formal", "Inicial", "Primario en curso", "Primario incompleto (incluye educación especial)",
                                     "Primario completo", "Secundario en curso", "Secundario incompleto", "Secundario completo", "Superior terciario o universitario en curso",
                                     "Superior terciario o universitario incompleto", "Superior terciario o universitario completo",
                                     "No sabe / No responde"))),
                                   column(4, selectInput("tiene_cud", "Tiene CUD", choices = c("Sí", "No")))
                                 ),
                                 fluidRow(
                                   column(3, selectizeInput("situacion_habitacional", "Situación Habitacional", choices = c(
                                     "Casa/depto propio", "Casa/depto alquilado", "Casa/depto cedido", "Pensión", 
                                     "Internado / Institucionalizado", "Refugio", "Situación de calle"), selected = NULL,
                                     options = list(create = TRUE, placeholder = 'Escriba la opción'))),
                                   column(3, selectInput("provincia", "Provincia", choices = provincias)),
                                   column(3, uiOutput("localidad_ui")),
                                   column(3, textInput("barrio", "Barrio"))
                                 ),
                                 fluidRow(
                                   column(3, selectInput("redes_de_apoyo", "Redes de Apoyo", choices = c(
                                     "Ninguna", "Escasa", "Buena", "Familia/Amigos", "Institución", "Familia/Amigos e Institución"))),
                                   column(3, selectInput("trabajo", "Trabajo", choices = c("No tiene", "Esporádico", "Estable"))),
                                   column(3, selectInput("ingresos_economicos", "Ingresos Económicos", choices = c(
                                     "Sin ingresos", "Pensión no Contributiva (PNC)", "Subsidio", "Salario informal","Salario informal + subsidio",  "Salario formal"))),
                                   column(3, selectInput("situacion_judicial", "Situación Judicial", choices = c(
                                     "Sin causas Judiciales","Con causa abierta", "Con causa cerrada", "Desconoce")))
                                 )
                               ),
                               box(
                                 title = "Consumo",
                                 status = "warning",
                                 solidHeader = TRUE,
                                 width = 6,
                                 fluidRow(
                                   column(6, textInput("consumo", "Consumo actual", placeholder = "Droga más prevalente")),
                                   column(6, selectInput("policonsumo", "Policonsumo", choices = c("No", "Sí"))),
                                   column(6, textInput("sustancia", "Sustancia de inicio", placeholder = "Completar con una sola droga")),
                                   column(6, numericInput("edad_inicio", "Edad de inicio", value = NULL)),
                                   column(4, selectInput("referencia_aps", "Referencia APS", choices = c(
                                     "Sólo clínica médica", "Referencia con seguimiento", "Referencia sin seguimiento", "No está referenciado"))),
                                   column(4, textInput("derivado_de", "Derivado de:")),
                                   column(4, selectInput("trat_prev", "Tratamientos previos", choices = c("No", "Entre 1 y 2", "3 o más")))
                                 )
                               ),
                               box(
                                 status = "warning",
                                 solidHeader = TRUE,
                                 width = 10,
                                 fluidRow(
                                   column(12, textInput("obs", "Observaciones"))
                                 )
                               ),
                               box(
                                 status = "warning",
                                 solidHeader = TRUE,
                                 width = 2,
                                 fluidRow(
                                   column(12, actionButton("guardar", "Guardar Registro"))
                                 ),
                                 fluidRow(
                                   column(12, " "),
                                   column(12, downloadButton("download_data", "Descargar Base de Datos"))
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
                                   column(4, textInput("buscar_dni", "Buscar por DNI")),
                                   column(2, actionButton("buscar", "Buscar"))
                                 ),
                                 uiOutput("resultados_busqueda")
                               )
                             )
                    )
                  )
                )
  )
)

# Definir la lógica del servidor de la aplicación Shiny
server <- function(input, output, session) {
  
  # Acción al presionar el botón "Buscar"
  observeEvent(input$buscar, {
    data <- read_data()
    registro <- data[data$DNI == input$buscar_dni, ]
    
    if (nrow(registro) > 0) {
      updateTextInput(session, "apellido_nombres", value = registro$Apellido_nombres[1])
      updateNumericInput(session, "dni", value = registro$DNI[1])
      updateDateInput(session, "fecha_nacimiento", value = as.Date(registro$Fecha_de_nacimiento[1], format = "%Y-%m-%d"))
      updateNumericInput(session, "contacto", value = registro$Contacto[1])
      updateDateInput(session, "psico_fecha", value = as.Date(registro$Entrevista_psicológica_fecha[1], format = "%Y-%m-%d"))
      updateSelectInput(session, "psico", selected = registro$Entrevista_psicológica_asistencia[1])
      updateDateInput(session, "psiqui_fecha", value = as.Date(registro$Entrevista_psiquiátrica_fecha[1], format = "%Y-%m-%d"))
      updateSelectInput(session, "psiqui", selected = registro$Entrevista_psiquiátrica_asistencia[1])
      updateDateInput(session, "ts_fecha", value = as.Date(registro$Entrevista_ts_fecha[1], format = "%Y-%m-%d"))
      updateSelectInput(session, "ts", selected = registro$Entrevista_ts_asistencia[1])
      updateSelectInput(session, "tratamiento", selected = registro$Tratamiento[1])
      updateSelectInput(session, "nivel_educativo", selected = registro$Nivel_educativo[1])
      updateSelectInput(session, "situacion_habitacional", selected = registro$Situacion_habitacional[1])
      updateSelectInput(session, "provincia", selected = registro$Provincia[1])
      updateSelectInput(session, "localidad", selected = registro$Localidad[1])
      updateTextInput(session, "barrio", value = registro$Barrio[1])
      updateSelectInput(session, "redes_de_apoyo", selected = registro$Redes_de_apoyo[1])
      updateSelectInput(session, "tiene_cud", selected = registro$Tiene_CUD[1])
      updateSelectInput(session, "trabajo", selected = registro$Trabajo[1])
      updateSelectInput(session, "ingresos_economicos", selected = registro$Ingresos_económicos[1])
      updateSelectInput(session, "situacion_judicial", selected = registro$Situación_Judicial[1])
      updateSelectInput(session, "referencia_aps", selected = registro$Referencia_APS[1])
      updateTextInput(session, "derivado_de", value = registro$Derivado_de[1])
      updateSelectInput(session, "policonsumo", selected = registro$Policonsumo[1])
      updateTextInput(session, "sustancia", value = registro$Sustancia_actual[1])
      updateNumericInput(session, "edad_inicio", value = registro$Edad_de_inicio[1])
      updateTextInput(session, "sustancia", value = registro$Sustancia_de_inicio[1])
      updateSelectInput(session, "trat_prev", selected = registro$Tratamientos_previos[1])
      updateTextInput(session, "obs", value = registro$Observaciones[1])
    } else {
      showModal(modalDialog(
        title = "Registro no encontrado",
        "No se encontró ningún registro con el DNI proporcionado.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Modificar el botón de guardar para actualizar registros existentes
  observeEvent(input$guardar, {
    data <- read_data()
    registro <- data[data$DNI == input$dni, ]
    
    if (nrow(registro) > 0) {
      data[data$DNI == input$dni, ] <- data.frame(
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
        Edad = ifelse(is.na(input$fecha_nacimiento), NA, as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(as.Date(input$fecha_nacimiento, format = "%d/%m/%Y"), "%Y"))),
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
        Sustancia_actual = ifelse(is.null(input$sustancia), NA, input$sustancia),
        Edad_de_inicio = ifelse(is.na(input$edad_inicio), NA, input$edad_inicio),
        Sustancia_de_inicio = ifelse(is.null(input$sustancia), NA, input$sustancia),
        Tratamientos_previos = ifelse(is.null(input$trat_prev), NA, input$trat_prev),
        Observaciones = ifelse(is.null(input$obs), NA, input$obs),
        stringsAsFactors = FALSE
      )
    } else {
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
        Edad = ifelse(is.na(input$fecha_nacimiento), NA, as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(as.Date(input$fecha_nacimiento, format = "%d/%m/%Y"), "%Y"))),
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
        Sustancia_actual = ifelse(is.null(input$sustancia), NA, input$sustancia),
        Edad_de_inicio = ifelse(is.na(input$edad_inicio), NA, input$edad_inicio),
        Sustancia_de_inicio = ifelse(is.null(input$sustancia), NA, input$sustancia),
        Tratamientos_previos = ifelse(is.null(input$trat_prev), NA, input$trat_prev),
        Observaciones = ifelse(is.null(input$obs), NA, input$obs),
        stringsAsFactors = FALSE
      )
      data <- rbind(data, new_entry)
    }
    
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
  
  # Validación del campo DNI (tiene que estar sí o sí)
  iv <- InputValidator$new()
  iv$add_rule("dni", sv_required(" "))
  iv$enable()
  
  # Crear el uiOutput para las localidades
  output$localidad_ui <- renderUI({
    selectInput("localidad", "Localidad", choices = NULL)
  })
  
  # Actualizar las localidades en función de la provincia seleccionada
  observeEvent(input$provincia, {
    localidades <- localidades_por_provincia[[input$provincia]]
    updateSelectInput(session, "localidad", choices = localidades)
  })
  
  # Acción al presionar el botón "Guardar Registro"
  observeEvent(input$guardar, {
    # Leer los datos actuales
    data <- read_data()
    data$Edad <- as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(as.Date(data$Fecha_de_nacimiento, format = "%d/%m/%Y"), "%Y"))
    
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
      Edad = ifelse(is.na(input$fecha_nacimiento), NA, as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(as.Date(input$fecha_nacimiento, format = "%d/%m/%Y"), "%Y"))),
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
      Sustancia_actual = ifelse(is.null(input$sustancia), NA, input$sustancia),
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
}

# Ejecutar la aplicación Shiny
shinyApp(ui, server)
