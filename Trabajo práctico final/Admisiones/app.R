library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(shinydashboard)
library(readxl)
library(writexl)

# Cargar la base de datos existente
datos <- read_excel("ADMISIÓN.xlsx")

datos$DNI <- as.character(trimws(datos$DNI))

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Centro de Ayuda"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visualización de Datos", tabName = "visualizacion", icon = icon("chart-bar")),
      menuItem("Nueva Admisión", tabName = "nueva_admision", icon = icon("user-plus")),
      menuItem("Modificar Registros", tabName = "modificar_registros", icon = icon("edit")),
      menuItem("Descargar Base de Datos", tabName = "descargar_bd", icon = icon("download"))
    )
  ),
  dashboardBody(
    tabItems(
      # Página de Visualización de Datos
      tabItem(tabName = "visualizacion",
              fluidRow(
                box(plotOutput("grafico_edad"), width = 6),
                box(plotOutput("grafico_Nivel_educativo"), width = 6),
                box(DTOutput("tabla_datos"), width = 12)
              )
      ),
      # Página de Nueva Admisión
      tabItem(tabName = "nueva_admision",
              fluidPage(
                titlePanel("Nueva Admisión"),
                sidebarLayout(
                  sidebarPanel(
                    tabsetPanel(
                      tabPanel("Información Personal",
                               textInput("Apellido_nombres", "Apellido y Nombres"),
                               textInput("DNI", "DNI"),
                               textInput("Contacto", "Contacto"),
                               dateInput("Fecha_de_nacimiento", "Fecha de Nacimiento"),
                               numericInput("Edad", "Edad", value = NA),
                               textInput("Nivel_educativo", "Nivel Educativo"),
                               textInput("Situacion_habitacional", "Situación Habitacional"),
                               textInput("Redes_de_apoyo", "Redes de Apoyo"),
                               checkboxInput("Tiene_CUD", "Tiene CUD"),
                               textInput("Trabajo", "Trabajo"),
                               numericInput("Ingresos_económicos", "Ingresos Económicos", value = NA),
                               textInput("Situacion_judicial", "Situación Judicial")
                      ),
                      tabPanel("Tratamiento",
                               dateInput("Entrevista_psicológica_fecha", "Entrevista Psicológica - Fecha"),
                               checkboxInput("Entrevista_psicologica_asistencia", "Entrevista Psicológica - Asistencia"),
                               dateInput("Entrevista_psiquiátrica_fecha", "Entrevista Psiquiátrica - Fecha"),
                               checkboxInput("Entrevista_psiquiatrica_asistencia", "Entrevista Psiquiátrica - Asistencia"),
                               dateInput("Entrevista_ts_fecha", "Entrevista con TS - Fecha"),
                               checkboxInput("Entrevista_ts_asistencia", "Entrevista con TS - Asistencia"),
                               textInput("Tratamiento", "Tratamiento")
                      ),
                      tabPanel("Historial",
                               textInput("Referencia_APS", "Referencia APS"),
                               textInput("Equipo_referencia", "Equipo Referencia"),
                               textInput("Consumo_actual", "Consumo Actual"),
                               numericInput("Edad_de_inicio", "Edad de Inicio", value = NA),
                               textInput("Sustancia_de_inicio", "Sustancia de Inicio"),
                               textInput("Tratamientos_previos", "Tratamientos Previos"),
                               textInput("Observaciones", "Observaciones")
                      )
                    ),
                    actionButton("submit", "Guardar")
                  ),
                  mainPanel(
                    textOutput("resultado")
                  )
                )
              )
      ),
      # Página de Modificar Registros
      tabItem(tabName = "modificar_registros",
              fluidPage(
                titlePanel("Modificar Registros"),
                sidebarLayout(
                  sidebarPanel(
                    textInput("dni_buscar", "DNI de la persona a modificar"),
                    actionButton("buscar", "Buscar"),
                    uiOutput("editar_formulario"),
                    actionButton("guardar_cambios", "Guardar Cambios")
                  ),
                  mainPanel(
                    textOutput("resultado_modificacion")
                  )
                )
              )
      ),
      # Página de Descargar Base de Datos
      tabItem(tabName = "descargar_bd",
              fluidPage(
                titlePanel("Descargar Base de Datos"),
                downloadButton("downloadData", "Descargar Base de Datos en formato XLSX")
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Visualización de Datos
  output$grafico_edad <- renderPlot({
    ggplot(datos, aes(x = Edad)) + 
      geom_histogram(binwidth = 1) + 
      labs(title = "Distribución de Edades", x = "Edad", y = "Frecuencia")
  })
  
  output$grafico_Nivel_educativo <- renderPlot({
    ggplot(datos, aes(x = Nivel_educativo)) + 
      geom_bar() + 
      labs(title = "Nivel Educativo", x = "Nivel Educativo", y = "Frecuencia")
  })
  
  output$tabla_datos <- renderDT({
    datatable(datos)
  })
  
  # Nueva Admisión
  observeEvent(input$submit, {
    nuevo_registro <- data.frame(
      Apellido_nombres = input$Apellido_nombres,
      DNI = as.character(input$DNI),
      Contacto = input$Contacto,
      Fecha_de_nacimiento = as.Date(input$Fecha_de_nacimiento),
      Edad = input$Edad,
      Nivel_educativo = input$Nivel_educativo,
      Situacion_habitacional = input$Situacion_habitacional,
      Redes_de_apoyo = input$Redes_de_apoyo,
      Tiene_CUD = ifelse(is.null(input$Tiene_CUD), FALSE, input$Tiene_CUD),
      Trabajo = input$Trabajo,
      Ingresos_económicos = input$Ingresos_económicos,
      Situacion_judicial = input$Situacion_judicial,
      Entrevista_psicológica_fecha = as.Date(input$Entrevista_psicológica_fecha),
      Entrevista_psicologica_asistencia = ifelse(is.null(input$Entrevista_psicologica_asistencia), FALSE, input$Entrevista_psicologica_asistencia),
      Entrevista_psiquiátrica_fecha = as.Date(input$Entrevista_psiquiátrica_fecha),
      Entrevista_psiquiatrica_asistencia = ifelse(is.null(input$Entrevista_psiquiatrica_asistencia), FALSE, input$Entrevista_psiquiatrica_asistencia),
      Entrevista_ts_fecha = as.Date(input$Entrevista_ts_fecha),
      Entrevista_ts_asistencia = ifelse(is.null(input$Entrevista_ts_asistencia), FALSE, input$Entrevista_ts_asistencia),
      Tratamiento = input$Tratamiento,
      Referencia_APS = input$Referencia_APS,
      Equipo_referencia = input$Equipo_referencia,
      Consumo_actual = input$Consumo_actual,
      Edad_de_inicio = input$Edad_de_inicio,
      Sustancia_de_inicio = input$Sustancia_de_inicio,
      Tratamientos_previos = input$Tratamientos_previos,
      Observaciones = input$Observaciones,
      stringsAsFactors = FALSE
    )
    
    datos <<- rbind(datos, nuevo_registro)
    
    # Guardar la base de datos actualizada
    write_xlsx(datos, "ADMISIÓN.xlsx")
    
    output$resultado <- renderText("Registro guardado exitosamente")
  })
  
  # Modificar Registros
  observeEvent(input$buscar, {
    # Agregar mensaje de depuración
    print(paste("Buscando DNI:", input$dni_buscar))
    print(head(datos))  # Muestra los primeros registros de datos para verificar
    
    registro <- datos %>% filter(DNI == as.character(input$dni_buscar))
    
    # Mostrar el número de registros encontrados
    print(paste("Número de registros encontrados:", nrow(registro)))
    
    output$editar_formulario <- renderUI({
      if (nrow(registro) == 1) {
        print("Registro encontrado, preparando el formulario de edición.")
        
        # Convertir las fechas a formato adecuado si no son válidas
        fecha_nacimiento <- tryCatch(as.Date(registro$Fecha_de_nacimiento), error = function(e) NA)
        fecha_psico <- tryCatch(as.Date(registro$Entrevista_psicológica_fecha, format="%d/%m/%Y"), error = function(e) NA)
        fecha_psiqui <- tryCatch(as.Date(registro$Entrevista_psiquiátrica_fecha, format="%d/%m/%Y"), error = function(e) NA)
        fecha_ts <- tryCatch(as.Date(registro$Entrevista_ts_fecha, format="%d/%m/%Y"), error = function(e) NA)
        
        list(
          textInput("editar_Apellido_nombres", "Apellido y Nombres", value = registro$Apellido_nombres),
          textInput("editar_dni", "DNI", value = registro$DNI),
          textInput("editar_contacto", "Contacto", value = registro$Contacto),
          dateInput("editar_fecha_de_nacimiento", "Fecha de Nacimiento", value = fecha_nacimiento),
          numericInput("editar_edad", "Edad", value = as.numeric(registro$Edad)),
          textInput("editar_Nivel_educativo", "Nivel Educativo", value = registro$Nivel_educativo),
          textInput("editar_Situacion_habitacional", "Situación Habitacional", value = registro$Situacion_habitacional),
          textInput("editar_Redes_de_apoyo", "Redes de Apoyo", value = registro$Redes_de_apoyo),
          checkboxInput("editar_tiene_cud", "Tiene CUD", value = ifelse(is.na(registro$Tiene_CUD), FALSE, registro$Tiene_CUD == "TRUE")),
          textInput("editar_trabajo", "Trabajo", value = registro$Trabajo),
          numericInput("editar_ingresos_económicos", "Ingresos Económicos", value = as.numeric(registro$Ingresos_económicos)),
          textInput("editar_situacion_judicial", "Situación Judicial", value = registro$Situacion_judicial),
          dateInput("editar_entrevista_psicológica_fecha", "Entrevista Psicológica - Fecha", value = fecha_psico),
          checkboxInput("editar_entrevista_psicologica_asistencia", "Entrevista Psicológica - Asistencia", value = ifelse(is.na(registro$Entrevista_psicologica_asistencia), FALSE, registro$Entrevista_psicologica_asistencia == "Presente")),
          dateInput("editar_entrevista_psiquiátrica_fecha", "Entrevista Psiquiátrica - Fecha", value = fecha_psiqui),
          checkboxInput("editar_entrevista_psiquiatrica_asistencia", "Entrevista Psiquiátrica - Asistencia", value = ifelse(is.na(registro$Entrevista_psiquiatrica_asistencia), FALSE, registro$Entrevista_psiquiatrica_asistencia == "Presente")),
          dateInput("editar_entrevista_ts_fecha", "Entrevista con TS - Fecha", value = fecha_ts),
          checkboxInput("editar_entrevista_ts_asistencia", "Entrevista con TS - Asistencia", value = ifelse(is.na(registro$Entrevista_ts_asistencia), FALSE, registro$Entrevista_ts_asistencia == "Presente")),
          textInput("editar_tratamiento", "Tratamiento", value = registro$Tratamiento),
          textInput("editar_referencia_aps", "Referencia APS", value = registro$Referencia_APS),
          textInput("editar_equipo_referencia", "Equipo Referencia", value = registro$Equipo_referencia),
          textInput("editar_consumo_actual", "Consumo Actual", value = registro$Consumo_actual),
          numericInput("editar_edad_de_inicio", "Edad de Inicio", value = as.numeric(registro$Edad_de_inicio)),
          textInput("editar_sustancia_de_inicio", "Sustancia de Inicio", value = registro$Sustancia_de_inicio),
          textInput("editar_tratamientos_previos", "Tratamientos Previos", value = registro$Tratamientos_previos),
          textInput("editar_observaciones", "Observaciones", value = registro$Observaciones)
        )
      } else {
        print("No se encontró ningún registro con el DNI proporcionado.")
        "No se encontró ningún registro con el DNI proporcionado"
      }
    })
  })
  
  observeEvent(input$guardar_cambios, {
    datos <<- datos %>% 
      mutate(
        Apellido_nombres = ifelse(DNI == as.character(input$dni_buscar), input$editar_Apellido_nombres, Apellido_nombres),
        Contacto = ifelse(DNI == as.character(input$dni_buscar), input$editar_contacto, Contacto),
        Fecha_de_nacimiento = ifelse(DNI == as.character(input$dni_buscar), as.Date(input$editar_fecha_de_nacimiento), Fecha_de_nacimiento),
        Edad = ifelse(DNI == as.character(input$dni_buscar), input$editar_edad, Edad),
        Nivel_educativo = ifelse(DNI == as.character(input$dni_buscar), input$editar_Nivel_educativo, Nivel_educativo),
        Situacion_habitacional = ifelse(DNI == as.character(input$dni_buscar), input$editar_Situacion_habitacional, Situacion_habitacional),
        Redes_de_apoyo = ifelse(DNI == as.character(input$dni_buscar), input$editar_Redes_de_apoyo, Redes_de_apoyo),
        Tiene_CUD = ifelse(DNI == as.character(input$dni_buscar), input$editar_tiene_cud, Tiene_CUD),
        Trabajo = ifelse(DNI == as.character(input$dni_buscar), input$editar_trabajo, Trabajo),
        Ingresos_económicos = ifelse(DNI == as.character(input$dni_buscar), input$editar_ingresos_económicos, Ingresos_económicos),
        Situacion_judicial = ifelse(DNI == as.character(input$dni_buscar), input$editar_situacion_judicial, Situacion_judicial),
        Entrevista_psicológica_fecha = ifelse(DNI == as.character(input$dni_buscar), as.Date(input$editar_entrevista_psicológica_fecha), Entrevista_psicológica_fecha),
        Entrevista_psicologica_asistencia = ifelse(DNI == as.character(input$dni_buscar), input$editar_entrevista_psicologica_asistencia, Entrevista_psicologica_asistencia),
        Entrevista_psiquiátrica_fecha = ifelse(DNI == as.character(input$dni_buscar), as.Date(input$editar_entrevista_psiquiátrica_fecha), Entrevista_psiquiátrica_fecha),
        Entrevista_psiquiatrica_asistencia = ifelse(DNI == as.character(input$dni_buscar), input$editar_entrevista_psiquiatrica_asistencia, Entrevista_psiquiatrica_asistencia),
        Entrevista_ts_fecha = ifelse(DNI == as.character(input$dni_buscar), as.Date(input$editar_entrevista_ts_fecha), Entrevista_ts_fecha),
        Entrevista_ts_asistencia = ifelse(DNI == as.character(input$dni_buscar), input$editar_entrevista_ts_asistencia, Entrevista_ts_asistencia),
        Tratamiento = ifelse(DNI == as.character(input$dni_buscar), input$editar_tratamiento, Tratamiento),
        Referencia_APS = ifelse(DNI == as.character(input$dni_buscar), input$editar_referencia_aps, Referencia_APS),
        Equipo_referencia = ifelse(DNI == as.character(input$dni_buscar), input$editar_equipo_referencia, Equipo_referencia),
        Consumo_actual = ifelse(DNI == as.character(input$dni_buscar), input$editar_consumo_actual, Consumo_actual),
        Edad_de_inicio = ifelse(DNI == as.character(input$dni_buscar), input$editar_edad_de_inicio, Edad_de_inicio),
        Sustancia_de_inicio = ifelse(DNI == as.character(input$dni_buscar), input$editar_sustancia_de_inicio, Sustancia_de_inicio),
        Tratamientos_previos = ifelse(DNI == as.character(input$dni_buscar), input$editar_tratamientos_previos, Tratamientos_previos),
        Observaciones = ifelse(DNI == as.character(input$dni_buscar), input$editar_observaciones, Observaciones)
      )
    
    # Guardar la base de datos actualizada
    write_xlsx(datos, "ADMISIÓN.xlsx")
    
    output$resultado_modificacion <- renderText("Registro modificado exitosamente")
  })
  
  # Descargar la base de datos en formato XLSX
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("ADMISIÓN", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Guardar la base de datos en el archivo especificado
      write_xlsx(datos, file)
      # Verificar que el archivo se ha guardado correctamente
      if (file.exists(file)) {
        print("Archivo guardado correctamente")
      } else {
        print("Error al guardar el archivo")
      }
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
