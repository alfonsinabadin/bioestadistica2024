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
library(shinyWidgets)
library(ggplot2)
library(ggthemes)
library(DT)
library(openxlsx)

# Importar base ----------------------------------------------------------------
base <- function(){
  data <- read_excel("Base completa.xlsx")
  return(data)
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
    /* Estilo para todos los botones en la aplicación */
      .btn {
        background-color: #ec7e14 !important;
        border-color: #ec7e14 !important;
        color: white !important;
      }
      .btn:hover {
        background-color: #d96a0f !important;
        border-color: #d96a0f !important;
      }

      /* Estilo específico para el botón 'Buscar' */
      #search_button {
        background-color: #ec7e14 !important;
        border-color: #ec7e14 !important;
        color: white !important;
      }
      #search_button:hover {
        background-color: #d96a0f !important;
        border-color: #d96a0f !important;
      }

      /* Estilo específico para el botón 'Modificar registro' */
      #modify_button {
        background-color: #ec7e14 !important;
        border-color: #ec7e14 !important;
        color: white !important;
      }
      #modify_button:hover {
        background-color: #d96a0f !important;
        border-color: #d96a0f !important;
      }

      /* Estilo específico para el botón 'Cancelar búsqueda' */
      #cancel_search_button {
        background-color: #ec7e14 !important;
        border-color: #ec7e14 !important;
        color: white !important;
      }
      #cancel_search_button:hover {
        background-color: #d96a0f !important;
        border-color: #d96a0f !important;
      }
      .modal-dialog {
      width: 95% !important;
      max-width: 95% !important;
    }
    .modal-content {
      height: 90vh;
      overflow: hidden;
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
    
  ),
  
  nav_panel(
    tags$span("Consulta y modificación de registros", style = "font-size: 14px;"),
    class = "bslib-page-dashboard",
    icon = icon("pen-to-square"),
    
    fluidPage(
      useShinyjs(),  # Habilitar shinyjs para usar las funciones de mostrar y ocultar elementos
      
      # Mensaje de éxito en verde (oculto inicialmente)
      div(
        id = "success_message",
        style = "display: none; background-color: #4CAF50; color: white; padding: 10px; margin-bottom: 15px; text-align: center;",
        "Modificaciones guardadas con éxito"
      ), # ver
      
      # Buscador de DNI o Nombre
      textInput("search_input", "Buscar por DNI, Nombre o Apellido", ""),
      actionButton("search_button", "Buscar"),
      
      conditionalPanel(
        condition = "output.showTable == true",
        
        # Tabla de resultados de búsqueda
        div(dataTableOutput("search_results"),
            style = "font-size:12px"),
        
        # Botones en la esquina inferior derecha de la tabla
        tags$div(
          style = "margin-top: 15px; display: flex; gap: 10px; justify-content: flex-end;",
          actionButton("cancel_button", "Cancelar búsqueda", width = '150px'),
          actionButton("modify_button", "Modificar registro", width = '150px'),
          # solo manager?
          actionButton("delete_button", "Eliminar registro", width = '150px')
        )
      )
    )
  ),
  
  nav_panel(
    tags$span("Tablero de visualización", style = "font-size: 14px;"),
    class = "bslib-page-dashboard",
  ),
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  )
)

server <- function(input, output, session) {
  
  # Cargar la base de datos
  data <- base()
  
  # PESTAÑA MODIFICACIÓN DE REGISTRO---------------------------------------------------------
  # Filtrar los resultados de la búsqueda
  # Reactivo para almacenar los resultados de la búsqueda
  search_results <- reactiveVal(NULL)
  registro_seleccionado <- reactiveVal(NULL)
  
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
      mutate(`Fecha de registro` = format(`Fecha de registro`, "%d/%m/%Y"))
    # ponerle formato a todas las fechas para que se vean bien
    
    datatable(
      resultados_tabla,
      selection = "single",  # Permite seleccionar una fila
      rownames = FALSE,
      options = list(
        paging = FALSE,
        scrollX = TRUE,
        scrollY = "200px",
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
      
      # Mostrar el modal con los datos
      showModal(modalDialog(
        title = "Modificar Registro",
        size = "l",
        style = "width: 100%; height: 100vh; max-width: 100%; max-height: 100vh; overflow: hidden; padding: 0; margin: 0;",
        
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
                  selected = registro$`Estado de la Entrevista con Psicólogo`
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
                  selected = registro$`Estado de la Entrevista con Psiquiátra`
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
                  selected = registro$`Estado de la Entrevista con Trabajador Social`
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
                    "Cdd Baigorria", "Cdd Buen Pastor", "Centro de día Zeballos", "Derivado",
                    "Internación B.P.", "Internación Baig.", "Internación Cristalería",
                    "No finalizó admisión", "Rechaza tratamiento", "Seguimiento", ""
                  ),
                  selected = registro$`Tratamiento Elegido`
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
                      value = registro$`Fecha de Nacimiento`,
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
                    selectInput(
                      "tipo_vinculo_contacto_11",
                      label = tags$span("Vínculo", style = "font-size: 12px;"),
                      choices = c("","Propio","Papá","Mamá", "Hermano","Hermana", "Hijo","Hija", "Amigo", "Amiga"),
                      selected = registro$`Tipo de Vínculo con el Contacto 1`
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
                    selectInput(
                      "tipo_vinculo_contacto_21",
                      label = tags$span("Vínculo", style = "font-size: 12px;"),
                      choices = c("","Propio","Papá","Mamá", "Hermano","Hermana", "Hijo","Hija", "Amigo", "Amiga"),
                      selected = registro$`Tipo de Vínculo con el Contacto 2`
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
                    selectInput(
                      "tipo_vinculo_contacto_31",
                      label = tags$span("Vínculo", style = "font-size: 12px;"),
                      choices = c("","Propio","Papá","Mamá", "Hermano","Hermana", 
                                  "Hijo","Hija", "Amigo", "Amiga"),
                      selected = registro$`Tipo de Vínculo con el Contacto 3`
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
          wellPanel(
            style = "min-height: 460px; margin-top: 20px;", 
            
            # Inicio del consumo
            fluidRow(
              h4("Inicio del consumo", style = "font-size: 15px; font-weight: bold;"),
              column(
                width = 4,
                numericInput(
                  inputId = "edad_inicio_consumo1",
                  label = tags$span("Edad de inicio de consumo", style = "font-size: 12px;"),
                  value = registro$`Edad de Inicio de Consumo`
                )
              ),
              # Campo sustancia de inicio
              column(
                width = 4,
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
                width = 4,  
                conditionalPanel(
                  condition = "input.sustancia_inicio_consumo1 == 'Otra'", # Se activa si se selecciona 'Otra'
                  textInput(
                    inputId = "otra_sustancia1",
                    label = tags$span("Especifique la sustancia", style = "font-size: 12px;"),
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
                width = 8,
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
                width = 8,
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
                    selected = {
                      sustancias_seleccionadas <- c()
                      if (!is.null(registro$`Consumo actual con Cocaína`) && !is.na(registro$`Consumo actual con Cocaína`) && registro$`Consumo actual con Cocaína` == "Si") sustancias_seleccionadas <- c(sustancias_seleccionadas, "Cocaína")
                      if (!is.null(registro$`Consumo actual con Alcohol`) && !is.na(registro$`Consumo actual con Alcohol`) && registro$`Consumo actual con Alcohol` == "Si") sustancias_seleccionadas <- c(sustancias_seleccionadas, "Alcohol")
                      if (!is.null(registro$`Consumo actual con Marihuana`) && !is.na(registro$`Consumo actual con Marihuana`) && registro$`Consumo actual con Marihuana` == "Si") sustancias_seleccionadas <- c(sustancias_seleccionadas, "Marihuana")
                      if (!is.null(registro$`Consumo actual con Crack`) && !is.na(registro$`Consumo actual con Crack`) && registro$`Consumo actual con Crack` == "Si") sustancias_seleccionadas <- c(sustancias_seleccionadas, "Crack")
                      if (!is.null(registro$`Consumo actual con Nafta Aspirada`) && !is.na(registro$`Consumo actual con Nafta Aspirada`) && registro$`Consumo actual con Nafta Aspirada` == "Si") sustancias_seleccionadas <- c(sustancias_seleccionadas, "Nafta")
                      if (!is.null(registro$`Consumo actual con Pegamento`) && !is.na(registro$`Consumo actual con Pegamento`) && registro$`Consumo actual con Pegamento` == "Si") sustancias_seleccionadas <- c(sustancias_seleccionadas, "Pegamento")
                      if (!is.null(registro$`Consumo actual con Psicofármacos`) && !is.na(registro$`Consumo actual con Psicofármacos`) && registro$`Consumo actual con Psicofármacos` == "Si") sustancias_seleccionadas <- c(sustancias_seleccionadas, "Psicofármacos")
                      if (!is.null(registro$`Consumo actual con Otras`) && !is.na(registro$`Consumo actual con Otras`) && registro$`Consumo actual con Otras` == "Si") sustancias_seleccionadas <- c(sustancias_seleccionadas, "Otra")
                      sustancias_seleccionadas
                    }
                  )
                )
              ),
              
              # Campo emergente para "Otra"
              column(
                width = 4,
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
                width = 3,
                selectInput(
                  "derivacion1",
                  label = tags$span("Derivación", style = "font-size: 12px;"),
                  choices = c("No informado", "", "Si", "No"),
                  selected = registro$Derivación
                )
              ),
              
              # Campo de derivado de
              column(
                width = 3,
                textInput(
                  inputId = "derivado_de1",
                  label = tags$span("Derivado de", style = "font-size: 12px;"),
                  placeholder = "",
                  value = registro$`Derivado de`
                )
              ),
              
              # Campo de número de tratamientos previos
              column(
                width = 3,
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
                width = 3,
                style = "margin-bottom: 10px;", 
                conditionalPanel(
                  condition = "input.num_tratamientos_previos1 > 0",  # Activo si el número de tratamientos es mayor que 0
                  textInput(
                    inputId = "lugar_ultimo_tratamiento",
                    label = tags$span("Lugar de Último Tratamiento", style="font-size: 12px;"),
                    value = ifelse(!is.null(registro$`Lugar de Último Tratamiento`) && 
                                     !is.na(registro$`Lugar de Último Tratamiento`), 
                                   registro$`Lugar de Último Tratamiento`, 
                                   "")  # Precarga el valor si está disponible
                  )
                )
              )
            )
          ),
          # Situación Socioeconómica, Jurídica y de Salud ------------------------------------------------------------
          wellPanel(
            style = "min-height: 520px; margin-top: 20px;",
            
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
                    inputId = "otra_situacion_habitacional_actual",
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
                    ""
                  ),
                  selected = registro$`Situación Laboral Actual`)
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
              )
            ),
            fluidRow(
              style = "margin-top: 20px;",
              column(
                width = 4,
                selectInput(
                  "situacion_judicial1",
                  label = tags$span("Situación Judicial", style = "font-size: 12px;"),
                  choices = c(
                    "Sin causas", 
                    "Con causa cerrada", 
                    "Con causa abierta", 
                    "Desconoce", 
                    "No informada", 
                    "Otra",
                    ""
                  ),
                  selected = registro$`Situación Judicial`
                )
              )
            )
          ),
          # Red de apoyo y referencia
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
                    "Referencia con seguimiento", 
                    "Referencia sin seguimiento", 
                    "No está referenciado", 
                    "No informada",
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
          ),
          # Informacion Adicional
          wellPanel(
            style = "min-height: 160px; margin-top: 20px;",
            
            h4("Información Adicional", style = "font-size: 15px; font-weight: bold;"),
            
            fluidRow(
              # Campo de Observaciones
              column(
                width = 12,  # Cambia el ancho según sea necesario
                textAreaInput(
                  inputId = "observaciones1",
                  label = tags$span("Observaciones", style = "font-size: 12px;"),
                  value = registro$Observaciones,
                  width = "100%",
                  height = "80px"  # Ajusta la altura según sea necesario
                )
              )
            )
          )
        ),
        
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("save_button", "Guardar cambios")
        ),
        easyClose = TRUE
      ))
      
      observeEvent(input$provincia1, {
        localidades <- localidades_por_provincia[[input$provincia1]]
        updateSelectInput(
          session,
          inputId = "localidad1",
          choices = localidades,
          selected = registro$Localidad  # Mantiene el valor seleccionado por defecto
        )
      })
      
      
    } else {
      showNotification("Por favor, seleccione un registro para modificar.", type = "warning")
    }
  }) 
}

shinyApp(ui, server)

