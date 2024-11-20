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
      ),
      
      # Buscador de DNI o Nombre
      textInput("search_input", "Buscar por DNI o Nombre", ""),
      actionButton("search_button", "Buscar"),
      
      conditionalPanel(
        condition = "output.showTable == true",
        
        # Tabla de resultados de búsqueda
        dataTableOutput("search_results"),
        
        # Botones en la esquina inferior derecha de la tabla
        tags$div(
          style = "margin-top: 15px; display: flex; gap: 10px; justify-content: flex-end;",
          actionButton("cancel_button", "Cancelar búsqueda", width = '150px'),
          actionButton("modify_button", "Modificar registro", width = '150px')
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
      search_results(resultados)
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
    
    datatable(
      resultados_tabla,
      selection = "single",  # Permite seleccionar una fila
      rownames = FALSE,
      options = list(
        paging = FALSE,
        scrollX = TRUE,
        scrollY = "250px",
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
        size = "xl",
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
                  inputId = "Estado_de_Entrevista_Psicologo",
                  label = tags$span("Estado", style = "font-size: 12px;"),
                  choices = list("Presente", "Ausente", "Pendiente", "No necesaria", "No asignada", ""),
                  selected = registro$`Estado de la Entrevista con Psicológo`
                ),
                dateInput(
                  inputId = "Fecha_de_Entrevista_Psicologo",
                  label = tags$span("Fecha de la entrevista", style = "font-size: 12px; white-space: nowrap;"),
                  value = as.Date(registro$`Fecha de la Entrevista con Psicológo`, format = "%Y-%m-%d"),
                  format = "dd/mm/yyyy"
                )
              ),
              
              # Columna para Entrevista con Psiquiatra
              div(
                style = "flex: 1; min-width: 300px;",
                h4("Entrevista con Psiquiatra", style = "font-size: 15px; font-weight: bold; margin-bottom: 10px;"),
                selectInput(
                  inputId = "Estado_de_Entrevista_Psiquiatra",
                  label = tags$span("Estado", style = "font-size: 12px;"),
                  choices = list("Presente", "Ausente", "Pendiente", "No necesaria", "No asignada", ""),
                  selected = registro$`Estado de la Entrevista con Psiquiátra`
                ),
                dateInput(
                  inputId = "Fecha_de_Entrevista_Psiquiatra",
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
                  inputId = "Estado_de_Entrevista_Trabajador_Social",
                  label = tags$span("Estado", style = "font-size: 12px;"),
                  choices = list("Presente", "Ausente", "Pendiente", "No necesaria", "No asignada", ""),
                  selected = registro$`Estado de entrevista con Trabajador Social`
                ),
                dateInput(
                  inputId = "Fecha_de_Entrevista_Trabajador_Social",
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
                  inputId = "Tratamiento_Elegido",
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
                style = "min-height: 400px;", 
                h4("Datos del Registro", style = "font-size: 15px; font-weight: bold; margin-bottom: 10px;"),
                textInput(inputId = "id_registro", 
                          label = "ID de Registro", 
                          value = registro$`ID de registro`),
                dateInput(
                  inputId = "Fecha_de_Registro",
                  label = tags$span("Fecha de Registro", style = "font-size: 12px; white-space: nowrap;"),
                  value = as.Date(registro$`Fecha de registro`, format = "%Y-%m-%d"),
                  format = "dd/mm/yyyy"
                ),
                h4("Historial de Registro", style = "font-size: 15px; font-weight: bold; margin-bottom: 10px;"),
                textInput(inputId = "id_persona", 
                          label = "ID de la persona", 
                          value = registro$`ID de la persona`)
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
                    width = 4,
                    selectInput(
                      inputId = "recuerda_dni",
                      label = tags$span("¿Recuerda el DNI?", style = "font-size: 12px;"),
                      choices = c("","Si", "No", "No tiene" = "S/D"),
                      selected = registro$`Recuerda DNI`
                    )
                  ),
                  
                  # Campo DNI
                  column(
                    width = 3,
                    numericInput(
                      inputId = "dni",
                      label = tags$span("DNI", style = "font-size: 12px;"),
                      value = registro$DNI
                    )
                  ),
                  
                  # Apellido y Nombre
                  column(
                    width = 5,
                    textInput(
                      inputId = "apellido_nombre",
                      label = tags$span("Apellido, Nombre (Apodo)", style = "font-size: 12px;"),
                      value = registro$`Apellido, Nombre`
                    )
                  )
                ),
                
                fluidRow(
                  # Fecha de nacimiento
                  column(
                    width = 4,
                    dateInput(
                      inputId = "fecha_nacimiento",
                      label = tags$span("Fecha de nacimiento", style = "font-size: 12px;"),
                      value = registro$`Fecha de Nacimiento`,
                      format = "dd/mm/yyyy",  
                      min = Sys.Date() - years(100),  # Limitar a 110 años atrás
                      max = Sys.Date()  # Limitar a la fecha de hoy
                    )
                  ),
                  
                  # Edad
                  column(
                    width = 2,
                    numericInput(
                      "edad",
                      tags$span("Edad", style = "font-size:10px;"),
                      value = registro$`Edad del registro`
                    )
                  ),
                  
                  # Campo sexo biológico
                  column(
                    width = 3,
                    selectInput(
                      "sexo_biologico",
                      label = tags$span("Sexo biológico", style = "font-size: 12px;"),
                      choices = c("No informado","Femenino", "Masculino", ""), 
                      selected = registro$`Sexo biológico`
                    )
                  ),
                  
                  # Campo género
                  column(
                    width = 3,
                    selectInput(
                      "genero",
                      label = tags$span("Género", style = "font-size: 12px;"),
                      choices = c("No informado","Mujer", "Hombre", "Trans (feminidades)", "Trans (masculinidades)", "Otro", ""),  
                      selected = registro$Género
                    )
                  )
                ),
                
                fluidRow(
                  # Campo Provincia
                  column(
                    width = 4,
                    selectInput(
                      "provincia",
                      label = tags$span("Provincia de residencia", style = "font-size: 12px;"),
                      choices = provincias,  # Lista de provincias inicial
                      selected = registro$Provincia  # Valor seleccionado por defecto
                    )
                  ),
                  
                  # Campo localidad
                  column(
                    width = 4,
                    selectInput(
                      inputId = "localidad",
                      label = tags$span("Localidad", style = "font-size: 12px;"),
                      choices = localidades_por_provincia,  
                      selected = registro$Localidad  # Valor seleccionado por defecto
                    )
                  ),
                  
                  # Campo barrio
                  column(
                    width = 4,
                    textInput(
                      inputId = "barrio",
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
                      inputId = "telefono_contacto_1",
                      label = tags$span("Teléfono", style = "font-size: 12px;"),
                      value = registro$`Teléfono de Contacto 1`
                    )
                  ),
                  
                  # Contacto 1 - Tipo de vínculo
                  column(
                    width = 4,
                    selectInput(
                      "tipo_vinculo_contacto_1",
                      label = tags$span("Vínculo", style = "font-size: 12px;"),
                      choices = c("","Propio","Papá","Mamá", "Hermano","Hermana", "Hijo","Hija", "Amigo", "Amiga"),
                      selected = registro$`Tipo de Vínculo con el Contacto 1`
                    )
                  ),
                  
                  # Contacto 1 - Nombre
                  column(
                    width = 4,
                    textInput(
                      inputId = "nombre1",
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
                      inputId = "telefono_contacto_2",
                      label = tags$span("Teléfono", style = "font-size: 12px;"),
                      value = registro$`Teléfono de Contacto 2`
                    )
                  ),
                  
                  # Contacto 2 - Tipo de vínculo
                  column(
                    width = 4,
                    selectInput(
                      "tipo_vinculo_contacto_2",
                      label = tags$span("Vínculo", style = "font-size: 12px;"),
                      choices = c("","Propio","Papá","Mamá", "Hermano","Hermana", "Hijo","Hija", "Amigo", "Amiga"),
                      selected = registro$`Tipo de Vínculo con el Contacto 2`
                    )
                  ),
                  
                  # Contacto 2 - Nombre
                  column(
                    width = 4,
                    textInput(
                      inputId = "nombre2",
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
                      inputId = "telefono_contacto_3",
                      label = tags$span("Teléfono", style = "font-size: 12px;"),
                      value = registro$`Teléfono de Contacto 3`
                    )
                  ),
                  
                  # Contacto 3 - Tipo de vínculo
                  column(
                    width = 4,
                    selectInput(
                      "tipo_vinculo_contacto_3",
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
                      inputId = "nombre3",
                      label = tags$span("Nombre", style = "font-size: 12px;"),
                      value = registro$`Nombre del Contacto 3`
                    )
                  )
                )
              )
            )
          ),
          # Datos del consumo -------------------------------------------------
          wellPanel(
            style = "height: auto; min-height: 400px; margin-top: 20px;", 
            # Inicio del consumo
            fluidRow(
              h4("Inicio del consumo", style = "font-size: 15px; font-weight: bold;"),
              column(
                width = 6,
                numericInput(
                  inputId = "edad_inicio_consumo",
                  label = tags$span("Edad de inicio de consumo", style = "font-size: 12px;"),
                  value = registro$`Edad de Inicio de Cosumo`
                )
              ),
              column(
                width = 6,
                selectInput(
                  "sustancia_inicio_consumo",
                  label = tags$span("Sustancia de Inicio de Consumo", style = "font-size: 12px;"),
                  choices = c("NoAlcohol", "Cocaína", "Crack", "Marihuana", 
                              "Nafta aspirada", "Pegamento", "Psicofármacos", "Otra",""),
                  selected = registro$`Sustancia de inicio`
                )
              )
            ),
            
            # Consumo actual
            fluidRow(
              style = "margin-top: 20px;", # Espacio entre filas
              h4("Consumo actual", style = "font-size: 15px; font-weight: bold;"),
              column(
                width = 6,
                selectInput(
                  "persona_consume",
                  label = tags$span("¿Consume actualmente?", style = "font-size: 12px;"),
                  choices = c("No informado","","Si","No"),
                  selected = registro$`¿Consume actualmente?`
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

      
    } else {
      showNotification("Por favor, seleccione un registro para modificar.", type = "warning")
    }
  })
}

shinyApp(ui, server)
