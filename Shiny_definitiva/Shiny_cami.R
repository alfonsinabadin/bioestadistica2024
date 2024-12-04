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
  # Reactivo para almacenar los resultados de la búsqueda
  search_results <- reactiveVal(NULL)
  registro_seleccionado <- reactiveVal(NULL)
  registro_reactivo <- reactiveVal()
  
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
    # Verificar que las fechas sean válidas
    if (isTruthy(input$fecha_nacimiento1) && isTruthy(input$fecha_registro1)) {
      tryCatch({
        # Calcular edad en años
        edad <- as.numeric(difftime(
          as.Date(input$fecha_registro1),
          as.Date(input$fecha_nacimiento1),
          units = "days"
        )) %/% 365
        
        # Asegurarse de que la edad esté dentro de un rango razonable
        if (!is.na(edad) && edad >= 1 && edad <= 99) {
          updateNumericInput(session, "edad1", value = edad)
        } else {
          updateNumericInput(session, "edad1", value = NA)
        }
      }, error = function(e) {
        # En caso de error, limpiar el campo edad
        updateNumericInput(session, "edad1", value = NA)
      })
    } else {
      # Si faltan fechas, limpiar el campo edad
      updateNumericInput(session, "edad1", value = NA)
    }
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
    # Si el valor está vacío o es NULL, no hay error
    if (is.null(value) || value == "") {
      return(NULL)
    }
    
    # Verificar que el valor tenga como máximo 2 dígitos
    if (nchar(value) > 2) {
      return(tags$span("La edad no puede tener más de 2 dígitos.", style = "font-size:10px;"))
    }
    
    # Verificar que el valor contenga solo números
    if (!grepl("^[0-9]+$", value)) {
      return(tags$span("Solo se admiten números.", style = "font-size:10px;"))
    }
    
    # Convertir el valor a numérico para validaciones adicionales
    edad_inicio_consumo1 <- as.numeric(value)
    
    # Verificar que el input$edad1 no sea NULL o NA antes de compararlo
    if (!is.null(input$edad1) && !is.na(input$edad1)) {
      if (edad_inicio_consumo1 > input$edad1) {
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
      if (grepl("[^a-zA-Z0-9 ]", value)) {
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
      if (!grepl("^[a-zA-Z0-9 ]+$", value)) {
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
             `Fecha de Nacimiento` = format(`Fecha de Nacimiento`, "%d/%m/%Y"),
             `Fecha de la Entrevista con Psicólogo` = format(`Fecha de la Entrevista con Psicólogo`, "%d/%m/%Y"),
             `Fecha de la Entrevista con Psiquiátra` = format(`Fecha de la Entrevista con Psiquiátra`, "%d/%m/%Y"),
             `Fecha de la Entrevista con Trabajador Social` = format(`Fecha de la Entrevista con Trabajador Social`, "%d/%m/%Y"))
    
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
                    selectizeInput(
                      "tipo_vinculo_contacto_11",
                      label = tags$span("Vínculo", style = "font-size: 12px;"),
                      choices = c("","Propio","Papá","Mamá", "Hermano","Hermana", "Hijo","Hija", "Amigo", "Amiga"),
                      selected = registro$`Tipo de Vínculo con el Contacto 1`,
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
                      choices = c("","Propio","Papá","Mamá", "Hermano","Hermana", "Hijo","Hija", "Amigo", "Amiga"),
                      selected = registro$`Tipo de Vínculo con el Contacto 2`,
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
                      choices = c("","Propio","Papá","Mamá", "Hermano","Hermana", 
                                  "Hijo","Hija", "Amigo", "Amiga"),
                      selected = registro$`Tipo de Vínculo con el Contacto 3`,
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
          wellPanel(
            style = "margin-top: 20px;", 
            
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
          ),
          # Situación Socioeconómica, Jurídica y de Salud ------------------------------------------------------------
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
              column(
                width = 6,
                conditionalPanel(
                  condition = "input.situacion_laboral_actual1 == 'Otra'",  # Verifica si selecciona "Otra"
                  textInput(
                    inputId = "otra_situacion_laboral_actual1",
                    label = tags$span("Especifique la situación habitacional", style = "font-size: 12px;"),
                    value = ifelse(
                      !is.null(registro$`Situación Laboral Actual - Otra`) && !is.na(registro$`Situación Laboral Actual - Otra`),
                      registro$`Situación Laboral Actual - Otra`,  
                      ""  
                    )
                  ) 
                )
              )
            ),
            fluidRow(
              column(
                width = 8,
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
                width = 4,
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
          ),
          # Red de apoyo y referencia
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
            style = "margin-top: 20px;",
            
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

