library(shiny)
library(shinyWidgets)
library(readxl)
library(writexl)
library(DT)

# Definir la ruta del archivo Excel
file_path <- "ADMISIÓN.xlsx"

# Leer los datos del archivo Excel
read_data <- function() {
  read_excel(file_path)
}

# Escribir los datos al archivo Excel
write_data <- function(data) {
  write_xlsx(data, file_path)
}

# Definir la interfaz de usuario
ui <- fluidPage(
  navbarPage("Admisiones - Comunidad Padre Misericordioso",
             tabPanel("Nuevo Registro",
                      fluidRow(
                        column(2, textInput("apellido", "Apellido")),
                        column(2, textInput("nombres", "Nombres"))
                      ),
                      fluidRow(
                        column(2, textInput("dni", "DNI")),
                        column(2, textInput("contacto", "Contacto"))
                      ),
                      fluidRow(
                        column(2, dateInput("fecha_nacimiento", "Fecha de Nacimiento", format = "dd/mm/yyyy")),
                        column(2, textInput("edad", "Edad"))
                      ),
                      fluidRow(
                        column(3, selectInput("nivel_educativo", "Nivel Educativo", selected = NULL ,choices = c(
                          "Ningún nivel educativo", "Primario incompleto", "Primario completo",
                          "Secundario incompleto", "Secundario Completo",
                          "Nivel Superior incompleto", "Nivel superior completo", "Otro"
                        ))),
                        column(4, selectizeInput("situacion_habitacional", "Situación Habitacional", choices = c(
                          "Casa/depto propio", "Casa/depto alquilado", "Casa/depto cedido",
                          "Internado en efector de salud", "Refugio", "Situación de calle",
                          "Pensión", "Institucionalizado", "En institución penal",
                          "En institución de SM", "En institución terapéutica"), selected = NULL,
                          options = list(create = TRUE,placeholder = 'Escriba la opción')))
                      ),
                      fluidRow(
                        column(4, selectInput("redes_de_apoyo", "Redes de Apoyo", choices = c(
                          "Ninguna", "Escasa", "Buena", "Escasa", "Nula", "Familia + Institucionales", 
                          "Familia", "Sin vínculos actualmente", "Institucionales", 
                          "Familia + Amistades"
                        ))),
                        column(4, selectInput("tiene_cud", "Tiene CUD", choices = c("Sí", "No"))),
                        column(4, selectInput("trabajo", "Trabajo", choices = c(
                          NA, "No tiene", "Estable", "Esporádico", "Secundaria incompleta"
                        )))
                      ),
                      fluidRow(
                        column(4, selectInput("ingresos_economicos", "Ingresos Económicos", choices = c(
                          NA, "Sin ingresos", "PNC nacional", "Salario informal + subsidio", "Salario informal"
                        ))),
                        column(4, selectInput("situacion_judicial", "Situación Judicial", choices = c(
                          NA, "Con causa cerrada", "Sin causas Judiciales", "Desconoce"
                        ))),
                        column(4, selectInput("referencia_aps", "Referencia APS", choices = c(
                          "Sólo clínica médica", "Referencia con seguimiento", NA, 
                          "Referencia sin seguimiento", "No está referenciado"
                        )))
                      ),
                      fluidRow(
                        column(4, selectInput("equipo_referencia", "Equipo de Referencia", choices = c(
                          "C.S. Juana Azurduy", "Centro de salud toba", NA, "Centro de vida: Las flores.", 
                          "Centro de vida: Las flores", "carrasco", "Centro de vida La lata", 
                          "Pquia Ma. Auxiliadora", "Buen Pastor CDD", "Centro de vida La Lata", 
                          "COLONIA OLIVEROS", "Reconstruyendo vidas.", "Eva Perón / Madres territoriales.", 
                          "Agudo Ávila", "Agudo avila", "Centro de salud Maradona", 
                          "Centro de salud Villa flores /Roldan", "Serv. penotenciario unidad 6", 
                          "Tto ambulatorio actualmente en el agudo.", "Preso en coronda.", "CRSM Agudo Avila", 
                          "Toxicologa Martinez", "Refugio Buen Pastor", "Madres territoriales", 
                          "Hospital Eva peron.", "Betina zubeldia", "Toxicologa Martinez.", 
                          "Centro de Vida Las flores", "Area consumos problematicos Municipalidad de Villa constitucion", 
                          "Centro de Vida La Lata", "Centro de Vida Del Valle .", 
                          "Se envio Propuesta de tratamiento en Centro de Dia Zeballos Via mail el 29/11para autorizacion del juez", 
                          "HEEP baigorria", "Hospital provincial", "TS Roxana Raimondi", 
                          "Defensora Laura Blacich", "APRECOD", "CARRASCO - APRECOD", "APRECOD - CERPJ", 
                          "Cristalería", "CENTRO DE SALUD EMAUS", "APRECOD/ AGUDO AVILA", 
                          "MADRES TERRITORIALES", "SALUD MENTAL", "APRECOD - Hospital Provincial", 
                          "CENTRO DE VIDA LUDUEÑA", "CDS CASIANO CASAS", "Buen pastor / CDV la lata", 
                          "HECA / APRECOD", "APRECOD?? chequear", "CENTRO DE SALUD N° 16 \"Pablo 6to\"", 
                          "Buen pastor, refugio y CDD", "CDV LAS FLORES", "SEDRONAR", 
                          "APRECOD - CASA PUEBLO mov evita", "APRECOD - HOSPITAL CENTENARIO", 
                          "Psiq. Colonia oliveros", "SAMCO SANDFORD", "REFUGIO BUEN PASTOR", 
                          "CENTRO DE VIDA LA LATA", "HECCA", "Clínica \"Es por acá\"", "CDV TIO ROLO", 
                          "HECA", "CDV La Lata", "Hospital Casilda", "APRECOD / AGUDO", 
                          "CENTRO DE VIDA \"LAS FLORES\"", "APRECOD - Madres Territoriales", 
                          "PSICÓLOGO MAXIMILIANO BLANCHE", "-", "APRECOD / A. AVILA", "CAI APRECOD", 
                          "(crista)", "CENTENARIO", "CPM hogar baigorria (eze)", "HEEP - APRECOD", 
                          "APRECOd", "Clinica ALEM", "ex paciente CDD. dejo hace 4m, se define hacer 1 entrevista sola", 
                          "Agudo Avila", "(ver observaciones)", "Oficio Judicial", 
                          "no sabe si de APRECOD (ver)", "colonia oliveros", "casa Bolten Roldan", 
                          "centro de vida la lata (prioridad)", "refugio buen pastor", "aprecod", 
                          "clinica alem - la posta - aprecod", "buen pastor", "policlinico san martin", 
                          "HEEP equipo matricial", "CERPJ", "aprecod (fue dsp)", "manos a la obra villa constitucion", 
                          "Aprecod (fue dsp)", "CDV la lata", "padre fabian", "cdv la lata", 
                          "APRECOD - Hogar de cristo", "colonia oliveros - hospital provincial", 
                          "fue a APRECOD", "ex paciente", "APRECOD - Samco Ibarlucea", "DEFENSORIA", 
                          "Iglesia Yapeyú Casilda", "Colonia Oliveros", "CAI - APRECOD (SAN JORGE)", 
                          "APRECOD - ALEM", "CAPS Juana Azurduy", "Grupo abrazando la vida VGG", 
                          "CDV San Martin Sur", "Hospital villa constitucion", "Espontáneo", 
                          "U. P 16 perez", "aprecod (ellos dicen que fueron, no hay registro de eso)", 
                          "CDV Ludueña", "APRECOD - HECA", "(dice que fue a aprecod)", 
                          "APRECOD - GAMEN", "hogar padre juan, zona norte", "CS San Martin A", 
                          "La posta - APRECOD", "APRECOD- AGUDO AVILA", "APRECOD/MADRES TERITORIALES", 
                          "SEDRONAR / HOGARES DE CRISTO", "APRECOD - Municipalidad de Funes"
                        ))),
                        column(4, selectInput("consumo_actual", "Consumo Actual", choices = c(
                          NA, "Alcohol", "Policonsumo", "Cocaína", "Marihuana", "Crack", 
                          "Cocaína y alcohol", "Crack y marihuana", "Psicofármacos", "Crack y alcohol", 
                          "Cocaína y psicofármacos", "Cocaína y marihuana", "nafta aspirada", 
                          "Marihuana y alcohol", "Otras"
                        )))
                      ),
                      fluidRow(
                        column(4, textInput("edad_inicio", "Edad de Inicio")),
                        column(4, selectInput("sustancia_inicio", "Sustancia de Inicio", choices = c(
                          NA, "Cocaína", "Pegamento", "Marihuana", "Alcohol", "Psicofármacos", "Otras"
                        ))),
                        column(4, selectInput("tratamientos_previos", "Tratamientos Previos", choices = c(
                          NA, "entre 1 y 2", "No", "más de 3"
                        )))
                      ),
                      fluidRow(
                        column(12, textAreaInput("observaciones", "Observaciones", rows = 3))
                      ),
                      actionButton("submit", "Registrar")
             ),
             tabPanel("Modificación de Registro",
                      fluidRow(
                        column(4, textInput("dni_modificar", "DNI del Registro")),
                        column(4, actionButton("buscar", "Buscar"))
                      ),
                      fluidRow(
                        column(6, textInput("apellido_modificar", "Apellido")),
                        column(6, textInput("nombres_modificar", "Nombres"))
                      ),
                      fluidRow(
                        column(4, textInput("dni_modificar_val", "DNI")),
                        column(4, dateInput("fecha_nacimiento_modificar", "Fecha de Nacimiento", format = "dd/mm/yyyy")),
                        column(4, textInput("edad_modificar", "Edad"))
                      ),
                      fluidRow(
                        column(4, textInput("contacto_modificar", "Contacto")),
                        column(4, selectInput("nivel_educativo_modificar", "Nivel Educativo", choices = c(
                          "Primaria incompleta", "Secundaria incompleta", "Nivel Superior incompleto", 
                          "Secundaria en curso", NA, "Secundaria completa", "Primaria completa", 
                          "Primaria en curso", "Ningún nivel educativo", "Nivel superior completo", 
                          "Nivel superior incompleto", "Nivel superior en curso"
                        ))),
                        column(4, selectizeInput("situacion_habitacional_modificar", "Situación Habitacional", choices = c(
                          "Internado en efector de salud", "Casa/depto", NA, "Refugio", "Situación de calle", 
                          "Pensión", "Institucionalizado", "En inst. penal", "En inst. de SM", 
                          "Casa/depto propio", "Casa/depto alquilado", "Casa/depto cedido", 
                          "En inst. terapéutica"
                        )))
                      ),
                      fluidRow(
                        column(4, selectInput("redes_de_apoyo_modificar", "Redes de Apoyo", choices = c(
                          NA, "Buena", "Escasa", "Nula", "Familia + Institucionales", 
                          "Familia", "Sin vínculos actualmente", "Institucionales", 
                          "Familia + Amistades"
                        ))),
                        column(4, selectInput("tiene_cud_modificar", "Tiene CUD", choices = c("Sí", "No"))),
                        column(4, selectInput("trabajo_modificar", "Trabajo", choices = c(
                          NA, "No tiene", "Estable", "Esporádico", "Secundaria incompleta"
                        )))
                      ),
                      fluidRow(
                        column(4, selectInput("ingresos_economicos_modificar", "Ingresos Económicos", choices = c(
                          NA, "Sin ingresos", "PNC nacional", "Salario informal + subsidio", "Salario informal"
                        ))),
                        column(4, selectInput("situacion_judicial_modificar", "Situación Judicial", choices = c(
                          NA, "Con causa cerrada", "Sin causas Judiciales", "Desconoce"
                        ))),
                        column(4, selectInput("referencia_aps_modificar", "Referencia APS", choices = c(
                          "Sólo clínica médica", "Referencia con seguimiento", NA, 
                          "Referencia sin seguimiento", "No está referenciado"
                        )))
                      ),
                      fluidRow(
                        column(4, selectInput("equipo_referencia_modificar", "Equipo de Referencia", choices = c(
                          "C.S. Juana Azurduy", "Centro de salud toba", NA, "Centro de vida: Las flores.", 
                          "Centro de vida: Las flores", "carrasco", "Centro de vida La lata", 
                          "Pquia Ma. Auxiliadora", "Buen Pastor CDD", "Centro de vida La Lata", 
                          "COLONIA OLIVEROS", "Reconstruyendo vidas.", "Eva Perón / Madres territoriales.", 
                          "Agudo Ávila", "Agudo avila", "Centro de salud Maradona", 
                          "Centro de salud Villa flores /Roldan", "Serv. penotenciario unidad 6", 
                          "Tto ambulatorio actualmente en el agudo.", "Preso en coronda.", "CRSM Agudo Avila", 
                          "Toxicologa Martinez", "Refugio Buen Pastor", "Madres territoriales", 
                          "Hospital Eva peron.", "Betina zubeldia", "Toxicologa Martinez.", 
                          "Centro de Vida Las flores", "Area consumos problematicos Municipalidad de Villa constitucion", 
                          "Centro de Vida La Lata", "Centro de Vida Del Valle .", 
                          "Se envio Propuesta de tratamiento en Centro de Dia Zeballos Via mail el 29/11para autorizacion del juez", 
                          "HEEP baigorria", "Hospital provincial", "TS Roxana Raimondi", 
                          "Defensora Laura Blacich", "APRECOD", "CARRASCO - APRECOD", "APRECOD - CERPJ", 
                          "Cristalería", "CENTRO DE SALUD EMAUS", "APRECOD/ AGUDO AVILA", 
                          "MADRES TERRITORIALES", "SALUD MENTAL", "APRECOD - Hospital Provincial", 
                          "CENTRO DE VIDA LUDUEÑA", "CDS CASIANO CASAS", "Buen pastor / CDV la lata", 
                          "HECA / APRECOD", "APRECOD?? chequear", "CENTRO DE SALUD N° 16 \"Pablo 6to\"", 
                          "Buen pastor, refugio y CDD", "CDV LAS FLORES", "SEDRONAR", 
                          "APRECOD - CASA PUEBLO mov evita", "APRECOD - HOSPITAL CENTENARIO", 
                          "Psiq. Colonia oliveros", "SAMCO SANDFORD", "REFUGIO BUEN PASTOR", 
                          "CENTRO DE VIDA LA LATA", "HECCA", "Clínica \"Es por acá\"", "CDV TIO ROLO", 
                          "HECA", "CDV La Lata", "Hospital Casilda", "APRECOD / AGUDO", 
                          "CENTRO DE VIDA \"LAS FLORES\"", "APRECOD - Madres Territoriales", 
                          "PSICÓLOGO MAXIMILIANO BLANCHE", "-", "APRECOD / A. AVILA", "CAI APRECOD", 
                          "(crista)", "CENTENARIO", "CPM hogar baigorria (eze)", "HEEP - APRECOD", 
                          "APRECOd", "Clinica ALEM", "ex paciente CDD. dejo hace 4m, se define hacer 1 entrevista sola", 
                          "Agudo Avila", "(ver observaciones)", "Oficio Judicial", 
                          "no sabe si de APRECOD (ver)", "colonia oliveros", "casa Bolten Roldan", 
                          "centro de vida la lata (prioridad)", "refugio buen pastor", "aprecod", 
                          "clinica alem - la posta - aprecod", "buen pastor", "policlinico san martin", 
                          "HEEP equipo matricial", "CERPJ", "aprecod (fue dsp)", "manos a la obra villa constitucion", 
                          "Aprecod (fue dsp)", "CDV la lata", "padre fabian", "cdv la lata", 
                          "APRECOD - Hogar de cristo", "colonia oliveros - hospital provincial", 
                          "fue a APRECOD", "ex paciente", "APRECOD - Samco Ibarlucea", "DEFENSORIA", 
                          "Iglesia Yapeyú Casilda", "Colonia Oliveros", "CAI - APRECOD (SAN JORGE)", 
                          "APRECOD - ALEM", "CAPS Juana Azurduy", "Grupo abrazando la vida VGG", 
                          "CDV San Martin Sur", "Hospital villa constitucion", "Espontáneo", 
                          "U. P 16 perez", "aprecod (ellos dicen que fueron, no hay registro de eso)", 
                          "CDV Ludueña", "APRECOD - HECA", "(dice que fue a aprecod)", 
                          "APRECOD - GAMEN", "hogar padre juan, zona norte", "CS San Martin A", 
                          "La posta - APRECOD", "APRECOD- AGUDO AVILA", "APRECOD/MADRES TERITORIALES", 
                          "SEDRONAR / HOGARES DE CRISTO", "APRECOD - Municipalidad de Funes"
                        ))),
                        column(4, selectInput("consumo_actual_modificar", "Consumo Actual", choices = c(
                          NA, "Alcohol", "Policonsumo", "Cocaína", "Marihuana", "Crack", 
                          "Cocaína y alcohol", "Crack y marihuana", "Psicofármacos", "Crack y alcohol", 
                          "Cocaína y psicofármacos", "Cocaína y marihuana", "nafta aspirada", 
                          "Marihuana y alcohol", "Otras"
                        )))
                      ),
                      fluidRow(
                        column(4, textInput("edad_inicio_modificar", "Edad de Inicio")),
                        column(4, selectInput("sustancia_inicio_modificar", "Sustancia de Inicio", choices = c(
                          NA, "Cocaína", "Pegamento", "Marihuana", "Alcohol", "Psicofármacos", "Otras"
                        ))),
                        column(4, selectInput("tratamientos_previos_modificar", "Tratamientos Previos", choices = c(
                          NA, "entre 1 y 2", "No", "más de 3"
                        )))
                      ),
                      fluidRow(
                        column(12, textAreaInput("observaciones_modificar", "Observaciones", rows = 3))
                      ),
                      actionButton("modificar", "Modificar")
             ),
             tabPanel("Visualización de Datos",
                      dataTableOutput("tabla")
             )
  )
)

# Definir el servidor
server <- function(input, output, session) {
  
  # Leer los datos del archivo Excel
  data <- reactiveVal(read_data())
  
  # Registrar nuevo registro
  observeEvent(input$submit, {
    new_data <- data()
    nuevo_registro <- data.frame(
      Apellido_nombres = paste(input$apellido, input$nombres, sep = ", "),
      DNI = input$dni,
      Entrevista_psicológica_fecha = NA,
      Entrevista_psicológica_asistencia = NA,
      Entrevista_psiquiátrica_fecha = NA,
      Entrevista_psiquiátrica_asistencia = NA,
      Entrevista_ts_fecha = NA,
      Entrevista_ts_asistencia = NA,
      Tratamiento = NA,
      Contacto = input$contacto,
      Fecha_de_nacimiento = as.character(input$fecha_nacimiento),
      Edad = as.character(input$edad),
      Nivel_educativo = input$nivel_educativo,
      Situacion_habitacional = input$situacion_habitacional,
      Redes_de_apoyo = input$redes_de_apoyo,
      Tiene_CUD = input$tiene_cud,
      Trabajo = input$trabajo,
      Ingresos_económicos = input$ingresos_economicos,
      Situación_Judicial = input$situacion_judicial,
      Referencia_APS = input$referencia_aps,
      Equipo_referencia = input$equipo_referencia,
      Consumo_actual = input$consumo_actual,
      Edad_de_inicio = as.character(input$edad_inicio),
      Sustancia_de_inicio = input$sustancia_inicio,
      Tratamientos_previos = input$tratamientos_previos,
      Observaciones = input$observaciones
    )
    new_data <- rbind(new_data, nuevo_registro)
    write_data(new_data)
    data(new_data)
    showNotification("Registro añadido con éxito", type = "message")
  })
  
  # Buscar registro para modificación
  observeEvent(input$buscar, {
    registro <- data()[data()$DNI == input$dni_modificar, ]
    if(nrow(registro) > 0) {
      updateTextInput(session, "apellido_modificar", value = sub(",.*", "", registro$Apellido_nombres))
      updateTextInput(session, "nombres_modificar", value = sub(".*, ", "", registro$Apellido_nombres))
      updateTextInput(session, "dni_modificar_val", value = registro$DNI)
      updateDateInput(session, "fecha_nacimiento_modificar", value = as.Date(registro$Fecha_de_nacimiento, format = "%d/%m/%Y"))
      updateTextInput(session, "edad_modificar", value = registro$Edad)
      updateTextInput(session, "contacto_modificar", value = registro$Contacto)
      updateSelectInput(session, "nivel_educativo_modificar", selected = registro$Nivel_educativo)
      updateSelectizeInput(session, "situacion_habitacional_modificar", selected = registro$Situacion_habitacional)
      updateSelectInput(session, "redes_de_apoyo_modificar", selected = registro$Redes_de_apoyo)
      updateSelectInput(session, "tiene_cud_modificar", selected = registro$Tiene_CUD)
      updateSelectInput(session, "trabajo_modificar", selected = registro$Trabajo)
      updateSelectInput(session, "ingresos_economicos_modificar", selected = registro$Ingresos_economicos)
      updateSelectInput(session, "situacion_judicial_modificar", selected = registro$Situación_Judicial)
      updateSelectInput(session, "referencia_aps_modificar", selected = registro$Referencia_APS)
      updateSelectInput(session, "equipo_referencia_modificar", selected = registro$Equipo_referencia)
      updateSelectInput(session, "consumo_actual_modificar", selected = registro$Consumo_actual)
      updateTextInput(session, "edad_inicio_modificar", value = registro$Edad_de_inicio)
      updateSelectInput(session, "sustancia_inicio_modificar", selected = registro$Sustancia_de_inicio)
      updateSelectInput(session, "tratamientos_previos_modificar", selected = registro$Tratamientos_previos)
      updateTextAreaInput(session, "observaciones_modificar", value = registro$Observaciones)
    } else {
      showNotification("DNI no encontrado", type = "error")
    }
  })
  
  # Modificar registro existente
  observeEvent(input$modificar, {
    new_data <- data()
    index <- which(new_data$DNI == input$dni_modificar_val)
    if(length(index) > 0) {
      new_data[index, ] <- list(
        Apellido_nombres = paste(input$apellido_modificar, input$nombres_modificar, sep = ", "),
        DNI = input$dni_modificar_val,
        Entrevista_psicológica_fecha = NA,
        Entrevista_psicológica_asistencia = NA,
        Entrevista_psiquiátrica_fecha = NA,
        Entrevista_psiquiátrica_asistencia = NA,
        Entrevista_ts_fecha = NA,
        Entrevista_ts_asistencia = NA,
        Tratamiento = NA,
        Contacto = input$contacto_modificar,
        Fecha_de_nacimiento = as.character(input$fecha_nacimiento_modificar),
        Edad = as.character(input$edad_modificar),
        Nivel_educativo = input$nivel_educativo_modificar,
        Situacion_habitacional = input$situacion_habitacional_modificar,
        Redes_de_apoyo = input$redes_de_apoyo_modificar,
        Tiene_CUD = input$tiene_cud_modificar,
        Trabajo = input$trabajo_modificar,
        Ingresos_economicos = input$ingresos_economicos_modificar,
        Situación_Judicial = input$situacion_judicial_modificar,
        Referencia_APS = input$referencia_aps_modificar,
        Equipo_referencia = input$equipo_referencia_modificar,
        Consumo_actual = input$consumo_actual_modificar,
        Edad_de_inicio = as.character(input$edad_inicio_modificar),
        Sustancia_de_inicio = input$sustancia_inicio_modificar,
        Tratamientos_previos = input$tratamientos_previos_modificar,
        Observaciones = input$observaciones_modificar
      )
      write_data(new_data)
      data(new_data)
      showNotification("Registro modificado con éxito", type = "message")
    } else {
      showNotification("ID no encontrado", type = "error")
    }
  })
  
  # Mostrar tabla de datos
  output$tabla <- renderDataTable({
    datatable(data())
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)


