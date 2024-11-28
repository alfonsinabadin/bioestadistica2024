# Observador para guardar los cambios
observeEvent(input$save_button, {
  # Verifica si hay un registro seleccionado
  selected <- input$search_results_rows_selected
  
  if (length(selected) > 0) {
    # Extrae los datos actuales de la base de datos reactiva
    data_modificada <- data()
    
    # Actualiza los valores según los inputs del modal
    data_modificada[selected, `Estado de la Entrevista con Psicológo`] <- input$estado_psicologo1
    data_modificada[selected, `Fecha de la Entrevista con Psicológo`] <- as.character(input$fecha_entrevista_psicologo1)
    data_modificada[selected, `Estado de la Entrevista con Psiquiátra`] <- input$estado_psiquiatra1
    data_modificada[selected, `Fecha de la Entrevista con Psiquiátra`] <- as.character(input$fecha_entrevista_psiquiatra1)
    data_modificada[selected, `Estado de entrevista con Trabajador Social`] <- input$estado_ts1
    data_modificada[selected, `Fecha de la Entrevista con Trabajador Social`] <- as.character(input$fecha_entrevista_ts1)
    data_modificada[selected, `Tratamiento Elegido`] <- input$tratamiento_elegido1
    data_modificada[selected, `Recuerda DNI`] <- input$recuerda_dni1
    data_modificada[selected, DNI] <- as.character(input$dni1)
    data_modificada[selected, `Apellido, Nombre`] <- input$apellido_nombre1
    data_modificada[selected, `Fecha de Nacimiento`] <- as.character(input$fecha_nacimiento1)
    data_modificada[selected, `Edad del registro`] <- as.character(input$edad1)
    data_modificada[selected, `Sexo biológico`] <- input$sexo_biologico1
    data_modificada[selected, Género] <- input$genero1
    data_modificada[selected, Provincia] <- input$provincia1
    data_modificada[selected, Localidad] <- input$localidad1
    data_modificada[selected, Barrio] <- input$barrio1
    data_modificada[selected, `Teléfono de Contacto 1`] <- as.character(input$telefono_contacto_11)
    data_modificada[selected, `Tipo de Vínculo con el Contacto 1`] <- input$tipo_vinculo_contacto_11
    data_modificada[selected, `Nombre del Contacto 1`] <- input$nombre_contacto_11
    data_modificada[selected, `Teléfono de Contacto 2`] <- as.character(input$telefono_contacto_21)
    data_modificada[selected, `Tipo de Vínculo con el Contacto 2`] <- input$tipo_vinculo_contacto_21
    data_modificada[selected, `Nombre del Contacto 2`] <- input$nombre_contacto_21
    data_modificada[selected, `Teléfono de Contacto 3`] <- as.character(input$telefono_contacto_31)
    data_modificada[selected, `Tipo de Vínculo con el Contacto 3`] <- input$tipo_vinculo_contacto_31
    data_modificada[selected, `Nombre del Contacto 3`] <- input$nombre_contacto_31
    data_modificada[selected, `Edad de Inicio de Cosumo`] <- as.character(input$edad_inicio_consumo1)
    data_modificada[selected, `Sustancia de inicio`] <- input$sustancia_inicio_consumo1
    data_modificada[selected, `Inicio con Otras - Descripción`] <- input$otra_sustancia1
    data_modificada[selected, `¿Consume actualmente?`] <- input$persona_consume1
    data_modificada[selected, `Sustancias de Consumo Actual`] <- paste(input$sustancias_consumo_actual1, collapse = ", ")
    data_modificada[selected, `Consumo actual con Otras - Descripción`] <- input$otra_sustancia_actual1
    data_modificada[selected, Derivación] <- input$derivacion1
    data_modificada[selected, `Derivado de`] <- input$derivado_de1
    data_modificada[selected, `Número de Tratamientos Previos`] <- as.character(input$num_tratamientos_previos1)
    data_modificada[selected, `Lugar de Último Tratamiento`] <- input$lugar_ultimo_tratamiento
    data_modificada[selected, `Nivel Máximo Educativo Alcanzado`] <- input$nivel_educativo_max1
    data_modificada[selected, CUD] <- input$cud1
    data_modificada[selected, `Situación Habitacional Actual`] <- input$situacion_habitacional_actual1
    data_modificada[selected, `Situación Habitacional Actual - Otra`] <- input$otra_situacion_habitacional_actual
    data_modificada[selected, `Situación Laboral Actual`] <- input$situacion_laboral_actual1
    data_modificada[selected, `Ingresos Económicos`] <- paste(input$ingreso_economico1, collapse = ", ")
    data_modificada[selected, `Situación Judicial`] <- input$situacion_judicial1
    data_modificada[selected, `Redes de Apoyo`] <- paste(input$redes_apoyo1, collapse = ", ")
    data_modificada[selected, `Referencia a APS`] <- input$referencia_aps1
    data_modificada[selected, `Equipo de Referencia`] <- input$equipo_referencia1
    data_modificada[selected, Observaciones] <- input$observaciones1
    
    # Actualiza la base de datos reactiva
    data(data_modificada)
    
    # Cierra el modal
    removeModal()
    
    # Notificación de éxito
    showNotification("El registro se guardó correctamente.", type = "message")
  } else {
    showNotification("No se pudo guardar el registro. Seleccione un registro válido.", type = "error")
  }
})





# Vale
# Observador para guardar los cambios
observeEvent(input$save_button, {
  
  # Verifica si hay un registro seleccionado
  selected <- input$search_results_rows_selected
  
  if (length(selected) > 0) {
    
    # Extrae los datos actuales de la base de datos reactiva
    data_modificada <- base()
    
    # Actualiza los valores según los inputs del modal
    data_modificada[selected, "Estado de la Entrevista con Psicológo"] <- input$estado_psicologo1
    data_modificada[selected, "Fecha de la Entrevista con Psicológo" ] <- as.Date(input$fecha_entrevista_psicologo1, format="%Y-%m-%d")
    data_modificada[selected, "Estado de entrevista con Psiquiátra"] <- input$estado_psiquiatra1
    data_modificada[selected, "Fecha de la Entrevista con Psiquiátra"] <- as.Date(input$fecha_entrevista_psiquiatra1, format="%Y-%m-%d")
    data_modificada[selected, "Estado de entrevista con Trabajador Social"] <- input$estado_ts1
    data_modificada[selected, "Fecha de la Entrevista con Trabajador Social"] <- as.Date(input$fecha_entrevista_ts1, format="%Y-%m-%d")
    data_modificada[selected, "Tratamiento Elegido"] <- input$tratamiento_elegido1
    data_modificada[selected, "Recuerda DNI" ] <- input$recuerda_dni1
    #data_modificada[selected, "DNI"] <- as.character(input$dni1)
    data_modificada[selected, "Apellido, Nombre"] <- input$apellido_nombre1
    #data_modificada[selected, "Fecha de Nacimiento"] <- as.character(input$fecha_nacimiento1)
    #data_modificada[selected, "Edad del registro"] <- as.character(input$edad1)
    data_modificada[selected, "Sexo biológico"] <- input$sexo_biologico1
    data_modificada[selected, "Género"] <- input$genero1
    data_modificada[selected, "Provincia"] <- input$provincia1
    data_modificada[selected, "Localidad"] <- input$localidad1
    data_modificada[selected, "Barrio"] <- input$barrio1
    #data_modificada[selected, "Teléfono de Contacto 1"] <- as.character(input$telefono_contacto_11)
    data_modificada[selected, "Tipo de Vínculo con el Contacto 1"] <- input$tipo_vinculo_contacto_11
    data_modificada[selected, "Nombre del Contacto 1"] <- input$nombre_contacto_11
    #data_modificada[selected, "Teléfono de Contacto 2"] <- as.character(input$telefono_contacto_21)
    data_modificada[selected, "Tipo de Vínculo con el Contacto 2"] <- input$tipo_vinculo_contacto_21
    data_modificada[selected, "Nombre del Contacto 2"] <- input$nombre_contacto_21
    #data_modificada[selected, "Teléfono de Contacto 3"] <- as.character(input$telefono_contacto_31)
    data_modificada[selected, "Tipo de Vínculo con el Contacto 3"] <- input$tipo_vinculo_contacto_31
    data_modificada[selected, "Nombre del Contacto 3"] <- input$nombre_contacto_31
    #data_modificada[selected, "Edad de Inicio de Cosumo"] <- as.character(input$edad_inicio_consumo1)
    data_modificada[selected, "Sustancia de inicio"] <- input$sustancia_inicio_consumo1
    data_modificada[selected, "Inicio con Otras - Descripción"] <- input$otra_sustancia1
    data_modificada[selected, "¿Consume actualmente?"] <- input$persona_consume1
    #data_modificada[selected, `Sustancias de Consumo Actual`] <- paste(input$sustancias_consumo_actual1, collapse = ", ")
    data_modificada[selected, "Consumo actual con Otras - Descripción"] <- input$otra_sustancia_actual1
    data_modificada[selected, "Derivación"] <- input$derivacion1
    data_modificada[selected, "Derivado de"] <- input$derivado_de1
    #data_modificada[selected, "Número de Tratamientos Previos"] <- as.character(input$num_tratamientos_previos1)
    data_modificada[selected, "Lugar de Último Tratamiento"] <- input$lugar_ultimo_tratamiento
    data_modificada[selected, "Nivel Máximo Educativo Alcanzado"] <- input$nivel_educativo_max1
    data_modificada[selected, "CUD"] <- input$cud1
    data_modificada[selected, "Situación Habitacional Actual"] <- input$situacion_habitacional_actual1
    data_modificada[selected, "Situación Habitacional Actual - Otra"] <- input$otra_situacion_habitacional_actual
    data_modificada[selected, "Situación Laboral Actual"] <- input$situacion_laboral_actual1
    data_modificada[selected, "Ingresos Económicos"] <- paste(input$ingreso_economico1, collapse = ", ")
    data_modificada[selected, "Situación Judicial"] <- input$situacion_judicial1
    data_modificada[selected, "Redes de Apoyo"] <- paste(input$redes_apoyo1, collapse = ", ")
    data_modificada[selected, "Referencia a APS"] <- input$referencia_aps1
    data_modificada[selected, "Equipo de Referencia"] <- input$equipo_referencia1
    data_modificada[selected, "Observaciones"] <- input$observaciones1
    
    # Actualiza la base de datos reactiva
    data(data_modificada)
    
    # Cierra el modal
    removeModal()
    
    # Notificación de éxito
    showNotification("El registro se guardó correctamente.", type = "message")
  } else {
    showNotification("No se pudo guardar el registro. Seleccione un registro válido.", type = "error")
  }
})