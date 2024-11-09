# Librerías
library(shiny)
library(plotly)
library(dplyr)

# Cargar datos
data("iris")

# Interfaz de usuario
ui <- fluidPage(
  
  titlePanel("Análisis Interactivo de Iris"),
  
  sidebarLayout(
    sidebarPanel(
      # Se puede agregar más elementos de filtro aquí si se desea
    ),
    
    mainPanel(
      fluidRow(
        column(width = 8, plotlyOutput("scatter_plot"), textOutput("scatter_text")),
        column(width = 4, plotlyOutput("pie_chart"), textOutput("pie_text"))
      )
    )
  )
)

# Lógica del servidor
server <- function(input, output, session) {
  
  # Valor reactivo para almacenar las especies visibles (no ocultas en la leyenda del gráfico de dona)
  visible_species <- reactiveVal(unique(iris$Species))
  
  # Detectar cambios en la visibilidad de los elementos de la leyenda del gráfico de dona
  observeEvent(event_data("plotly_relayout", source = "pie_chart"), {
    relayout_data <- event_data("plotly_relayout", source = "pie_chart")
    
    # Detectar qué especies están ocultas en la leyenda
    hidden_indices <- relayout_data$`hiddenlabels`  # Verificar el campo de "hiddenlabels"
    
    # Actualizar visible_species con las especies visibles
    if (!is.null(hidden_indices)) {
      visible_species(setdiff(unique(iris$Species), hidden_indices))
    } else {
      visible_species(unique(iris$Species))
    }
  })
  
  # Crear un `data frame` reactivo que filtre según las especies visibles
  filtered_data <- reactive({
    iris %>%
      mutate(Species = factor(Species, levels = unique(iris$Species))) %>%
      filter(Species %in% visible_species()) %>%
      complete(Species)
  })
  
  # Gráfico de dispersión
  output$scatter_plot <- renderPlotly({
    df <- filtered_data()
    
    scatter_plot <- ggplot(df, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
      geom_point(size = 3) +
      labs(title = "Dispersión de Sepal Length vs Sepal Width",
           x = "Sepal Length",
           y = "Sepal Width") +
      theme_minimal()
    
    ggplotly(scatter_plot)
  })
  
  # Texto dinámico debajo del gráfico de dispersión
  output$scatter_text <- renderText({
    paste("Especies visibles en dispersión:", paste(visible_species(), collapse = ", "))
  })
  
  # Gráfico de torta para la proporción de especies
  output$pie_chart <- renderPlotly({
    species_data <- iris %>%
      count(Species) %>%
      mutate(
        Species = factor(Species, levels = unique(iris$Species)),
        percent = n / sum(n) * 100
      )
    
    plot_ly(
      species_data,
      labels = ~Species,
      values = ~percent,
      type = 'pie',
      source = "pie_chart",
      textinfo = 'label+percent',
      hole = 0.4  # Dona
    ) %>%
      layout(title = "Proporción de Especies")
  })
  
  # Texto dinámico debajo del gráfico de torta
  output$pie_text <- renderText({
    paste("Especies visibles en el gráfico de dona:", paste(visible_species(), collapse = ", "))
  })
}

# Ejecutar la aplicación
shinyApp(ui, server)
