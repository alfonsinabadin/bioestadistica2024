# Librerías
library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(plotly)
library(rlang)

# Data prep
# Importar base

ui <- page_navbar(
  
  theme = bs_theme(
    bg = "#e1e1e1",
    fg = "black",
    primary = "#ec7e14",
    secondary = "#fbc91c",
    success = "#009E73",
    base_font = font_google("Montserrat")
  ),
  
  lang = "en",
  
  title = tags$span(
    style = "align-items: center;",
    tags$img(
      src = "pmlogo.png",
      width = "50px",
      height = "auto",
      style = "margin-bottom: 5px;"
    ),
    tags$span("Gestión de registros - Padre misericodioso", style = "font-size: 20px;color:#ec7e14;")
  ),
  
  nav_spacer(),
  
  nav_panel(
    tags$span("Nuevo registro", style = "font-size: 18px;"),
    class = "bslib-page-dashboard"
  ),
  
  nav_panel(
    tags$span("Consulta y modificación de registros", style = "font-size: 18px;"),
    class = "bslib-page-dashboard"
  ),
  nav_panel(
    tags$span("Tablero de visualización", style = "font-size: 18px;"),
    class = "bslib-page-dashboard"
  ),
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)