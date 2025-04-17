library(shiny)
library(shinybrowser)
library(bslib)
library(shinyWidgets)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(DT)
library(zip)
library(BiocManager)
options(repos = BiocManager::repositories())
library(EBImage)



ui <- fluidPage(
  detect(),
  title = "Fish Aging Structures Quiz",
  theme = bs_theme(bootswatch = "slate"),
  uiOutput("titleUI"),
  uiOutput("mainUI")
)

server <- function(input, output, session) {
  
  #Code for loading data tables
  source("functions/load_tables.R",local = TRUE)$value
  #Code for Main UI
  source("functions/mainUI.R",local = TRUE)$value
  #Code for Setup UI
  source("functions/setupUI.R",local = TRUE)$value
  #Code for setup options
  source("functions/setup_server.R",local = TRUE)$value
  #Code for Quiz UI
  source("functions/quizUI.R",local = TRUE)$value
  #Code for Quiz server
  source("functions/quiz_server.R",local = TRUE)$value
  #Code for Results UI
  source("functions/resultsUI.R",local = TRUE)$value
  #Code for Results server
  source("functions/results_server.R",local = TRUE)$value
}

# Run the application 
shinyApp(ui = ui, server = server)
