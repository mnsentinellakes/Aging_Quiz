#UI for results
output$resultsUI = renderUI({
  
  fluidRow(
    column(
      width = 7,
      uiOutput("resultsplotUI")
    ),
    column(
      width = 5,
      uiOutput("statsUI"),
      tags$br(),
      tags$br(),
      uiOutput("resultstableUI"),
      tags$br(),
      downloadBttn(
        outputId = "resultsdld",
        style = "simple",
        color = "success",
        size = "lg"
      )
    )
  )
})

#UI for plot
output$resultsplotUI = renderUI({
  
  wellPanel(
    style = "background: #f5f5f5; color: #333333; border: solid #333333 2px;",
    fluidRow(
      column(
        width = 3,
        pickerInput(
          inputId = "plotchoice",
          choices = c("Count","Percentage")
        )
      )
    ),
    plotlyOutput("resultsplot",height = paste0((imageheight() * 0.9),"px"))
  )
})

#Render plotly plot
output$resultsplot = renderPlotly({
  
  dataplots = finalplots()
  
  dataplot = dataplots[[input$plotchoice]]
  
  ggplotly(
    dataplot
  )
})

#Average Percent Error UI
output$statsUI = renderUI({
  cellstylehead = "border: solid 2px #3a3f44; padding: 7px; text-align: left; background: #52575c; color: #f5f5f5;"
  cellstylebody = "border: solid 2px #3a3f44; padding: 5px; text-align: center; background: #f5f5f5;"
  wellPanel(
    style = "background: #7a8288; color: #333333; border: solid #333333 2px;",
    tagList(
      HTML("<CENTER>"),
      tags$table(
        tags$tr(
          tags$td(
            style = cellstylehead,
            HTML("Quiz Length")
          ),
          tags$td(
            style = cellstylehead,
            HTML("Mean Residuals")
          ),
          tags$td(
            style = cellstylehead,
            HTML("Median Residuals")
          ),
          tags$td(
            style = cellstylehead,
            HTML("Mode Residuals")
          )
          
        ),
        tags$tr(
          tags$td(
            style = cellstylebody,
            HTML(nrow(quizresults()))
          ),
          tags$td(
            style = cellstylebody,
            HTML(meanresid())
          ),
          tags$td(
            style = cellstylebody,
            HTML(medianresid())
          ),
          tags$td(
            style = cellstylebody,
            HTML(moderesid())
          )
        )
      ),
      tags$br(),
      tags$table(
        tags$tr(
          tags$td(
            style = cellstylehead,
            HTML("Residuals < 0")
          ),
          tags$td(
            style = cellstylehead,
            HTML("Residuals = 0")
          ),
          tags$td(
            style = cellstylehead,
            HTML("Residuals > 0")
          ),
          tags$td(
            style = cellstylehead,
            HTML("Non-Zero Residuals above 25%")
          )
        ),
        tags$tr(
          tags$td(
            style = cellstylebody,
            HTML(paste0(residbelow(),"%"))
          ),
          tags$td(
            style = cellstylebody,
            HTML(paste0(residzero(),"%"))
          ),
          tags$td(
            style = cellstylebody,
            HTML(paste0(residabove(),"%"))
          ),
          tags$td(
            style = cellstylebody,
            HTML(nonzeroresid())
          )
        )
      ),
      tags$br(),
      tags$table(
        tags$tr(
          tags$td(
            style = cellstylehead,
            HTML("Index of Average Percent Error")
          ),
          tags$td(
            style = cellstylehead,
            HTML("Median Percent Error")
          ),
          tags$td(
            style = cellstylehead,
            HTML("Coefficient of Variation")
          )
        ),
        tags$tr(
          tags$td(
            style = cellstylebody,
            HTML(iAPEstore())
          ),
          tags$td(
            style = cellstylebody,
            HTML(medianpestore())
          ),
          tags$td(
            style = cellstylebody,
            HTML(cvstore())
          )
        )
      ),
      HTML("</CENTER>")
    )
  )
})

output$resultstableUI = renderUI({
  wellPanel(
    style = "background: #7a8288; color: #333333; border: solid #333333 2px;",
    DTOutput("resultstable")
  )
})