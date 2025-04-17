#UI for Setup Panel
output$setupUI = renderUI({
  wellPanel(
    style = "background: #f5f5f5; color: #333333;",
    tagList(
      tags$h3("Setup"),
      fluidRow(
        column(
          width = 6,
          wellPanel(
            style = "background: #f5f5f5; color: #333333; border: solid #333333 2px;",
            fluidRow(
              column(
                width = 6,
                textInput(
                  inputId = "firstname",
                  label = "First Name"
                ),
                textInput(
                  inputId = "organization",
                  label = "Organization"
                ),
                pickerInput(
                  inputId = "species",
                  label = "Species",
                  multiple = TRUE,
                  choices = speciesoptions()$Name,
                  
                  options = list(style = "color:#f5f5f5;")
                ),
                pickerInput(
                  inputId = "structures",
                  label = "Structures",
                  multiple = TRUE,
                  choices = structureoptions(),
                  options = list(style = "color:#f5f5f5;")
                )
              ),
              column(
                width = 6,
                textInput(
                  inputId = "lastname",
                  label = "Last Name"
                ),
                pickerInput(
                  inputId = "quizlength",
                  label = "Quiz Length",
                  choices = seq(from = 5,to = 100,by = 5),
                  options = list(style = "color:#f5f5f5;")
                ),
                pickerInput(
                  inputId = "locations",
                  label = "Locations",
                  multiple = TRUE,
                  choices = locationoptions(),
                  options = list(style = "color:#f5f5f5;")
                ),
                pickerInput(
                  inputId = "techniques",
                  label = "Techniques",
                  multiple = TRUE,
                  choices = techniqueoptions(),
                  options = list(style = "color:#f5f5f5;")
                )
              )
            )
          )
        ),
        column(
          width = 6,
          style = "text-align: center;",
          fluidRow(
            column(
              width = 4
            ),
            column(
              width = 4,
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              wellPanel(
                style = "background: #28b78d; color: #333333; border: solid #333333 2px;",
                actionBttn(
                  inputId = "quizgen",
                  label = "Generate Quiz",
                  style = "jelly",
                  color = "success",
                  size = "lg"
                )
              )
            ),
            column(
              width = 4
            )
          ),
          uiOutput("warningsUI")
        )
      )
    )
  )
})

#Warning Text UI
output$warningsUI = renderUI({
  HTML(warningtext())
}) 