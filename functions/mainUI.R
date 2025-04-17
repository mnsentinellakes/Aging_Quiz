# panelstatus = reactiveVal("Settings")
panelstatus = reactiveVal("Setup")

output$titleUI = renderUI({
  tagList(
    wellPanel(
      style = "background: #7a8288; color: #f5f5f5;",
      tags$h1(
        
        "Fish Aging Structures Quiz")
    ),
    tags$br()
  )
})


output$mainUI = renderUI({
  if (panelstatus() == "Setup"){
    uiOutput("setupUI")
  }else if (panelstatus() == "Quiz"){
    uiOutput("quizUI")
  }else if (panelstatus() == "Results"){
    uiOutput("resultsUI")
  }
})

