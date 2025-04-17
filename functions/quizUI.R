#UI for quiz section
output$quizUI = renderUI({
  
  wellPanel(
    style = "background: #f5f5f5; color: #333333;",
    # actionBttn(
    #   inputId = "backtosetup",
    #   label = "Back to Setup",
    #   style = "fill",
    #   color = "danger",
    #   size = "sm"
    # ),
    # tags$br(),
    fluidRow(
      column(
        width = 2,
        tags$h3("Quiz"),
        tags$br(),
        tags$br(),
        uiOutput("quizleftsideUI")
      ),
      column(
        width = 7,
        tags$div(
          style = "border: 2px solid black; background: #7a8288; padding: 0px",
          HTML("<CENTER>"),
          # tags$br(),
          uiOutput("structureimageUI"),
          HTML("</CENTER>"),
          # tags$br(),
        )
      ),
      column(
        width = 3,
        uiOutput("quizrightsideUI")
      )
    )
  )
})

output$structureimageUI = renderUI({
  isolate(displayOutput("image",height = paste0(imageheight(),"px")))
})

#Code for the leftside table
output$quizleftsideUI = renderUI({
  
  cellstylehead = "border: solid 2px #3a3f44; padding: 7px; text-align: left; background: #52575c; color: #f5f5f5;"
  cellstylebody = "border: solid 2px #3a3f44; padding: 5px; text-align: center;"
  
  #Formula for standardized table header font
  headfont = function(title){
    title = HTML(paste0("<font size = 4>",title,"</font>"))
    return(title)
  }
  
  tagList(
    tags$table(
      tags$tr(
        style = "border: solid 2px #3a3f44;",
        tags$td(
          style = cellstylehead,
          headfont("Species")
        ),
        tags$td(
          style = cellstylebody,
          uiOutput("speciesnameUI")
        )
      ),
      tags$tr(
        style = "background: #f5f5f5; border: solid 2px #3a3f44; color: #52575c;",
        tags$td(
          style = cellstylehead,
          headfont("Month of Capture")
        ),
        tags$td(
          style = cellstylebody,
          uiOutput("monthcaptureUI")
        )
      ),
      tags$tr(
        style = "background: #f5f5f5; border: solid 2px #3a3f44; color: #52575c;",
        tags$td(
          style = cellstylehead,
          headfont("Location")
        ),
        tags$td(
          style = cellstylebody,
          uiOutput("locationnameUI")
        )
      ),
      tags$tr(
        style = "background: #f5f5f5; border: solid 2px #3a3f44; color: #52575c;",
        tags$td(
          style = cellstylehead,
          headfont("Structure")
        ),
        tags$td(
          style = cellstylebody,
          uiOutput("structurenameUI")
        )
      ),
      tags$tr(
        style = "background: #f5f5f5; border: solid 2px #3a3f44; color: #52575c;",
        tags$td(
          style = cellstylehead,
          headfont("Technique")
        ),
        tags$td(
          style = cellstylebody,
          uiOutput("techniquenameUI")
        )
      )
    )
  )
})

#Species Name Text UI
output$speciesnameUI = renderUI({
  quizinfo = masterquiz()
  
  quizinfo = quizinfo %>% filter(order == quizselection())
  
  code2species(species(),quizinfo$Species)
})

#Capture Month Text UI
output$monthcaptureUI = renderUI({
  
  quizinfo = masterquiz()
  
  quizinfo = quizinfo %>% filter(order == quizselection())
  
  format(quizinfo$Capture_Date,"%B")
})

#Location Text UI
output$locationnameUI = renderUI({
  
  quizinfo = masterquiz()
  
  quizinfo = quizinfo %>% filter(order == quizselection())
  quizinfo$Location_1
})

#Structure Text UI
output$structurenameUI = renderUI({
  
  quizinfo = masterquiz()
  
  quizinfo = quizinfo %>% filter(order == quizselection())
  code2structures(quizinfo$Structure)
})

#Technique Text UI
output$techniquenameUI = renderUI({
  
  quizinfo = masterquiz()
  
  quizinfo = quizinfo %>% filter(order == quizselection())
  str_to_title(quizinfo$Age_Technique)
})

#Code for the rightside buttons and age
output$quizrightsideUI = renderUI({
  
  tagList(
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    tags$br(),
    wellPanel(
      HTML("<CENTER>"),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          tags$h5("Enter Age Estimate"),
          fluidRow(
            column(
              width = 3
            ),
            column(
              width = 6,
              uiOutput("ageentryUI")
            ),
            column(
              width = 3
            )
          )
        ),
        column(
          width = 3
        )
      ),
      tags$br(),
      uiOutput("savebttnUI"),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      fluidRow(
        column(
          width = 4,
          uiOutput("leftbttnUI")
        ),
        column(
          width = 4,
          wellPanel(
            style = "background:white;",
            HTML(paste0("<font size = 4><b><table><tr><td>",textOutput("quizcounterselectionUI"),"</td><td>/",
                        input$quizlength,"</td></tr></table></b></font>"))
          )
        ),
        column(
          width = 4,
          style = "text-align:right;",
          uiOutput("rightbttnUI")
        )
      )
    ),
    uiOutput("quizwarningsUI")
  )
})


output$quizcounterselectionUI = renderText({
  quizselection()
})

#Render image for display
output$image = renderDisplay({
  return(display(img(),method = 'browser'))
})

#UI for age entry
output$ageentryUI = renderUI({
  
  # isolate({
  quizages = masterquiz()
  quizages = quizages %>% filter(order == quizselection())
  
  numericInput(
    inputId = "age",
    label = NULL,
    value = quizages$Age_Answer,
    min = 0
  )
  # })
})

#UI for save button
output$savebttnUI = renderUI({
  
  bttnstatus = masterquiz()
  bttnstatus = bttnstatus %>% filter(order == quizselection())
  
  if (is.na(bttnstatus$Age_Answer)){
    actionBttn(
      inputId = "savebttn",
      label = "Save Answer",
      style = "simple",
      color = "danger",
      size = "lg",
      block = TRUE
    )
  }else{
    actionBttn(
      inputId = "savebttn",
      label = "Saved, Click to Update",
      style = "simple",
      color = "success",
      size = "lg",
      block = TRUE
    )
  }
})

#UI for Previous button
output$leftbttnUI = renderUI({
  
  if (quizselection() > 1){ 
    
    uiOutput("leftbttninputUI")
  }
  
})

output$leftbttninputUI = renderUI({
  isolate({
    actionBttn(
      inputId = "leftbttn",
      label = " Previous",
      style = "simple",
      color = "success",
      size = "lg",
      block = TRUE
    ) 
  })
  
  
})


#UI for Next and Finish buttons
output$rightbttnUI = renderUI({
  if (quizselection() < as.numeric(input$quizlength)){
    uiOutput("rightbttninputUI")
  }else{
    uiOutput("finishbttninputUI")
  }
})

output$rightbttninputUI = renderUI({
  
  isolate({
    actionBttn(
      inputId = "rightbttn",
      label = "Next",
      style = "simple",
      color = "success",
      size = "lg",
      block = TRUE
    )
  })
})

output$finishbttninputUI = renderUI({
  isolate({
    actionBttn(
      inputId = "finishbttn",
      label = "Finish",
      style = "simple",
      color = "danger",
      size = "lg",
      block = TRUE
    )
  })
  
})


output$quizwarningsUI = renderUI({
  HTML(quizwarning())
})