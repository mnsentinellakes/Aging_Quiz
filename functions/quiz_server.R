#ReactiveVal indicating quiz row selected
quizselection = reactiveVal(1)

#Calculate image dimensions for display
imageheight = reactiveVal()
# imagewidth = reactiveVal()

observe({
  if (!is.null(masterquiz())){
    #Read quiz info
    
    #Set image height to a percentage of the screen size
    imgheight = ceiling(get_height() *0.8)
    
    imageheight(imgheight)
    
    #Get the ratio between the screen height and original height
    # heightratio = imgheight / imgdim[1]
    
    #Calculate width
    # imgwidth = ceiling(imgdim[2] * heightratio)
    
    # imagewidth(imgwidth)
  }
  
})

#Load selected image
img = reactiveVal()

#Initial image
observe({
  validate(
    need(!is.null(masterquiz()),"Loading...")
  )
  if (quizselection() == 1){
    isolate({
      quizimg = masterquiz()
      
      quizimg = quizimg %>% filter(order == 1)
      
      imgsrc = paste0("data/images/",quizimg$Reference_ID,".jpg")
      
      img(readImage(imgsrc))
    })
  }
  
})

#Subsequent images
observeEvent(
  input$leftbttn | input$rightbttn,
  {
    validate(
      need(!is.null(masterquiz()),"Loading..."),
      need(!is.null(quizselection()),"Loading...")
    )
    
    quizimg = masterquiz()
    
    quizimg = quizimg %>% filter(order == quizselection())
    
    imgsrc = paste0("data/images/",quizimg$Reference_ID,".jpg")
    
    img(readImage(imgsrc))
  }
)



#Left Button Action
observeEvent(
  input$leftbttn,
  {
    quizselection(quizselection() - 1)
    
    
    if (is.na(input$age)){
      sendSweetAlert(
        title = "Answer is Blank",
        text = "The previous answer was not completed. Either go back and fill in the answer now or come back before finishing the quiz.",
        type = "warning"
      )
    }
  }
)

#Right Button Action
observeEvent(
  input$rightbttn,
  {
    quizselection(quizselection() + 1)
    
    if (is.na(input$age)){
      sendSweetAlert(
        title = "Answer is Blank",
        text = "The previous answer was not been completed. Either go back and fill in the answer now or come back before finishing the quiz.",
        type = "warning"
      )
    }
    
  }
)

debounced_age = debounce(reactive(input$age),500)

#Add Answers to master quiz table
observeEvent(
  input$savebttn,
  {
    if (!is.na(input$age)){
      quizdata = masterquiz()
      
      quizdata$Age_Answer[which(quizdata$order == quizselection())] = debounced_age()
      
      masterquiz(quizdata)
    }
  }
)

#Create Results ReactiveVal
quizresults = reactiveVal()

#Warning for missed questions
quizwarning = reactiveVal()

#Finish Button Action
observeEvent(
  input$finishbttn,
  {
    
    finishedquiz = masterquiz()
    quizexperts = experts()
    
    if (all(!is.na(finishedquiz$Age_Answer))){
      
      finishedquiz = finishedquiz %>% left_join(quizexperts)
      
      finishedquiz = finishedquiz %>% group_by(Reference_ID) %>% 
        mutate(Min_Age = min(Age_Assignment),Max_Age = max(Age_Assignment)) %>%
        ungroup() %>% mutate(Answer_Resid = if_else(Age_Answer >= Min_Age & Age_Answer <= Max_Age,0,
                                                    if_else(Age_Answer < Min_Age,Age_Answer - Min_Age,Age_Answer - Max_Age))) %>%
        select(Reference_ID,Fish_ID,Image_ID,Species,Structure,Capture_Date,Total_Length_mm,Sex,Age_Technique,Location_1,Min_Age,Max_Age,
               First_Name,Last_Name,Organization,Age_Answer,Answer_Resid,order) %>%
        dplyr::rename(Location = Location_1) %>% distinct() 
      
      quizresults(finishedquiz)
      panelstatus("Results")
    } else {
       missedanswers = finishedquiz$order[which(is.na(finishedquiz$Age_Answer))]
       
       missedanswerstext = "<font color = #e9322d><center>"
       for (i in missedanswers){
         missedanswerstextrow = paste0("Question ",i," has not been completed<br>")
         missedanswerstext = paste0(missedanswerstext,missedanswerstextrow)
       }
       
       missedanswerstext = paste0(missedanswerstext,"</center></font>")
       
      quizwarning(missedanswerstext)
    }
  }
)