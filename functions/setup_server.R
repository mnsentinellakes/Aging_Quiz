speciesoptions = reactiveVal()
locationoptions = reactiveVal()
structureoptions = reactiveVal()
techniqueoptions = reactiveVal()
masterquiz = reactiveVal()

#Function for converting species names to codes
species2code = function(speciesdf,names){
  speciescodes = speciesdf$Code[which(speciesdf$Name %in% names)]
  
  return(speciescodes)
}

#Function for converting species codes to names
code2species = function(speciesdf,codes){
  speciesnames = speciesdf$Name[which(speciesdf$Code %in% codes)]
  
  return(speciesnames)
}


#Function for converting structures to code
structures2code = function(structures){
  
  structureslist = data.frame("Name" = c("Otoliths","Scales"),"Code" = c("OT","SC"))
  
  selectstructure = structureslist$Code[which(structureslist$Name %in% structures)]
  
  return(selectstructure)
}

#Function for converting code to structures
code2structures = function(code){
  
  structureslist = data.frame("Name" = c("Otoliths","Scales"),"Code" = c("OT","SC"))
  
  selectcode = structureslist$Name[which(structureslist$Code %in% code)]
  
  return(selectcode)
}

#Create Vector of species
observe({
  masterspecies = master()
  speciestable = species()
  
  speciestable = speciestable %>% filter(Code %in% masterspecies$Species) %>% select(Code,Name)
  
  # specieslist = speciestable$Code
  
  # names(specieslist) = speciestable$Name
  
  speciesoptions(speciestable)
  
})

#Create vector of locations
observe({
  masterlocation = master()
  masterlocation = unique(masterlocation$Location_1)
  
  locationoptions(masterlocation)
  
})

#Create vector of structures
observe({
  masterstructures = master()
  
  structureoptions(code2structures(masterstructures$Structure))
  
})

#Create vector of techniques
observe({
  mastertechniques = master()
  
  mastertechniques = str_to_title(unique(mastertechniques$Age_Technique))
  
  techniqueoptions(mastertechniques)
})

warningtext = reactiveVal()

#Generate Button click Process
observeEvent(
  input$quizgen,
  {
    
    #Check that all inputs have been filled out
    if (nchar(input$firstname) < 1 | nchar(input$lastname) < 1 | nchar(input$organization) < 1 | is.null(input$species) | 
        is.null(input$locations) | is.null(input$techniques) | is.null(input$structures)){
      print("Missing Info")
      buildwarningtext = "<font color = #e9322d>"
      
      #Check First Name has been filled out
      if (nchar(input$firstname) < 1){
        buildwarningtext = paste0(buildwarningtext,"Warning: Missing First Name<br>")
      }
      
      #Check Last Name has been filled out
      if (nchar(input$lastname) < 1){
        buildwarningtext = paste0(buildwarningtext,"Warning: Missing Last Name<br>")
      }
      
      #Check Organization has ben filled out
      if (nchar(input$organization) < 1){
        buildwarningtext = paste0(buildwarningtext,"Warning: Missing Organization<br>")
      }
      
      #Check Species have been selected
      if (is.null(input$species)){
        buildwarningtext = paste0(buildwarningtext,"Warning: Missing Species<br>")
      }
      
      #Check Locations have been selected
      if (is.null(input$locations)){
        buildwarningtext = paste0(buildwarningtext,"Warning: Missing Locations<br>")
      }
      
      #Check Structures have been selected
      if (is.null(input$structures)){
        buildwarningtext = paste0(buildwarningtext,"Warning: Missing Structures<br>")
      }
      
      #Check Techniques have beens selected
      if (is.null(input$techniques)){
        buildwarningtext = paste0(buildwarningtext,"Warning: Missing Techniques")
      }
      
      buildwarningtext = paste0(buildwarningtext,"</font>")
      
      warningtext(buildwarningtext)
    }else{
      
      #Import master and species tables
      mastergen = master()
      speciesgen = species()
      
      buildwarningtext = "Filtering Selection<br>"
      warningtext(buildwarningtext)
      
      #Select rows from mastertable based upon user inputs
      mastergen = mastergen %>% 
        filter(Species %in% species2code(speciesgen,input$species) & Location_1 %in% input$locations &
                 Structure %in% structures2code(input$structures) & Age_Technique %in% tolower(input$techniques))
      
      
      buildwarningtext = paste0(buildwarningtext,"Selecting Samples<br>")
      warningtext(buildwarningtext)
      masterlength = nrow(mastergen)
      
      #Generate random integers based on the length
      randsample = sample.int(n = masterlength,size = as.numeric(input$quizlength))
      
      #Select random sample from mastergen table
      mastergen = mastergen[randsample,]
      
      #Label rows
      mastergen$order = seq(from = 1,to = nrow(mastergen), by = 1)
      
      #Add First Name Field
      mastergen$First_Name = input$firstname
      
      #Add Last Name Field
      mastergen$Last_Name = input$lastname
      
      #Add Organization
      mastergen$Organization = input$organization
      
      #Add Answer Field
      mastergen$Age_Answer = NA
      
      buildwarningtext = paste0(buildwarningtext,"Quiz Generated")
      warningtext(buildwarningtext)
      masterquiz(mastergen)
      
      panelstatus("Quiz")
    }
  }
)