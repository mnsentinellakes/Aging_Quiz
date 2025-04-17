#Index of average percent error
iAPE = function(dataset){
  
  ape = function(age,assess){
    (abs(assess - age))/assess
  }
  
  dataset = dataset %>% rowwise() %>% mutate(Mean_Age = mean(c(Max_Age,Min_Age)))
  
  answer = (sum(ape(age = dataset$Age_Answer,assess = dataset$Mean_Age)) / nrow(dataset)) * 100
  
  answer = round(answer,digits = 2)
  
  return(answer)
  
}

medianpe = function(dataset){
  ape = function(age,assess){
    (abs(assess - age))/assess
  }
  
  dataset = dataset %>% rowwise() %>% mutate(Mean_Age = mean(c(Max_Age,Min_Age)))
  
  answer = median(ape(age = dataset$Age_Answer,asses = dataset$Mean_Age) * 100)
  
  answer = round(answer,digits = 2)
  
  return(answer)
}

#Coefficient of Variation
cv = function(dataset){
  
  ape = function(age,assess){
    (abs(assess - age))/assess
  }
  
  dataset = dataset %>% rowwise() %>% mutate(Mean_Age = mean(c(Max_Age,Min_Age)))
  
  meanexpage = mean(c(dataset$Max_Age,dataset$Min_Age))
  
  sdresid = sd(dataset$Age_Answer - dataset$Mean_Age)
  
  answer = round((sdresid / meanexpage) * 100,digits = 2)
  
  return(answer)
}

meanresid = reactiveVal()
medianresid = reactiveVal()
moderesid = reactiveVal()

residbelow = reactiveVal()
residzero = reactiveVal()
residabove = reactiveVal()
nonzeroresid = reactiveVal()

iAPEstore = reactiveVal()
medianpestore = reactiveVal()
cvstore = reactiveVal()

#Calculate Mean Residuals
observe({
  if (panelstatus() == "Results"){
    meanres = quizresults()
    
    meanres = round(mean(meanres$Answer_Resid),digits = 2)
    
    meanresid(meanres)
    
    print("Mean Residuals")
  }
})

#Calculate Median Residuals
observe({
  if(panelstatus() == "Results"){
    medianres = quizresults()
    
    medianres = round(median(medianres$Answer_Resid),digits = 2)
    
    medianresid(medianres)
    print("Median Residuals")
  }
})

#Calculate Mode Residuals
observe({
  if(panelstatus() == "Results"){
    
    get_mode_all <- function(x) {
      freq <- table(x)
      modes <- names(freq)[freq == max(freq)]
      as.numeric(modes)
    }
    
    moderes = quizresults()
    
    moderes = get_mode_all(moderes$Answer_Resid)
    
    moderes = paste(sort(moderes),sep = ",")
    
    moderesid(moderes)
    
    print("Mode Residuals")
  }
})

#Calculate Percent of Residuals below 0
observe({
  if(panelstatus() == "Results"){
    belowresid = quizresults()
    
    belowresid = (nrow(belowresid[which(belowresid$Answer_Resid < 0),]) / nrow(belowresid)) * 100
    
    residbelow(belowresid)
    
    print("Below Residuals")
  }
})

#Calculate Percent of Residuals equal to 0
observe({
  if(panelstatus() == "Results"){
    zeroresid = quizresults()
    
    zeroresid = (nrow(zeroresid[which(zeroresid$Answer_Resid == 0),]) / nrow(zeroresid)) * 100
    
    residzero(zeroresid)
    
    print("Zero Residuals")
  }
})

#Calculate Percent of Residuals above 0
observe({
  if(panelstatus() == "Results"){
    aboveresid = quizresults()
    
    aboveresid = (nrow(aboveresid[which(aboveresid$Answer_Resid > 0),]) / nrow(aboveresid)) * 100
    
    residabove(aboveresid)
    
    print("Above Residuals")
  }
})

#Get non-zero residuals that account for more than 25% of the answers
observe({
  if(panelstatus() == "Results"){
    nonzero = quizresults()
    
    nonzero = nonzero %>% group_by(Answer_Resid) %>% mutate(count = n()) %>% ungroup() %>% mutate(percent = count / n()) %>% 
      filter(percent >= 0.25) %>% select(Answer_Resid,percent) %>% distinct() %>% filter(Answer_Resid != 0)
    
    
    if (nrow(nonzero) > 0){
      nonzero = toString(paste(as.character(sort(nonzero$Answer_Resid)),sep = ","))
    }else{
      nonzero = "None"
    }
    
    nonzeroresid(nonzero)
    
    print("NonZero Residuals")
  }
})

#calculate iAPE
observe({
  if(panelstatus() == "Results"){
    calciape = quizresults()
    
    iapecalc = iAPE(calciape)
    
    iAPEstore(iapecalc)
    
    print("iAPE")
  }
})

#Calculate Median Percent Error
observe({
  if (panelstatus() == "Results"){
    medianpedata = quizresults()
    
    medianperesults = medianpe(medianpedata)
    
    medianpestore(medianperesults)
    
    print("Median PE")
  }
})

#Calculate Coefficient of Variation
observe({
  if (panelstatus() == "Results"){
    cvdata = quizresults()
    
    cvresults = cv(cvdata)
    
    cvstore(cvresults)
    
    print("Coefficient of Variation")
  }
})


# #Calculate Mean Percent Error
# observe({
#   if (panelstatus() == "Results"){
#     meanpe = quizresults()
#     
#     meanpe = meanpe %>% mutate(Mean_Age = mean(c(Max_Age,Min_Age))) %>% 
#       mutate(Perc_Err = abs((Mean_Age - Age_Answer) / Mean_Age) * 100)
#     
#     
#     
#     meanpe = round(mean(meanpe$Perc_Err),digits = 2 )
#     
#     mpe(meanpe)
#   }
# })

#Plots
#Plot
finalplots = reactiveVal()

observe({
  if (panelstatus() == "Results"){
    resultsdata = quizresults()
    norm_plot = norm_dist()
    
    countdata = resultsdata %>% count(Answer_Resid)
    
    colnames(countdata) = c("Residuals","Count")
    
    countdata = countdata %>% mutate(Percentage = (Count / sum(Count)) * 100)
    
    norm_plot$scaled_values = round(nrow(resultsdata) * norm_plot$Y,digits = 0)
    
    norm_plot$Y = NULL
    
    colnames(norm_plot) = c("Residuals","Count")
    
    #Count Plot Settings
    ymaxcount = max(c(norm_plot$Count,countdata$Count))
    xmaxcount = max(c(abs(countdata$Residuals)))
    xrangecount = seq(from = (-1 * xmaxcount),to = xmaxcount,by = 1)
    
    message(max(countdata$Percentage))
    
    #Percentage Plot Settings
    if (max(countdata$Percentage) > 50){
      line_50 = geom_hline(
        aes(
          yintercept = 50
        ),
        color = "black",
        linewidth = 2
      )
      
      line_25 =  geom_hline(
        aes(
          yintercept = 25
        ),
        color = "gray20",
        linewidth = 1.5
      )
    }else if (max(countdata$Percentage) < 50 & max(countdata) > 25){
      line_50 = NULL
      
      line_25 =  geom_hline(
        aes(
          yintercept = 25
        ),
        color = "gray20",
        linewidth = 1.5
      )
    }else{
      line_50 = NULL
      
      line_25 = NULL
    }
    
    fills = c("Actual" = "#ee5f5b","Expected" = "#62c462")
    
    countplot = ggplot() +
      geom_col (
        data = norm_plot,
        aes(
          x = Residuals,
          y = Count,
          fill = "Expected"
        ),
        show.legend = TRUE,
        width = 0.95,
        # alpha = 0.75
      ) +
      geom_col(
        data = countdata,
        aes(
          x = Residuals,
          y = Count,
          fill = "Actual"
        ),
        show.legend = TRUE,
        # color = "#3a3f44",
        width = 0.6,
        # alpha = 0.5
      ) +
      labs(fill = "") +
      scale_x_continuous(breaks = xrangecount,expand = c(0,0),limits = c(min(xrangecount) - 0.5,max(xrangecount) + 0.5)) +
      scale_y_continuous(breaks = seq(from = 0,to = ymaxcount,by = 1),expand = c(0,0),limits = c(0,ymaxcount + 0.2)) +
      # xlab("Residuals") +
      # ylab("Count") +
      scale_fill_manual(values = fills) +
      theme_classic() +
      theme(
        legend.position = "top",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.major.y = element_line(linewidth = 0.5,color = "#e9ecef")
      )
    
    percplot = ggplot(
      data = countdata
    ) +
      geom_col(
        aes(
          x = Residuals,
          y = Percentage
        ),
        fill = "#ee5f5b",
        show.legend = FALSE,
        width = 0.95
      ) + 
      line_50 +
      line_25 +
      scale_x_continuous(breaks = xrangecount,expand = c(0,0),limits = c(min(xrangecount) - 0.5,max(xrangecount) + 0.5)) +
      scale_y_continuous(breaks = c(seq(from = 0,to = max(countdata$Percentage),by = 10),25),expand = c(0,0),
                         labels = function(x)paste0(x,"%")) +
      theme_classic() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        # axis.text.y = element_text(face = ifelse(levels(countdata$Percentage) %in% c(25,50),"bold","plain")),
        panel.grid.major.y = element_line(linewidth = 0.5,color = "#e9ecef")
      )
    
    dataplots = list("Count" = countplot,"Percentage" = percplot)
    
    finalplots(dataplots)
  }
})

#Build Final Table for Display and Download
finaltable = reactiveVal()
observe({
  if (panelstatus() == "Results"){
    resultstable = quizresults() 
    
    resultstable = resultstable %>% select(c(Reference_ID,Species,Structure,Capture_Date,Total_Length_mm,Sex,Age_Technique,Location,Min_Age,
                                             Max_Age,First_Name, Last_Name,Organization,Age_Answer,Answer_Resid))
    
    colnames(resultstable)[15] = "Residual"
    
    finaltable(resultstable)
  }
})

#Results Tables
output$resultstable = renderDT(
  options = list(
    info = FALSE,
    scrollY = "250px",
    searching = FALSE,
    paging = FALSE
  ),{
    resultsDT = finaltable()
    
    resultsDT = resultsDT %>% select(c(Capture_Date,Min_Age,Max_Age,Age_Answer,Residual))
    resultsDT
    
  }
)

#Stats table
statstable = reactiveVal()
observe({
  if (panelstatus() == "Results"){
    validate(
      need(meanresid(),"Processing..."),
      need(medianresid(),"Processing..."),
      need(moderesid(),"Processing..."),
      need(residbelow(),"Processing..."),
      need(residzero(),"Processing..."),
      need(residabove(),"Processing..."),
      need(nonzeroresid(),"Processing..."),
      need(iAPEstore(),"Processing..."),
      need(medianpestore(),"Processing..."),
      need(cvstore(),"Processing...")
    )
    
    statstablebuild = data.frame(
      "Mean_Residuals" = meanresid(),
      "Median_Residuals" = medianresid(),
      "Mode_Residuals" = moderesid(),
      "Percent_Residuals_Below_Zero" = residbelow(),
      "Percent_Residuals_Zero" = residzero(),
      "Percent_Residuals_Above_Zero" = residabove(),
      "Non_Zero_Residuals_Above_25_Percent" = nonzeroresid(),
      "Index_of_Average_Percent_Error" = iAPEstore(),
      "Median_Percent_Error" = medianpestore(),
      "Coefficient_of_Variation" = cvstore()
    )
    
    statstable(statstablebuild)
  }
})

#Copy pictures
# observe({
#   if (panelstatus() == "Results"){
#     pictable = quizresults()
#     
#     refids = pictable$Reference_ID
#     
#     pic_paths = paste0("data\\images\\",refids,".jpg")
#     
#     file.copy(pic_paths,"temp")
#   }
# })

output$resultsdld <- downloadHandler(
  filename = function() {
    "results.zip"
  },
  content = function(fname) {
    temp_dir <- tempdir()  # Temporary directory for the ZIP creation
    
    # Paths to plots and tables
    count_plot_path <- file.path(temp_dir, "count_plot.png")
    percent_plot_path <- file.path(temp_dir, "percent_plot.png")
    results_table_path <- file.path(temp_dir, "results_table.csv")
    stats_table_path <- file.path(temp_dir, "stats_table.csv")
    
    # Save Plots
    outputplots <- finalplots()
    ggsave(count_plot_path, outputplots[["Count"]], width = 1440, height = 1024, units = "px")
    ggsave(percent_plot_path, outputplots[["Percentage"]], width = 1440, height = 1024, units = "px")
    
    # Save Tables
    write.csv(finaltable(), results_table_path, row.names = FALSE)
    write.csv(statstable(), stats_table_path, row.names = FALSE)
    
    # Copy Pictures to Temporary Directory
    pictable <- quizresults()
    refids <- pictable$Reference_ID
    
    # Assuming pictures are stored in "data/images" within the app directory
    pic_paths <- file.path("data/images", paste0(refids, ".jpg"))
    pic_dest_paths <- file.path(temp_dir, paste0(refids, ".jpg"))
    
    # Check if files exist and copy them
    file_copied <- mapply(file.copy, from = pic_paths, to = pic_dest_paths, overwrite = TRUE)
    if (any(!file_copied)) {
      stop("Some pictures could not be copied to the temporary directory.")
    }
    
    # Collect all files to include in the ZIP
    files_to_zip <- c(
      count_plot_path,
      percent_plot_path,
      results_table_path,
      stats_table_path,
      pic_dest_paths
    )
    
    # Ensure all files exist
    missing_files <- files_to_zip[!file.exists(files_to_zip)]
    if (length(missing_files) > 0) {
      stop(paste("The following files are missing:", paste(missing_files, collapse = ", ")))
    }
    
    # Create ZIP archive
    zip::zipr(zipfile = fname, files = files_to_zip, root = temp_dir)
    
    # Log success
    print("ZIP file created successfully.")
  }
)


# output$resultsdld = downloadHandler(
#   filename = function(){
#     "results.zip"
#   },
#   content = function(fname){
#     
#     temp_dir = tempdir()
#     
#     #Copy over pictures
#     pictable = quizresults()
#     
#     refids = pictable$Reference_ID
#     
#     pic_paths = paste0("data\\images\\",refids,".jpg")
#     
#     # file.copy(pic_paths,temp_dir)
#     
#     #Save Plots
#     outputplots = finalplots()
#     countplot = outputplots[["Count"]]
#     percentplot = outputplots[["Percentage"]]
#     
#     
#     
#     ggsave(
#       filename = paste0(temp_dir,"\\count_plot.png"),
#       plot = countplot,
#       device = "png",
#       width = 1440,
#       height = 1024,
#       units = "px"
#     )
#     
#     ggsave(
#       filename = paste0(temp_dir,"\\percent_plot.png"),
#       plot = percentplot,
#       device = "png",
#       width = 1440,
#       height = 1024,
#       units = "px"
#     )
#     
#     
#     #Save Results Table
#     write.csv(finaltable(),paste0(temp_dir,"\\results_table.csv"),row.names = FALSE)
#     
#     #Save Statistics File
#     write.csv(statstable(),paste0(temp_dir,"\\stats_table.csv"),row.names = FALSE,)
#     
#     #Copy Quiz Images
#     #Get Quiz Numbers
#     
#     # imgfiles = paste0(temp_dir,"\\",refids,".jpg")
#     plotfiles = paste0(temp_dir,"\\",c("count_plot.png","percent_plot.png"))
#     tablefiles = paste0(temp_dir,"\\",c("results_table.csv","stats_table.csv"))
#     
#     zipfileslist = c(plotfiles,tablefiles)
#     
#     print(list.files(temp_dir))
#     
#     print("Initiate Zip")
#     zip(
#       zipfile = fname,
#       files = paste0(temp_dir,"\\count_plot.png"),
#       include_directories = FALSE,
#       recurse = FALSE,
#       mode = "cherry-pick"
#     )
#     print("count_plot.png zipped")
#     
#     zip_append(
#       zipfile = fname,
#       files = paste0(temp_dir,"\\percent_plot.png"),
#       include_directories = FALSE,
#       recurse = FALSE,
#       mode = "cherry-pick"
#     )
#     print("percent_plot.png zipped")
#     
#     zip_append(
#       zipfile = fname,
#       files = paste0(temp_dir,"\\results_table.csv"),
#       include_directories = FALSE,
#       recurse = FALSE,
#       mode = "cherry-pick"
#     )
#     print("results_table zipped")
#     
#     zip_append(
#       zipfile = fname,
#       files = paste0(temp_dir,"\\stats_table.csv"),
#       include_directories = FALSE,
#       recurse = FALSE,
#       mode = "cherry-pick"
#     )
#     print("stats_table zipped")
#     
#     zip_append(
#       zipfile = fname,
#       files = pic_paths,
#       include_directories = FALSE,
#       recurse = FALSE,
#       mode = "cherry-pick"
#     )
#   
#   print("pic_paths zipped")
#     #Clear Cache
#     # file.remove(zipfileslist)
#     
#   }
# )