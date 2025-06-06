# R function to set file dates 
# December 19, 2021
# Raymond Nelson

#########################################



# Sys.setFileTime("test.txt", "1980-12-04")



####### locate the exams in the current working directory ########



{
  
  # source the getExamNames.R script to load the getCharts() and uniqueNames() functions
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/getExamNames.R'), echo=FALSE)
  
  # getCharts() will locate NCCA ASCII charts in the cwd
  # uniqueNames() is used to make a list of uniques exams in a directory
  
  print("search the working directory for NCCA ASCII text output")
  
  {
    
    searchPattern1 <- NULL
    searchPattern2 <- NULL
    searchPattern3 <- NULL
    searchPattern4 <- NULL
    
    uniqueExamNames1 <- NULL
    uniqueExamNames2 <- NULL
    uniqueExamNames3 <- NULL
    uniqueExamNames4 <- NULL
    
    # Lafayette uses the &
    searchPattern1 <- "^D&+"
    # Stoelting #
    # 2010 changed to !
    searchPattern3 <- "^D#+"
    # Limestone %
    searchPattern4 <- "^D%+"
    # 2010 changed to @
    # Axciton $ character needs to be double escaped because it is a special character for grep
    searchPattern2 <- "^D\\$"
    
    # call the two functions together to make a list of unique exams in the current working directory
    if(!is.null(searchPattern1)) uniqueChartNames1 <- getCharts(x=searchPattern1, uniqueTests=FALSE)
    if(!is.null(searchPattern2)) uniqueChartNames2 <- getCharts(x=searchPattern2, uniqueTests=FALSE)
    if(!is.null(searchPattern3)) uniqueChartNames3 <- getCharts(x=searchPattern3, uniqueTests=FALSE)
    if(!is.null(searchPattern4)) uniqueChartNames4 <- getCharts(x=searchPattern4, uniqueTests=FALSE)
    
    if(length(uniqueChartNames1)==0) searchPattern1 <- NULL
    if(length(uniqueChartNames2)==0) searchPattern2 <- NULL
    if(length(uniqueChartNames3)==0) searchPattern3 <- NULL
    if(length(uniqueChartNames4)==0) searchPattern4 <- NULL
    
  }
  
  #### select an exam from the vector of exam names
  
  selectCharts <- "ALL"
  # selectChart <- 32
  
  # keep only those selected exam numbers
  if(selectCharts[1] != "ALL") {
    uniqueChartNames <- 
      c(uniqueChartNames1, uniqueChartNames2, uniqueChartNames3, uniqueChartNames4)[selectExams]
  } else uniqueChartNames <- 
    c(uniqueChartNames1, uniqueChartNames2, uniqueChartNames3, uniqueChartNames4)
  
  print(paste("Found", length(uniqueChartNames), "charts"))
  print(uniqueChartNames)
  
}



resetFileDate <- FALSE
# resetFileDate <- TRUE


if(isTRUE(resetFileDate)) {
  
  for(i in 1:length(uniqueChartNames)) {
    
    Sys.setFileTime(uniqueChartNames[i], "2016-03-14")
    
  }
  
  resetFileDate <- FALSE
}
