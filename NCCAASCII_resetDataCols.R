# R function to reset the data columns in the _Data data frame
# Feb 19, 2026
# Raymond Nelson
#
# call reset_Data_Fn() to initialize all added columns in the _DATA ddata frame 
# without the need to parse the NCCA ASCII data from the text files
#
####




source(paste0(RPath, "NCCAASCII_dataParse.R"), echo = FALSE)

source(paste0(RPath, 'addColumns.R'), echo=FALSE)



# getUniqueExams <- function(x="*_Data$") { unique(str_sub(ls(pattern=x, pos=1),1, -6)) }



uniqueExamNames <- getUniqueExams()




reset_Data_Fn <- function(x=uniqueExamNames, saveCSV=FALSE, makeDF=TRUE, output=FALSE) {
  # R function to reset the data columns in the _Data data frame
  # Feb 19, 2026
  # Raymond Nelson
  ####
  # used to re-initialize all added columns in the _Data data frame
  # that was initialized in the dataParse() function in the NCCA_ASCII_dataParse.R script
  # dataParse is called by parseUniqueExams() in the NCCAASCII_Parse.R script            
  # 
  # it is normally not necessary to call this function in the workFlow.R script
  # because added columns are intitialized when parsing the NCCA ASCII data
  # when the _Data data frame is initialized
  ####
  # x is a vector of unique exam namesin the global envir
  ####
  
  {
    if(!exists("uniqueExamNames")) uniqueExamNames <- x
  }
  
  if(length(uniqueExamNames) == 0) return("no exams to parse")
  
  #### iterate over the exams ####
  
  i=1
  for(i in 1:length(uniqueExamNames)) {
    
    # get the names of the files for the exam charts
    
    thisExamName <- uniqueExamNames[i]
    
    # save the exam name for observation and inspection
    assign("examName", thisExamName, pos=1)
    
    assign("i", i, envir=.GlobalEnv)
    
    print(paste("exam", i, "of", length(uniqueExamNames), "examName:", thisExamName))
    
    thisDataFileName <- paste0(thisExamName, "_Data")
    
    examDF <- get(thisDataFileName)
    
    theseColNames <- colnames(examDF)
    # omit previously added columns
    
    lastCol <- grep("c_", theseColNames)[1]-1
    if(is.na(lastCol)) lastCol <- 16
    
    if(lastCol < 16) {
      # look for missing Move1 column
      if(all(!grepl("Move1", theseColNames))) {
        theseColNames <- c(theseColNames, "Move1")
        lastCol <- lastCol + 1
      }
      # look for missing PPG1 column
      if(all(!grepl("PPG1", theseColNames))) {
        theseColNames <- c(theseColNames, "PPG1")
        lastCol <- lastCol + 1
      }
      
      if(lastCol < 16) stop("check columns RN")
    }
    
    theseColNames <- theseColNames[c(1:lastCol)]
    
    examDF <- examDF[,theseColNames]
    
    #### add the centered data columns ####
    
    {
      
      # make a vector of names of the data columns for some new columns for the centered data
      newColNames <- paste0("c_", names(examDF[11:ncol(examDF)]))
      
      # add the names to the output data frame
      for (m in 1:length(newColNames)) {
        examDF <- cbind(examDF, rep(0, times=nrow(examDF)))
        # name each new column
        names(examDF)[ncol(examDF)] <- newColNames[m]
      } 
      
    }
    
    #### call a function to add columns for signal processing ####
    
    {
      
      # source(paste0(RPath, 'addColumns.R'), echo=FALSE)
      
      examDF <- addColumnsFn(x=examDF)
      
    }
    
    #### output ####
    
    {
      
      # create the output file name
      # outName <- paste(str_sub(currentChartName, 1, -10), "_Data", sep = "")
      
      
      # save the data as a CSV
      if (saveCSV==TRUE) write.csv(examDF, 
                                   file=thisDataFileName, 
                                   row.names = FALSE)
      
      # save the data as a data frame 
      if(makeDF==TRUE) assign(x=thisDataFileName, value=examDF, envir=.GlobalEnv)
      
    }
    
    # visible output is the name of the last input exam
    print(thisExamName)
    
  } # end i loop over unique exam names
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  # return the last
  if(output==TRUE) { return(examDF) } else {
    return(uniqueExamNames)
  }
  
} # end reset_Data_Fn()



# call the function
# reset_Data_Fn(x=uniqueExamNames, saveCSV=FALSE, makeDF=TRUE)
