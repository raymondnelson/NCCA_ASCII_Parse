############### R Functions to Parse stimulus events from all charts for each exam ########
# 3-10-2014 Raymond Nelson
# 6-12-2015
#
#
# this script contains the following functions
#
# stimCheck()
# to to determine whether stimulus text is identical for questions on all charts
#
#
###

# function to determine whether stimulus text is identical for questions on all charts

stimCheck <- function(x="_Stimuli$", saveCSV=TRUE, makeDF=TRUE) {
  # function to determine whether stimulus text is identical for questions on all charts
  # x input is a character string to identify the Stimuli data frames
  # output is a data frame and csv warning regarding differences 
  
  
  #   stimNames <- list.files(path = ".", pattern = x, 
  #                            all.files = FALSE, 
  #                            full.names = FALSE, 
  #                            recursive = FALSE, 
  #                            ignore.case = FALSE, 
  #                            include.dirs = FALSE)
  
  stimNames <- ls(pattern=x, pos=1)
  
  # add a parameter to load the .Stimuli data if necessary
  
  library(stringr)
  
  # uniqueExamNames <- unique(str_sub(stimNames, 1L, -9L)) # not needed
    
  # loop over each unique exam
  # i <- 1 # for testing
  for (i in 1:length(stimNames)) {
    # make a list of charts for each unique series  
    
    # get the unique exam name
    # examName <- strtrim(stimNames[i], (nchar(stimNames[i]) - 8)
    examName <- str_sub(stimNames[i], start=1, end=nchar(stimNames[i])-8)
    
    # name <- paste(uniqueExamNames[h], "....", "_stimuli.csv", sep = "") # not needed
    
    # get the data frame 
    tempDF <- get(stimNames[i], pos=1)
    
    # get the chart names
    uniqueCharts <- unique(as.character(tempDF[,3]))
    # length(uniqueCharts)
    
    # get the numbers of the unique series
    uniqueSeriesNames <- unique(substr(uniqueCharts, start=1, stop=1))
    # not used at theis point
    
    #### make a loop for each unique series
    # h <- 1
    for (h in 1:length(uniqueSeriesNames)) {
      seriesName <- uniqueSeriesNames[h]
      
      # set the warningMessage to NULl
      warningMessage <- NULL
    
      if(identical(as.character(tempDF$Label), as.character(tempDF$Statement))==TRUE) {
        warningMessage <- c(warningMessage, "01 Stimulus text not available. It is assumed that all stimulus text statement are identical for all charts.")
      } # end if
      
      # make a working data frame from the event matrix
      workingDF <- get(paste0(examName, "_", seriesName, "_eventMatrix"), pos=1)
    
      # get the names for charts with CQs and RQs
      chartNames <- as.character(workingDF$chartName)
      
      # get the number of charts in the series
      numberCharts <- length(chartNames)
      
      # get the question tags
      uniqueEvents <- unique(as.vector(c(t(workingDF[,-c(1:2)]))))
      
      # and the number of uniqueEvents
      numberUniqueEvents <- length(uniqueEvents)
    
      # loop to read the stimulus text statements from each chart
      diffTable <- NULL # output data frame for differences
      diffSum <- 0 # number of differences 
      # j <- 1
      for (j in 1:numberCharts) {
        chartNameA <- chartNames[j]
        diffResult <- NULL
        
        # stimuliA <- read.csv(chartNameA, stringsAsFactors = FALSE)
        # stimTextA <- stimuliA$Statement
        stimuliA <- get(paste0(examName, "_Stimuli"), pos=1)
        stimTextA <- as.character(stimuliA$Statement[stimuliA$chartName==chartNameA])
        lengthA <- length(stimTextA)
        
        # nested loop to read each statement for each chart
        # k <- 1
        for (k in 1:numberCharts) {
          chartNameB <- chartNames[k]
          # stimuliB <- read.csv(chartNameB, stringsAsFactors = FALSE)
          # stimTextB <- stimuliB$Statement
          stimuliB <- get(paste0(examName, "_Stimuli"), pos=1)
          stimTextB <- as.character(stimuliB$Statement[stimuliB$chartName==chartNameB])
          matchText <- stimTextA %in% stimTextB
          lengthMatch <- length(which(matchText))
          diffResult <- c(diffResult, (lengthA == lengthMatch))
        } # end nested loop
        
        diffSum <- diffSum + length(which(diffResult == FALSE))
        diffTable <- cbind(diffTable, diffResult)
      
      } # end loop to read the stimulus text statements for each chart
      
      colnames(diffTable) <- chartNames
      
      # construct a warning if event statements are not identical for all charts
      if (diffSum > 0) {
        diffMessage <- paste0("02 Stimulus text is not identical for some charts of ", examName, "_", seriesName, ".")
        warningMessage <- c(warningMessage, diffMessage)
      } # end warning
        
    ### output
    
    # save the warning message
    if(length(warningMessage > 0)) {
      # name of the warning message
      warnName <- paste0(examName, "_", seriesName, "_stim_warnings")
      # make a vector in the global env
      assign(warnName, as.data.frame(warningMessage), pos=1)
      # save the warning message as txt
      fileName <- file(paste0(warnName, ".txt"), "w")
      writeLines(warningMessage, con=fileName)
      close(fileName)
    } # end if save warning message
    
    # set the output name for the txt and data frame
    outName <- paste0(examName,
                      "_", 
                      seriesName,
                      "_stim_text_warning")
    
    if(makeDF==TRUE) {
      assign(outName, as.data.frame(diffTable), pos = 1)
      
    } # end if makeDF
    
    if(saveCSV==TRUE) {
      write.table(diffTable, file = paste0(outName, ".csv"),
                  append = FALSE, 
                  quote = TRUE, 
                  sep = ",", 
                  eol = "\n", 
                  na = "NA", 
                  dec = ".", 
                  row.names = TRUE, 
                  col.names = TRUE, 
                  qmethod = "double", 
                  fileEncoding = "UTF-8")
    } # end if saveCSV
    
    } # end loop over each unique series
    
  } # end loop over each unique exam

} # end stimCheck()

# stimCheck(x="_Stimuli$", saveCSV=TRUE, makeDF=TRUE)
