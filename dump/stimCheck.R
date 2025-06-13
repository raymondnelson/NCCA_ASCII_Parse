############# R Function to parse NCCA ASCII header information ##############
# 6-12-2015 Raymond Nelson 7-28-2015
# 
#
#
# this script contains the following functions
#
# stimCheck()
# to to determine whether stimulus text is identical for questions on all charts
#
#########################################


# library(stringr)


###################



# function to determine whether stimulus text is identical for questions on all charts

# x="_Stimuli$"
# saveCSV=TRUE
# makeDF=TRUE

stimCheck <- function(x="_Stimuli$") {
  # function to determine whether stimulus text is identical for questions on all charts
  # x input is a character string to identify the Stimuli data frames
  # output is a data frame and csv warning regarding differences 
  
  #   stimNames <- list.files(path = ".", pattern = x, 
  #                            all.files = FALSE, 
  #                            full.names = FALSE, 
  #                            recursive = FALSE, 
  #                            ignore.case = FALSE, 
  #                            include.dirs = FALSE)
  
  library(stringr)
  
  stimNames <- ls(pattern=x, pos=1)
  
  if(length(stimNames) > 0) {
    
    # add a parameter to load the .Stimuli data if necessary
    
    # uniqueExamNames <- unique(str_sub(stimNames, 1L, -9L)) # not needed
    
    # loop over each unique exam
    # i=1
    for (i in 1:length(stimNames)) {
      # make a list of charts for each unique series  
      
      # get the unique exam name
      # examName <- strtrim(stimNames[i], (nchar(stimNames[i]) - 8)
      examName <- str_sub(stimNames[i], start=1, end=nchar(stimNames[i])-8)
      
      # name <- paste(uniqueExamNames[h], "....", "_stimuli.csv", sep = "") # not needed
      
      # get the data frame 
      examDF <- get(stimNames[i], pos=1)
      
      # get the chart names
      # uniqueCharts <- unique(as.character(examDF[,3]))
      # length(uniqueCharts)
      
      # get the numbers of the unique series
      # uniqueSeriesNames <- unique(substr(uniqueCharts, start=1, stop=1))
      uniqueSeriesNames <- as.character(unique(examDF$seriesName))
      # not used at theis point
      
      #############
      
      seriesDF <- NULL
      
      #### make a loop for each unique series
      # j=1
      for (j in 1:length(uniqueSeriesNames)) {
        seriesName <- uniqueSeriesNames[j]
        
        # set the warningMessage to NULl
        warningMessage <- NULL
        
        # check to see if stimulus text are available and set a warning if not
        if(identical(as.character(examDF$Label), as.character(examDF$Statement))==TRUE) {
          warningMessage <- c(warningMessage, paste("01 Stimulus text not available.", 
                                                    "Stimuli are assumed identical for all charts in series",
                                                    seriesName))
        }
        
        # make a working data frame from the event matrix
        seriesDF <- get(paste0(examName, "_", seriesName, "_eventMatrix"), pos=1)
        
        # skip the series if no charts in the eventMatrix
        if(is.null(seriesDF)) next()
        
        # get the names for charts with CQs and RQs
        chartNames <- as.character(seriesDF$chartName)
        
        # get the number of charts in the series
        numberCharts <- length(chartNames)
        
        # get the unique question tags for all charts in the series
        uniqueEvents <- unique(as.vector(c(t(seriesDF[,-c(1:2)]))))
        
        # and the number of uniqueEvents
        numberUniqueEvents <- length(uniqueEvents)
        
        ############
        
        # loop to read the stimulus text statements from each chart
        diffTable <- NULL # output data frame for differences
        diffSum <- 0 # number of differences 
        # k=1
        for (k in 1:numberCharts) {
          # compare each chart to the others
          chartNameA <- chartNames[k]
          diffResult <- NULL
          
          # stimuliA <- read.csv(chartNameA, stringsAsFactors = FALSE)
          # stimTextA <- stimuliA$Statement
          # get the _Stimuli data frame with all text stimuli for all charts 
          stimuliA <- get(paste0(examName, "_Stimuli"), pos=1)
          # get all text statements for the current chart k
          stimTextA <- as.character(stimuliA$Statement[stimuliA$chartName==chartNameA])
          lengthA <- length(stimTextA)
          
          # nested loop to read each statement for each chart
          # k <- 1
          for (k in 1:numberCharts) {
            chartNameB <- chartNames[k]
            # stimuliB <- read.csv(chartNameB, stringsAsFactors = FALSE)
            # stimTextB <- stimuliB$Statement
            # get a data frame of all stimuli for all charts in the series
            stimuliB <- get(paste0(examName, "_Stimuli"), pos=1)
            # get the text statements for the current chart k
            stimTextB <- as.character(stimuliB$Statement[stimuliB$chartName==chartNameB])
            # compare A and B charts
            matchText <- stimTextA %in% stimTextB
            lengthMatch <- length(which(matchText))
            # cat the result with the diffResult
            diffResult <- c(diffResult, (lengthA == lengthMatch))
          } # end nested loop
          
          # sum the number of differences found
          diffSum <- diffSum + length(which(diffResult == FALSE))
          
          # bind the diffResult to the diffTable
          diffTable <- cbind(diffTable, diffResult)
          
          # probably this can be reduced by half instead of comparing every chart to every other
          
        } # end loop over k charts
        
        colnames(diffTable) <- chartNames
        rownames(diffTable) <- chartNames
        
        # construct a warning if event statements are not identical for all charts
        if (diffSum > 0) {
          diffMessage <- paste0("02 Stimulus text is not identical for some charts in series", seriesName)
          warningMessage <- c(warningMessage, diffMessage)
        } # end warning
        
        ### output
        
        # save the warning message
        if(length(warningMessage > 0)) {
          # name of the warning message
          warnName <- paste0(examName, "_", seriesName, "_stim_warnings")
          # make a vector in the global env
          assign(warnName, as.data.frame(warningMessage), pos=1)
          if(saveCSV==TRUE) {
            # save the warning message as txt
            fileName <- file(paste0(warnName, ".txt"), "w")
            writeLines(warningMessage, con=fileName)
            close(fileName)
          } # end if save CSV
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
                      row.names = FALSE, 
                      col.names = TRUE, 
                      qmethod = "double", 
                      fileEncoding = "UTF-8")
        } # end if saveCSV
        
      } # end loop over each j unique series
      
    } # end loop over each i unique exam
    
  } # end if(length(stimNames) > 0)
  
  print(paste("stimulus events checked for", i, "exams"))
  
} # end stimCheck()

# stimCheck(x="_Stimuli$")


