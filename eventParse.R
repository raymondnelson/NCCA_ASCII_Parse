############# R Function to parsing NCCA ASCII header information ##############
# 6-12-2015 Raymond Nelson
# 
#
#
# this script contains the following functions
#
# eventParse()
# to identify charts with min 2 CQs and min 2 RQs
# will also make a warning message if each event is not present in all charts
#
# stimCheck()
# to to determine whether stimulus text is identical for questions on all charts
#
#########################################

library(stringr)

#########################################

# function to identify and keep charts that have 2 CQs and 2 RQs 
# and verify that all stimulus events 

eventParse <- function(x="_Stimuli$", saveCSV=TRUE, makeDF=TRUE, type=NULL) {
  # function to identify and keep charts that have 2 CQs and 2 RQs 
  # and verify that all stimulus events 
  # x is a regex string to identify the stimuli data frames
  # with 1 data frame with all event stimili for all charts for each exam
  #
  # type can be CQT, CIT, SPOT, RI, ACQT or NULL
  
  
  ####
  
  # first make a vector of names of event stimuli data frames
  stimDFs <- ls(pattern=x, pos=1)
  
  #### loop over the events ("*_Stimuli") for each exam
  # i <- 1 
  for (i in 1:length(stimDFs)) {
    print(stimDFs[i])
    # create an empty warning message
    
    # work with one of the stimuli data frames
    tempDF <- get(stimDFs[i])
    # str(workingDF)
    
    # get the exam name
    examName <- substr(stimDFs[i], start=1, stop=nchar(stimDFs[i])-8)
    
    # get the chart names
    uniqueCharts <- unique(as.character(tempDF[,3]))
    # length(uniqueCharts)
    
    # get the numbers of the unique series
    # uniqueSeriesNames <- unique(substr(uniqueCharts, start=1, stop=1))
    uniqueSeriesNames <- unique(as.character(tempDF[,2]))
    
    # not used at theis point
    
    #### make a loop for each unique series
    # h <- 1
    for (h in 1:length(uniqueSeriesNames)) {
      seriesName <- uniqueSeriesNames[h]
      
      # set the warningMessage to NULl
      warningMessage <- NULL
      
      # need to make separate data frames or add a column for the series
      workingDF <- tempDF[substr(tempDF$chartName, 
                                 start=1, 
                                 stop=1)==seriesName,]
      
      # get the chart names
      uniqueCharts <- unique(as.character(workingDF[,3]))
      
      # length(uniqueCharts)
      
      ### use a loop to get the event labels for each unique chart 
      # and make a vector for event labels for each unique chart
      eventVectorNames <- NULL
      numberEvents <- NULL
      # j <- 1
      for (j in 1:length(uniqueCharts)) {
        # get the vector of event lables for each chart
        eventVector <- workingDF[workingDF$chartName==uniqueCharts[j],]$Label
        # remove "" elements
        eventVector <- eventVector[which(eventVector != "")]
        # determine the number of events and concatenate it to a vector
        numberEvents <- as.character(c(numberEvents, length(eventVector)))
        # set the vector name and assign the lables to the vector
        dfName <- paste0(uniqueCharts[j], "_labels") # may need to append "x."
        assign(dfName, eventVector)
        # add the name of the event vector to a vector of names
        eventVectorNames <- c(eventVectorNames, dfName)
      } # end loop for unique charts
      
      ### create a warning message if the number of events not the same for all charts
      if (length(unique(numberEvents))!=1) {
        warningMessage <- c(warningMessage, "01 Charts in this series have different numbers of stimulus events.")
      } # end if
      
      ### a loop to fix the length of all event vectors to the max length for each exam
      maxEvents <- max(as.integer(numberEvents))
      numberEventVectors <- length(eventVectorNames)
      tempVector <- NULL
      # k <- 1
      for (k in 1:numberEventVectors) {
        tempVector <- get(eventVectorNames[k])
        tempVector <- c(tempVector, rep("-", maxEvents - length(tempVector)))
        # assign(paste("eventVector", k, sep = ""), tempVector)
        assign(eventVectorNames[k], tempVector)
        # maybe write the csv to include the padded events
        #
      } # end loop to fix the length of all event vectors to the max for each exam
      
      if (type=="CQT") {
        
        ### select charts with 2 or more CQs and RQs
        # also make a vector of all event labels for all charts with 2 CQs and 2 RQs
        
        retainEventVectorNames <- NULL # vector for the names of charts with 2 CQs and 2 RQs
        allEventNames <- NULL
        # l <- 1 # for testing
        for (l in 1:length(eventVectorNames)) {
          eventVectorName <- eventVectorNames[l] 
          RQs <- length(grep("R", get(eventVectorName), 
                             ignore.case = TRUE, 
                             perl = FALSE, 
                             value = FALSE,
                             fixed = FALSE, 
                             useBytes = FALSE, 
                             invert = FALSE)) >= 2 # make a logical scalar for RQs >= 2  
          CQs <- length(grep("C", get(eventVectorName), 
                             ignore.case = TRUE, 
                             perl = FALSE, 
                             value = FALSE,
                             fixed = FALSE, 
                             useBytes = FALSE, 
                             invert = FALSE)) >= 2 # make a logical scalar for CQs >= 2
          ## keep only those charts with >= 2 RQs and >= 2 CQs
          if (CQs & RQs) {
            # keep the vector
            retainEventVectorNames <- c(retainEventVectorNames, eventVectorNames[l])    
            # make a long vector of all events from all charts with 2 CQs and RQs
            allEventNames <- c(allEventNames, get(eventVectorNames[l]))
          } # end if
          ## warning if some charts in the series do not have >=2 CQs and >=2 RQs
          if(!CQs | !RQs) {
            warningMessage <- c(warningMessage, "02 Some charts in the series are excluded because they do not include >=2 CQs and >=2 RQs.")
          }
          
        } # end loop retain charts with min 2 CQs and min 2 RQs
        
      } # end if type == CQT
      
      # if (type==NULL) retainEventVectorNames <- tempVector # 6-14-15 need to fix this
      
      # make a vector of unique event names
      # unique events may be less than max events because of repetion
      uniqueEventNames <- unique(allEventNames)
      
      ### make an output data frame 
      tempOutVector <- NULL
      outputDF <- NULL
      # m <- 1
      for (m in 1:length(retainEventVectorNames)) {
        tempOutVector <- get(retainEventVectorNames[m])
        # add the chart name
        tempOutVector <- c(substr(retainEventVectorNames[m], 
                                  start=1, 
                                  stop=nchar(retainEventVectorNames[m])-7), 
                           tempOutVector)
        tempOutVector <- c(examName, tempOutVector)
        # add the exam name
        outputDF <- rbind(outputDF, tempOutVector)
      } # end for loop to make the output data frame
      outputDF <- as.data.frame(outputDF)
      row.names(outputDF) <- NULL
      names(outputDF) <- c("examName", 
                           "chartName", 
                           paste0("E", 1:(ncol(outputDF)-2)))
      
      #### ouput 
      
      # set the output name
      outName <- paste0(examName, "_", seriesName, "_eventMatrix")
      
      if (makeDF==TRUE) {
        assign(outName, outputDF, pos=1)
      }
      
      if (saveCSV==TRUE) {
        write.table(outputDF, file = paste0(outName, ".csv"), 
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
      }
      
      # save the warning message
      if(length(warningMessage > 0)) {
        # name of the warning message
        warnName <- paste0(examName, "_", seriesName, "_event_warnings")
        # make a vector in the global env
        assign(warnName, as.data.frame(warningMessage), pos=1)
        # save the warning message as txt
        fileName <- file(paste0(warnName, ".txt"), "w")
        writeLines(warningMessage, con=fileName)
        close(fileName)
      }
    } # end loop for each unique series
    
  } # end loop to parse the events for each exam
  
  # return(retainEventVectorNames)
  
} # end eventParse function

# eventParse(x="_Stimuli$", saveCSV=TRUE, makeDF=TRUE, type="CQT")

##################

# modify this to make a data frame of all events for all charts and series regardless 
# 
# of of the 
# 
# and specify the selection of CQT RI CIT or ACQT

###################

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


######################


# eventParse function make a stimulus matrix for the events
# for each series, keeping only the charts with >=2 CQs and >=2 RQS,
# and will set the length of the stimulus sequence to the max length
# for each series

eventParse(x="_Stimuli$", saveCSV=TRUE, makeDF=TRUE, type="CQT")


#######################


# function to determine whether stimulus text is identical for questions on all charts
# x input is a character string to identify the Stimuli data frames
# output is a data frame and csv warning regarding differences 

stimCheck(x="_Stimuli$", saveCSV=TRUE, makeDF=TRUE)

