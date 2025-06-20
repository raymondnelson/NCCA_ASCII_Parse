# R function to fix the DLST/TES question lables
# June, 20, 2025
# Raymond Nelson
#
# called by the workflow.R script 
#
####

# source(paste0(RPath, 'toMinSec.R'), echo=FALSE)


fixDLSTLabelsFn <- function(x="*_Data$", y="_Stimuli$", pre=8, post=20) {
  # R function to fix the DLST/TES question labels in the time series data and events table
  # June, 20, 2025
  # Raymond Nelson
  #
  # 1C1, 1R1, 1R2, 1C2, 2R1, 2R2, 2C1, 3R1, 3R2, 2C2, 4R1, 4R2, 3C1
  # necesary when examiners use simple question labels 
  # C1, R1, R1, C2, R1, R2, C1, R1, R2, C2, R1, R2, C1
  #
  # called by the workflow.R script 
  #
  # will iterate over the exams in the global environment
  #
  # visible output is a vector of exam names
  # 
  # x input is the time series data frames
  #
  # y input is the stimulus event data frame
  #
  ####
  
  {
    
    if(!exists("x")) x <- "*_Data$"
    if(!exists("y")) y <- "_Stimuli$"
    
    dataNames <- unique(str_sub(ls(pattern=x, pos=1),1 , -6))
    stimuliNames <- unique(str_sub(ls(pattern=y, pos=1),1 , -9))
    
    # stop if unequal 
    if(!all(dataNames == stimuliNames)) {
      # if(length(stimuliNames) > length(dataNames)) {
      #   
      # }
      
      stop()
      
    }
    
  }
  
  #### iterate over each exam in the list ####
  
  i=1
  for(i in 1:length(dataNames)) {
    
    {
      
      #### get the data and stimuli frame for this exam ####
      
      examName <- dataNames[i]
      print(examName)
      # get the names of time series lists for all unique series in each exam
      
      dataSearchString <- paste0("*", examName, "_Data", "*")
      
      stimuliSearchString <- paste0("*", examName, "_Stimuli", "*")
      
      # get the time series data for this exam
      examDF <- get(glob2rx(dataSearchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
      if(is.null(examDF)) return()
      # View(examDF)
      
      assign("examDF", examDF, envir=.GlobalEnv)
      
      examDF$examName <- as.character(examDF$examName)
      examDF$seriesName <- as.character(examDF$seriesName)
      examDF$chartName <- as.character(examDF$chartName)
      
      # get the stimulus data fram for the exam
      stimuliDF <- get(glob2rx(stimuliSearchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
      if(is.null(stimuliDF)) return()
      
      stimuliDF$Begin <- as.character(stimuliDF$Begin)
      stimuliDF$End <- as.character(stimuliDF$End)
      stimuliDF$Answer <- as.character(stimuliDF$Answer)
      
      newStimuliDF <- stimuliDF
      
      uniqueSeries <- unique(examDF$seriesName)
      
      # initialize new object for the _Data
      newExamDF <- examDF
      
    }
    
    #### slice and iterate over each unique series ####
    
    j=1
    for(j in 1:length(uniqueSeries)) {
      
      {
        
        seriesRows <- which(examDF$seriesName == uniqueSeries[j])
        
        seriesDF <- examDF[seriesRows,]
        
        assign("seriesDF", seriesDF, envir=.GlobalEnv)
        
        uniqueCharts <- unique(seriesDF$chartName)
        
      }
      
      #### slice and iterate over each chart ####
      
      k=1
      for(k in 1:length(uniqueCharts)) {
        
        {
          
          # slice the chart data
          chartRows <- 
            which(examDF$seriesName == uniqueSeries[j] & examDF$chartName == uniqueCharts[k])
          chartDF <- examDF[chartRows,]
          # View(chartDF)
          
          assign("chartDF", chartDF, envir=.GlobalEnv)
          
          # slice the events for this series and this chart
          theseChartEventRows <- 
            which(stimuliDF$seriesName == uniqueSeries[j] & 
                    stimuliDF$chartName == uniqueCharts[k])
          chartEventsDF <- stimuliDF[theseChartEventRows,]
          
          # ensure the data are numeric
          chartEventsDF$Begin <- as.integer(chartEventsDF$Begin)
          chartEventsDF$End <- as.integer(chartEventsDF$End)
          chartEventsDF$Answer <- as.integer(chartEventsDF$Answer)
          
          # View(chartDF)
          # View(chartEventsDF)
          
          # initialize some new data frames
          newChartDF <- chartDF
          newChartEventsDF <- chartEventsDF
          
        }
        
        #### get the CQs and RQs ####
        
        {
          
          allEventDFLabels <- chartEventsDF$Label
          
          allDataLabels <- unique(chartDF$Label)
          allDataLabels <- allDataLabels[!(allDataLabels %in% c("", "NO","YES", "ANS"))]
          
          # stop if unequal
          if(any(!(allDataLabels %in% allEventDFLabels), !(allEventDFLabels %in% allDataLabels))) {
            stop("problem with question labels")
          }
          
          # get the CQs
          CQLabels <- allEventDFLabels[grep("C", allEventDFLabels)]
          
          # get the RQs
          RQLabels <- allEventDFLabels[grep("R", allEventDFLabels)]
          
          uniqueCQs <- unique(CQLabels)
          uniqueRQs <- unique(RQLabels)
          
          if(length(uniqueCQs) == 0 || length(uniqueRQs) == 0) next()
          
          if(str_sub(uniqueCQs[1], 1, 1) == "1" && str_sub(uniqueRQs[1], 1, 1) == "1") {
            # next chart if the DLST labels are already correct
            next()
          }
        }
        
        #### compute the corrected DLST labels    ####
        
        {
          
          newEventDFLabels <- allEventDFLabels
          
          # RQCount <- rep(NA, times=length(uniqueRQs))
          # CQCount <- rep(NA, times=length(uniqueCQs))
          # 
          # l=1
          # for(l in 1:length(uniqueRQs)) {
          #   RQCount[l] <- length(which(RQLabels == uniqueRQs[l]))
          # }
          # 
          # l=1
          # for(l in 1:length(uniqueCQs)) {
          #   CQCount[l] <- length(which(CQLabels == uniqueCQs[l]))
          # }
          
          CQCounter1 <- 0
          CQCounter2 <- 0
          
          m=1
          for(m in 1:length(newEventDFLabels)) {
            thisEvent <- newEventDFLabels[m]
            # next iteration if not CQ
            if(!(grepl("C", thisEvent))) next()
            if(thisEvent == uniqueCQs[1]) {
              CQCounter1 <- CQCounter1 + 1
              newCQLabel <- paste0(CQCounter1, thisEvent)
              newEventDFLabels[m] <- newCQLabel
            }
            if(thisEvent == uniqueCQs[2]) {
              CQCounter2 <- CQCounter2 + 1
              newCQLabel <- paste0(CQCounter2, thisEvent)
              newEventDFLabels[m] <- newCQLabel
            }
          }
          
          RQCounter1 <- 0
          RQCounter2 <- 0
          
          m=1
          for(m in 1:length(newEventDFLabels)) {
            thisEvent <- newEventDFLabels[m]
            # next iteration if not CQ
            if(!(grepl("R", thisEvent))) next()
            if(thisEvent == uniqueRQs[1]) {
              RQCounter1 <- RQCounter1 + 1
              newRQLabel <- paste0(RQCounter1, thisEvent)
              newEventDFLabels[m] <- newRQLabel
            }
            if(thisEvent == uniqueRQs[2]) {
              RQCounter2 <- RQCounter2 + 1
              newRQLabel <- paste0(RQCounter2, thisEvent)
              newEventDFLabels[m] <- newRQLabel
            }
          }
          
          # newEventDFLabels now has the correct question labels
          # allEventDFLabels has the old question labels
          
        }
        
        #### replace the labels in the time chartEventsDF ####
        
        {
          
          chartEventsDF$eventLabel <- newEventDFLabels
          chartEventsDF$Label <- newEventDFLabels
          
        }
        
        #### replace the labels in the time series data         
        
        {
          
          # use the newEventDFLabels vector to locate the _Data rows for each event
          
          n=1
          for(n in 1:length(newEventDFLabels)) {
            thisEvent <- newEventDFLabels[n]
            # next event if not CQ or RQ
            if(!(grepl("[CR]", thisEvent[1]))) next()
            # the the data rows from the chartEventsDF
            thisEventRow <- which(chartEventsDF$eventLabel == thisEvent)
            thisEventBeginRow <- chartEventsDF$Begin[thisEventRow]
            theseDataRows <- c(chartEventsDF$Begin[thisEventRow]:chartEventsDF$End[thisEventRow])
            # edit the label in the time series data
            chartDF$eventLabel[thisEventBeginRow] <- thisEvent
            chartDF$Label[theseDataRows] <- thisEvent
          }
          
          # unique(chartDF$eventLabel)
          
        }
        
        #### submit the newChartDF for output ####
        
        newExamDF[chartRows,] <- chartDF
        
        stimuliDF[theseChartEventRows,] <- chartEventsDF
        
      } # end k loop over charts
      
    } # end j loop over series
    
    # View(newExamDF)
    # View(newStimuliDF)
    
    #### assign the trimmed _Data to the global envir ####
    
    examDFName <- str_sub(dataSearchString, 2, -2)
    assign(examDFName, newExamDF, envir=.GlobalEnv)
    
    #### assign the corrected _Stimuli to thee global envir ####
    
    stimuliDFName <-  str_sub(stimuliSearchString, 2, -2)
    assign(stimuliDFName, stimuliDF, envir=.GlobalEnv)
    
  } # end i loop over exams
  
  return(dataNames)
  
} # end fixDLSTLabelsFn()



# fisDLSTLabelsFn(x="*_Data$", y="_Stimuli$")

