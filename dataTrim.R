# R function to trip excess time from _Data
# Aug 4, 2020
# Raymond Nelson
#
# called by the workflow.R script 
#
####

# source(paste0(RPath, 'toMinSec.R'), echo=FALSE)


dataTrimFn <- function(x="*_Data$", y="_Stimuli$", pre=8, post=20) {
  # R function to trip excess time from _Data
  # Aug 4, 2020
  # Raymond Nelson
  #
  # remove excess time prior to X and after XX
  # and adjust event indices
  #
  # called by the workflow.R script 
  #
  # will iterate over the exams in the global environment
  #
  # visible output is a vector of exam names
  # 
  # x input is the time series data frame
  #
  # y input is the stimulus event data frame
  #
  # pre input is the number of seconds before the first event
  # 
  # post input is the number of seconds after the last event onset
  #
  ####
  
  {
    
    if(!exists("x")) x <- "*_Data$"
    if(!exists("y")) y <- "_Stimuli$"
    
    if(!exists("Pre")) pre <- 8
    if(!exists("post")) post <- 20
    
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
      
      # initialize new object for the trimmed _Data
      newExamDF <- NULL
      
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
          chartRows <- which(seriesDF$chartName == uniqueCharts[k])
          chartDF <- seriesDF[chartRows,]
          
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
        
        #### trim excess time from the end of chart ####
        
        {
          
          # View(newChartDF)
          # View(newChartEventsDF)
          
          lastEventBegin <- chartEventsDF[nrow(chartEventsDF),'Begin']
          
          if(length(lastEventBegin) > 0) {
            # check if more than 25 seconds remains after last event onset
            postXXTime <- (nrow(chartDF) - as.numeric(lastEventBegin)) / cps
            
            # adjust the new chart DF if excess time after the last event
            if(postXXTime > post) {
              # select a new end for the exam
              lastSample <- lastEventBegin + sample( c(((post-3):post) * cps), 1)
              newChartDF <- newChartDF[(1:lastSample),]
            }
            
            # uncommented this Feb 22, 2025
            # # make sure that the XX ends before the chart end
            XXEnd <- nrow(newChartDF) - sample(c((1*cps),(2*cps)), 1)
            lastEventEnd <- as.numeric((newChartEventsDF$End[nrow(newChartEventsDF)]))
            if(lastEventEnd > XXEnd) {
              newChartDF$Label[(XXEnd+1):nrow(newChartDF)] <- ""
              thisChartEventsRow <- which(newChartEventsDF$Label == "XX")
              newChartEventsDF$End[nrow(newChartEventsDF)] <- XXEnd
              newChartEventsDF$Answer[nrow(newChartEventsDF)] <- XXEnd
            }
            
          }
          
        }
        
        #### trim excess time from the onset of chart ####
        
        {
          
          firstEventBegin <- chartEventsDF[1,'Begin']
          
          if( !is.na(firstEventBegin) && length(firstEventBegin) > 0 ) {
            preXTime <- (firstEventBegin - 1) / cps
            
            if(preXTime > pre) {
              # check the time before the first
              firstSample <- firstEventBegin - sample( c(((pre-3):pre) * cps), 1) 
              
              if(firstSample < 1) firstSample <- 1
              newChartDF <- newChartDF[(firstSample:nrow(newChartDF)),]
              
              {
                # fix the time scale
                # Mar 3, 2024
                source(paste0(RPath, 'toMinSec.R'), echo=FALSE)
                startTime <- fromMinSecFn(newChartDF$Time[1])
                newTime <- fromMinSecFn(newChartDF$Time) - startTime
                newTime <- toMinSecFn(as.numeric(newTime))
                newChartDF$Time <- newTime
              }
              
              # then adjust the events in the stimuli data frame
              newChartEventsDF$Begin <- 
                as.character((chartEventsDF$Begin - firstSample) + 1)
              newChartEventsDF$End <- 
                as.character((chartEventsDF$End - firstSample) + 1)
              newChartEventsDF$Answer <- 
                as.character((chartEventsDF$Answer - firstSample) + 1)
              
              newChartEventsDF$Answer[is.na(newChartEventsDF$Answer)] <- ""
              
              # pass the newChartEventsDF back to the simuli data frame ####
              newStimuliDF[theseChartEventRows,] <- newChartEventsDF
            }
            
          }
          
        }
        
        #### submit the newChartDF for output ####
        
        newExamDF <- rbind(newExamDF, newChartDF)
        
      } # end k loop over charts
      
    } # end j loop over series
    
    # View(newExamDF)
    # View(newStimuliDF)
    
    #### assign the trimmed _Data to the global envir ####
    
    examDFName <- str_sub(dataSearchString, 2, -2)
    assign(examDFName, newExamDF, envir=.GlobalEnv)
    
    #### assign the corrected _Stimuli to thee global envir ####
    
    stimuliDFName <-  str_sub(stimuliSearchString, 2, -2)
    assign(stimuliDFName, newStimuliDF, envir=.GlobalEnv)
    
  } # end i loop over exams
  
  return(dataNames)
  
} # end dataTrimFn()

# dataTrimFn(x="*_Data$", y="_Stimuli$")

