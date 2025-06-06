# R function to spoof a missing X and XX announcement 
# 5/7/2021
# Raymond Nelson

###########################



# X and XX announcements may not be exported by some polygraph systems





spoofXXXFn <- function(uniqueExams=uniqueExams) {
  # to spoof a missing X and XX announcement 
  # 5/7/2021
  # Raymond Nelson
  #
  # X and XX announcements may not be exported by some polygraph systems
  # 
  # uniqueExams is a vector of exam names in the CWD
  #
  ####
  
  ####  loop over each exam in the list ####
  
  i=1
  for(i in 1:length(uniqueExams)) {
    
    {
      
      examName <- uniqueExams[i]
      
      # get the names of time series lists for all unique series in each exam
      searchString <- paste0("*", examName, "_Data", "*")
      examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
      # examDF <- get(examName, pos=1)
      
      examOnsetRow <- 1
      examEndRow <- nrow(examDF)
      
      # assign("examDF", examDF, pos=1)
      # assign("examName", examName, pos=1)
      
      if(showNames==TRUE) print(paste("exam:", i, "of", length(uniqueExams), examName))
      
      # get the names of each unique series in the exam
      uniqueSeries <- as.character(unique(examDF$seriesName))
      
      {
        
        ####   get the stimulus events data frame for the exam   ####
        
        # if(exists("eventDF")) rm("eventDF")
        eventDFName <- paste0(examName, "_Stimuli")
        if(exists(eventDFName)) {
          eventDF <- get(eventDFName, pos=1)
        } #  commented out 8-4-2017 else next()
        
        # set these to numeric
        eventDF$Begin <- as.numeric(eventDF$Begin)
        eventDF$End <- as.numeric(eventDF$End)
        eventDF$Answer <- as.numeric(eventDF$Answer)
        
        # View(eventDF)
        
        # change the data types - this should be done during parsing for efficiency
        # eventDF$seriesName <- as.character(eventDF$seriesName)
        # eventDF$Event <- as.numeric(eventDF$Event)
        # eventDF$Begin <- as.numeric(eventDF$Begin)
        # eventDF$End <- as.numeric(eventDF$End)
        # eventDF$Answer <- as.numeric(eventDF$Answer)
        
        # reassign the _Stimuli and eventDF data frames
        # assign(paste0(examName, "_Stimuli"), eventDF, pos=1)
        # assign("eventDF", eventDF, pos=1)
        # View(eventDF)
        
        # initialize a newEventDF for this exam
        newEventDF <- NULL
        
      }
      
    }
    
    ######## loop over each unique series ########
    
    j=1
    for(j in 1:length(uniqueSeries)) {
      
      {
        
        seriesName <- uniqueSeries[j]
        
        # get the list of time series data for the charts in the exam
        # seriesDF <- examDF[examDF$seriesName==uniqueSeries[j],]
        seriesDF <- examDF[examDF$seriesName==seriesName,]
        
        # seriesOnsetRow <- which(examDF$seriesName==uniqueSeries[j])[1]
        seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
        seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
        
        # assign("seriesDF", seriesDF, pos=1)
        # assign("seriesName", seriesName, pos=1)
        
        if(showNames==TRUE) print(paste("series", uniqueSeries[j]))
        
        uniqueCharts <- as.character(unique(seriesDF$chartName))
        
      }
      
      ######  loop over each chart in the series #######
      
      k=1
      for(k in 1:length(uniqueCharts)) {
        
        {
          
          chartName <- uniqueCharts[k]
          
          # get the data frame with the time series data for each chart in the series
          # chartDF <- seriesDF[seriesDF$chartName==uniqueCharts[k],]
          chartDF <- seriesDF[seriesDF$chartName==chartName,]
          
          if(nrow(chartDF)<600) next()
          
          # chartOnsetRow <- which(seriesDF$chartName==uniqueCharts[k])[1]
          chartOnsetRow <- which(seriesDF$chartName==chartName)[1]
          chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
          
          # assign("chartDF", chartDF, pos=1)
          # assign("chartName", chartName, pos=1)
          
          if(showNames==TRUE) print(uniqueCharts[k])
          
          # make a vector of event names
          # used to get the first and last event 
          # eventNames <- toupper(chartDF$eventLabel[chartDF$eventLabel!=""])
          eventNames <- toupper(chartDF$Label[chartDF$Label!=""])
          
          # make a vector of row indices for each event
          eventIndices <- which(chartDF$Label!="")
          
          # remove excluded events
          # eventIndices <- eventIndices[-which(eventNames %in% excludeEvents)]
          # eventNames <- eventNames[!(eventNames %in% excludeEvents)]
          
          #### get indices for the first event onset and last event end ####
          
          if(length(eventNames) == 0) {
            print("no stimulus events. none processed")
            # next()
            firstEvent=1
            lastEventEnd=nrow(chartDF)
          } 
          
          firstEvent <- getFirstLastEventFn(x=chartDF)[1]
          lastEventEnd <- getFirstLastEventFn(x=chartDF)[2] - 450
          lastEvent <- getFirstLastEventFn(x=chartDF)[2] - 450
          # assign("firstLastEvents", firstLastEvents, pos=1)
          
          # fix condition where there are no events that are not excluded
          if(is.na(firstEvent)) {
            firstEvent <- 1
            lastEvent <- nrow(chartDF)
            lastEventEnd <- nrow(chartDF)
          }
          # assign("firsEvent", firstEvent, envir = .GlobalEnv)
          # assign("lastEventEnd", lastEventEnd, envir = .GlobalEnv)
          
        }
        
        #### select the events for the current chart ####
        
        {
          
          if(!exists("eventDF")) {
            # sumbit the chartDF and increment to the next chart if no events
            examDF[useRows,] <- chartDF
            next()
          }
          # View(eventDF)
          
          # get the rows for this series and this chart
          # eventDF was loaded in the i loop for the exam
          useRows <- which(eventDF$chartName == chartName & 
                             eventDF$seriesName == seriesName)
          
          if(length(useRows) == 0) {
            # submit the chartDF to the examDF and increment the chart
            # cannot use useRows because this is the examDF not eventDF
            examDF[examDF$chartName==chartName & examDF$seriesName==seriesName,] <-
              chartDF
            next()
          }
          
          #### slice the events data frame for this chart ####
          
          chartEventsDF <- eventDF[useRows,]
          # View(chartEventsDF)
          # nrow(chartEventsDF)
          # chartEventsDF$Label
          # View(chartDF)
          
          # increment the k loop if no events
          if(nrow(chartEventsDF) == 0) {
            examDF[useRows,] <- chartDF
            next()
          }
          
          uniqueEventsChart <- unique(chartEventsDF$Label)
          
          allEventsChart <- chartEventsDF$Label
          
          # View(chartEventsDF)
          
        }
        
        ######## planned action for each chart here ########
        
        # checK the chartDF for X announcements
        
        if( all( !(c("X") %in% unique(chartEventsDF$Label)) ) ) {
          
          # spoof if X is missing
          
          # firstEvent
          # lastEvent
          
          # add the X to the first 2 to 4 seconds
          
          XLoc <- sample(c(60:120), 1)
          
          chartDF$Label[XLoc:(XLoc+1)] <- "X"
          chartDF$eventLabel[XLoc] <- "X"
          
          # add the XX to the last -8 to -6 seconds
          
          # XXLoc <- nrow(chartDF) + sample(c(-240:-180), 1)
          # 
          # chartDF$Label[XXLoc:(XXLoc+1)] <- "XX"
          # chartDF$eventLabel[XXLoc] <- "XX"
          
          # construct the X announcement row
          
          XRow <- 
            c(chartEventsDF[1,1:3], 1, "X", "X", XLoc, (XLoc+1), (XLoc+2), "X")
          names(XRow) <- names(chartEventsDF)
          
          # str_pad(1, 2, "left", "0")
          
          # construct the X announcement row
          
          # XXRow <- 
          #   c(chartEventsDF[1,1:3], (nrow(chartEventsDF)+2), "XX", "XX", XXLoc, (XXLoc+1), (XXLoc+2), "XX")
          # names(XXRow) <- names(chartEventsDF)
          
          # construct the eventd data frame and save it
          
          # thisChartEventsDF <- rbind(XRow, chartEventsDF, XXRow)
          thisChartEventsDF <- rbind(XRow, chartEventsDF)
          # View(thisChartEventsDF)
          
          # fix the event numbers
          thisChartEventsDF$Event <- 
            str_pad(c(1:nrow(thisChartEventsDF)), 2, "left", "0")
          
          # build up the new events data frame
          newEventDF <- rbind(newEventDF, thisChartEventsDF)
          
          # save the events data frame to the global envir
          assign(paste0(examName, "_Stimuli"), newEventDF, pos=1)
          
        } else {
          
          # if X is not missing
          
          thisChartEventsDF <- chartEventsDF
          
          # newEventDF <- rbind(newEventDF, chartEventsDF)
          
        } # end if for missing X
        
        # checK the chartDF for XX announcements
        
        if( all( !(c("XX") %in% unique(chartEventsDF$Label)) ) ) {
          
          # spoof if XX is missing
          
          # firstEvent
          # lastEvent
          
          # add the X to the first 2 to 4 seconds
          
          # XLoc <- sample(c(60:120), 1)
          # 
          # chartDF$Label[XLoc:(XLoc+1)] <- "X"
          # chartDF$eventLabel[XLoc] <- "X"
          
          # add the XX to the last -3 to -2 seconds
          
          XXLoc <- nrow(chartDF) + sample(c(-120:-90), 1)
          
          chartDF$Label[XXLoc:(XXLoc+25)] <- "XX"
          chartDF$eventLabel[XXLoc] <- "XX"
          
          # construct the X announcement row
          
          # XRow <- 
          #   c(chartEventsDF[1,1:3], 1, "X", "X", XLoc, (XLoc+1), (XLoc+2), "X")
          # names(XRow) <- names(chartEventsDF)
          
          # str_pad(1, 2, "left", "0")
          
          # construct the XX announcement row
          
          XXRow <- 
            c(chartEventsDF[1,1:3], (nrow(chartEventsDF)+2), "XX", "XX", XXLoc, (XXLoc+25), (XXLoc+27), "XX")
          names(XXRow) <- names(thisChartEventsDF)
          
          # construct the eventd data frame and save it
          
          # thisChartEventsDF <- rbind(XRow, chartEventsDF, XXRow)
          
          # use thisChartEventsDF instead of chartEventsDF Jun 6, 2023
          thisChartEventsDF <- rbind(thisChartEventsDF, XXRow)
          # View(thisChartEventsDF)
          
          # fix the event numbers
          thisChartEventsDF$Event <- 
            str_pad(c(1:nrow(thisChartEventsDF)), 2, "left", "0")
          
          # build up the new events data frame
          newEventDF <- rbind(newEventDF, thisChartEventsDF)
          
          # # save the events data frame to the global envir
          # assign(paste0(examName, "_Stimuli"), newEventDF, pos=1)
          
        } else {
          
          # if XX is not missing 
          
          newEventDF <- rbind(newEventDF, thisChartEventsDF)
          
        } # end if for missing XX
        
        ######## end of planned action for each chart #######
        
        #### save the chartDF to the seriesDF ####
        
        # seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        # save the chartDF to the examDF
        examDF[(chartOnsetRow+seriesOnsetRow-1):(chartEndRow+seriesOnsetRow-1),]  <- chartDF
        
      } # end for loop over each k chart in each series
      
      # save the seriesDF to the examDF
      # not needed when the chartDF is saved directly to the examDF
      # examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF 
      
    } # end loop over j unique series
    
    # save the examDF to the global environment
    assign(paste0(examName, "_Data"), examDF, pos=1)
    
    # save the events data frame to the global envir
    assign(paste0(examName, "_Stimuli"), newEventDF, pos=1)
    
  } # end loop over i unique exams
  
  print(paste(i, "exams processed"))
  
  return(uniqueExams)
  
} # end spoofXXXFn function



