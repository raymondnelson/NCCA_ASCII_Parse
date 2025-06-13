# R function to process the time series data for PCASS
# including EDA and PLE sensors
# July 4, 2020
# Raymond Nelson

###########################




# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[2]




PCASS_SigProcFn <- function(x=uniqueExams,
                    y=uniqueExamNames,
                    makeDF=TRUE ) {
  # function to process PCAASS data  using NCCA ASCII output
  # PCASS includes only the EDA and vasomotor data
  # x input is a vector of unique exam names in the working directory
  # the input data is the output from the function in the preProc.R script and preProc() function
  # output=TRUE will output the time series for the last exam in the input vector of exam names
  # showNames=TRUE will print the exam names, series names, and chart names to the console
  #
  ####
  
  uniqueExams <- x
  
  #### private functions ####
  
  MASmooth <- function(x=myData, y=round(.25*cps,0), times=1) {
    # function to calculate a smoothed average of the time series data
    # x input is a time series vector
    # y input is the number of offset samples
    # times is the number of times to smooth the data
    ####
    # initialize the output vector 
    xOut <- x
    # loop over the number of times
    for (j in 1:times) {
      # buffer will be double the offset value + 1
      input_buffer <- x[1:(2*y+1)]
      # for loop to compute the moving average
      # starts at sample y + 1
      for (i in (y+1):(length(x)-y)) { 
        # replace the middle value of the buffer with the mean
        xOut[i] <- mean(input_buffer) 
        # increment the input buffer
        input_buffer <- c(input_buffer[2:length(input_buffer)], x[i+y+1])
      } 
    } 
    return(xOut)
  }
  
  lowPass.886 <- function(x, GAIN = 1.174704212e+01, zplane = 0.8297443748) {
    # low pass .886Hz filter is equivalent to a .5 sec moving average
    # used by the Lafayette legacy EDA filter
    # first order Butterworth filter
    # x is a column vector of the EDA time series data
    ####
    xv1 <- x[1]
    yv1 <- 0
    output <- rep(NA, length(x))
    for (i in 1:length(x)) {
      xv0 <- xv1
      xv1 <- x[i] / GAIN
      yv0 <- yv1
      yv1 <- (xv1 + xv0) + (zplane * yv0)
      output[i] <- yv1
      # output <- c(output, yv1)
    }
    return(output)
  } 

  highPass.04 <- function(x, GAIN = 1.004188815e+00, zplane = 0.9916573165) {
    # high pass .04Hz filter
    # used by the Lafayette legacy EDA filter
    # first order Butterworth filter
    # x is a column vector of the EDA time series data
    ####
    xv1 <- x[1]
    yv1 <- 0
    output <- rep(NA, length(x))
    for (i in 1:length(x)) {
      xv0 <- xv1
      xv1 <- x[i] / GAIN
      yv0 <- yv1
      yv1 <- (xv1 - xv0) + (zplane * yv0)
      output[i] <- yv1
    }
    return(output)
  } 
  
  fixPeak <- function(x=, times=4) {
    # crude function to fix peaks that are repeated on 2 or samples
    # could be more elegant with recursion
    # input x is the time series data for a sensor
    # times is the number of times to repeat, from 1 to 8
    ####
    if(length(which(is.na(x)))==length(x)) { return(x) }
    # this function will keep the first of all adjacent max and min peaks
    # and interpolate the subsequent equal peak with the next sample
    for (j in times) {
      for (i in 9:(length(x)-9)) {
        if(x[i]==x[(i-1)]) { x[i] <- mean(c(x[(i-1)], x[(i+1)])) }
        if(x[i]==x[(i-2)]) { x[i] <- mean(c(x[(i-2):(i-1)], x[(i+1)])) }
        if(x[i]==x[(i-3)]) { x[i] <- mean(c(x[(i-3):(i-1)], x[(i+1)])) }
        if(x[i]==x[(i-4)]) { x[i] <- mean(c(x[(i-4):(i-1)], x[(i+1)])) }
        if(x[i]==x[(i-5)]) { x[i] <- mean(c(x[(i-5):(i-1)], x[(i+1)])) }
        if(x[i]==x[(i-6)]) { x[i] <- mean(c(x[(i-6):(i-1)], x[(i+1)])) }
        if(x[i]==x[(i-7)]) { x[i] <- mean(c(x[(i-7):(i-1)], x[(i+1)])) }
        if(x[i]==x[(i-8)]) { x[i] <- mean(c(x[(i-8):(i-1)], x[(i+1)])) }
      }
    }
    return(x)
  } 
  
  NAInterp <- function(x) {
    # function to interpolate NA values in the time series data
    # x input is a vector of time series data
    ####
    # check if the entire input vector is NA
    if(length(which(is.na(x)))==length(x)) { return(x) }
    # fix the situation when the first value is NA
    if(is.na(x[1])) { x[1] <- x[min(which(!is.na(x)))] }
    # fix if the last value is NA
    if(is.na(x[length(x)])) { x[length(x)] <- x[max(which(!is.na(x)))] }
    # then use a loop to fix remaining values
    for(i in 2:(length(x)-1)) {
      if(is.na(x[i])) { x[i] <- mean(x[i-1],x[min(which(!is.na(x)))]) } 
    } 
    return(x)
  } 
  
  highPass.338628 <- function(data=chartDF$c_PL, GAIN = 1.035475913e+00, zplane = 0.9314790191) {
    # high pass filter at .338628Hz
    # to constrain the PLE data to a stable baseline
    # first order Butterworth filter
    ####
    xv1 <- data[1]
    yv1 <- 0
    output <- rep(NA, length(data))
    for (i in 1:length(data)) {
      xv0 <- xv1
      xv1 <- data[i] / GAIN
      yv0 <- yv1
      yv1 <- (xv1 - xv0) + (zplane * yv0)
      output[i] <- yv1
    }
    return(output)
  }
  
  ####  iterate over each exam in the list ####
  
  # i=1
  for(i in 1:length(uniqueExams)) {
    
    {
      
      examName <- uniqueExams[i]
      
      # get the names of time series lists for all unique series in each exam
      searchString <- paste0("*", examName, "_Data", "*")
      examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
      # examDF <- get(examName, pos=1)
      
      examOnsetRow <- 1
      examEndRow <- nrow(examDF)
      
      assign("examDF", examDF, pos=1)
      assign("examName", examName, pos=1)
      
      print(examName)
      
      # get the names of each unique series in the exam
      uniqueSeries <- as.character(unique(examDF$seriesName))
      
    }
    
    #### loop over each unique series ####
    
    # j=1
    for(j in 1:length(uniqueSeries)) {
      
      {
        
        seriesName <- uniqueSeries[j]
        
        # get the list of time series data for the charts in the exam
        # seriesDF <- examDF[examDF$seriesName==uniqueSeries[j],]
        seriesDF <- examDF[examDF$seriesName==seriesName,]
        
        # seriesOnsetRow <- which(examDF$seriesName==uniqueSeries[j])[1]
        seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
        seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
        
        assign("seriesDF", seriesDF, pos=1)
        assign("seriesName", seriesName, pos=1)
        
        print(paste("series", uniqueSeries[j]))
        
        uniqueCharts <- as.character(unique(seriesDF$chartName))
        
      }
    
      ####  loop over each chart in the series ####
      
      # k=3
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
          
          assign("chartDF", chartDF, pos=1)
          assign("chartName", chartName, pos=1)
          
          print(uniqueCharts[k])
          
          # make a vector of event names
          # used to get the first and last event 
          # eventNames <- toupper(chartDF$eventLabel[chartDF$eventLabel!=""])
          eventNames <- toupper(chartDF$Label[chartDF$Label!=""])
          
          # make a vector of row indices for each event
          eventIndices <- which(chartDF$Label!="")
          
        }
        
        #### get indices for the first event onset and last event end ####
        
        { 
          
          if(length(eventNames) == 0) {
            print("no stimulus events. none processed")
            # next()
            firstEvent=1
            lastEventEnd=nrow(chartDF)
          } 
          
          firstEvent <- getFirstLastEventFn(x=chartDF)[1]
          lastEventEnd <- getFirstLastEventFn(x=chartDF)[2]
          
          # fix condition where there are no events that are not excluded
          if(is.na(firstEvent)) {
            firstEvent <- 1
            lastEvent <- nrow(chartDF)
            lastEventEnd <- nrow(chartDF)
          }
          
          assign("firsEvent", firstEvent, envir = .GlobalEnv)
          assign("lastEventEnd", lastEventEnd, envir = .GlobalEnv)
          
        }
        
        #### PCASS EDA signal processing ####
        
        {
          
          print("  PCASS EDA signal processing")
          
          # fix NA values
          chartDF$c_PCASS_EDA <- NAInterp(chartDF$c_EDA1)
          
          # get the onset value in case it is not already zero
          onsetValue <- chartDF$c_PCASS_EDA[1]
          
          # locate the AutoEDA data at onset = zero
          chartDF$c_PCASS_EDA <- chartDF$c_PCASS_EDA - onsetValue
          
          # low pass smoothing filter
          chartDF$c_PCASS_EDA <- lowPass.886(x=chartDF$c_PCASS_EDA)
          
          # high pass centering filter
          chartDF$c_PCASS_EDA <- highPass.04(x=chartDF$c_PCASS_EDA)
          
          # additional smoothing to improve feature extraction
          chartDF$c_PCASS_EDA <- MASmooth(x=chartDF$c_PCASS_EDA, 
                                          y=8, 
                                          times=1)
          
          # relocate the filtered data to the onset value
          chartDF$c_PCASS_EDA <- chartDF$c_PCASS_EDA + onsetValue
          
        }
        
        #### PLE signal processing ####
        
        {
          
          print("  PCASS PLE signal processing")
          
          # use a filter the constrain the PLE data to a stable baseline
          chartDF$c_PCASSCardio <- highPass.338628(x=chartDF$PLE1)
          
        }
        
        ####
        
        # save the chartDF to the seriesDF
        seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        # could just save the chartDF to the examDF
        # examDF[(chartOnsetRow+seriesOnsetRow-1):(chartEndRow+seriesOnsetRow-1),]  <- chartDF
        
      } # end for loop over each k chart in each series
      
      # save the seriesDF to the examDF
      examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF 
      # not needed when the chartDF is saved directly to the examDF
      
    } # end loop over j unique series
    
    # save the examDF to the global environment
    if(makeDF==TRUE) assign(paste0(examName, "_Data"), examDF, pos=1)
    
  } # end loop over i unique exams
  
  print(paste(i, "exams processed"))
  
  return(uniqueExams)
  
} # end PCASS_SigProcFn() function






