# R function to identify unresponsive data
# 12-20-2016
# Raymond Nelson
# 
######

dataCheckFn <- function(x=chartDF$c_AutoEDA, 
                       sec=10, 
                       times=30, 
                       omit=10, 
                       firstRow=NULL, 
                       lastRow=NULL, 
                       sVal=40 ) {
  # R function to check for unresponsive EDA data
  # 12/20/2016
  # Raymond Nelson
  ####
  #
  # x input is the time series EDA vector from the chartDF data frame
  # sec is the number of seconds to sample
  # times is the number of random sample segments to get from the time series data
  # ignore is an integer that specifies the number of largest sample segments,
  # to ignore in the scaling calcluation
  # firstRow is the index of the onset of the first stimulus event
  # lastRow is the index of the end of the scoring window for the last event
  # sVal is the scale value to use when determining small responses
  # sVal default is 2% of the y axis range
  #
  # output is one of two messages: "none", "unresponsive data"
  #
  #### 
  
  {
    tsData <- as.numeric(x)
    dataLength <- length(tsData)
    if(is.null(firstRow)) firstRow <- 1
    if(is.null(lastRow)) lastRow <- dataLength
  
    # initialize the default output message
    outputMessage <- "none"
  }

  if(dataLength >= 35 * cps) {
    # for charts >=  35 seconds
    # sample the data
    
    # first set the number of indices to include in each sample
    sampleLength <- sec * cps 
    
    # get the sample onset row indices using random numbers
    sampleOnset <- sample(c(firstRow:(lastRow-sampleLength)), 
                          size=times, 
                          replace=TRUE )
                          
    # then get the sample offset indices
    sampleOffset <- sampleOnset + sampleLength - 1
    
    # initialize a data frame for the sample segments
    DF <- as.data.frame(matrix(NA, nrow=times, ncol=sampleLength))
    # the data frame rows will hold the samples from the time series input 
    
    # populate the data frame
    for(i in 1:times) {
      DF[i,] <- tsData[sampleOnset[i]:sampleOffset[i]]
    } 
    # View(DF)
    
    # define a private function to get the range from the DF rows
    dfRangeFn <- function(x) { abs(max(x)-min(x)) }
    
    # apply the private dfRangeFn to each row of the DF to get the range of each row
    # apply is vectorized and needs no loop
    dfRange <- apply(DF, 1, dfRangeFn)
    
    # sort and remove the largest changes using the omit input parameter 
    dfRange <- sort(dfRange, decreasing = TRUE)[(omit+1):length(dfRange)]
    
    # calculate the mean dfRange
    dfRangeMean <- mean(dfRange)
    
    # set the output message if the mean range is very small
    if(dfRangeMean <= sVal) outputMessage <- "unresponsive data"
  } else {
    # for charts < 35 seconds
    
    
    if(max(tsData) == min(tsData)) {
      outputMessage <- "unresponsive data"
    }
    if(	abs(max(tsData) - min(tsData)) <=  sVal ) {
     	outputMessage <- "unresponsive data"
    }	
  } # end else for charts < 35 seconds
  
  #### output
  
  # print the output message if the data are unresponsive
  if(outputMessage != "none") print(outputMessage)
  # if(outputMessage != "none") outputMessage <- paste(outputMessage, "chart", chartName) 
  
  assign("dataWarning", outputMessage, pos=1)
  
  # chartDF[columnName] <- outputMessage
  
  ####
  
  if(outputMessage != "none") {
    return(outputMessage)
  } else {
    return(0)
  }
  
} # end dataCheckFn()  


# dataCheckFn(x=chartDF$c_AutoEDA, 
#             sec=5, 
#             times=30, 
#             omit=10, 
#             firstRow=NULL, 
#             lastRow=NULL, 
#             sVal=200)

