# R function to identify unresponsive data
# 12-20-2016
# Raymond Nelson
# 
######

EDACheckFn <- function(x=chartDF$c_AutoEDA, 
                       sec=5, 
                       times=30, 
                       omit=10, 
                       firstRow=NULL, 
                       lastRow=NULL, 
                       sVal=200) {
  # helper function to check for unresponsive EDA 
  # 12/20/2016
  # Raymond Nelson
  ### input
  # x input is the time series EDA vector from the chartDF data frame
  # sec is the number of seconds to sample
  # times is the number of random samples to get
  # ignore is an integer that specifies the number of largest samples,
  # to ignore in the scaling calcluation
  # firstRow is the index of the onset of the first stimulus event
  # lastRow is the endex of the end of the scoring window for the last stimulus event
  # sVal is the scale value to use when determining small responses
  ### output
  # output is one of two messages: "none", "possible unresponsive EDA"
  ##########
  tsData <- as.numeric(x)
  dataLength <- length(tsData)
  if(is.null(firstRow)) firstRow <- 1
  if(is.null(lastRow)) lastRow <- dataLength
  # set the default output message
  outputMessage <- "none"
  ### 
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
    # make a data frame for the sample segments
    DF <- as.data.frame(matrix(NA, nrow=times, ncol=sampleLength))
    # the data frame rows will be the samples from the time series input 
    # populate the data frame
    for (i in 1:times) {
      DF[i,] <- tsData[sampleOnset[i]:sampleOffset[i]]
    }
    # View(DF)
    # make a private function to get the range from the DF rows
    dfRangeFn <- function(x) { max(x)-min(x) }
    # apply the range function to each row of the DF to get the range of each row
    # apply is vectorized and needs no loop
    dfRange <- apply(DF, 1, dfRangeFn)
    # sort and remove the largest changes using the omit input parameter 
    dfRange <- sort(dfRange, decreasing = TRUE)[(omit+1):length(dfRange)]
    # calculate the mean dfRange
    dfRangeMean <- mean(dfRange)
    # set the output message if the mean range is very small
    if(dfRangeMean <= sVal/10) outputMessage <- "possible unresponsive data"
  } else {
    # for charts < 35 seconds
    if(max(tsData) == min(tsData)) {
      outputMessage <- "possible unresponsive EDA"
    } 
  } # end else for charts < 35 seconds
  # output
  print(outputMessage)
  if(outputMessage != "none") outputMessage <- paste(outputMessage, "chart", chartName) 
  assign("edaWarning", outputMessage, pos=1)
  return(NULL)
} # end EDACheckFn()  


# EDACheckFn(x=chartDF$c_AutoEDA, sec=5, times=25, omit=5, firstRow=NULL, lastRow=NULL)

