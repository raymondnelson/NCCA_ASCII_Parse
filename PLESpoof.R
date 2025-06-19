# spoof the PLE data when it is missing

# function to smooth the time series data to the midpoint
MASmooth <- function(x=myData, y=round(.25*cps,0), times=5) {
  # function to calculate a smoothed average of the time series data
  # called by dataParse function in the NCCAASCIIParseHelperFunctions.R script
  # x input is a time series vector
  # y input is the number of offset samples
  # times is the number of times to smooth the 
  ###
  # make the output vector 
  # beginning and end of the output vector are the mean of 2 * the buffer at begin and end of x
  xOut <- x
  # loop over the number of times
  for (j in 1:times) {
    # for loop to compute the moving average
    # buffer will be double the offset value + 1
    input_buffer <- x[1:(2*y+1)]
    # starts at sample y + 1
    for (i in (y+1):(length(x)-y)) { 
      # replace the middle value of the buffer with the mean
      xOut[i] <- mean(input_buffer) 
      # increment the input buffer
      input_buffer <- c(input_buffer[2:length(input_buffer)], x[i+y+1])
    } 
    # replace the input vector
    x <- xOut
  } # end loop over times
  return(xOut)
} # end MASmooth function()


PLESpoofFn <- function(myDF=myDF, samples=15, times=2) {
  # function to spoof missing PLE sensor data
  # called by the dataParse function that is called by NCCAASCIIParse function
  # controlled by the PLESpoof=TRUE switch in the workFlow.R script
  #
  # myDF input is a data frame 
  # requires the MASmooth function
  #
  ####
  
  # subtract the cardio mid line from the cardio data to constrain the cardio to baseline
  myDF$PPG1 <- myDF$Cardio1 - MASmooth(x=myDF$Cardio1, 22, 2)
  
  # fix NA values
  myDF$PPG1[is.na(myDF$PPG1)] <- min(myDF$PPG1[100:(nrow(myDF)-100)], na.rm=TRUE)

  # fix the beginning and end
  myDF$PPG1[1:22] <- myDF$PPG1[23:44]
  myDF$PPG1[(nrow(myDF)-21):nrow(myDF)] <- myDF$PPG1[(nrow(myDF)-44):(nrow(myDF)-23)]
  
  # assign("myDF", myDF, envir=.GlobalEnv)
  return(myDF)
  
} # end PLESpoofFn()
