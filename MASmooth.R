MASmooth <- function(x=chartData$Cardio1, y=7, times=2) {
  # function to get the diastolic peaks from the cardio data
  # x input is a time series vector
  # y input is the number of offset samples 
  # times is the number of times to recursively smooth the data
  # buffer will be double the offset value
  xOut <- c(rep(x[1], times=y), numeric(length=(length(x)-y)))
  input_buffer <- c(x[1:y], rep(x[y+1], times=y)) 
  # loop over the number of times
  for (j in 1:times) {
    # for loop to compute the moving average
    for (i in (y+1):(length(x)-(2*y))) {
      xOut[i-y] <- mean(input_buffer)
      input_buffer <- c(input_buffer[2:(2*y)], x[i])
    } # end for loop for moving average
    # re-initialize the input
    x <- xOut
    input_buffer <- c(x[1:y], rep(x[y+1], times=y))
  } # end loop over times
  return(na.omit(xOut))
} # end MASmooth function

