# R functions to compute the slope direction of a time series input vector
# August 19, 2023
# Raymond Nelson

# abstracted from the amplitudeExtractPCFn()

getTheSlopeFn <- function(tsData) {
  # August 19, 2023
  # private function to get the slope direction for a time series input vector
  diff1 <- diff(tsData)
  return(c(0, ifelse(diff1==0,
                     # ifelse is vectorized and requires no control loop
                     theSlope <- 0,
                     ifelse(diff1>0,
                            theSlope <- 1,
                            theSlope <- -1) ) ) )
}

getPosSlopeFn <- function(theSlope) {
  # August 19, 2023
  # private function to get the positive slope segments of a time series input vector
  return(c(ifelse(theSlope[2:length(theSlope)] == 1,
                     # check every value + preceding
                     ifelse( ( theSlope[2:length(theSlope)] +
                                 theSlope[1:(length(theSlope)-1)] ) < 2,
                             1,
                             0 ),
                     0 ), 0) )
}

getNegSlopefn <- function(theSlope) {
  # private function to get the negative slope segments of a time series input vector
  # August 19, 2023
  negSlope <- ifelse(theSlope[2:length(theSlope)] == -1,
                     # ifelse is vectorized and needs no control loop
                     # locate the onset of neg slope
                     # by checking each slope value with the next
                     ifelse(( theSlope[2:length(theSlope)] +
                                theSlope[1:(length(theSlope)-1)] ) == -2,
                            xPeak <- 0,
                            xPeak <- -1),
                     xPeak <- 0) 
  return(c(negSlope, 
           # Feb 10,2023 select the peak not the point after the peak
           0 ))
}

