testRangeFn <- function(xRange=seq(.7,.8,.005), 
                      yRange=seq(.8,.9,.005), 
                      size=100000) {
  # R function to check the proportion of values in 1 range that are greater than another
  
  outDAT <- rep(NA, length=size)
  
  xSample <- sample(xRange, size=size, replace=TRUE)
  
  ySample <- sample(xRange, size=size, replace=TRUE)
  
  for(i in 1:length(outDAT)) {
    outDAT[i] <- xSample[i] > ySample[i]
  }
  length(which(outDAT))
    
  return(length(which(outDAT)) / size)
}
