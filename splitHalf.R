
SplitHalfTukey <- function(x=minMaxAmp) {
  # function to check the split halfs against each other
  # x is a vector of input measurements
  
  # outputVector <- rep(NA, times=length(x))
  outputVector <- NULL
  
  split.row <- ceiling(length(x) / 2)

  first.x <- x[1:(split.row)]
  second.x <- x[(split.row+1):length(x)]
  
  i <- length(first.x)
  
  # use an if condition to call the function recursively
  if(i > 8) {
    print(paste("segment length:", i))
    # call this function recursively
    first.out <- SplitHalfTukey(x=first.x)
    # print(first.out)
    second.out <- SplitHalfTukey(x=second.x)
    # print(second.out)
    both.out <- sort(c(first.out, (second.out + length(first.x))))
    print(both.out)
    outputVector <- both.out
    # print(outputVector)
  }
    
  first.mean <- mean(first.x)
  first.q25 <- quantile(first.x, .25)
  first.q75 <- quantile(first.x, .75)
  first.iq.range <- first.q75 - first.q25
  
  second.mean <- mean(second.x)  
  second.q25 <- quantile(second.x, .25)
  second.q75 <- quantile(second.x, .75)
  second.iq.range <- second.q75 - second.q25 
  
  first.low.in <- which(first.x <= second.q25 - 1.5 * second.iq.range)
  first.up.in <- which(first.x >= second.q75 + 1.5 * second.iq.range)
  first.low.out <- which(first.x <= second.q25 - 3 * second.iq.range)
  first.up.out <- which(first.x >= second.q75 + 3 * second.iq.range)
  
  second.low.in <- which(second.x <= first.q25 - 1.5 * first.iq.range)
  second.up.in <- which(second.x >= first.q75 + 1.5 * first.iq.range)
  second.low.out <- which(second.x <= first.q25 - 3 * first.iq.range)
  second.up.out <- which(second.x >= first.q75 + 3 * first.iq.range)
  
  first.sig <- sort(c(first.low.out, first.up.out))
  
  second.sig <- sort(c(second.low.out, second.up.out))
  
  sig.both <- sort(c(first.sig, (second.sig + length(first.x))))
  
  outputVector <- sort(as.numeric(c(outputVector, sig.both)))
  
  # remove duplicates
  # outputVector <- outputVector[which(c(1, diff(outputVector)) != 0)]
  
  print("return")
  return(outputVector)
}

print(SplitHalfTukey(x=minMaxAmp))
