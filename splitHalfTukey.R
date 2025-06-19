
SplitHalfTukey <- function(x=maxAmp, cycles=8, side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=5, scaleVal=1) {
  # function to check the split halfs against each other
  # x is a vector of input measurements
  # cycles is a numeric value to specify the number of cycles in the evaluation buffer
  # buffer length to set the number of samples use instead of the length of the input vector
  # side is a parameter that can specify "both" "upper" or "lower"
  # fence is a parameter that can specify "both" "inner" or "outer"
  # inner fence is typically 25thPercentile - 1.5*IQR and 75thPercentile + 1.5*IQR
  # outer fence is typically 25thPercentile - 3*IQR and 75thPercentile + 3*IQR
  # expVal is a numerical value for exponential transformation of the input vector
  # scaleVal is numerical value for linear scaling of the input vector
  # output is 
  ###
    
  # outputVector <- rep(NA, times=length(x))
  outputVector <- NULL
  
  x <- x * scaleVal
  x <- x^expVal
  
  # need to work out that fractional exponents of negative roots are well defined
  # for odd denominators 
  # if(x<0) {
  #   x <- -(abs(x)^)
  # }
  # # find out if the root is odd
  # denomVal <- 1/expVal
  
  split.row <- ceiling(length(x) / 2)

  first.x <- x[1:(split.row)]
  second.x <- x[(split.row+1):length(x)]
  
  i <- length(first.x)
  
  # use an if condition to call the function recursively
  if(i >= cycles) {
    # print(paste("segment length:", i))
    # call this function recursively
    first.out <- SplitHalfTukey(x=first.x)
    # print(first.out)
    second.out <- SplitHalfTukey(x=second.x)
    # print(second.out)
    both.out <- sort(c(first.out, (second.out + length(first.x))))
    # print(both.out)
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
  
  # locate the outliers in the first half 
  # by comparing each cycle with the second half
  if(side=="lower" | side=="both") {
    if(fence=="inner" | fence=="both") {
      first.low.in <- which(first.x <= second.q25 - innerFence * second.iq.range)
    }
    if(fence=="outer" | fence=="both") {
      first.low.out <- which(first.x <= second.q25 - outerFence * second.iq.range)
    }
  } 
  if(side=="upper" | side=="both") {
    if(fence=="inner" | fence=="both") {
      first.up.in <- which(first.x >= second.q75 + innerFence * second.iq.range)
    }
    if(fence=="outer" | fence=="both") {
      first.up.out <- which(first.x >= second.q75 + outerFence * second.iq.range)
    }
  }
  
  # locate the outliers in the second half by comparison with the first
  second.low.in <- which(second.x <= first.q25 - innerFence * first.iq.range)
  second.up.in <- which(second.x >= first.q75 + innerFence * first.iq.range)
  second.low.out <- which(second.x <= first.q25 - outerFence * first.iq.range)
  second.up.out <- which(second.x >= first.q75 + outerFence * first.iq.range)
  
  # combine and sort inner and outer fence outliers for the first half
  first.sig <- sort(c(first.low.out, first.up.out))
  # combine and sort inner and outer fence outliers for the second half
  second.sig <- sort(c(second.low.out, second.up.out))
  
  # combine and sort the outliers for the 2 half
  sig.both <- sort(c(first.sig, (second.sig + length(first.x))))
  
  # comptine the outliers for both halfs outliers from the recursion
  outputVector <- sort(as.numeric(c(outputVector, sig.both)))
  
  # remove duplicates
  # outputVector <- outputVector[which(c(1, diff(outputVector)) != 0)]
  # simpler method
  outputVector <- sort(unique(outputVector))
  
  # print("return")
  return(outputVector)
}

# print(SplitHalfTukey(x=minMaxAmp))
