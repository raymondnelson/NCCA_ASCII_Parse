
cardioLeakFn <- function(x=chartDF$c_CardioMA, midVal=0) {
  # R function to to check for leaking or descending cardio sensor data
  # in the cardioLeakDown.R script
  # Oct 25, 2020
  # x input is the slow moving cardio data mean
  # midVal input is the middle value of the y axis display
  # output is a message
  ###
  xLen <- length(x)
  # exit if less than 20 seconds
  if(xLen < 600) return
  # set the selection ranges for the first and second halfs
  start1 <- round(xLen * .1)
  end1 <- round(xLen * .35)
  start2 <- round(xLen * .65)
  end2 <- round(xLen * .9)
  # get the means
  mean1 <- mean(x[start1:end1])
  mean2 <- mean(x[start2:end2])
  # a fault is indicated 
  # if the first half is > midVal and second half is < midVal
  # if(mean1 > midVal && mean2 < midVal) {
  if(mean1 - mean2 >= 500) {
    # cardio leak message if the difference exceeds 25% of the y-axis
    cardioLeakMsg <- "POSSIBLE CARDIO SENSOR PROBLEM"
  } else {
    cardioLeakMsg <- "none"
  }
  return(cardioLeakMsg)
}


