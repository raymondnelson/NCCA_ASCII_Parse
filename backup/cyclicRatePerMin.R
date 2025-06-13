# calculate the respiration rate

# ratePerMin <- function(x=peaks) {
#   # function to calculate the cyclic rate per minute
#   # for time series data
#   # input is a vector of peak indices from the maxPeak or minPeak function
#   # output is the mean rate 
#   round( 60 / ( mean(diff(peaks)) / cps ) )
# }

ratePerMin <- function(x=chartDF$c_UPneumo, peaks="upper", buffer=40, cps=cps) {
  # function to calculate the cyclic rate per minute
  # for cardio and pneumo time series data
  # x input is a a time series vector
  # peaks input is a switch to choose "upper" or "lower" peaks
  # buffer input is the number of pre and post index samples to include in the search space
  # output is the mean rate 
  ifelse(peaks=="lower",
         Peaks <- minPeak(x=x, y=buffer, firstLast=FALSE),
         Peaks <- maxPeak(x=x, y=buffer, firstLast=FALSE) 
  )  
  return( round(60 / ( mean(diff(Peaks)) / cps ) , 2) )
}

# ratePerMin(x=chartDF$c_UPneumo, peaks="upper", buffer=40)
# ratePerMin(x=chartDF$c_Cardio1, peaks="upper", buffer=7)
