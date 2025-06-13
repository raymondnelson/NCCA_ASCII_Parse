# 10-27-2015 Raymond Nelson
# 10-29-2015 fixed NA problem


# dataVector <- segmentDF$c_UPneumo[responseOnsetRow:responseEndRow]
# verbalAnswer <- which(segmentDF$Event=="answerRow") - (which(segmentDF$Event=="onsetRow")+1) + 1


# maxPeakPn <- function(x, y=40, firstLast=FALSE) {
#   # helper function to get the positive slope peaks from the pneumo time series data
#   # will keep the index number of max peak samples
#   # x input is a time series vector
#   # y input is the number of offset samples 
#   # xOut is a vector of peak indices to compute the cyclic rate or interpolate the line
#   xOut <- rep(NA, times=(length(x)))
#   if(firstLast==TRUE){
#     xOut[1] <- 1 # keep the first
#     xOut[length(xOut)] <- length(xOut) # keep the last
#   }
#   # buffer will be double the offset value
#   input_buffer <- x[2:(2*y+1)]
#   for (i in 2:(length(x)-(2*y))) {
#     input_buffer <- c(input_buffer[2:(2*y)], x[i+(2*y)])
#     # check to see if the middle value of the buffer is the max
#     ifelse(input_buffer[(y+1)]==max(input_buffer),
#            xOut[i+y+1] <- c(i+y+1), # +1 because we started at 2
#            next()
#     ) # end if
#   } # end for loop
#   return(as.numeric(na.omit(xOut)))
# }  # end maxPeakPn function


##########################################


robustPneumoMeasurement <- function(dataVector, verbalAnswer , cps=30) {
  # function to compute a pneumo excursion measurement
  # that is robust against common answer distortion artifacts
  # and can also be made more robust against 
  # other identified respiration artifacts
  
  # Respiratory excursion is the sum of 
  # the absolute difference of all successive respiration samples.
  
  # The robust excursion measurement is achieved by removing 
  # the data from 1 second before to 1 second after 
  # the point of verbal answer, and can be made more robust by 
  # removing other identified artifact segments 
  # from the input data vector.
  
  # A dimmensionally stable measurement can be obtained by 
  # first summing the excursion lengths not for the entire segment,
  # but for a 1 second moving average across the non-artifacted samples,
  # and then averaging the excursion sum by the number of 
  # 1 second segments of non-artifacted sample differences.
  
  # input dataVector is the time series data for 15 seconds
  # input verbalAnswer is the sample index, in the dataVector,
  # where the verbal answer is located
  # input cps is the data rate (30 samples per second x 15 = 150 samples)
  
  # output RExcursion is the mean of a moving average of excursion lengths
  # for successive samples for a 1 second moving average of all
  # non-artifacted respiration samples in the input data vector
  
  # excursion is the sum of absolute differences in y-axis change
  
  ##############################
  
  # RExcursion <- NULL
  # compute the absolute difference in respiratory excursion
  REVector1 <- c(0, abs(diff(dataVector))) 
  # set the buffer around the verbal answer 
  ansBufferOn <- verbalAnswer - (1 * cps) 
  # to work effectively with missed key durung stim presentation
  if(ansBufferOn < 1) ansBufferOn <- 1 
  ansBufferOff <- verbalAnswer + (1 * cps) + 1
  # use of 0 during the 1 sec prior to verbal answer will prevent NAs until 1 second prior 
  REVector1[ansBufferOn:(verbalAnswer-1)] <- 0 
  REVector1[verbalAnswer:ansBufferOff] <- NA 
  # then compute the 1 second moving sum of absolute difference
  REVector2 <- NULL
  for (i in 1:(length(REVector1)-cps+1)) {
    REVector2 <- c( REVector2, sum( REVector1[ i:(i+cps-1) ] ) ) 
    # any sum that includes an NA input value will be NA
    # this will ensure that artifacted segments are not included
  } # end for loop
  # remove NA sums
  # REVector3 <- REVector2[!is.na(REVector2)] 
  # could be done without defining a new vector
  # and the mean of REVector3 
  # REVector4 <- mean(REVector3)

  # output
  # if(is.null(RExcursion)) RExcursion <- REVector4 
  
  # return(RExcursion)
  return( mean(REVector2[!is.na(REVector2)]) )
} # end robustPneumoMeasurement function


# robutstPneumoMeasurement(dataVector, verbalAnswer)

#############################





###########



# # function to compute the sum of absolute differences in y axis exursion
# pneumoExtract <- function(x, y) {
#   # function to extract the excursion length measurement from pneumo data
#   # x = vector of time series data for the measured stimulus segment
#   # y = time series vector of events for the measured stimulus segment 
#   # including "resonseOnsetRow" "responseEndRow" "aBuffOn" and "aBuffOff"
#   responseOnsetRow <- x[which(x=="responseOnsetRow")]
#   responseEndRow <- x[which(x=="responseEndRow")]
#   aBuffOn <- x[which(x=="aBuffOn")]
#   aBuffOf <- x[which(x=="aBuffOff")]
#   x <- x[c(responseOnsetRow:aBuffOn, aBuffOff:responseEndRow)]
#   return(sum(abs(diff(x))))  
# } # end pneumoExtract function
# 
# # pneumoExtract()

