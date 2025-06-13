# function to make a list of events for each stimulus segment
# 
# segmentEventList()
#
#################################






# may need to modify the latency values for each channel


segmentEventList <- function(x=PF090316_Stimuli, y=6, z=25) {
  # function to get the stimulus events for a question segment
  # x is the "*_Stimuli" data frame for a chart
  # y is the row number of the selected event
  # z is the number of seconds to select
  # assumes a 30cps sampling rate
  # output is a named list
  onsetRow <- as.numeric(x[y,5])
  offsetRow <- as.numeric(x[y,6])
  answerRow <- as.numeric(x[y,7])
  # prestimRow <- onsetRow - (30*5)
  # latencyRow <- onsetRow + (30*.5)
  # ROWEndRow <- answerRow + (30*5)
  endRow <- onsetRow + (30*z)
  # named vector for ouput
  output <- c(onsetRow,
                     offsetRow,
                     answerRow,
                     # prestimRow,
                     # latencyRow,
                     # ROWEndRow,
                     endRow)
  names(output)=c("onsetRow",
                      "offsetRow",
                      "answerRow",
                      # "prestimRow",
                      # "latencyRow",
                      # "ROWEndRow",
                      "endRow")
  return(output)
  
} # end segmentEventList function

segEvents <- segmentEventList(x=PF090316_Stimuli, y=6)