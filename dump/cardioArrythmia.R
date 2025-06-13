# function to calculate cardio arrhythmia,
# 4-26-2017
# Raymond Nelson

################



################### calculate the cardio rate ####################

cardioArrhythmiaFn <- function(x=chartDF$c_Cardio1) {
  # function to calculate cardio arrhythmia,
  # 4-26-2017
  # Raymond Nelson
  #
  # x input is the chartDF
  # output is a message about cardio arrhythmia
  #
  # called by the cardioRateFn()
  # uses ratePerMin and bufferLenFn
  ######################
  
  cardioData <- x
  
  # get the pulse rate using systolic peaks
  systRate <- ratePerMin(cardioData,
                         buffer=9,
                         peaks="upper",
                         lowPass=TRUE)
  
  # compute the systolic buffer lengths 
  systBufferLen <- bufferLenFn(x=systRate, y=.6)
  # 3-17-2017 was y = .5 and y=.6, default is .67
  
  # get the pulse rate useing diastolic peaks
  diastRate <- ratePerMin(cardioData,
                          buffer=9,
                          peaks="lower",
                          lowPass=TRUE)
  
  # compute the diastolic buffer lengths 
  diastBufferLen <- bufferLenFn(x=diastRate, y=.6)
  # 3-17-2017 was y = .5 and y=.6, default is .67
  
  cardioRate <- systRate
  
  # compare the systolic and diastolic cardio rates
  if(exp(-abs(log(systRate/diastRate))) >= .95) {
    # systolic and diastolic rates are similar
    # cardioRate <- systRate
    # chartDF$c_CardioRate <- cardioRate
    # cardioMsg <- paste("cardio rate:", cardioRate)
    cardioMsg <- "possible cardio arrythmia"
    print(cardioMsg)
  } else {
    # systolic and diastolic rates are dissimilar
    # chartDF$Cardio1_a <- paste("outside normal range", cardioRate1)
    # cardioRate <- systRate
    # chartDF$c_CardioRate <- cardioRate
    # chartDF$CardioArrhythmia_a <-  "possible cardio arrythmia"
    # cardioMsg <- paste("cardio rate:", cardioRate, "possible cardio arrythmia")
    cardioMsg <- "none"
    print(cardioMsg)
    # stop("check for cardio arrythmia")
  } 
  
  return(cardioMsg)
  
} # end cardioArrhythmiaFn()

