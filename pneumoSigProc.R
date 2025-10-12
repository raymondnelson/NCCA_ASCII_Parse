# pneumo signal processing
# 5-1-2016
# Raymond Nelson
####
#
# three functions in this script
#
# lowPass.886() Butterworth filter to smooth the pneumo time series data
#
# MASmooth() moving average filter to smooth the pneumo time series data
#
# main function
# pneumoSigProcFn()
# 
#######################



# x=chartDF
# first=firstEvent
# last=lastEventEnd



lowPass.886 <- function(x, GAIN = 1.174704212e+01, zplane = 0.8297443748) {
  # lowPass.886() 
  # to reduce noise and improve the diagnostic coeficient of the pneumo data
  # also called for the raw EDA data to execute the Lafayette legacy Auto EDA
  # first order Butterworth filter
  # to smooth peneumo and raw EDA data 
  # to improve the diagnostic coefficient by reducing high frequency noise
  # x is a column vector from the time series 
  xv1 <- x[1]
  yv1 <- 0
  output <- rep(NA, length(x))
  # output <- NULL
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 + xv0) + (zplane * yv0)
    output[i] <- yv1
    # output <- c(output, yv1)
  }
  return(output)
} # end lowpass.886() function



MASmooth <- function(x=myData, y=round(.25*cps,0), times=5) {
  # function to calculate a smoothed average of the time series data
  # x input is a time series vector
  # y input is the number of offset samples
  # times is the number of times to smooth the data
  ###
  # make the output vector 
  # beginning and end of the output vector are the mean of 2 * the buffer at begin and end of x
  xOut <- x
  # loop over the number of times
  for (j in 1:times) {
    # for loop to compute the moving average
    # buffer will be double the offset value + 1
    input_buffer <- x[1:(2*y+1)]
    # starts at sample y + 1
    for (i in (y+1):(length(x)-y)) { 
      # replace the middle value of the buffer with the mean
      xOut[i] <- mean(input_buffer) 
      # increment the input buffer
      input_buffer <- c(input_buffer[2:length(input_buffer)], x[i+y+1])
    } 
    # replace the input vector
    x <- xOut
  } # end loop over times
  return(xOut)
} # end MASmooth function()


##### source a script to load the pneumo caliper functions ####


{
  
  source("~/Dropbox/R/NCCA_ASCII_Parse/pneumoCaliperFunctions.R", echo=FALSE)

}
       

################### main function ###################



pneumoSigProcFn <- function(x=chartDF, PneumoL=PneumoLowPass, first=firstEvent, last=lastEventEnd) {
  # PneumoL is a switch to deactivate the pneumo smoothing filter
  
  chartDF <- x
  
  # reset the zero-centered pneumo data
  chartDF$c_UPneumo <- chartDF$UPneumo - chartDF$UPneumo[1]
  chartDF$c_LPneumo <- chartDF$LPneumo - chartDF$LPneumo[1]

  # set the range
  chartDF$c_UPneumo <-
    setColRange(DAT=chartDF$c_UPneumo, y=colRange, firstRow=first, lastRow=last)
  chartDF$c_LPneumo <-
    setColRange(DAT=chartDF$c_LPneumo, y=colRange, firstRow=first, lastRow=last)
  
  # fix NA values
  chartDF$c_UPneumo <- NAInterp(chartDF$c_UPneumo)
  chartDF$c_LPneumo <- NAInterp(chartDF$c_LPneumo)
  
  # smooth the pneumo data equivalent to .5 second moving average
  # abstract this to another channel to avoid refiltering the pneumo data when re-run
  if(PneumoLowPass==TRUE) { 
    chartDF$c_UPneumoSm <- lowPass.886(x=chartDF$c_UPneumo)
    chartDF$c_LPneumoSm <- lowPass.886(x=chartDF$c_LPneumo)
  } else {
    chartDF$c_UPneumoSm <- chartDF$c_UPneumo
    chartDF$c_LPneumoSm <- chartDF$c_LPneumo
  }
  
  if(isTRUE(morePnSmooth)) {
    # morePnSmooth is initialized in the NCCAASCII_init.R script
    # 5-19-2017 smooth it just a little more to reduce high freq noise with some cases
    chartDF$c_UPneumoSm <- MASmooth(x=chartDF$c_UPneumoSm, y=round(.25*cps), times=1)
    chartDF$c_LPneumoSm <- MASmooth(x=chartDF$c_LPneumoSm, y=round(.25*cps), times=1)
  }
  
  if(isTRUE(evenMorePnSmooth)) {
    # evenMorePnSmooth is initialized in the NCCAASCII_init.R script
    # 5-19-2017 smooth it just a little more to reduce high freq noise with some cases
    chartDF$c_UPneumoSm <- MASmooth(x=chartDF$c_UPneumoSm, y=round(.25*cps), times=2)
    chartDF$c_LPneumoSm <- MASmooth(x=chartDF$c_LPneumoSm, y=round(.25*cps), times=2)
  }
  
  ## compute the cyclic rate per min for each respiration cycle ##
  
  {
    
    # source("~/Dropbox/R/NCCA_ASCII_Parse/pneumoCaliperFunctions.R", echo=FALSE)
    
    # <>
    
    UPneumoRate <- 
      ratePerMinFn(lowPass2hz.2ndFn(x=chartDF$c_UPneumoSm), buffer=40, peaks="upper", lowPass=TRUE)
    LPneumoRate <- 
      ratePerMinFn(lowPass2hz.2ndFn(x=chartDF$c_LPneumoSm), buffer=40, peaks="upper", lowPass=TRUE)
    
    pneumoBufferLen <- bufferLenFn(UPneumoRate)
    
    chartDF$c_UPneumoRate <- 
      pneumoRatePerCycleFn(x=chartDF$c_UPneumoSm, buffer=pneumoBufferLen, peaks="upper", lowPass=TRUE)
      
    chartDF$c_LPneumoRate <- 
      pneumoRatePerCycleFn(x=chartDF$c_LPneumoSm, buffer=pneumoBufferLen, peaks="upper", lowPass=TRUE)
    
    # chartDF$c_UPneumoExcursion <- respirationLineExcursionFn(x=chartDF$c_UPneumoSm)
    # chartDF$c_LPneumoExcursion <- respirationLineExcursionFn(x=chartDF$c_LPneumoSm)
    
    # chartDF$c_UpneumoAmp
    # chartDF$c_UpneumoAmp 
    
    answerBufferIndices <- getAnswerIndicesFn(x=chartDF$Label, ansBuffLen=pneumoAnsBuff, buffer=TRUE)
    
    # July 16, 2025 protected against problems when there are no answers
    if(answerBufferIndices[1] != "") {
      chartDF$c_UPneumoRate0 <- chartDF$c_UPneumoRate
      chartDF$c_UPneumoRate0[answerBufferIndices] <- NA
      
      chartDF$c_LPneumoRate0 <- chartDF$c_LPneumoRate
      chartDF$c_LPneumoRate0[answerBufferIndices] <- NA
    }
    
  }
  
  ## scaling and offsetting is now done in another function ##
  
  {
    # # processing of the additional pneumo channels is done after scaling and offsetting
    # 
    # # difference between upper and lower
    # # chartDF$c_PneumoDiff <- chartDF$c_UPneumoSm - chartDF$c_LPneumoSm
    # 
    # # respiration mid lines
    # chartDF$c_UPneumoMid <- MASmooth(x=chartDF$c_UPneumoSm, y=round(30*cps,0), times=1) # was y=40, times=8 11/19/2015
    # chartDF$c_UPneumoMid <- MASmooth(x=chartDF$c_UPneumoMid, y=round(3.75*cps,0), times=1)
    # 
    # chartDF$c_LPneumoMid <- MASmooth(x=chartDF$c_LPneumoSm, y=round(30*cps,0), times=1)
    # chartDF$c_LPneumoMid <- MASmooth(x=chartDF$c_LPneumoMid, y=round(3.75*cps,0), times=1)
    # 
    # # thoracic inhalation and exhalation lines
    # chartDF$c_UPneumoInh <- interpolatePeaks(x=maxPeak(x=chartDF$c_UPneumoSm, y=round(.2*cps,0)), # was 1.333sec
    #                                          y=chartDF$c_UPneumoSm[maxPeak(x=chartDF$c_UPneumoSm, y=round(.2*cps,0))])
    # chartDF$c_UPneumoExh <- interpolatePeaks(x=minPeak(x=chartDF$c_UPneumoSm, y=round(.2*cps,0)),
    #                                          y=chartDF$c_UPneumoSm[minPeak(x=chartDF$c_UPneumoSm, y=round(.2*cps,0))])
    # 
    # # abdominal inhalation and exhalation lines
    # chartDF$c_LPneumoInh <- interpolatePeaks(x=maxPeak(x=chartDF$c_LPneumoSm, y=round(.2*cps,0)),
    #                                          y=chartDF$c_LPneumoSm[maxPeak(x=chartDF$c_LPneumoSm, y=round(.2*cps,0))])
    # chartDF$c_LPneumoExh <- interpolatePeaks(x=minPeak(x=chartDF$c_LPneumoSm, y=round(.2*cps,0)),
    #                                          y=chartDF$c_LPneumoSm[minPeak(x=chartDF$c_LPneumoSm, y=round(.2*cps,0))])
  }
  
  # compute the pneumo difference channels
  
  # chartDF$c_UPneumoInhDiff <- chartDF$c_UPneumoInh - chartDF$c_UPneumoMid
  # chartDF$c_UPneumoExhDiff <- chartDF$c_UPneumoExh - chartDF$c_UPneumoMid
  # chartDF$c_UPneumoDiff <- chartDF$c_UPneumoInh - chartDF$c_UPneumoExh
  # chartDF$c_LPneumoInhDiff <- chartDF$c_LPneumoInh - chartDF$c_LPneumoMid
  # chartDF$c_LPneumoExhDiff <- chartDF$c_LPneumoExh - chartDF$c_LPneumoMid
  # chartDF$c_LPneumoDiff <- chartDF$c_LPneumoInh - chartDF$c_LPneumoExh
  
  return(chartDF)
  
} # end newPneumoSigProcFn()



