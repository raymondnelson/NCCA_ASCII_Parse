# EDA signal processing
# 5-1-2016
# Raymond Nelson


# source('~/Dropbox/R/NCCA_ASCII_Parse/sigProc_extra.R', echo=FALSE)

EDASigProcFn <- function(x=chartDF, 
                         EDAHigh=TRUE, 
                         EDALow=TRUE, 
                         EDAFilter="none",
                         first=firstEvent, 
                         last=lastEventEnd ) {
  # apply the filter functions to the data frame columns
  # first and last are used for scaling and offsettin the data
  
  chartDF <- x
  
  # EDAHigh=EDAH
  # EDALow=EDAL
  # EDAFilter=EDAFilter
  
  # re-set the zero-centered manual EDA
  chartDF$c_EDA1 <- chartDF$EDA1 - chartDF$EDA1[1]
  
  # first=firstEvent 
  # last=lastEventEnd
  
  # set the range
  chartDF$c_EDA1 <-  
    setColRange(DAT=chartDF$c_EDA1, y=colRange, firstRow=first, lastRow=last)
  
  # fix NA values
  chartDF$c_EDA1 <- NAInterp(chartDF$c_EDA1)
  chartDF$c_ManualEDA <- NAInterp(chartDF$c_EDA1)
  
  # if the EA data are available
  if( "c_EA" %in% colnames(chartDF) &
     length(which(is.na(chartDF$c_EA))) != nrow(chartDF) )  {
    chartDF$c_EA <- NAInterp(chartDF$c_EA)
  }
  
  # get the onset value in case it is not already zero
  onsetValue <- chartDF$c_EDA1[1]
  
  # locate the AutoEDA and ManualEDA data at onset = zero
  chartDF$c_AutoEDA <- chartDF$c_EDA1 - onsetValue
  chartDF$c_ManualEDA <- chartDF$c_EDA1 - onsetValue
  
  # set the EA data to zero if available
  if( "c_EA" %in% colnames(chartDF) &
      length(which(is.na(chartDF$c_EA))) != nrow(chartDF) ) {
    chartDF$c_EA <- chartDF$c_EA - chartDF$c_EA[1]
  }
  
  # call the low pass filter function to clean up some high frequency noise
  # if(EDALow==TRUE) chartDF$c_AutoEDA <- lowPass.2(x=chartDF$c_AutoEDA)
  # if(EDALow==TRUE) chartDF$c_AutoEDA <- lowPass.886(x=chartDF$c_AutoEDA)
  # get the EDAFilter parameter from the global environment set in the init script
  
  # commented out 2-21-2017 because filter selection is in in the sigProc.R script
  # EDAFilter <- EDAFilt
  # print(EDAFilt)
  if(!exists("EDAFilter")) EDAFilter <- EDAFilt
  print(paste0("    EDA Mode: ", EDAFilter))
  # stop()
  
  #### Manual EDA smoothing ####
  
  # smoothing filter for the manual EDA 
  # chartDF$c_ManualEDA <- lowPass.886(x=chartDF$c_ManualEDA)
  
  # Dec 2, 2022
  chartDF$c_ManualEDA <- lowPass12hz(x=chartDF$c_ManualEDA)
  chartDF$c_ManualEDA <- lowPass.443(x=chartDF$c_ManualEDA)
  
  ############ Auto-centering EDA ############
  
  ######## auto low pass ########
  
  # Dec 2, 2022
  chartDF$c_AutoEDA <- lowPass12hz(x=chartDF$c_AutoEDA)
  
  # print(EDAFilter)
  if(!exists("EDALow")) EDALow <- TRUE
  if(EDALow==TRUE) {
    chartDF$c_AutoEDA <- switch(EDAFilter, 
                                "axc"=lowPass.886(x=chartDF$c_AutoEDA),
                                "laf13"=lowPass.2(x=chartDF$c_AutoEDA),
                                "laf0"=lowPass.5(x=chartDF$c_AutoEDA),
                                "lafX"=lowPass.05(x=chartDF$c_AutoEDA),
                                "lafD"=lowPass.3(x=chartDF$c_AutoEDA),
                                "lim"=lowPass.0467(x=chartDF$c_AutoEDA),
                                "leg"=lowPass.886(x=chartDF$c_AutoEDA),
                                # "laf18"=lowPass12hz(x=chartDF$c_AutoEDA),
                                "laf18"=lowPass.443(x=chartDF$c_AutoEDA),
                                "test"=lowPass.3hz.3rd(x=chartDF$c_AutoEDA),
                                "Det2"=lowPass.886(x=chartDF$c_AutoEDA),
                                "resp"=lowPass_resp(x=chartDF$c_AutoEDA),
                                "none"=lowPass.886(x=chartDF$c_AutoEDA),
                                "man"=lowPass.886(x=chartDF$c_AutoEDA) )
  } else {
    # Feb 9, 2024
    # to reduce high frequency noise in the Axciton cases 
    # which do not use the LIC high pass
    chartDF$c_AutoEDA <- lowPass12hz(x=chartDF$c_AutoEDA)
    chartDF$c_AutoEDA <- lowPass.443(x=chartDF$c_AutoEDA)
    # this is identical to the Manual EDA
  }
  
  ######## auto high pass ########
  
  # call the high pass filter function to center the data
  # if(EDAHigh==TRUE) chartDF$c_AutoEDA <- highPass.03(x=chartDF$c_AutoEDA)
  # if(EDAHigh==TRUE) chartDF$c_AutoEDA <- highPass.04(x=chartDF$c_AutoEDA)
  if(!exists("EDAHigh")) EDAHigh <- TRUE
  if(EDAHigh==TRUE) {
    chartDF$c_AutoEDA <- switch(EDAFilter,
                                "axc"=highPass.1(x=chartDF$c_AutoEDA),
                                "laf13"=highPass.03(x=chartDF$c_AutoEDA),
                                "laf0"=highPass.01(x=chartDF$c_AutoEDA),
                                "lafX"=highPass.05(x=chartDF$c_AutoEDA),
                                # "lafD"=highPass.07(x=chartDF$c_AutoEDA),
                                "lafD"=highPass.2(x=chartDF$c_AutoEDA),
                                "lim"=highPass.0533(x=chartDF$c_AutoEDA),
                                "leg"=highPass.04(x=chartDF$c_AutoEDA),
                                "laf18"=highPass.0159(x=chartDF$c_AutoEDA),
                                "test"=highPass.06(x=chartDF$c_AutoEDA),
                                "Det2"=detrended2EDAFilter(x=chartDF$c_AutoEDA),
                                "resp"=highPass_resp(x=chartDF$c_AutoEDA),
                                "none"=chartDF$c_AutoEDA,
                                "man"=chartDF$c_AutoEDA 
    														)
  }
  
  # additional smoothing for the auto EDA
  # if(EDAFilter == "test") { chartDF$c_AutoEDA <- lowPass.443hz.2nd(x=chartDF$c_AutoEDA) }
  # if(EDAFilter == "laf18") { chartDF$c_AutoEDA <- lowPass.443(x=chartDF$c_AutoEDA) }
  
  
  
  # 5-15-2017
  # to further reduce high frequency noise
  # commented out Oct 31, 2020
  # chartDF$c_AutoEDA <- MASmooth(x=chartDF$c_AutoEDA, y=8, times=1)
  # chartDF$c_ManualEDA <- MASmooth(x=chartDF$c_ManualEDA, y=8, times=1)
  
  # 2-7-2020
  if(isTRUE(moreEDASmooth)) {
    # moreEDASmooth is initialized in the NCCAASCII_init.R script
    # changed Oct 31, 2020 from y=10, times=1
    chartDF$c_AutoEDA <- MASmooth(x=chartDF$c_AutoEDA, y=8, times=2)
    chartDF$c_ManualEDA <- MASmooth(x=chartDF$c_ManualEDA, y=8, times=2)
  }
  
  #######   relocate the filtered data to the onset value == 0  ########
  
  chartDF$c_AutoEDA <- chartDF$c_AutoEDA - chartDF$c_AutoEDA[1]
  chartDF$c_ManualEDA <- chartDF$c_ManualEDA - chartDF$c_ManualEDA[1]
  
  # no additional smoothing on the exported Auto EDA 
  
  
  ####### second EDA sensor ########
  
  # Jan 17, 2023
  
  # if(sum(pmatch(names(chartDF), "c_EDA2", nomatch=0)) != 0) {
  
  # simplified and fixed Sep 11, 2023
  if("c_EDA2" %in% names(chartDF)) {
    
    chartDF$c_EDA2 <- NAInterp(chartDF$c_EDA2)
    
    onsetValue2 <- chartDF$c_EDA2[1]
    
    chartDF$c_EDA2 <- chartDF$c_EDA2 - onsetValue2
    chartDF$c_ManualEDA2 <- chartDF$c_EDA2 - onsetValue2
    
    chartDF$c_ManualEDA2 <- lowPass12hz(x=chartDF$c_ManualEDA2)
    chartDF$c_ManualEDA2 <- lowPass.443(x=chartDF$c_ManualEDA2)
    
  }

	######################################################################################
  
  # compute the moving average to show the tonic EDL activity
  # commented out 3-15-2017 because it is done in the scaleOffset function
  # chartDF$c_AutoEDAMid <- MASmooth(x=chartDF$c_AutoEDA, y=round(5*cps,0), times=6) # was y=150, times=6 11/26/2015

  # compute  the response peaks and interpolated peak lines
  # chartDF$c_AutoEDAPeak <- interpolatePeaks(x=maxPeak(x=chartDF$c_AutoEDA, y=round(1*cps,0)),
  #                                           y=chartDF$c_AutoEDA[maxPeak(x=chartDF$c_AutoEDA, y=round(1*cps,0))])
  # chartDF$c_AutoEDABase <- interpolatePeaks(x=minPeak(x=chartDF$c_AutoEDA, y=round(1.333*cps,0)),
  #                                           y=chartDF$c_AutoEDA[minPeak(x=chartDF$c_AutoEDA, y=round(1.333*cps,0))])

  # compute the distance from peaks to mid
  # chartDF$c_AutoEDAPeakDiff <- chartDF$c_AutoEDAPeak - chartDF$c_AutoEDAMid
  # chartDF$c_AutoEDABaseDiff <- chartDF$c_AutoEDAMid - chartDF$c_AutoEDABase
  
  ######################################################################################
  
  ####### call the EDA Band Pass filter to evaluate data stability and quality ######
  
  # chartDF$c_EDAFilt <- chartDF$c_EDA1 - onsetValue
  # 
  # lowPass <- lowPass8th1hz_resp
  # highPass <- highPass8th.5hz_resp
  # 
  # chartDF$c_EDAFilt <- lowPass(x=chartDF$c_EDAFilt)
  # chartDF$c_EDAFilt <- highPass(x=chartDF$c_EDAFilt)
  # 
  # # # compute the moving average to show the tonic EDL activity
  # chartDF$c_EDAFiltMid <- MASmooth(x=chartDF$c_EDAFilt, y=round(5*cps,0), times=6)
  
  # # compute  the response peaks and interpolated peak lines
  # chartDF$c_EDAFiltMax <- interpolatePeaks(x=maxPeak(x=chartDF$c_EDAFilt, y=round(.75*cps,0)),
  #                                           y=chartDF$c_EDAFilt[maxPeak(x=chartDF$c_EDAFilt, y=round(1*cps,0))])
  # chartDF$c_EDAFiltMin <- interpolatePeaks(x=minPeak(x=chartDF$c_EDAFilt, y=round(.75*cps,0)),
  #                                           y=chartDF$c_EDAFilt[minPeak(x=chartDF$c_EDAFilt, y=round(1*cps,0))])
  
  # compute the difference between Max and Min
  # chartDF$c_EDAFiltDiff <- chartDF$c_EDAFiltMax - chartDF$c_EDAFiltMin
  
  return(chartDF)
  
} # end EDASigProcFn()





# lowPass12hz <- function(x, GAIN = 1.32491969623291e+000, zplane = -0.50952544949443) {
#   # lowPass12hz()
#   # low pass filter to improve the diagnostic coeficient of the Auto EDA data
#   # will add the AutoEDA column to the data frame
#   # first order Butterworth filter
#   xv1 <- x[1]
#   yv1 <- 0
#   output <- rep(NA, length(x))
#   for (i in 1:length(x)) {
#     xv0 <- xv1
#     xv1 <- x[i] / GAIN
#     yv0 <- yv1
#     yv1 <- (xv1 + xv0) + (zplane * yv0)
#     output[i] <- yv1
#   }
#   return(output)
# } # end lowPass12hz function
# 
# 
# 
# 
# highPass.0159 <- function(x, GAIN = 1.00166504564511e+000, zplane = 0.99667544424686) {
#   # highPass.0159()
#   # high pass filter to auto center the Auto EDA
#   # to improve the visual appearance and diagnostic coeficient of the EDA data
#   # auto centering filter
#   # first order Butterworth filter
#   xv1 <- x[1]
#   yv1 <- 0
#   output <- rep(NA, length(x))
#   for (i in 1:length(x)) {
#     xv0 <- xv1
#     xv1 <- x[i] / GAIN
#     yv0 <- yv1
#     yv1 <- (xv1 - xv0) + (zplane * yv0)
#     output[i] <- yv1
#   }
#   return(output)
# } # end highPass.0159 function
# 
# 
# 
# 
# 
# lowPass.443 <- function(x, GAIN = 2.254050840e+01, zplane = 0.9112708567) {
#   xv1 <- x[1]
#   yv1 <- 0
#   output <- rep(NA, length(x))
#   # output <- NULL
#   for (i in 1:length(x)) {
#     xv0 <- xv1
#     xv1 <- x[i] / GAIN
#     yv0 <- yv1
#     yv1 <- (xv1 + xv0) + (zplane * yv0)
#     output[i] <- yv1
#     # output <- c(output, yv1)
#   }
#   return(output)
# } # end lowpass.443 function
# 
# 
# 
# 
# 
# 
# # define the low pass .886 function
# lowPass.886 <- function(x, GAIN = 1.174704212e+01, zplane = 0.8297443748) {
#   # lowPass.886() 
#   # to reduce noise and improve the diagnostic coeficient of the pneumo data
#   # also called for the raw EDA data to execute the Lafayette legacy Auto EDA
#   # first order Butterworth filter
#   # to smooth peneumo and raw EDA data 
#   # to improve the diagnostic coefficient by reducing high frequency noise
#   # x is a column vector from the time series 
#   xv1 <- x[1]
#   yv1 <- 0
#   output <- rep(NA, length(x))
#   # output <- NULL
#   for (i in 1:length(x)) {
#     xv0 <- xv1
#     xv1 <- x[i] / GAIN
#     yv0 <- yv1
#     yv1 <- (xv1 + xv0) + (zplane * yv0)
#     output[i] <- yv1
#     # output <- c(output, yv1)
#   }
#   return(output)
# } # end lowpass.886() function
# 
# 
# 
# MASmooth <- function(x=myData, y=round(.25*cps,0), times=5) {
#   # function to calculate a smoothed average of the time series data
#   # x input is a time series vector
#   # y input is the number of offset samples
#   # times is the number of times to smooth the data
#   ###
#   # make the output vector 
#   # beginning and end of the output vector are the mean of 2 * the buffer at begin and end of x
#   xOut <- x
#   # loop over the number of times
#   for (j in 1:times) {
#     # for loop to compute the moving average
#     # buffer will be double the offset value + 1
#     input_buffer <- x[1:(2*y+1)]
#     # starts at sample y + 1
#     for (i in (y+1):(length(x)-y)) { 
#       # replace the middle value of the buffer with the mean
#       xOut[i] <- mean(input_buffer) 
#       # increment the input buffer
#       input_buffer <- c(input_buffer[2:length(input_buffer)], x[i+y+1])
#     } 
#     # replace the input vector
#     x <- xOut
#   } # end loop over times
#   return(xOut)
# } # end MASmooth function()




