# R function to compute a pneumo excursion measurement
# 10-27-2015 Raymond Nelson
# 10-29-2015 fixed NA problem
# modified May 17, 2025
# Raymond Nelson
####


# dataVector <- segmentDF$c_UPneumo[responseOnsetRow:responseEndRow]
# verbalAnswer <- which(segmentDF$Event=="answerRow") - (which(segmentDF$Event=="onsetRow")+1) + 1


pneumoMeasurementFn <- function(dataVector, verbalAnswer=NULL, pnBufferLen=pneumoMeasurementBuffer, output="mean") {
  # R function to compute a pneumo excursion measurement
  # modified May 17, 2025
  # Raymond Nelson
  ####
  # 2015Oct called by the pneumoExtractFn() during feature extraction
  
  # 2025May17 also called during signal processing, to compute the RLE vector caliper functions
  
  # intended to be more robust against common answer distortion artifacts
  # may also be made more robust against 
  # other identified respiration artifacts
  
  # Respiratory excursion is the sum of 
  # the absolute difference of all successive respiration samples.
  
  # The robust excursion measurement is achieved by removing 
  # the data surrounding the point of verbal answer, 
  # and can potentially be made more robust by 
  # removing other identified artifact segments (deep breaths, apnea)
  # from the input data vector.
  # Alternatively, artifacted segments should be excluded from feature extraction
  
  # A dimensionally stable measurement can be obtained by 
  # using a moving window across window of evaluation (WOE),
  # and computing the excursion sum for each iteration of the moving window,
  # and then aggregating all excursion lengths during the WOE via averaging,
  # 2021-08-27 may be more stable by averaging not summing
  # using only the non-artifacted excursion values
  # artifacted excursion values will compute to NA).
  
  # This provides a measurement that is dimensionally standarized
  # across a range of possible measurement periods. 
  # It permits the apples-to-apples comparison of different WOE lengths
  
  ####
  
  # input dataVector is the time series respiration data (normally for 15 seconds)
  # input vector can also be the respiration data for an entire chart
  # input verbalAnswer is the sample index where the verbal answer is located
  # input verbalAnswer should remain NULL when this is called during signal processing (chart)
  
  # output="vector" will return a vector of the same length as the input vector
  
  # output RExcursion is the mean of excursion lengths during the WOE,
  # excluding the answer buffer and artifacts

  ####
  
  pneumoMeasurementBufferLen <- round(pnBufferLen * cps)
  
  # Oct 10, 2024 - to prevent errors when attempting to score short segments
  if(length(dataVector) < pneumoMeasurementBufferLen) return(NULL)
  
  # compute the absolute difference in respiratory excursion
  REVector1 <- c(0, abs(diff(dataVector))) 
  
  if(length(verbalAnswer) == 0) verbalAnswer <- NULL

  # check a parameter in NCCAASCII_init.R script to include or exclude the answer buffer
  if(!excludePneumoAnswerBuffer) verbalAnswer <- NULL
  
  if(!is.null(verbalAnswer))  {
    
    # set the buffer around the verbal answer 
    # pneumoAnsBuff is set the NCCAASCII_init.R script
    ansBufferOn <- verbalAnswer - round(pneumoAnsBuff * cps) 
    ansBufferOff <- verbalAnswer + round(pneumoAnsBuff * cps) - 1
    
    # to work effectively with missed ans key during stim presentation
    if(ansBufferOn < 1) ansBufferOn <- 1 
    if(ansBufferOff > length(dataVector)) ansBufferOff <- length(dataVector)
    
    ansBuffRows <- c(ansBufferOn:ansBufferOff)
    
    # set the answer buffer to 0 to prevent measurement and extraction of the answer seg
    # use of 0 during the 1 sec prior to verbal answer will prevent NAs until 1 second prior 
    # REVector1[ansBufferOn:(verbalAnswer-1)] <- 0 
    # REVector1[ansBufferOn:(ansBufferOn+1*cps-1)] <- 0 
    
    # use NA instead of zero
    REVector1[ansBuffRows] <- NA
    
    # length(REVector1)
    
  }
  
  REVector1 <- REVector1[which(!is.na(REVector1))]
  
  # initialize another vector of NA 
  REVector2 <- rep(NA, length=length(REVector1))
  
  if(length(REVector2) < pneumoMeasurementBufferLen) {
    pneumoMeasurementBufferLen <- length(REVector2)
  }
                               
  # if(output=="mean") {
  #   REVector2 <- REVector2[c(1:(length(REVector2) - pneumoMeasurementBufferLen))]
  #   REVector2 <- REVector2[c(1:length(REVector2))]
  # }
  
  # compute the sum of absolute differences for the moving window
  # i=pneumoMeasurementBufferLen
  for (i in pneumoMeasurementBufferLen:length(REVector2)) {
    REVector2[(i-pneumoMeasurementBufferLen+1)] <-  sum( REVector1[(i-pneumoMeasurementBufferLen+1):i] )
    # any sum that includes an NA input value will be NA
    # this will ensure that artifacted segments are not included
    # but artifacted segments have been excluded earlier in this function
    # and there should be no NA sums in REVector2
  } 
  
  REVector2 <- round(REVector2)
  
  # head(REVector2, 300)
  # tail(REVector2, 300)
  
  # fix NA values when the out
  if(output=="vector") {
    for(i in 2:length(REVector2)) {
      if(is.na(REVector2[i])) {
        REVector2[i] <- REVector2[(i-1)]
      }
    }
  }
  
  # ts.plot(REVector2)
  
  if(output=="mean") {
    return( mean(REVector2, na.rm=TRUE) )
  } else {
    return(REVector2)
  }
  
} # end pneumoMeasurementFn() function


# pneumoMeasurementFn(dataVector, verbalAnswer)



