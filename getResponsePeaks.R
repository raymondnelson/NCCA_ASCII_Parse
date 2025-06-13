# R Function to identify response peaks in the EDA and cardio mid-line
# August 18, 2023
# Raymond Nelson
#
# abstracted from the amplituceExtractPCFn() in the amplitudeExtractPC.R script
#
####


getResponsePeaksFn <- function(tsData, 
                               latRow, 
                               ROWEndRow, 
                               endRowA,
                               endRow, 
                               addLat,
                               strictROW=FALSE, 
                               strictWOE=FALSE ) {
  # R Function to identify response peaks in the EDA and cardio mid-line
  # August 18, 2023
  # Raymond Nelson
  #
  # abstracted from the amplituceExtractPCFn() in the amplitudeExtractPC.R script
  #
  # called by the amplitudeExtractPCFn()
  #
  # tsData input is the time series data for EDA or cardio mid-line
  # including the prestimulus and poststimulus segments
  #
  # latRow input is the latency end row 
  # ROWEndRow input is the end of the ROW
  # endRow input is the end of the WOE
  # strictROW is a boolean to allow + slope segments after complexity during the WOE
  # strictWOE is a boolean to score to a + slope peak that starts in the ROW and ends outside the WOE
  #
  # output is a vector of peak indices for the stumulus segment
  #
  ####
  
  #### make a vector of slope values ####
  
  {
    theSlope <- getTheSlopeFn(tsData)
    # theSlope inclues 0 1 and -1 values
  }
  
  ####   locate the response peak indices   ####
  
  {
    # response peaks are the onset of negative slope segments
    # or the end of positive slope segments
    
    theSlope <- getTheSlopeFn(tsData)
    
    xNegSlope <- getNegSlopefn(theSlope)
    
    # keep only the peak indices
    xPeak <- which(xNegSlope == -1)
    
    # there may be no peak indices if the data descend persistently
    if(length(xPeak) == 0) xPeak <- NA
    
    # keep only those xPeak indices after latency
    xPeak <- xPeak[which(xPeak >= (latRow + (addLat*cps)))]
  }
  
  #### remove peaks prior to the latency row ####
  
  {
  
    xPeak <- xPeak[xPeak > latRow]
  
  }
  
  ####  add the endRow as a peak ####
  
  {
    # endRow is the end of the EW
    
    xPeak <- sort(unique(c(xPeak, endRow)))
    # this will also remove NA values in case there is no xPeakAdd
    
    # Jun 23, 2023
    if(all(theSlope[endRow:length(theSlope)]==1)) {
      # if the time series data are all + slope from endRow to the end of the segment
      # add the end row for the segment
      xPeak <- c(xPeak, length(theSlope))
    }
    
    # there will always be at least 1 xPeak at this point
  }
  
  #### strict Window option to keep the first latePeak after endRow ####
  
  {
    # for the strictWOE==FALSE option 
    
    if(length(which(xPeak > endRow)) > 0) {
      # only if there are any peaks after the EW
      # keep the first peak after the EW
      xPeakAdd <- xPeak[min(which(xPeak > endRow), na.rm=TRUE)]
      # keep the xPeakAdd only if the slope remain positive after endRowA
      # use the theSlope vector to remove the latePeak after neg slope
      #  use endRowA to exclude + slope segments late in the EW
      if(!all(theSlope[endRowA:(xPeakAdd-1)] == 1)) {
        # endRowA
        # set the late peak to NA if the data are negative after the short EW
        xPeakAdd <- NA
      }
      # a later step may remove the late peak
      # depending on the strictWOE environment parameter
    } else {
      xPeakAdd <- NA
    }
    
    # keep only those xPeak indices that are 
    # before the end of the evaluation window
    # for now
    xPeak <- xPeak[xPeak <= endRow]
    #  xPeakAdd (late peak) is added next 
    
    if(!isTRUE(strictWOE)) {
      # add one peak after the endRow  
      # contingent upon the strictWOE environment parameter
      # set in the NCCAASCII_init.R script
      xPeak <- sort(c(xPeak, xPeakAdd))
    }
  }
  
  #### remove late ascending segments for strictROW==FALSE ####
  
  if(!isTRUE(strictROW)) {
    # June 23, 2023
    # when using the extended ROW
    # exclude peaks after the first -1 (neg slope) row after endRowA
    
    # for both strictEW=TRUE and strictEW=FALSE
    
    # locate the index at which to end the extended the ROW
    # endRowA is the portion of the WOE during with all + slope segments may be used
    if(all(theSlope[endRowA:length(theSlope)]==1)) {
      # intialize an extra ROW end index
      extROWEnd <- length(theSlope)
      # use the number of rows in the segmentDF if the time series data are uniformly +
    } else if(length(which(theSlope[endRowA:length(theSlope)] == -1)) == 0) {
      # if the there are no negative slope indices in the segment 
      extROWEnd <- xPeak[length(xPeak)]
    } else {
      # if there are any - slope indices after endRowA
      # keep the first peak (onset of - slope) after endRowA
      extROWEnd <- endRowA + min(which(theSlope[endRowA:length(theSlope)] == -1)) - 1
      # peaks after extROWEnd will be excluded
    }    
    
    xPeak <- xPeak[which(xPeak <= extROWEnd)]
    
    # this is necessary only for strictROW==FALSE
    
    # when strictROW==TRUE 
    # all ascending segments that begin after the ROW are always removed
    
  }
  
  #### strict ROW option ####
  
  if(isTRUE(strictROW)) {
    # exclude xPeak indices after data begin to descend after ROWEndRow
    if(length(!is.na(xPeak)) > 0) {
      # sort the xPeak indices and remove NAs
      xPeak <- sort(xPeak)
      # use a loop to inspect the slope prior to each peak after ROWEndRow
      for(n in 1:length(xPeak)) {
        if(length(xPeak[n])==0) next()
        if(is.na(xPeak[n])) next()
        if(xPeak[n] > ROWEndRow) {
          # only for xPeak indices after ROWEndRow
          if(any(theSlope[ROWEndRow:(xPeak[n]-1)] == -1)) {
            xPeak[n] <- NA
          } # end if any - slope after ROWEndRow
        } # end if for xPeak after ROWEndRow
      } # end loop n for xPeak indices
      # sort again and remove NAs
      xPeak <- sort(xPeak)
    }
  }
  
  
  # output is a vector of peak row indices 
  return(xPeak)
  
} # end getResponsePeaksFn()