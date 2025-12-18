# R Function to identify response onset indices in the EDA and cardio mid-line
# August 18, 2023
# Raymond Nelson
#
# abstracted from the amplituceExtractPCFn() in the amplitudeExtractPC.R script
#
####



# source("~/Dropbox/R/NCCA_ASCII_Parse/slopeChange.R", echo=FALSE)



getResponseOnsetsFn <- function(tsData, 
                                xPeak,
                                onsetRow,
                                latRow, 
                                ROWEndRow,
                                endRow,
                                slopeChangeRule, 
                                addLat) {
  # R Function to identify response onset indices in the EDA and cardio mid-line
  # August 18, 2023
  # Raymond Nelson
  #
  # abstracted from the amplituceExtractPCFn() in the amplitudeExtractPC.R script
  #
  ####
  
  options(warn = 2)
  
  ####  make a vector of slope values ####
  
  {
    # response onsets are the onset of positive slope segments
    
    theSlope <- getTheSlopeFn(tsData)
    # theSlope inclues 0 1 and -1 values
    
    xPosSlope <- getPosSlopeFn(theSlope)
    
    # keep only the onset indices
    xOnset <- which(xPosSlope == 1)
    
    # there may be no onset indices if the data descend persistently
    # or if the data ascend persistent from before question onset
    if(length(xOnset) == 0) xOnset <- NA
  }
  
  #### keep only xOnset indices after latency and during ROW ####
  
  {
    xOnset <- xOnset[which(xOnset >= latRow)]
    
    # keep only those xOnset indices that are 
    # before the end of the ROW
    xOnset <- xOnset[xOnset <= ROWEndRow]
    
    if(length(xOnset) == 0) xOnset <- NA
    
    # set xOnset to NA if no xPeak indices
    # Aug 14, 2020 seems to cause a problem with the cardio
    # if(all(!is.na(xPeak))) xOnset <- NA
  }
  
  #### extract a response onset via change in positive slope inflection ####
  
  # inflection=2 # statistical method
  
  # inflection is set in the NCCAASCII_init.R script
  
  if(inflection == 1) {
    
    # inflection==1 will pick the y-axis value at 2.5 seconds for all cases
    
    if(!exists("addLat")) addLat <- 2
    
    # use the slopeChangeRule parameter
    # 1 for EDA and 0 for cardio
    # this is a blunt approximation
    # instead of evaluating the change in slope activity
    # works surprisingly well
    if(slopeChangeRule == 1) {
      # but only if there are any non-NA peak indices
      # possible this could work OK under all conditions
      if(any(!is.na(xPeak))) {
        xOnset <- c(round(latRow+(addLat*cps), 0), xOnset)
        # indices may be out of sequence at this point
      }
      # there will always be at least 1 onset at this point
      # set the order and remove NAs
      xOnset <- sort(xOnset)
    }
    
    # impute the response onset only if no XOnset indices
    if(slopeChangeRule == 2) {
      # xOnset will be NA if no changes from - or 0 slope to + slope
      if(all(is.na(xOnset))) {
        if(any(!is.na(xPeak))) {
          xOnset <- c(round(latRow+(addLat*cps), 0), xOnset)
          # does not matter that the indices may be out of sequence
          xOnset <- sort(xOnset)
          # NAs are removed by sort()
        }
      }
    }
    
    xOnset <- unique(xOnset)
    # NAs are retained if any are present
    l
    
  } # end if( inflection == 1 )
  
  #### alternate method use the maxSlopeChangeFn ####
  
  if(inflection == 2) {
    
    # statistical method
    
    # source("~/Dropbox/R/NCCA_ASCII_Parse/slopeChange.R", echo=TRUE)
    
    # maxSlopeChangeFn() uses a moving t-test of variance for all
    # for adjacent 1s segments
    # with alpha=.001
    
    if(slopeChangeRule == 1) {
      
      # always use the slopeChangeRule
      
      if(any(!is.na(xPeak))) {
        # if there are any valid xPeak indices
        # source the maxSlopeChange script
        # source("~/Dropbox/R/NCCA_ASCII_Parse/slopeChange.R", echo=FALSE)
        # call the maxSlopeChangeFn()
        theseIdcs <- maxSlopeChangeFn(x=tsData, idx=TRUE)
        # keep only those that are after the latency row
        theseIdcs <- theseIdcs[theseIdcs >= latRow]
        # Aug 4, 2022
        # keep only those that are after sChangeLat
        # Aug 11, 2023 onsetRow was previously hardcoded to 301
        theseIdcs <- theseIdcs[theseIdcs >= (onsetRow + (sChangeLat*cps) - 1)]
        # Aug 4, 2022
        # put the slope change at the end of the pre and post segs
        # theseIdcs <- theseIdcs + (nPre*cps + nPost*cps)
        # August 23, 2023 to reduce the delay of the imputed response onset
        # put the slope change at the end of the pre len segment
        theseIdcs <- theseIdcs + (nPre*cps)
        # add the imputed onset indices to the xOnset vector
        xOnset <- sort(unique(c(theseIdcs, xOnset)))
      }
      # set the order and remove NAs
      xOnset <- sort(xOnset)
      
    }
    
    if(slopeChangeRule == 2) {
      # keep the 
      if(all(is.na(xOnset)) && any(!is.na(xPeak))) {
        theseIdcs <- maxSlopeChangeFn(x=tsData, idx=TRUE)
        # keep only those that are after the latency row
        # theseIdcs <- theseIdcs[theseIdcs >= latRow]
        theseIdcs <- theseIdcs[theseIdcs >= (onsetRow + (sChangeLat*cps) - 1)]
        xOnset <- c(theseIdcs, xOnset)
        # set the order and remove NAs
        xOnset <- sort(xOnset)
      }
    }
    
    xOnset <- unique(xOnset)
    # NAs are retained if any are present
    
  }
  
  #### repeat ### Sep 27, 2021 ####
  
  {
    # remove xOnset indices prior to the latency index
    xOnset <- xOnset[which(xOnset >= latRow)]
    
    # remove xOnset indices after the end of the ROW
    xOnset <- xOnset[xOnset <= ROWEndRow]
  }
  
  # #### exclude peaks after the data descend below the onset value ####
  # 
  # {
  #   # initialize this to NA to avoid problems
  #   postROWXOnset <- NULL
  #   # use the xPosSlope vector from earlier
  #   
  #   # get the pos slope onset indices after ROWEndRow
  #   postROWXOnset <- which(xPosSlope[ROWEndRow:endRow] == 1) + ROWEndRow - 1
  #   
  #   if(length(postROWXOnset) > 0) {
  #     # get the data values for post ROW onset indices
  #     postROWXOnsetVals <- tsData[postROWXOnset]
  #     minPostROWVal <- min(tsData[postROWXOnset])
  #     
  #     # get min xOnset
  #     thisMinOnset <- xOnset[which.min(tsData[xOnset])]
  #     minOnsetVal <- tsData[thisMinOnset]
  #   }
  # }
  
  return(xOnset)
  
} # end getResponseOnsetsFn()



