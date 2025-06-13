spoofActivity


# requires the Move1 column is present in the ExamDF


### spoof a missing activity sensor ###

yesSpoofActivity <- (forceActivitySpoof || !("Move1" %in% names(chartDF)))

# modified 2-1-2021 to spoof only when missing activity sensor

# 2-13-2021 init option to force/always spoof this data

if(all(activitySpoof && yesSpoofActivity)) {
  
  # activitySpoof is set in the NCCAASCII_init script
  
  # View(chartDF)
  
  chartDF <- activitySpoofFn(myDF=chartDF, weightCoefs=weightCoefsAS)
  # chartDF <- apply(chartDF[,4:6], 1, mean, weights=c(2,2,1))
  
  dataCols <- dataCols + 1
  # include the width for the added activity sensor column
  colWidths <- c(colWidths, colWidths[length(colWidths)])
  cNames <- c(cNames, "Move1")      
  
  # assign("chartDF", chartDF, pos=1)
  
} else 