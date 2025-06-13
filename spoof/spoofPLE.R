spoofPLE

# requires the PPG1 column is present in the examDF


### spoof a missing PLE sensor ###

yesSpoofPLE <- (forcePLESpoof || !("PPG1" %in% names(chartDF)))

if(isTRUE(PLESpoof) && yesSpoofPLE) {
  
  # runs only if isTRUE(forcePLESpoof) or if the PLE sensor is missing
  
  # PLESpoof is set in the NCCAASCII_init script
  
  # View(chartDF)
  
  chartDF <- PLESpoofFn(myDF=chartDF)
  
  dataCols <- dataCols + 1
  colWidths <- c(colWidths, colWidths[length(colWidths)])
  cNames <- c(cNames, "PPG1")
  
}

