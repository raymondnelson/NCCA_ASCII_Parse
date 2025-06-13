# EDA artifact extraction


edaArtifactFn <- function(x=chartDF) {
  
  chartDF <- x
  
  ### finger movement artifacts 
  
  # make a vector to work with the EDA data
  EDAData <- chartDF$c_AutoEDA
  
  # set all values to NA if not less than the mid 
  # so that descending changes above the mide are not artifacted
  EDAData[which(chartDF$c_AutoEDA > chartDF$c_AutoEDAMid)] <- NA
  
  # source('~/Dropbox/R/NCCA_ASCII_Parse/TukeyFences.R')
  
  # then compute the lower tukey fence 
  # chartDF$c_AutoEDA_a[tukeyFence2(x=EDAData,y=EDAMid,z=3,fence="lower")] <- "Artifact"
  EDAArtifact <- tukeyFence4(x=EDAData,y=chartDF$c_AutoEDA,z=15,fence="lower")
  chartDF$AutoEDA_a[EDAArtifact] <- "Artifact"
  
  
  
  ### add the artifacts to the chart data frame
  
  #         chartDF$AutoEDA_a[maxAmpChange] <- "Artifact"
  #         # chartDF$AutoEDAMid_a[maxAmpChange] <- "Artifact"
  #         # chartDF$AutoEDABase_a[minAmpChange] <- "Artifact"
  #         chartDF$AutoEDAPeak_a[minAmpChange] <- "Artifact"
  #         # chartDF$AutoEDADiff_a <-
  
  
  
  
  return(chartDF)
  
}