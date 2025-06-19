# EDA artifact extraction


edaArtifactFn <- function(x=chartDF) {
  
  chartDF <- x
  
  chartDF$AutoEDA_a <- ""
  
  ######## check for unresponsive EDA data ########
  
  # source(paste0(RPath, 'dataCheck.R'), echo=FALSE))
  chartDF$AutoEDAUnresponse_a <- dataCheckFn(x=chartDF$c_AutoEDA)
    
  ######## non-specific physiological activity ########
  
  # source(paste0(RPath, 'nonstimArtifacts.R'), echo=FALSE)
  chartDF <- nonStimArtifactFn(x=chartDF)
  # which(chartDF$AutoEDA_a == "artifact1a") # prints as red
  
  ######## finger movement artifacts ########
  
  # assign("chartDF", chartDF, envir=.GlobalEnv)
  # stop()
  
  # source(paste0(RPath, 'EDAMvtArtifact.R'), echo=FALSE)
  AutoEDAArtifacts <- EDAMvtArtifactFn(tsData=chartDF$c_AutoEDA)
  # chartDF$Artifacts_a[AutoEDAArtifacts] <- "artifact"
  chartDF$AutoEDA_a[AutoEDAArtifacts] <- "artifact2"
  # which(chartDF$AutoEDA_a == "artifact2") # prints as brown
  # which(chartDF$AutoEDA_a == "")
  
  
  #### not sure what this one does June 2020 ####
  
  # possibly checks for values that are far below the mid
  
  # # make a vector to work with the EDA data
  # EDAData <- chartDF$c_AutoEDA
  # 
  # # set all values to NA if not less than the mid
  # # so that descending changes above the mid are not artifacted
  # EDAData[which(chartDF$c_AutoEDA > chartDF$c_AutoEDAMid)] <- NA
  # 
  # # then compute the lower tukey fence
  # # source(paste0(RPath, 'NCCA_ASCII_Parse/TukeyFences.R'))
  # # chartDF$c_AutoEDA_a[tukeyFence2(x=EDAData,y=EDAMid,z=3,fence="lower")] <- "Artifact"
  # EDAArtifact <- tukeyFence4(x=EDAData,y=chartDF$c_AutoEDA, z=15, fence="lower")
  # chartDF$AutoEDA_a[EDAArtifact] <- "Artifact"
  # # which(chartDF$AutoEDA_a == "Artifact") # prints as black
  
  
  
  #### add the artifacts to the chart data frame ####
  
  #         chartDF$AutoEDA_a[maxAmpChange] <- "Artifact"
  #         # chartDF$AutoEDAMid_a[maxAmpChange] <- "Artifact"
  #         # chartDF$AutoEDABase_a[minAmpChange] <- "Artifact"
  #         chartDF$AutoEDAPeak_a[minAmpChange] <- "Artifact"
  #         # chartDF$AutoEDADiff_a <-
  
  #### output ####
  
  return(chartDF)
  
}

