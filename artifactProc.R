# function to process artifacts on all data channels
# 4/12/2025
# Raymond Nelson

################################



# library(stringr)
 


# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[17]

{

  ## respiration ##
  source(paste0(RPath, 'pneumoArtifact.R'), echo=FALSE)
  # check for unresponsive respiration data
  # source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
  source(paste0(RPath, 'pneumoCheck.R'), echo=FALSE)
  source(paste0(RPath, 'tukeyFences.R'), echo=FALSE)
  
  sec <- .5
  cutProp <- .1
  # sec=3 cutProp=.5 works well 3-18-2017
  # sec=1 cutProb=.25 also works ok
  # sec2=4 cutProp2=.5 works well 3-19-2017
  sec2 <- 3
  cutProp2 <- .005
  
  source(paste0(RPath, 'TukeyFences.R'), echo=FALSE)

  ## EDA ##
  
  # source(paste0(RPath, 'edaArtifact.R'), echo=FALSE)
  # # load a function to check for unresponsive EDA data
  # source(paste0(RPath, 'dataCheck.R'), echo=FALSE)
  # # define a function for finger movement artifacts
  # source(paste0(RPath, 'EDAMvtArtifact.R'), echo=FALSE)
  # # define a function for non-stim activity that exceeds stim segments
  # source(paste0(RPath, 'nonstimArtifacts.R'), echo=FALSE)
  # # define a function to calculate finger movement artifacts
  # source(paste0(RPath, 'TukeyFences.R'), echo=FALSE)
  
  # Sep 2, 2023
  source(paste0(RPath, "checkEDATonicity.R"), echo=FALSE)
  
  # source(paste0(RPath, "CardioArtifacts.R"), echo=FALSE)
  
  # load the fillSlope function
  source(paste0(RPath, 'amplitudeExtractHelperFunctions.R'), echo=FALSE)
  
  # digital filters
  source(paste0(RPath, 'sigProc_extra.R'), echo=FALSE)
  
  
  ## cardio ##
  
  source(paste0(RPath, 'cardioArtifact.R'), echo=FALSE)
  source(paste0(RPath, 'rbpfProb.R'), echo=FALSE)
  source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
  source(paste0(RPath, 'TukeyFences.R'), echo=FALSE)
  source(paste0(RPath, 'cardioRate.R'), echo=FALSE)
  # source(paste0(RPath, 'cardioArrythmia.R'), echo=FALSE)
  source(paste0(RPath, 'artifactExtractHelper.R'), echo=FALSE)
  # load a function to check for unresponsive data
  # source(paste0(RPath, 'dataCheck.R'), echo=FALSE)
  
  
  ## PLE ##
  source(paste0(RPath, 'pleArtifact.R'), echo=FALSE)
  
  ## activity ##
  source(paste0(RPath, 'activityCheck.R'), echo=FALSE)
  source(paste0(RPath, 'activityArtifact.R'), echo=FALSE)
  source(paste0(RPath, "newActivityCheck.R"), echo=FALSE)

}

##############


# x=uniqueExams
# showNames=TRUE
# output=FALSE
# i=1
# j=1
# k=1


artifactProcFn<- function(x=uniqueExams) {
  # function to iterate over a vector of data frame names 
  # and locate data artifacts in the NCCA ASCII time series data
  # x is a vector of names of data frames that contain the
  # time series data fro all charts for each exam
  # showNames=TRUE will print the exam, series and chart names to the console
  # output=TRUE will return a data frame for the last input exam

  ####
  
  uniqueExams <- x

  # loop over each exam in the list 
  i=1
  for(i in 1:length(uniqueExams)) {
    
    {
      
      if(!exists("i")) i <- 1
      
      examName <- uniqueExams[i]
      
      # get the names of time series lists for all unique series in each exam
      searchString <- paste0("*", examName, "_Data", "*")
      examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
      
      if(showNames==TRUE) {
        print(paste(examName, i, "of", length(uniqueExams)))
      }
      
      examOnsetRow <- 1
      examEndRow <- nrow(examDF)
      
      assign("examDF", examDF, pos=1)
      # assign("examName", examName, pos=1)
      
      ####
      
      # get the names of unique series
      uniqueSeries <- as.character(unique(examDF$seriesName))
      
      # examDF$Artifacts_a <- 0
      # which(examDF$Artifacts_a != 0)
      
    }
    
    # loop over each unique series
    j=1
    for(j in 1:length(uniqueSeries)) {
      
      {
        
        if(!exists("j")) j <- 1
        
        seriesName <- uniqueSeries[j]
        
        if(showNames==TRUE) print(paste("series", seriesName))
        
        # get the list of time series data for the charts in the exam
        seriesRows <- which(examDF$seriesName==seriesName)
        seriesDF <- examDF[seriesRows,]
        
        seriesOnsetRow <- which(examDF$seriesName==uniqueSeries[j])[1]
        seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
        
        assign("seriesDF", seriesDF, pos=1)
        # assign("seriesName", seriesName, pos=1)
        
        # uniqueCharts <- names(seriesDF)
        uniqueCharts <- as.character(unique(seriesDF$chartName))
        
      }
      
      # loop over each chart in the series 
      k=1
      for(k in 1:length(uniqueCharts)) {
        
        {
          
          if(!exists("k")) k <- 1
          
          chartName <- uniqueCharts[k]
          
          # get the data frame with the time series data for each chart in the series
          # chartDF <- seriesDF[[k]]
          chartRows <- which(seriesDF$chartName==chartName)
          chartDF <- seriesDF[chartRows,]
          
          if(nrow(chartDF)<(20*cps)) next()
          
          if(showNames==TRUE) print(chartName)
          
          chartOnsetRow <- which(seriesDF$chartName==uniqueCharts[k])[1]
          chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
          
          eventNames <- toupper(chartDF$eventLabel[chartDF$eventLabel!=""])
          
          # chartDF$Artifacts_a <- 0
          # which(chartDF$Artifacts_a != 0)
          
          assign("chartDF", chartDF, pos=1)
          # assign("chartName", chartName, pos=1)
          
          # if(chartName == "02A") {
          #   assign("chartDF", chartDF, pos=1)
          #   assign("chartName", chartName, pos=1)
          #   stop()
          # }
          
        }
        
        ##############
        
        {
        
          # was previously in the feature extraction script.
          # 2025, May 10
          
          chartDF$AutoEDA_a <- 0
          chartDF$Artifacts_a <- 0
          chartDF$UPneumo_a <- 0
          chartDF$LPneumo_a <- 0
          chartDF$Pneumo_a <- 0
          chartDF$Cardio1_a <- 0
          if(sum(pmatch(names(chartDF), "c_PPG1", nomatch=0))!=0) {
            chartDF$PPG1_a <- 0
          }
            
        }
        
        ####################### pneumo artifacts ####################
        
        if(isTRUE(pneumoArtifacts)) {
          
          # if(showNames==TRUE)  print("  pneumo artifacts")
          # 
          # # check for unresponsive pneumo data
          # # source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
          # # source(paste0(RPath, 'tukeyFences.R'), echo=FALSE)
          # # source(paste0(RPath, 'pneumoCheck.R'), echo=FALSE)
          # 
          # # 2020-06-03 seems to be a problems with adding NUL
          # pneumoCheckMsg <- pneumoCheckFn(x1=chartDF$c_UPneumoSm, x2=chartDF$c_LPneumoSm, sec=5, times=30, omit=10, firstRow=NULL, lastRow=NULL)
          # 
          # chartDF$LPneumoUnresponse_a <- pneumoCheckMsg
          # chartDF$UPneumoUnresponse_a <- pneumoCheckMsg
          # 
          # # check for respiration artifacts
          # # source(paste0(RPath, 'pneumoArtifact.R'), echo=FALSE)
          # chartDF <- pneumoArtifactFn(x=chartDF)
          # # stop()
          # 
          # # which(chartDF$Pneumo_a == "Artifact")
          # # which(chartDF$UPneumo_a == "Artifact")
          # # which(chartDF$LPneumo_a == "Artifact")
          
        } 
        
        ####################### EDA artifacts ############################
        
        if(edaArtifacts == TRUE){
          
          # edaArtifacts is an environment variable that is set in the 
          
          if(showNames==TRUE) print("  EDA artifacts")
          
          # source(paste0(RPath, 'dataCheck.R'), echo=FALSE)
          # source(paste0(RPath, 'EDAMvtArtifact.R'), echo=FALSE)
          # source(paste0(RPath, 'nonstimArtifacts.R'), echo=FALSE)
          # source(paste0(RPath, 'TukeyFences.R'), echo=FALSE)
          # source(paste0(RPath, "checkEDATonicity.R"), echo=FALSE)
          
          # chartDF <- edaArtifactFn(x=chartDF)
          
          EDAMvtArtifactVc <- EDAMvtArtifactFn(tsData=chartDF$c_AutoEDA, preSec=4, zCut=3)
          
          chartDF$AutoEDA_a[which(EDAMvtArtifactVc != "0")] <- "Artifact"
          
          # chartDF$AutoEDA_a[which(chartDF$AutoEDA_a != "0")]
          
          # 2025May10
          # also put the artifacts in the artifact channel
          # for submission to the test of proportions
          # chartDF$Artifacts_a[which(chartDF$AutoEDA_a != "0")] <- "Artifact"
          
          # chartDF$AutoEDA_a[which(chartDF$AutoEDA_a != "0")]
          
        }
              
        ####################### cardio artifacts #########################
        
        if(isTRUE(cardioArtifacts)) {

          # if(showNames==TRUE) print("  cardio artifacts")
          # 
          # # source(paste0(RPath, 'rbpfProb.R'), echo=FALSE)
          # # source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
          # # source(paste0(RPath, 'TukeyFences.R'), echo=FALSE)
          # # source(paste0(RPath, 'cardioRate.R'), echo=FALSE)
          # # source(paste0(RPath, 'cardioArrythmia.R'), echo=FALSE)
          # # source(paste0(RPath, 'artifactExtractHelper.R'), echo=FALSE)
          # 
          # chartDF <- cardioArtifactFn(x=chartDF)

        }
        
        ####################### finger cuff artifacts ####################
        
        {
          # print("  finger cuff artifacts")
          # chartDF <- FCArtifactFn(x=chartDF)
        }
        
        ####################### PLE artifacts ###########################
        
        if(isTRUE(pleArtifacts)) {
          
          # inclPLE <- FALSE
          # 
          # # check to see if PLE data are present in the exam data frame
          # if(sum(pmatch(names(examDF), "c_PL", nomatch=0)) != 0) {
          # 
          #   inclPLE <- TRUE
          # 
          #   if(showNames==TRUE) print("  PLE artifacts")
          #   chartDF <- pleArtifactFn(x=chartDF)
          # 
          # } # end if for PLE
          
        }
        
        ######################## activity artifacts #########################
        
        if(isTRUE(activityArtifacts)) {
        
          inclActivity <- FALSE
          
          # check to see if activity sensor data are present in the examDF
          if( sum(pmatch(names(examDF), "c_Move1", nomatch=0)) != 0 ) {
            
            # only if there is some activity in the activity sensor data
            if( max(chartDF$c_Move1) != min(chartDF$c_Move1) ) {
              
              # source(paste0(RPath, 'activityCheck.R'), echo=FALSE)
              
              # source(paste0(RPath, "newActivityCheck.R", echo=FALSE)
              
              inclActivity <- TRUE
              
              if(showNames==TRUE) print("  activity sensor artifacts")
              # chartDF <- activityArtifactFn(x=chartDF)
              
              # stop()
              
              # chartDF$Move1_a <- ""
              # x$Move1_a <- ""
              
              # chartDF <- activityCheckFn(x=chartDF)
              # which(chartDF$Move1_a == "Artifact")
              # commented out April 12, 2025
              
              # April 12, 2025 new function is more efficient
              chartDF <- newActivityCheckFn(chartDF=chartDF)
              
              
            } # end if
            
          } else {
            print("  WARNING: no activity sensor data") # end if for activity sensor data
          }
          
        }
        
        ####
        
        # assign("chartDF", chartDF, envir=.GlobalEnv)
        # stop()
        
        seriesDF[chartRows,] <- chartDF
        
        # which(chartDF$Pneumo_a == "Artifact")
        # which(chartDF$UPneumo_a == "Artifact")
        # which(chartDF$LPneumo_a == "Artifact")
        # which(seriesDF$Move1_a == "Artifact")
        
        
        ### save the chartDF to the larger examDF for all charts
        # examDF[(chartOnsetRow+seriesOnsetRow-1):(chartEndRow+seriesOnsetRow-1),]  <- chartDF
        
      } # end iteration over k chart data frames 
      
      examDF[seriesRows,] <- seriesDF
      
      assign("seriesDF", seriesDF, envir=.GlobalEnv)
      
    } # end iteration over j series data frames
    
    #### save/assign the _Data to the global env
    assign(paste0(examName, "_Data"), examDF, pos=1)  
      
    assign("examDF", examDF, envir=.GlobalEnv)
    # stop()
    
    # which(examDF$Pneumo_a == "Artifact")
    # which(examDF$UPneumo_a == "Artifact")
    # which(examDF$LPneumo_a == "Artifact")
    
    # which(chartDF$Pneumo_a == "Artifact")
    # which(chartDF$UPneumo_a == "Artifact")
    # which(chartDF$LPneumo_a == "Artifact")
    
  } # end iteration over i exams
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  if(output==TRUE) return(examDF)
  
} # end artifactProc() function

# artifactProc(x=uniqueExams, showNames=TRUE, output=FALSE)





