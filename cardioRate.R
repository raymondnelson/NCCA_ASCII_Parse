# function to calculate the cardio rate and arrhythmia and unresponsive data


# uses the ratePerMin() function from the sigProcHelper.R script
# source(paste0(RPath, 'R/NCCA_ASCII_Parse/sigProcHelper.R'), echo=FALSE)

# source(paste0(RPath, 'R/NCCA_ASCII_Parse/dataCheck.R'), echo=FALSE)

# source(paste0(RPath, 'R/NCCA_ASCII_Parse/rbpfProb.R'), echo=FALSE)


cardioRateFn <- function(x=chartDF, y=NULL) {
  # function to calculate the cardio rate and arrhythmia and unresponsive data
  # 4-24-2017
  # 
  # also 
  # 
  # x input is the chart data from which the respiration and cardio channels are used
  # y input is the evaluation period in seconds
  # if y == NULL then the RBPF proportion is returned for the chart
  # if y != NULL then an RBPF vector is returned
  # 
  # output is the chartDF if y==NULL 
  # or the cardio rate and arrhythmia message if y!=NULL
  # 
  ####
  
  chartDF <- x
  
  # chartName
  
  {
    
    uniqueEvents <- unique(chartDF$eventLabel) 
    uniqueEvents <- uniqueEvents[uniqueEvents != ""]
    
    # get the first and last events
    # to ignore the trashy unused data segment before the X
    # use the getFirstLastEventFn() in the sigProcHelper.R script
    firstLast <- getFirstLastEventFn(x=chartDF)
    # print(firstLast)
    firstEvent <- firstLast['firstEvent']
    lastEvent <- firstLast['lastEventEnd']
    
    chartDF$c_CardioRate <- 0
    chartDF$CardioRateMessage_a <- 0
    
    chartDF$CardioUnresponse_a <- 0
    chartDF$CardioUnresponseMessage_a <- 0
    
    chartDF$CardioArrhythmia_a <- 0
    chartDF$CardioArrhythmiaMessage_a <- 0
    
    # chartDF$CardioMid_a <- ""
    chartDF$CardioRBPF_a  <- ""
    chartDF$CardioRBPFMessage_a <- "" 
    
  }
  
  ########### check for unresponsive data
  
  # unreponseMsg <- dataCheckFn(x=chartDF$c_Cardio1,
  # chartDF <- dataCheckFn(x=chartDF$c_Cardio1,
  #                             sec=5, 
  #                             times=30, 
  #                             omit=10, 
  #                             firstRow=firstEvent, 
  #                             lastRow=lastEvent, 
  #                             sVal=scaleVals['cardio'],
  #                             columnName="CardioUnresponse_a",
  #                             output="dataframe" )
  
  
  # chartDF$CardioUnresponse_a <- unreponseMsg
    
  ################### calculate the cardio arrhythmia ####################

	# 	dataVector <- chartDF$c_Cardio1
		
  #   # 2-21-2019 attempt to get a more robust estimate of the cardio rate
  #   cardioRate1 <- ratePerMin(dataVector,buffer=2)
  #   cardioRate2 <- ratePerMin(dataVector,buffer=4)
  #   cardioRate3 <- ratePerMin(dataVector,buffer=6)
  #   cardioRate4 <- ratePerMin(dataVector,buffer=8)
  #   cardioRate5 <- ratePerMin(dataVector,buffer=10)
  #   cardioRate6 <- ratePerMin(dataVector,buffer=12)
  #   cardioRate7 <- ratePerMin(dataVector,buffer=14)
  #   cardioRate8 <- ratePerMin(dataVector,buffer=16)
    
    # cardioRate <- median(c(cardioRate1, 
    #                        cardioRate2, 
    #                        cardioRate3, 
    #                        cardioRate4, 
    #                        cardioRate5, 
    #                        cardioRate6,
    #                        cardioRate7,
    #                        cardioRate8 ))
    
    # # cardioRate <- ratePerMin(dataVector,buffer=8)
    
    # # use the cardio rate to set the buffer length for peak extraction
    # bufferLen <- bufferLenFn(cardioRate)


  ####

  
  # cardioMsg <- cardioArrhythmiaFn(chartDF$c_Cardio1[firstEvent:lastEvent])
  
  # get the pulse rate using systolic peaks
  systRate <- ratePerMin(chartDF$c_Cardio1[firstEvent:lastEvent],
                         buffer=9,
                         peaks="upper",
                         lowPass=TRUE)
  
  # compute the systolic buffer lengths 
  systBufferLen <- bufferLenFn(x=systRate, y=.6)
  # 3-17-2017 was y = .5 and y=.6, default is .67
  
  # get the pulse rate useing diastolic peaks
  diastRate <- ratePerMin(chartDF$c_Cardio1[firstEvent:lastEvent],
                          buffer=9,
                          peaks="lower",
                          lowPass=TRUE)
  
  # compute the diastolic buffer lengths 
  diastBufferLen <- bufferLenFn(x=diastRate, y=.6)
  # 3-17-2017 was y = .5 and y=.6, default is .67
  
  # compare the systolic and diastolic cardio rates
  if(exp(-abs(log(systRate/diastRate))) >= .95) {
    # systolic and diastolic rates are similar
    # cardioRate <- systRate
    # chartDF$c_CardioRate <- cardioRate
    # cardioMsg <- paste("cardio rate:", cardioRate)
    cardioMsg <- "none"
    if(cardioMsg != "none") print(cardioMsg)
  } else {
    # systolic and diastolic rates are dissimilar
    # chartDF$Cardio1_a <- paste("outside normal range", cardioRate1)
    # cardioRate <- systRate
    # chartDF$c_CardioRate <- cardioRate
    # chartDF$CardioArrhythmia_a <-  "possible cardio arrythmia"
    # cardioMsg <- paste("cardio rate:", cardioRate, "possible cardio arrythmia")
    cardioMsg <- "possible cardio arrhythmia"
    print(cardioMsg)
    # stop("check for cardio arrythmia")
  } 
  
  chartDF$CardioArrhythmia_a <-  cardioMsg
  
  ################### calculate the cardio rate ####################
  
  cardioRate <- systRate
  chartDF$c_CardioRate <- cardioRate
  
  if(cardioMsg == "possible cardio arrhythmia") {
    cardioMsg <- paste("cardio rate:", cardioRate, cardioMsg)
  } else {
    cardioMsg <- paste("cardio rate:", cardioRate)
  }
  print(cardioMsg)
  
  ############ calculate beat to beat intervals ##############
  
  # get the systolic peaks
  systPeaks <- maxPeak(x=chartDF$c_Cardio1, y=systBufferLen)
  # length(maxPeaks)
  # calculate the systolic beat to beat intervals (samples)
  systBtoB <- diff(systPeaks)
  # calculate a vector of rate per min for each peak in the maxBtoB vector
  systBtoBRate <- 60 / (c(mean(systBtoB), systBtoB) / cps)
  
  # length(systBtoBRate)
  # mean(systBtoBRate)
  
  # get the diastolic peaks
  diastPeaks <- minPeak(x=chartDF$c_Cardio1, y=diastBufferLen)
  ## calculate the diastolic beat to beat intervals
  diastBtoB <- diff(diastPeaks)
  # calculate a vector of rate per min for each peak in the minBtoB vector
  diastBtoBRate <- 60 / (c(mean(diastBtoB), diastBtoB) / cps)
  
  # length(diastBtoBRate)
  # mean(diastBtoBRate)
  
  ####
  
  # calculate the RBPF score using the rbpfProbFn() in the rbpfProb.R script
  rbpfResult <- rbpfProbFn(x=chartDF, y=NULL)
  
  # add the rbpf score to the chart data frame
  if(strsplit(rbpfResult, ": ")[[1]][2] == "unusual respiration") {
    chartDF$CardioRBPF_a <- "unusual respiration"
  } else if(strsplit(rbpfResult, ": ")[[1]][2] %in% c("NA", "none")) {
    chartDF$CardioRBPF_a <- "none"
  } else {
    chartDF$CardioRBPF_a <- as.numeric(strsplit(rbpfResult, ": ")[[1]][2])
  }
  
  # add the message to the data frame if the rpbf score is significant
  if( !(strsplit(rbpfResult, ": ")[[1]][2] %in% 
     c("unusual respiration", "none", "NA")) ) {
    if(as.numeric(strsplit(rbpfResult, ": ")[[1]][2]) >= .95) {
      # chartDF$CardioMid_a <- rbpfResult
      chartDF$CardioRBPFMessage_a <- strsplit(rbpfResult, ": ")[[1]][1]
    } 
  }
  
  # View(chartDF)
  
  ######### iterate over the cardio data if y != NULL ###########
  
  # y=15
  if(!is.null(y)) {

    systRateVector <- rep(0, times=nrow(chartDF))
    systRateVector[systPeaks] <- systBtoBRate
    diastRateVector <- rep(0, times=nrow(chartDF))
    diastRateVector[diastPeaks] <- diastBtoBRate

    # first fill the 0s
    for(i in 2:length(systRateVector)) {
      if(systRateVector[i] == 0) systRateVector[i] <- systRateVector[(i-1)]
      if(diastRateVector[i] == 0) diastRateVector[i] <- diastRateVector[(i-1)]
    }

    # then average over the y perdiod
    startIdx <- y * cps
    for(i in startIdx:length(systRateVector)) {
      systRateVector[i] <- mean(systRateVector[(i-startIdx+1):i])
      diastRateVector[i] <- mean(diastRateVector[(i-startIdx+1):i])
    }

    # submit the cardio rate to the chartDF
    chartDF$c_CardioRate <- systRateVector

    # recalculate the cardio
    # compare the systolic and diastolic and add the result to the chartDF
    arrhythmiaResult <- exp(-abs(log(systRateVector/diastRateVector)))
    chartDF$CardioArrhythmia_a <- arrhythmiaResult

    # work on the arrhythmia message
    arrhythmiaMsgVector <- rep("", times=length(arrhythmiaResult))
    arrhythmiaMsgVector[which(arrhythmiaResult < .95)] <- "possible arrhythmia"
    chartDF$CardioArrhythmiaMessage_a <- arrhythmiaMsgVector
    # which(arrhythmiaMsgVector != "")
    # View(ChartDF)

    # initialize a vector for the message
    systRateMsg <- rep("", times=nrow(chartDF))
    diastRateMsg <- rep("", times=nrow(chartDF))
    cardioRateMsg <- rep("", times=nrow(chartDF))

    # set the cardioRate message
    systRateMsg[which(systRateVector < 60)] <- "bradychardia"
    systRateMsg[which(systRateVector > 100)] <- "tachychardia"
    diastRateMsg[which(diastRateVector < 60)] <- "bradychardia"
    diastRateMsg[which(diastRateVector > 100)] <- "tachychardia"
    cardioRateMsg[which(systRateMsg != "")] <- systRateMsg[systRateMsg != ""]
    cardioRateMsg[which(diastRateMsg != "")] <- diastRateMsg[diastRateMsg != ""]

    # submit the cardioRateMsg to the chart data frame
    chartDF$CardioRateMessage_a <- cardioRateMsg

  } # end if y is not NULL
  
  ####
  
  # return(chartDF)

  if(is.null(y)) {
    return(cardioMsg)
  } else {
    return(chartDF)
  }
  
} # end cardioRateFn()

# cardioRateFn(x=chartDF, y=NULL)
# cardioRateFn(x=chartDF, y=10)
# chartDF <- cardioRateFn(x=chartDF, y=NULL)


