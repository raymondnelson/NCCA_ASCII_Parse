# R function to identify cardio data artifacts 
# 2-28-2017
# Raymond Nelson
# 
##############


# cardio artifact extraction


# source the getSegment.R script
# source(paste0(RPath, 'R/NCCA_ASCII_Parse/getSegment.R'), echo=FALSE)


# source(paste0(RPath, 'R/NCCA_ASCII_Parse/rbpfProb.R'), echo=FALSE)

# source(paste0(RPath, 'R/NCCA_ASCII_Parse/sigProcHelper.R'), echo=FALSE)

# source(paste0(RPath, 'R/NCCA_ASCII_Parse/TukeyFences.R'), echo=FALSE)

# source(paste0(RPath, 'R/NCCA_ASCII_Parse/cardioRate.R'), echo=FALSE)

# source(paste0(RPath, 'R/NCCA_ASCII_Parse/cardioArrythmia.R'), echo=FALSE)

# source(paste0(RPath, 'R/NCCA_ASCII_Parse/artifactExtractHelper.R'), echo=FALSE)


cardioArtifactFn <- function(x=chartDF) {
  # function to extract cardio artifacts
  # was called by the artifactProc.R script that iterates 
  # over all charts for all series for all exams
  #
  # 3-2-2017 now called by the getChartFn() function in the getSegment.R script
  #
  # input is a data frame for a single chart
  # output is the chart data frame with the artifacts identified in the _a columns
  # 
  # requires the rbpfProp function in the rbpfProb.R script
  # also requires the sigProcHelper.R script
  # 
  ####

  chartDF <- x
  # nrow(chartDF)
  
  # chartName
  
  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/rbpfProb.R'), echo=FALSE)
  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/sigProcHelper.R'), echo=FALSE)
  
  ####### initialize the cardio artifact vectors in the chartDF ######
  
  {
    # getSegFn(exam=1,series=1,chart=1)
    # View(chartDF)
    
    chartDF$CardioSystolic_a <- ""
    chartDF$CardioDiastolic_a <- ""
    chartDF$CardioMid_a <- ""
    chartDF$CardioMA_a <- ""
    chartDF$Cardio1_a <- ""
  }
  
  ############
  
  # check the cardio rate, arrhythmia, and unresponsive data
  
  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/cardioRate.R'), echo=FALSE)
  chartDF <- cardioRateFn(x=chartDF, y=5)
  # mean(chartDF$c_CardioRate)
  
  
  #############
  
  # # calculate the RBPF score using the rbpfProbFn() in the rbpfProb.R script
  # # chartDF$CardioMid_a <- ""
  # chartDF$CardioRBPF_a  <- ""
  # chartDF$CardioRBPFMessage_a <- "" 
  # 
  # # call the pbpfProbFn()
  # rbpfResult <- rbpfProbFn(x=chartDF, y=NULL)
  # chartDF$CardioRBPF_a <- as.numeric(strsplit(rbpfResult, ": ")[[1]][2])
  # # add the message to the data frame if necesssary
  # if(as.numeric(strsplit(rbpfResult, ": ")[[1]][2]) >= .95) {
  #   # chartDF$CardioMid_a <- rbpfResult
  #   chartDF$CardioRBPFMessage_a <- strsplit(rbpfResult, ": ")[[1]][1]
  # } 
  # 
  # # View(chartDF)
  
  ###########
  
  # # re-calculate the cardio mid line
  # chartDF$c_CardioMid <- MASmooth(x=chartDF$c_Cardio1, y=round(.5*cps,0), times=3)
  # # chartDF$c_CardioMid[1]
  # # testMid <- lowPass1st.2953Hz(x=chartDF$c_Cardio1)
  # # testMid <- lowPass.886(x=chartDF$c_Cardio1)
  # # testMid <- lowPass.886(x=testMid)
  # # testMid <- lowPass.886(x=testMid)
  # # testMid[1]
  # # plot.ts(chartDF$c_Cardio1)
  # # plot.ts(chartDF$c_CardioMid)
  # # plot.ts(testMid)
  # # cor(chartDF$c_Cardio1[1000:1300], chartDF$c_CardioMid[1000:1300])
  # # cor(chartDF$c_CardioMid[1000:1300], testMid[1000:1300])
  # 
  # # re-process the slow moving average - normally done in the scale offset
  # # move this to the init script when done
  # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_Cardio1, y=round(1.5*cps,0), times=3)
  # # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_CardioMA, y=round(1*cps,0), times=1)
  # 
  # # calculate the cardio rate and buffer length
  # # was buffer=3 4-21-2-2017
  # cardioRate <- ratePerMin(chartDF$c_Cardio1,buffer=9,peaks="upper",lowPass=TRUE)
  # bufferLen <- bufferLenFn(x=cardioRate, y=.6)
  # 
  # # locate the systolic peaks
  # systPeaks <- maxPeak(x=chartDF$c_Cardio1, y=bufferLen)
  # # compute the systolic line
  # chartDF$c_CardioSystolic <- interpolatePeaks(x=systPeaks,
  #                                              y=chartDF$c_Cardio1[systPeaks])[1:nrow(chartDF)]
  # 
  # # locate the diastolic peaks
  # diastPeaks <- minPeak(x=chartDF$c_Cardio1, y=bufferLen)
  # # compute the diastolic line
  # chartDF$c_CardioDiastolic <- interpolatePeaks(x=diastPeaks,
  #                                               y=chartDF$c_Cardio1[diastPeaks])[1:nrow(chartDF)]
  
  ################### calculate the cardio rate ####################
  
  # get the pulse rate using systolic peaks
  systRate <- ratePerMin(chartDF$c_Cardio1,buffer=7,peaks="upper",lowPass=TRUE)
  
  # compute the systolic buffer lengths for the Tukey functions
  systBufferLen <- bufferLenFn(x=systRate, y=.6)
  # 3-17-2017 was y = .5 and y=.6, default is .67
  
  # get the pulse rate useing diastolic peaks
  diastRate <- ratePerMin(chartDF$c_Cardio1,buffer=7,peaks="lower",lowPass=TRUE)
  
  # compute the diastolic buffer length for the Tukey functions
  diastBufferLen <- bufferLenFn(x=diastRate, y=.6)
  # 3-17-2017 was y = .5 and y=.6, default is .67
  
  cardioRate <- systRate
  bufferLen <- systBufferLen
  
  # compare the systolic and diastolic cardio rates
  # if(exp(-abs(log(cardioRate1/cardioRate2))) >= .95) {
  #   cardioRate <- cardioRate1
  #   print(paste("cardio rate:", cardioRate1))
  # } else {
  #   # chartDF$Cardio1_a <- paste("outside normal range", cardioRate1)
  #   cardioRate <- cardioRate1
  #   print("possible cardio arrythmia")
  #   # stop("check for cardio arrythmia")
  # } 

  #######################
  
  # get the systolic peaks
  # maxPeaks <- maxPeak(x=chartDF$c_Cardio1, y=bufferLen1)
  # # length(maxPeaks)
  # # calculate the systolic beat to beat intervals (samples)
  # maxBtoB <- diff(maxPeaks)
  # # calculate a vector of rate per min for each peak in the maxBtoB vector
  # maxRate <- 60 / (c(mean(maxBtoB), maxBtoB) / cps)
  
  # get the diastolic peaks
  # minPeaks <- minPeak(x=chartDF$c_Cardio1, y=bufferLen2)
  # ## calculate the diastolic beat to beat intervals
  # minBtoB <- diff(minPeaks)
  # # calculate a vector of rate per min for each peak in the minBtoB vector
  # minRate <- 60 / (c(mean(minBtoB), minBtoB) / cps)
  
  # make a vector of cardioBuffers
  # cardioBuffer <- round(cardioRate/60*c(2:15, 15),0)
  cardioBuffer <- round(cardioRate/60*c(1:5),0)
  # 4-15-2017 1 to 3 seconds works well
  #length(cardioBuffer)
  
  ################# calculate changes in the beat to beat interval ##############
  
  #################### systolic peak to peak intervals
  
  # check for outliers
  # this will give the number of pulses for 10 seconds round(cardioRate/60*10,0)
  # maxBtoBChange <- tukeyFence5(x=maxRate, buffer=round(.333*cps,0), side="upper", fence="outer", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1, output="name")
  # maxExp <- 1
  # TFOuter <- 4.5
  # TFInner <- 1.5
  # TFSide <- "lower"
  # TFFence <- "outer"
  # maxBtoBChange1 <- tukeyFence5(x=maxRate, buffer=round(cardioRate/60*2,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=maxExp, scaleVal=1, output="logical")
  # maxBtoBChange2 <- tukeyFence5(x=maxRate, buffer=round(cardioRate/60*3,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=maxExp, scaleVal=1, output="logical")
  # maxBtoBChange3 <- tukeyFence5(x=maxRate, buffer=round(cardioRate/60*4,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=maxExp, scaleVal=1, output="logical")
  # maxBtoBChange4 <- tukeyFence5(x=maxRate, buffer=round(cardioRate/60*5,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=maxExp, scaleVal=1, output="logical")
  # maxBtoBChange5 <- tukeyFence5(x=maxRate, buffer=round(cardioRate/60*6,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=maxExp, scaleVal=1, output="logical")
  # maxBtoBChange6 <- tukeyFence5(x=maxRate, buffer=round(cardioRate/60*7,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=maxExp, scaleVal=1, output="logical")
  # maxBtoBChange7 <- tukeyFence5(x=maxRate, buffer=round(cardioRate/60*8,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=maxExp, scaleVal=1, output="logical")
  # maxBtoBChange8 <- tukeyFence5(x=maxRate, buffer=round(cardioRate/60*9,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=maxExp, scaleVal=1, output="logical")
  # maxBtoBChange9 <- tukeyFence5(x=maxRate, buffer=round(cardioRate/60*10,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=maxExp, scaleVal=1, output="logical")
  # maxBtoBChange10 <- tukeyFence5(x=maxRate, buffer=round(cardioRate/60*11,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=maxExp, scaleVal=1, output="logical")
  # maxBtoBChange11 <- tukeyFence5(x=maxRate, buffer=round(cardioRate/60*12,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=maxExp, scaleVal=1, output="logical")
  # maxBtoBChange12 <- tukeyFence5(x=maxRate, buffer=round(cardioRate/60*13,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=maxExp, scaleVal=1, output="logical")
  # maxBtoBChange13 <- tukeyFence5(x=maxRate, buffer=round(cardioRate/60*14,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=maxExp, scaleVal=1, output="logical")
  # maxBtoBChange14 <- tukeyFence5(x=maxRate, buffer=round(cardioRate/60*15,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=maxExp, scaleVal=1, output="logical")
  # maxBtoBChange15 <- tukeyFence5(x=maxRate, buffer=round(cardioRate/60*15,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=maxExp, scaleVal=1, output="logical")
  
  # maxBtoBChange4 <- tukeyFence5(x=maxRate, buffer=round(cardioRate/60*5,0), side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1, output="logical")
  # maxBtoBChange5 <- tukeyFence5(x=maxRate, buffer=round(cardioRate/60*10,0), side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1, output="logical")
  # maxBtoBChange6 <- tukeyFence5(x=maxRate, buffer=round(cardioRate/60*15,0), side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1, output="logical")
  # maxBtoBChange4 <- tukeyFence5(x=maxRate, buffer="NULL", side="upper", fence="both", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1, output="logical")
  # was outerFence=6 3-17-2017
  
  # tukeyFence5(x=maxRate, buffer=round(cardioRate/60*10,0), side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=maxExp, scaleVal=1, output="range")
  # tukeyFence5(x=maxRate, buffer=round(cardioRate/60*15,0), side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=maxExp, scaleVal=1, output="logical")
  # tukeyFence5(x=maxRate, buffer=round(cardioRate/60*21,0), side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=maxExp, scaleVal=1, output="logical")
  # tukeyFence5(x=maxRate, buffer="NULL", side="both", fence="outer", innerFence=1.5, outerFence=3, expVal=maxExp, scaleVal=1, output="logical")
  
  # maxBtoBChangeDF <- cbind.data.frame(maxBtoBChange1, maxBtoBChange2, maxBtoBChange3, maxBtoBChange4, maxBtoBChange5, maxBtoBChange6, maxBtoBChange7, maxBtoBChange8, maxBtoBChange9, maxBtoBChange10, maxBtoBChange11, maxBtoBChange12, maxBtoBChange13, maxBtoBChange14, maxBtoBChange15)
  # View(maxBtoBChangeDF)
  

  
  # commented out 4-12-2107
  # maxBtoBChangeDF <- c(1:length(maxPeaks))
  # i=15
  # for(i in 1:length(cardioBuffer)){
  #   maxBtoBChangeDF <- cbind.data.frame(maxBtoBChangeDF,
  #                                       tukeyFence5(x=maxRate,
  #                                                   buffer=cardioBuffer[i],
  #                                                   bufferStop=2,
  #                                                   side="lower",
  #                                                   fence="outer",
  #                                                   innerFence=1.5,
  #                                                   outerFence=4.5,
  #                                                   expVal=1,
  #                                                   scaleVal=1,
  #                                                   output="logical"
  #                                       )
  #   )
  # }
  # maxBtoBChangeDF <- maxBtoBChangeDF[2:ncol(maxBtoBChangeDF)]
  # colnames(maxBtoBChangeDF) <- paste0("buffer",c(1:length(cardioBuffer)))
  # # View(maxBtoBChangeDF)
  # 
  # 
  # 
  # maxBtoBChange <- rep(NA, times=length(maxRate))
  # # 4-3-2017 was checkTrue, 12
  # maxBtoBChange[which(apply(maxBtoBChangeDF, 1, checkTRUE, 14))] <- "outerUpper"
  # # use this line to inspect the data frame frequencies
  # # apply(maxBtoBChangeDF, 1, checkTRUE, 7, 'FREQ')
  # # maxBtoBChange
  # 
  # # chartName
  # # tukeyFence5(x=maxRate, buffer=round(cardioRate/60*15,0), side="upper", fence="outer", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1, output="range")
  # # tukeyFence5(x=maxRate, buffer="NULL", side="upper", fence="outer", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1, output="range")


  
  # maxBtoBChange <- tukeyArtifactFn(x=maxRate,
  #                                 y=cardioBuffer,
  #                                 side="lower",
  #                                 fence="outer",
  #                                 innerFence=1.5,
  #                                 outerFence=3,
  #                                 # was outerFence=4.5
  #                                 expVal=1,
  #                                 scaleVal=1,
  #                                 outputType="logical",
  #                                 samples=10,
  #                                 cutProp=.9)



  # mark the artifacts in the chartDF
  # chartDF$CardioSystolic_a[maxPeaks[which(!is.na(maxBtoBChange))]]  <- "ArtifactMaxBtoB"
  # # which(chartDF$CardioSystolic_a=="ArtifactMaxBtoB")
  
  ############# leaking or descending cardio data #############
  
  cardioLeakMsg  <- "none"
  
  dataLen <- nrow(chartDF)
  
  if(dataLen > 600) {
  
    cardioLeakMsg <- cardioLeakFn(x=chartDF$c_CardioMA)
  
  }

  ########################## diastolic peak to peak intervals
  
  # minBtoBChange <- tukeyArtifactFn(x=minRate,
  #                                 y=cardioBuffer,
  #                                 side="lower",
  #                                 fence="outer",
  #                                 innerFence=1.5,
  #                                 outerFence=3,
  #                                 # was outerFence=4.5
  #                                 expVal=1,
  #                                 scaleVal=1,
  #                                 outputType="logical",
  #                                 samples=10,
  #                                 cutProp=.9)


  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/workFlow.R'))

  # mark the artifacts in the chartDF
  # chartDF$CardioDiastolic_a[minPeaks[which(!is.na(minBtoBChange))]]  <- "ArtifactMinBtoB"
  # which(chartDF$CardioDiastolic_a=="ArtifactMinBtoB")
  
  ######################  compute changes in pulse amplitude   ##################
  
  ########################   systolic and diastolic amplitude
  
  # locate the min and max peaks
  minMaxPeak <- minMaxPeakFn(x=chartDF$c_Cardio1, y=bufferLen)
  # length(minMaxPeak)
  
  # calculate the abs diff for all min max peaks in the vector
  # minMaxAmp <- chartDF$c_CardioMid[maxPeaks] - chartDF$c_Cardio1[minPeaks]
  minMaxAmp <- abs(diff(chartDF$c_Cardio1[minMaxPeak]))
  # add a value at the beginning to ensure correct location of events
  minMaxAmp <- c(mean(minMaxAmp), minMaxAmp)
  # length(minMaxAmp)
  
  # call the tukeyArtifactFn in the artifactExtractHelper.R script
  minMaxChange <- tukeyArtifactFn(x=minMaxAmp,
                                  y=cardioBuffer,
                                  side="upper",
                                  fence="outer",
                                  innerFence=1.5,
                                  outerFence=3,
                                  expVal=1,
                                  scaleVal=1,
                                  outputType="logical",
                                  samples=10,
                                  cutProp=.67)
  
  # mark the artifacts in the chartDF
  # which(!is.na(minMaxChange))
  chartDF$CardioMid_a[minMaxPeak[which(!is.na(minMaxChange))]]  <- "ArtifactMinMaxAmp"
  
  # calculate the peak to MA distance
  inputVector <- abs(chartDF$c_Cardio1[minMaxPeak] - chartDF$c_CardioMA[minMaxPeak])
  
  peakChange <- tukeyArtifactFn(x=inputVector,
                                  y=cardioBuffer,
                                  side="upper",
                                  fence="outer",
                                  innerFence=1.5,
                                  outerFence=3,
                                  expVal=1,
                                  scaleVal=1,
                                  outputType="logical",
                                  samples=10,
                                  cutProp=.67)
  
  # mark the artifacts in the chartDF
  chartDF$CardioSystolic_a[minMaxPeak[which(!is.na(peakChange))]]  <- "ArtifactMaxAmp"
  
  ##################### systolic amplitude - involuntary movement
  
  # new 5-12-2016 compute significant changes in sistolic amplitude
  # chartDF$CardioSystolic_a <- ""
  # systInputVector <- abs(chartDF$c_Cardio1[maxPeaks] - chartDF$c_CardioMid[maxPeaks])
  # use the slow moving average
  # systInputVector <- abs(chartDF$c_Cardio1[maxPeaks] - chartDF$c_CardioMA[maxPeaks])
  # systInputVector <- abs(chartDF$c_Cardio1[maxPeaks] - chartDF$c_CardioDiastolic[maxPeaks])
  # length(systInputVector)
  
  # maxAmpChange <- tukeyArtifactFn(x=systInputVector,
  #                                 y=cardioBuffer,
  #                                 side="upper",
  #                                 fence="outer",
  #                                 innerFence=1.5,
  #                                 outerFence=3,
  #                                 # was outerFence=4.5
  #                                 expVal=1,
  #                                 scaleVal=1,
  #                                 outputType="logical",
  #                                 samples=10,
  #                                 cutProp=.51)
  
  # mark the artifacts in the chartDF
  # chartDF$CardioSystolic_a[maxPeaks[which(!is.na(maxAmpChange))]]  <- "ArtifactMaxAmp"
  # which(chartDF$CardioSystolic_a=="ArtifactMaxAmp")
  # maxPeaks[which(!is.na(maxAmpChange))]
  # which(!is.na(chartDF$CardioSystolic_a))
  
  #######################   diastolic amplitude - estrasystoles
  
  # getSegFn(exam=1,series=1,chart=1)
  
  # new 5-18-2016 compute significant changes in diastolic amplitude
  # chartDF$CardioDiastolic_a <- ""
  # diastInputVector <- abs(chartDF$c_CardioMid[minPeaks] - chartDF$c_Cardio1[minPeaks])
  # use the slow moving average instead for diastolic
  # diastInputVector <- abs(chartDF$c_CardioMA[minPeaks] - chartDF$c_Cardio1[minPeaks])
  # diastInputVector <- (chartDF$c_CardioSystolic[minPeaks] - chartDF$c_Cardio1[minPeaks])^.5
  # 4-3-2017 try the syst = diast

  # minAmpChange <- tukeyArtifactFn(x=diastInputVector,
  #                                 y=cardioBuffer,
  #                                 side="upper",
  #                                 fence="outer",
  #                                 innerFence=1.5,
  #                                 outerFence=3,
  #                                 # was outerFence=4.5
  #                                 expVal=1,
  #                                 scaleVal=1,
  #                                 outputType="logical",
  #                                 samples=10,
  #                                 cutProp=.51)

  # 6-14-2015 buffer=13 expVal=6
  # minAmpChange <- tukeyFence5(x=diastInputVector, buffer=round(cardioRate/60*15,0), side="upper", fence="outer", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1)
  # minAmpChange <- tukeyFence5(x=diastInputVector, buffer=round(cardioRate/60*10,0), side="upper", fence="outer", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1)
  # minAmpChange <- tukeyFence5(x=diastInputVector, buffer=round(cardioRate/60*5,0), side="upper", fence="outer", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1)
  # minAmpChange <- tukeyFence5(x=diastInputVector, buffer=round(cardioRate/60*3,0), side="upper", fence="outer", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1)
  # minAmpChange <- tukeyFence5(x=diastInputVector, buffer="NULL", side="upper", fence="outer", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1)
  
  # tukeyFence7(x=diastInputVector, buffer=round(cardioRate/60*15,0), side="upper", fence="outer", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1)
  # mark the artifacts in the chartDF
  # chartDF$CardioDiastolic_a[minPeaks[which(!is.na(minAmpChange))]]  <- "ArtifactMinAmp"
  # which(chartDF$CardioDiastolic_a=="ArtifactMinAmp")
  # minPeaks[which(!is.na(minAmpChange))]
  # which(!is.na(chartDF$CardioSystolic_a))
  
  ##################### general cardio instability 
  
  ### compute significant changes in cardio using the slower cardio moving average
  
  MAChange <- NULL
  # systolic Y value is less than the cardioMA
  MAChange <- c(MAChange, which(chartDF$c_CardioSystolic <= chartDF$c_CardioMA))
  # diastolic Y value is greater than the cardioMA
  MAChange <- c(MAChange, which(chartDF$c_CardioDiastolic >= chartDF$c_CardioMA))
  MAChange <- sort(MAChange)
  
  chartDF$CardioMA_a[MAChange] <- "ArtifactMA"
  
  
  ###################### submit the cardio artifacts to the chart artifact channel
  
  cardioArtifacts <- unique(sort(c(MAChange, 
                            minMaxPeak[which(!is.na(minMaxChange))],
                            minMaxPeak[which(!is.na(peakChange))] )))
  
  chartDF$Artifacts_a[cardioArtifacts] <- "cardioArtifact"
  
  #################
  
  return(chartDF)
  
} # end cArtifactFn()





