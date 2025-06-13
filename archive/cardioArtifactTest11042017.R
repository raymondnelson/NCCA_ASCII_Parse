
chartDF$CardioSystolic_a <- ""
chartDF$CardioDiastolic_a <- ""
chartDF$CardioMid_a <- ""
chartDF$CardioMA_a <- ""
chartDF$Cardio1_a <- ""

# re-process the slow moving average - nomrally done in the scale offset
# move this to the init script when done
chartDF$c_CardioMA <- MASmooth(x=chartDF$c_Cardio1, y=round(1.5*cps,0), times=3)
# chartDF$c_CardioMA <- MASmooth(x=chartDF$c_CardioMA, y=round(1*cps,0), times=1)

# calculate the cardio rate and buffer length
cardioRate <- ratePerMin(chartDF$c_Cardio1,buffer=3,peaks="upper",lowPass=TRUE)
bufferLen <- bufferLenFn(x=cardioRate, y=.6)

# locate the systolic peaks
maxOut <- maxPeak(x=chartDF$c_Cardio1, y=bufferLen)

# compute the systolic line
chartDF$c_CardioSystolic <- interpolatePeaks(x=maxOut, y=chartDF$c_Cardio1[maxOut])[1:nrow(chartDF)]

# locate the diastolic peaks
minOut <- minPeak(x=chartDF$c_Cardio1, y=bufferLen)

# compute the diastolic line
chartDF$c_CardioDiastolic <- interpolatePeaks(x=minOut, y=chartDF$c_Cardio1[minOut])[1:nrow(chartDF)]


######## calculate the cardio rate ########

# get the pulse rate using systolic peaks
cardioRate1 <- ratePerMin(chartDF$c_Cardio1,buffer=3,peaks="upper",lowPass=TRUE)

# compute the systolic buffer lengths for the Tukey functions
bufferLen1 <- bufferLenFn(x=cardioRate1, y=.6)
# 3-17-2017 was y = .5 and y=.6, default is .67

# get the pulse rate useing diastolic peaks
cardioRate2 <- ratePerMin(chartDF$c_Cardio1,buffer=3,peaks="lower",lowPass=TRUE)

# compute the diastolic buffer length for the Tukey functions
bufferLen2 <- bufferLenFn(x=cardioRate2, y=.6)
# 3-17-2017 was y = .5 and y=.6, default is .67

# compare the systolic and diastolic cardio rates
if(exp(-abs(log(cardioRate1/cardioRate2))) >= .95) {
  cardioRate <- cardioRate1
  print(paste("cardio rate:", cardioRate1))
} else {
  # chartDF$Cardio1_a <- paste("outside normal range", cardioRate1)
  cardioRate <- cardioRate1
  print("possible cardio arrythmia. diastolic and systolic rates are different")
  # stop("check for cardio arrythmia")
} 

# make a vector of cardioBuffers
cardioBuffer <- round(cardioRate/60*c(2:15, 15),0)
#length(cardioBuffer)

###### test for respiratory blood pressure fluctuation ######

chartDF$CardioMid_a <- ""
rbpfResult <- rbpfProbFn(x=chartDF)
if(rbpfResult >= .95) {
  chartDF$CardioMid_a <- rbpfResult
} 
##################### systolic and diastolic amplittude

# locate the min and max peaks
minMaxPeak <- minMaxPeakFn(x=chartDF$c_Cardio1, y=bufferLen1)
# length(minMaxPeak)

# calculate the abs diff for all min max peaks in the vector
# minMaxAmp <- chartDF$c_CardioMid[maxPeaks] - chartDF$c_Cardio1[minPeaks]
minMaxAmp <- abs(diff(chartDF$c_Cardio1[minMaxPeak]))
# add a value at the beginning to ensure correct location of events
minMaxAmp <- c(mean(minMaxAmp), minMaxAmp)
# length(minMaxAmp)

# make a vector of cardioBuffers
# cardioBuffer <- round(cardioRate/60*c(2:15, 15),0)
#length(cardioBuffer)

# calculate the outliers
# TFExp <- 1
# TFOuter <- 3
# TFInner <- 1.5
# TFSide <- "upper"
# TFFence <- "outer"
# minMaxChange1 <- tukeyFence5(x=minMaxAmp, buffer=round(cardioRate/60*2,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=TFExp, scaleVal=1, output="logical")
# minMaxChange2 <- tukeyFence5(x=minMaxAmp, buffer=round(cardioRate/60*3,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=TFExp, scaleVal=1, output="logical")
# minMaxChange3 <- tukeyFence5(x=minMaxAmp, buffer=round(cardioRate/60*4,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=TFExp, scaleVal=1, output="logical")
# minMaxChange4 <- tukeyFence5(x=minMaxAmp, buffer=round(cardioRate/60*5,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=TFExp, scaleVal=1, output="logical")
# minMaxChange5 <- tukeyFence5(x=minMaxAmp, buffer=round(cardioRate/60*6,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=TFExp, scaleVal=1, output="logical")
# minMaxChange6 <- tukeyFence5(x=minMaxAmp, buffer=round(cardioRate/60*7,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=TFExp, scaleVal=1, output="logical")
# minMaxChange7 <- tukeyFence5(x=minMaxAmp, buffer=round(cardioRate/60*8,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=TFExp, scaleVal=1, output="logical")
# minMaxChange8 <- tukeyFence5(x=minMaxAmp, buffer=round(cardioRate/60*9,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=TFExp, scaleVal=1, output="logical")
# minMaxChange9 <- tukeyFence5(x=minMaxAmp, buffer=round(cardioRate/60*10,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=TFExp, scaleVal=1, output="logical")
# minMaxChange10 <- tukeyFence5(x=minMaxAmp, buffer=round(cardioRate/60*11,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=TFExp, scaleVal=1, output="logical")
# minMaxChange11 <- tukeyFence5(x=minMaxAmp, buffer=round(cardioRate/60*12,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=TFExp, scaleVal=1, output="logical")
# minMaxChange12 <- tukeyFence5(x=minMaxAmp, buffer=round(cardioRate/60*13,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=TFExp, scaleVal=1, output="logical")
# minMaxChange13 <- tukeyFence5(x=minMaxAmp, buffer=round(cardioRate/60*14,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=TFExp, scaleVal=1, output="logical")
# minMaxChange14 <- tukeyFence5(x=minMaxAmp, buffer=round(cardioRate/60*15,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=TFExp, scaleVal=1, output="logical")
# minMaxChange15 <- tukeyFence5(x=minMaxAmp, buffer=round(cardioRate/60*15,0), side=TFSide, fence=TFFence, innerFence=TFInner, outerFence=TFOuter, expVal=TFExp, scaleVal=1, output="logical")
# 
# minMaxChangeDF <- cbind.data.frame(minMaxChange1, minMaxChange2, minMaxChange3, minMaxChange4, minMaxChange5, minMaxChange6, minMaxChange7, minMaxChange8, minMaxChange9, minMaxChange10, minMaxChange11, minMaxChange12, minMaxChange13, minMaxChange14, minMaxChange15)
# View(minMaxChangeDF)



minMaxChange <- tukeyFence5(x=minMaxAmp, buffer="NULL", side="upper", fence="outer", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1, output="logical")
chartDF$CardioMid_a[minMaxPeak[which(minMaxChange==TRUE)]] <- "ArtifactMinMaxAmp"



# new 4-4-2017
minMaxChangeDF <- c(1:length(minMaxAmp))
i=1
for(i in 1:length(cardioBuffer)){
  # 4-5-2017 was outerFence=3 but produces spurious artifacts
  minMaxChangeDF <- cbind.data.frame(minMaxChangeDF,
                                     tukeyFence5(x=minMaxAmp,
                                                 buffer=cardioBuffer[i],
                                                 bufferStop=1,
                                                 side="both",
                                                 fence="outer",
                                                 innerFence=1.5,
                                                 outerFence=3,
                                                 expVal=1,
                                                 scaleVal=1,
                                                 output="logical"
                                     )
  )
}
minMaxChangeDF <- minMaxChangeDF[2:ncol(minMaxChangeDF)]
colnames(minMaxChangeDF) <- paste0("buffer",c(1:length(cardioBuffer)))
# View(minMaxChangeDF)



minMaxChange <- rep(NA, times=length(minMaxAmp))
minMaxChange[which(apply(minMaxChangeDF, 1, checkTRUE, 14))] <- "outerUpper"
# apply(minMaxChangeDF, 1, checkTRUE, 7, 'FREQ')
# minMaxChange



# commented out 4-3-2017
# minMaxChange <- tukeyFence5(x=minMaxAmp, buffer="NULL", side="upper", fence="outer", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1, output="name")
# minMaxChange



# mark the artifacts in the chartDF
chartDF$CardioMid_a[minMaxPeak[which(!is.na(minMaxChange))]]  <- "ArtifactMinMaxAmp"

