# pneumo feature extraction
# 4-28-2016
# Raymond Nelson



#           begin <- onsetRow
#           end <- offsetRow
#           answer <- answerRow
#           if(answer <= end) answer <- end + 1

# get the responseOnsetRow and responseEndRow
responseOnsetRow <- segOnsetRow + 1 
# responseEndRow <- responseOnsetRow + (measuredSeg * cps) - 1 - 2
responseEndRow <- segEndRow + 1

aBuffOn <- answerRow - (1 * cps) - 1
if(aBuffOn <= segOnsetRow) aBuffOn <- segOnsetRow + 1
aBuffOff <- answerRow + (1 * cps) + 1
if(aBuffOff >= nrow(segmentDF)) aBuffOff <- nrow(segmentDF)

# correct for segments shorter than the measurement segment
if(responseEndRow > nrow(segmentDF)) {
  responseEndRow <- nrow(segmentDF) - 1
} 

# add the prestim and end rows to the data frame
#           segmentDF$UPneumoExtract[1] <- "prestimRow"
#           segmentDF$LPneumoExtract[1] <- "prestimRow"
#           segmentDF$UPneumoExtract[endRow] <- "endRow"
#           segmentDF$LPneumoExtract[endRow] <- "endRow"

# add the stimulus and answer data to the pneumo extract columns
#           chartDF$UPneumoExtract[segOnsetRow+startRow-1] <- "onsetRow"
#           chartDF$LPneumoExtract[segOnsetRow+startRow-1] <- "onsetRow"
#           chartDF$UPneumoExtract[offsetRow+startRow-1] <- "offsetRow"
#           chartDF$LPneumoExtract[offsetRow+startRow-1] <- "offsetRow"
#           chartDF$UPneumoExtract[answerRow+startRow-1] <- "answerRow"
#           chartDF$LPneumoExtract[answerRow+startRow-1] <- "answerRow"
segmentDF$UPneumoExtract[segOnsetRow] <- "onsetRow"
segmentDF$LPneumoExtract[segOnsetRow] <- "onsetRow"
segmentDF$UPneumoExtract[offsetRow] <- "offsetRow"
segmentDF$LPneumoExtract[offsetRow] <- "offsetRow"
segmentDF$UPneumoExtract[answerRow] <- "answerRow"
segmentDF$LPneumoExtract[answerRow] <- "answerRow"

# add the response onset and response end data to the 2 pneumo extract columns
#           chartDF$UPneumoExtract[responseOnsetRow+startRow-1] <- "responseOnsetRow"
#           chartDF$LPneumoExtract[responseOnsetRow+startRow-1] <- "responseOnsetRow"
#           chartDF$UPneumoExtract[responseEndRow+startRow-1] <- "responseEndRow"
#           chartDF$LPneumoExtract[responseEndRow+startRow-1] <- "responseEndRow"
segmentDF$UPneumoExtract[responseOnsetRow] <- "responseOnsetRow"
segmentDF$LPneumoExtract[responseOnsetRow] <- "responseOnsetRow"
segmentDF$UPneumoExtract[responseEndRow] <- "responseEndRow"
segmentDF$LPneumoExtract[responseEndRow] <- "responseEndRow"

# add the answer distortion buffer 
#           chartDF$UPneumoExtract[aBuffOn+startRow-1] <- "aBuffOn"
#           chartDF$UPneumoExtract[aBuffOff+startRow-1] <- "aBuffOff"
#           chartDF$LPneumoExtract[aBuffOn+startRow-1] <- "aBuffOn"
#           chartDF$LPneumoExtract[aBuffOff+startRow-1] <- "aBuffOff"  
segmentDF$UPneumoExtract[aBuffOn] <- "aBuffOn"
segmentDF$UPneumoExtract[aBuffOff] <- "aBuffOff"
segmentDF$LPneumoExtract[aBuffOn] <- "aBuffOn"
segmentDF$LPneumoExtract[aBuffOff] <- "aBuffOff"  

### get the pneumo measurement

# first use the maxPeak function in in the robustPenumoMeasurement.R script 
# to determine the respiration rate
UPRate <- ratePerMin(x=chartDF$c_UPneumo)

UPMeasurement <- ifelse(UPRate < 8,
                        "ONR_slow",
                        ifelse(UPRate > 24,
                               "ONR_fast",
                               "normal"))

# UPMeasurement <- "normal"
# 
# UPeaks <- maxPeak(segmentDF$c_UPneumo, y=40, firstLast=FALSE)
# if(length(UPeaks)>1) {
# if(round(60 / (mean(diff(UPeaks)) / cps), 2) < 10) { 
#   UPMeasurement <- "ONR_slow"
# }
# if(round(60 / (mean(diff(UPeaks)) / cps), 2) > 22) { 
#   UPMeasurement <- "ONR_fast"
#   # "ONR" signifies "outside the normal range"
# }
# } else UPMeasurement <- "ONR_slow"

LPRate <- ratePerMin(x=chartDF$c_LPneumo)

LPMeasurement <- ifelse(LPRate < 8,
                        "ONR_slow",
                        ifelse(LPRate > 24,
                               "ONR_fast",
                               "normal"))

# # chec the respiration rate
# LPMeasurement <- "normal"
# 
# LPeaks <- maxPeak(segmentDF$c_LPneumo, y=40, firstLast=FALSE)
# if(length(LPeaks)>1) {
# if(round(60 / (mean(diff(LPeaks)) / cps), 2) < 12) {
#   LPMeasurement <- "ONR_slow"
# }
# if(round(60 / (mean(diff(LPeaks)) / cps), 2) > 20) {
#   LPMeasurement <- "ONR_fast"
#   # "ONR" signifies "outside the normal range"
# }
# } else LPMeasurement <- "ONR_slow"

# compute the pneumo measurement only if both upper and lower are not ONR
if(UPMeasurement=="normal" & LPMeasurement=="normal") {
  UPMeasurement <- robustPneumoMeasurement(dataVector=segmentDF$c_UPneumo[responseOnsetRow:responseEndRow], 
                                           verbalAnswer=answerRow-responseOnsetRow+1)
  LPMeasurement <- robustPneumoMeasurement(dataVector=segmentDF$c_LPneumo[responseOnsetRow:responseEndRow], 
                                           verbalAnswer=answerRow-responseOnsetRow+1)
}

# add the excursion measurement
# first make a vector of the measurement rows
#          measureRows <- c((responseOnsetRow:aBuffOn), (aBuffOff:responseEndRow))

# 11-1-2015 commented out these next lines to see if the simpler version works          
#           responseOnsetRow <- responseOnsetRow+startRow-2
#           responseEndRow <- responseOnsetRow+(cps*measuredSeg)-1
#         if(responseEndRow > nrow(chartDF)) responseEndRow <- nrow(chartDF)
#           chartDF$UPneumoMeasure[(responseOnsetRow:responseEndRow)] <- 
#             rep((sum(abs(chartDF$c_UPneumoDiff[(measureRows+startRow-1)]))/(measuredSeg-2)), times=(responseEndRow-responseOnsetRow+1))
#           chartDF$LPneumoMeasure[(responseOnsetRow:responseEndRow)] <- 
#             rep((sum(abs(chartDF$c_LPneumoDiff[(measureRows+startRow-1)]))/(measuredSeg-2)), times=(responseEndRow-responseOnsetRow+1))

#           chartDF$UPneumoMeasure[(responseOnsetRow:responseEndRow)] <- 
#             ( sum(abs(chartDF$c_UPneumoDiff[(measureRows+startRow-1)])) / (measuredSeg-2) )
#           chartDF$LPneumoMeasure[(responseOnsetRow:responseEndRow)] <- 
#             ( sum(abs(chartDF$c_LPneumoDiff[(measureRows+startRow-1)])) / (measuredSeg-2) )

#           segmentDF$UPneumoMeasure[(responseOnsetRow:responseEndRow)] <- 
#             ( sum(abs(segmentDF$c_UPneumoDiff[(measureRows)])) / (measuredSeg-2) )
#           segmentDF$LPneumoMeasure[(responseOnsetRow:responseEndRow)] <- 
#             ( sum(abs(segmentDF$c_LPneumoDiff[(measureRows)])) / (measuredSeg-2) )

#           segmentDF$UPneumoMeasure[(responseOnsetRow:responseEndRow)] <- UPMeasurement
#           segmentDF$LPneumoMeasure[(responseOnsetRow:responseEndRow)] <- LPMeasurement
segmentDF$UPneumoMeasure[segOnsetRow:segEndRow] <- UPMeasurement
segmentDF$LPneumoMeasure[segOnsetRow:segEndRow] <- LPMeasurement
