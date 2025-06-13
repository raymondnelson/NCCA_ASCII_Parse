# compute a vector of max change amplitudes for each onset and subsequent peaks
#
# for each onset ignore peaks that occur 
# after subsequent onset vals less than the onset value
#
# for each onset ignor peaks that occur
# after the data have descended more than 1/2 way 
# from the previous max Peak after the onset
#
# myData is a time series vector
# xOnset is a vector of positive slope onset rows in myData, after latency and before ROWEndRow
# xPeak is a vector of positive slope peak rows in myData
#
#####################



# mySegmentLists <- ls(pattern="*_dataSegmentList$")
# myEventLists <- ls(pattern="*_eventList$")
# 
# mySegmentLists <- mySegmentLists[1]
# myEventLists <- myEventLists[1]
# 
# mySegmentDF <- get(mySegmentLists)[[10]]
# myEventDF <- get(myEventLists)[[10]]
# 
# # myData <- mySegmentDF$AutoEDA
# myData <- mySegmentDF$CardioMA
# 
# begin <- myEventDF$Begin
# end <- myEventDF$End
# answer <- myEventDF$Answer
# # if(answer == end) answer <- answer+1
# start <- mySegmentDF$Sample[1]
# lat <- .5
# nSmooth <- 4
# label <- myEventDF$Label
# segmentName <- paste(mySegmentDF$examName[1], mySegmentDF$chartName[1], myEventDF$Label, sep="_")
# 
# #########
# 
# 
# cps <- 30
# prestimSeg <- 5
# EDALat <- .5
# CardioLat <- .5
# ROWEnd <- 5
# measuredSeg <- 15
# 
# x <- myData




####################
# a helper function to remove all ascending onset and peak segments 
# after the data descend below the yChangeOnsetValue
#
# do this by monitoring the yAxis difference after the yChangeValue
# and remove all peaks after the first row where the diff is negative 
#

# negDiff <- function(x=yChangeOnset, y=yChangeOnsetValue, z=myData) {
#   # function to remove all peak segments after the data descend below the yChangeOnset
#   yChangeOnset <- x
#   yChangeOnsetValue <- y
#   myData <- z
#   #
#   myValues <- myData[(yChangeOnset+1):length(myData)]
#   myDiffs <- myValues - yChangeOnsetValue
#   stopRow <- yChangeOnset + which(myDiffs < 0)[1]
#   #
#   return(stopRow)
# } # end negDiff function



#####
# a helper function
# to exclude rows after the data descend 
# more than a proportion p from the previous highest peak

descentProp <- function(x=xOnset, y=xPeakLoop, z=myData, p=.5) {
  # function to exclude rows after the data descend more than a proportion p
  # from a the previous highest peak after response onset
  yChangeOnset <- x
  xPeakLoop <- y
  myData <- z
  prop <- p
  #
  xPeakLoopValues <- myData[xPeakLoop]
  xPeakLoopDiffs <- xPeakLoopValues - myData[yChangeOnset]
  cutValues <- prop * xPeakLoopDiffs
  myValues <- myData[(yChangeOnset+1):length(myData)]
  myDiffs <- myValues - myData[yChangeOnset]
  mySlope <- c(0, ifelse(diff(myValues)>0, 1, ifelse(diff(myValues)<0, -1, 0)))
  #
  # now look for descending myValues that are less than descCut for previous peaks
  #
  # make an empty vector
  cutVector <- numeric(length=length(myDiffs))
  # populate the cut vector
  for (i in 1:length(myDiffs)) {
    # make a vector myCutValues for the vector myDiffs
    # myCutValues needs to be 1/2 the diff of the preceeding max peak
    currentRow <- i + yChangeOnset - 1
    ifelse(length(which(xPeakLoop <= currentRow)) > 0,
           myCutValue <- max(cutValues[which(xPeakLoop <= currentRow)]),
           myCutValue <- 0)
    #
    # use i to check the max preceeding diff
    # if(myDiffs[i] <  myCutValue) { cutVector[i] <- myCutValue }
    cutVector[i] <- myCutValue
  } # end for loop
  #
  # print(cutVector)
  #
  # use only descending slope segments
  descRows <- which(mySlope == -1)
  # ignore the first several descending rows 
  # to avoid over sensitivity to high frequency noise
  #
  # descRows <- descRows[91:length(descRows)]
  cutVector <- c(rep(0, time=90), cutVector[91:length(cutVector)])
  #
  # keep only those descending rows for which 
  # difference is smaller than the cut point porportion
  cutRows <- descRows[myDiffs[descRows] < cutVector[descRows]]
  stopRow2 <- cutRows[1] + yChangeOnset - 1
  #   
  # use the last row in the data vector of stopRow2 is NA
  if(is.na(stopRow2) == TRUE) stopRow2 <- length(myData)
  #
  # return a scalar with the stop row 
  # after which peaks are excluded 
  return(stopRow2)
  #
} # end descentProp function



##########

# first make an empty vector for the loop output
yChange <- rep("", times=length(xOnset)) 

# i=6
# loop to select the max change for each xOnset row
for (i in 1:length(xOnsetVal)) {
  # this has to be in a loop to iteratively shorten the comparison of peak values
  # if the data descend below onset
  # or if the data descend a proportion p after the end of a positive slope segment
  #
  # row to stop including xPeak values if they descend below the xOnset value
  stopRow <- which(myData[(xOnset[i]+1):length(myData)] < xOnsetVal[i])[1] + xOnset[i]
  # there is no stop row when the data do not descend below onset, so use the last row instead
  if(is.na(stopRow)) stopRow <- length(myData)
  #
  # exclude xPeak rows in each loop after the data descend below the xOnsetVal 
  xPeakLoop <- xPeak[which((xPeak > xOnset[i]) & (xPeak < stopRow))]
  # 
  # row to stop including xPeak values if the data descend a proportiont p of the previous max Peak
  stopRow2 <- descentProp(x=xOnset[i], y=xPeakLoop, z=myData, p=.5)
  if(is.na(stopRow2)) stopRow2 <- length(myData)
  #
  # exclude xPeak rows in each loop after the data descend to a proportion p
  # from maximum xPeak change prior to each xPeak
  xPeakLoop <- xPeakLoop[which((xPeakLoop > xOnset[i]) & (xPeakLoop <= stopRow2))]
  #
  # use the xPeakLoop vector to determine the max xPeak for each xOnset
  yChange[i] <- xPeakLoop[which.max(myData[xPeakLoop[xPeakLoop >= xOnset[i]]] - xOnsetVal[i])]
  # yChange is a vector of xPeak rows for the max increase following each xOnsetVal
  #
  # loop output is a vector yChange to index the peak row for for each xOnset row
  #
} # end for loop

# remove NAs that may result from different vector lengths
yChange <- as.numeric(na.omit(yChange))

# compute the differences between onset and peak values
yChangeDiffs <- myData[yChange] - myData[xOnset]

# compute the index of the max change from xOnset to xPeak
yChangeMaxIndex <- which.max(yChangeDiffs)

# compute the onset
yChangeOnset <- xOnset[yChangeMaxIndex]
yChangeOnsetValue <- myData[yChangeOnset]

# compute the peak
yChangePeak <- yChange[yChangeMaxIndex]
yChangePeakValue <- myData[yChange[yChangeMaxIndex]]

# and finally the change from onset to peak
yChangeValue <- yChangePeakValue - yChangeOnsetValue
yChangeValue <- myData[yChange[yChangeMaxIndex]] - myData[xOnset[yChangeMaxIndex]]

print(yChangeOnset)
print(yChangeOnsetValue)
print(yChangePeak)
print(yChangePeakValue)
print(yChangeValue)



# ### get the response onset
# 
# ### need a loop to select the next onset 
# 
# # get the max change value from the vector. this is the response onset row 
# yChangeMaxIndex <- which.max(as.numeric(yChange))
# 
# # use the first if the index fails or is null
# if(length(yChangeMaxIndex) == 0) yChangeMaxIndex <- 1
# 
# 
# ##### now make a loop to use subsequent onset points if necessary
# 
# for (i in yChangeMaxIndex:length(yChange)) { 
#   # added loop 7-18-2015 to select a subsequent xOnset if the data descend 1/2 way
#   
#   # get the onset row by indexing the max change value
#   yChangeOnset <- xOnset[yChangeMaxIndex]
#   print(yChangeOnset)
#   
#   # get the onset value
#   yChangeOnsetValue <- myData[yChangeOnset]
#   
#   ####
#   
#   # make a short vector of the xPeak row >=  xOnset
#   xPeakLoop <- xPeak[xPeak >= yChangeOnset] # could use which() but it makes no difference
#   print(xPeakLoop)
#   
#   
#   
#   stopRow <- negDiff(x=yChangeOnset, y=yChangeOnsetValue, z=myData)
#   
#   # remove peaks if and after the data descend below the reposnse onset value
#   if(!is.na(stopRow)) xPeakLoop <- xPeakLoop[xPeakLoop <= stopRow]
#   
#   ####
#   
# 
#   
#   stopRow2 <- descentRule(x=yChangeOnset, y=xPeakLoop, z=myData, p=.5)
#   
#   # remove peaks after the data descend beyond the specified proportion
#   if(descentStop == TRUE) { 
#     if(!is.na(stopRow2)) xPeakLoop <- xPeakLoop[xPeakLoop < stopRow2] 
#   } # end if
#   
# }