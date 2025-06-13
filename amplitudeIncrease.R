

amplitudeIncreaseFn <- function(x) {
  # R function to compute the amplitude of increase 
  # in EDA and cardio data
  # to compare activity in the prestimulus segment with the stimulus segment
  # 11-18-2016
  # raymond nelson
  ######
  #
  # x input is vector of time series values from the data segment
  # 
  # requires helper functions from the amplitudeExtractHelperFunctions.R script
  #
  # 1) identify all onset indices
  # defined as the onset of a positive slope segment
  # including the segment onset index (1)
  # 2) identify all peak points
  # defined as the onset of a negative slope segment
  # including the segment end index example: length(segment)
  # 3) compute the inverse of the max difference between each onset 
  # and all subsequent peak points and select the max peak for each onset
  # 4) select the onset and peak with the max difference
  #
  # output is a named vector of two values for onsetRow and endRow with the max difference
  # 
  ####################
  tsDataSegment <- x
  # first make a vector of slope changes
  xSlope <- fillSlope(smoothSlope(slopeDir(tsDataSegment)))
  # make vector of positive slope onset indices 
  onsetIndices <- positiveOnset(positiveSlope(xSlope))
  # always include the segment onset as a postive slope onset
  onsetIndices[1] <- 1
  onsetIndices <- which(onsetIndices == 1)
  # make a vector of peak indices
  peakIndices <- slopePeak(xSlope)
  # always include the segment end as a peak
  peakIndices[length(peakIndices)] <- 1
  peakIndices <- which(peakIndices == 1)
  # compute the inverse of the max diff between each onset and subsequent peaks
  # using the inverse this way allows the use of onset - peak to achieve a positive value
  maxIndices <- integer(length=length(onsetIndices))
  for (i in 1:length(onsetIndices)) {
    # use only those peaks that occur after the i onset
    usePeaks <- which(peakIndices > onsetIndices[i])
    # get the max peak
    maxRow <- which.max(-(tsDataSegment[onsetIndices[i]] - tsDataSegment[peakIndices[usePeaks]]))
    maxIndices[i] <- peakIndices[usePeaks][maxRow]
  }
  differenceDF <- cbind.data.frame("onset"=onsetIndices, "peak"=maxIndices)
  # select the onset and peak with the max difference
  useMax <- which.max(-(tsDataSegment[differenceDF$onset] - tsDataSegment[differenceDF$peak]))
  # output the result
  return(c("onset"=differenceDF$onset[useMax], "peak"=differenceDF$peak[useMax]))
} # end amplitudeIncreaseFn function 

