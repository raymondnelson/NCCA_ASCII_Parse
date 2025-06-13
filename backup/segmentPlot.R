# function to plot a chart segment using ggplot2
#
# x input is a data frame of the time series segment for a stimulus question
#
# all information for the plot should be contained in the data frame
# 
# the function can be applied recursively 
# to a list of data frames for all segements for each chart series and exam
#
# cps is a scalar in the global environment because ggplot executes in the global environment
# ROWEnd is also a scalar in the global env
# latency is also a scalar
# prestimSeg is also a scalar
#
#################################


library(ggplot2) # need to load grid library for arrows
library(grid) # for ggplot2 arrows


cps <- 30
prestimSeg <- 5
EDAlat <- .5
ROWEnd <- 5
measuredSeg <- 15


mySegmentLists <- ls(pattern="*_dataSegmentList$")
# mySegmentLists <- PF090316_1.01A_dataSegmentList_backup

dataSegmentList_test <- PF090316_1.01A_dataSegmentList[2]
mySegmentLists <- "dataSegmentList_test"

##############

# a function to iterate the segmentPlot function
# over a list of exams, series, charts, and stimulus segments

# mySegmentLists <- ls(pattern="*_dataSegmentList$")

# mySegmentLists <- mySegmentLists[1]


segmentPlotFn <- function(x=mySegmentLists) {
  # function to iterate the segmentPlot function
  # x is a vector of names of lists of data frames for each stimulus segment
  #
  ####
  
  
  ####
  
  # first make function to plot a single data frame for a stimulus segment
  
  segmentPlot <- function(x=segmentDF) {
    # x is a dataframe for a stimulus segment 
    # including all time series and event data
    # and also including the measurement columns
    
    stimSegment <- as.data.frame(x)
    
    # assign the stim segment to the global environment so that ggplot can find it
    assign("stimSegment", stimSegment, pos=1)
    
    examName <- unique(as.character(stimSegment$examName))
    seriesName <- unique(stimSegment$seriesName)
    chartName <- unique(stimSegment$chartName)
    stimulusName <- unique(stimSegment$Label[stimSegment$Label!=""])[1]
    
    plotTitle <- paste(examName, chartName, stimulusName)
    
    # scale the data for plotting
    stimSegment$UPneumoS <- stimSegment$UPneumoS * 1
    stimSegment$LPneumoS <- stimSegment$LPneumoS * 1
    stimSegment$AutoEDA <- stimSegment$AutoEDA * 1
    stimSegment$Cardio1 <- stimSegment$Cardio1 * 10
    
    # offset the data for plotting
    yOffset <- c(125, 50, 0, -75)
    UPneumoOffset <- stimSegment$UPneumoS[1]
    LPneumoOffset <- stimSegment$LPneumoS[1]
    AutoEDAOffset <- stimSegment$AutoEDA[1]
    Cardio1Offset <- stimSegment$Cardio1[1]
    stimSegment$UPneumoS <- stimSegment$UPneumoS - UPneumoOffset + yOffset[1]
    stimSegment$LPneumoS <- stimSegment$LPneumoS - LPneumoOffset + yOffset[2]
    stimSegment$AutoEDA <- stimSegment$AutoEDA - AutoEDAOffset + yOffset[3]
    stimSegment$Cardio1 <- stimSegment$Cardio1 - Cardio1Offset + yOffset[4]
    
    # set the offset for the sensor data
    
    # make the plot
    g <- ggplot()
    # ggplot will look in the global
    g <- g + geom_line(data=stimSegment, aes(x=(1:nrow(stimSegment)), y=AutoEDA), color="green4")
    g <- g + geom_line(data=stimSegment, aes(x=(1:nrow(stimSegment)), y=Cardio1), color="red")
    g <- g + geom_line(data=stimSegment, aes(x=(1:nrow(stimSegment)), y=UPneumoS), color="blue1")
    g <- g + geom_line(data=stimSegment, aes(x=(1:nrow(stimSegment)), y=LPneumoS), color="blue4")
    # g <- g + geom_line(data=stimSegment, aes(x=(1:nrow(stimSegment)), y=Move1), color="grey20")
    # g <- g + geom_line(data=stimSegment, aes(x=(1:nrow(stimSegment)), y=Aux02), color="grey20")
    
    ### vertical lines 
    # onset line
    onsetRow <- which(stimSegment$Events=="onsetRow")
    assign("onsetRow", onsetRow, pos=1)
    g <- g + geom_vline(aes(xintercept=as.numeric(onsetRow)))
    # EDA latency
    EDALatRow <- which(stimSegment$Events=="EDALatRow")
    assign("EDALatRow", EDALatRow, pos=1)
    g <- g + geom_vline(aes(xintercept=as.numeric(EDALatRow)), color="grey80")
    # offset line
    offsetRow <- which(stimSegment$Events=="offsetRow")
    assign("offsetRow", offsetRow, pos=1)
    g <- g + geom_vline(aes(xintercept=as.numeric(offsetRow)))
    # answer line
    answerRow <- which(stimSegment$Events=="answerRow")
    assign("answerRow", answerRow, pos=1)
    g <- g + geom_vline(aes(xintercept=as.numeric(answerRow)), color="grey40")
    # end of response onset window
    ROWEndRow <- which(stimSegment$Events=="ROWEndRow")
    assign("ROWEndRow", ROWEndRow, pos=1)
    g <- g + geom_vline(aes(xintercept=as.numeric(ROWEndRow)), color="grey80")
    # end of scoring window
    endRow <- which(stimSegment$Events=="endRow")
    assign("endRow", endRow, pos=1)
    g <- g + geom_vline(aes(xintercept=as.numeric(endRow)), color="white")
    
    ### shaded areas
    # scoring window shaded area
    g <- g + annotate("rect", 
                      xmin=as.numeric(onsetRow), 
                      xmax=as.numeric(endRow), 
                      ymin=-150, 
                      ymax=150, 
                      alpha=.1, 
                      fill="blue")
    # stimulus question shaded area
    g <- g + annotate("rect", 
                      xmin=as.numeric(onsetRow), 
                      xmax=as.numeric(offsetRow), 
                      ymin=-135, 
                      ymax=135, 
                      alpha=.3, 
                      fill="grey20")
    
    # Pneumo measurement lines
    
    g <- g + geom_line(data=stimSegment[onsetRow:endRow,], aes(x=onsetRow:endRow, y=UPneumoS), color="blue1", size=1.25)
    g <- g + geom_line(data=stimSegment[onsetRow:endRow,], aes(x=onsetRow:endRow, y=LPneumoS), color="blue4", size=1.25)
    
    # EDA measurement lines
    
    yOffset <- stimSegment$AutoEDA[1]
    
    EDAxOn <- which(stimSegment$AutoEDAExtract=="responseOnsetRow")
    EDAxOff <- which(stimSegment$AutoEDAExtract=="responseEndRow")
    EDAyOn <- stimSegment$AutoEDA[EDAxOn]
    EDAyOff <- stimSegment$AutoEDA[EDAxOff]
    
    # horzontal segment measurement
    g <- g + annotate("segment",
                      x=EDAxOff,
                      xend=EDAxOn,
                      y=EDAyOn,
                      yend=EDAyOn,
                      color="purple",
                      size=.5,
                      arrow=arrow(length=unit(0.4, "cm")))
    
    # vertical segment
    # geom_segment and annotate are two different ways of adding the lines
    # annotate is probably better
    g <- g + annotate("segment",
                      x=EDAxOff,
                      xend=EDAxOff,
                      y=EDAyOn,
                      yend=EDAyOff,
                      color="purple",
                      size=.5,
                      arrow=arrow(length=unit(0.4, "cm")))
    
    # using geomsegment() is more difficult
    #   g <- g + geom_segment(aes(x=autoEDA["responsePeakRow"], 
    #                             xend=autoEDA["responsePeakRow"], 
    #                             y=autoEDA["responseOnsetValue"], 
    #                             yend=autoEDA["responsePeakValue"]), 
    #                         color="purple", 
    #                         size=.5, 
    #                         arrow=arrow(length=unit(0.4, "cm")))
    
    ### plot appearance
    g <- g + ylab("y-change")
    g <- g + xlab("x-time")
    
    g <- g + ggtitle(plotTitle)
    
    g <- g + theme_bw()
    
    return(g)
    
  } # end segmentPlot function
  
  # segmentPlot(x=segmentDF)
  
  ####
  
  
  
  # use the same vector name in the function environment
  mySegmentLists <- x
  
  # iterate over the names of data segment lists
  # i <- 1
  for (i in 1:length(mySegmentLists)) {
    
    # get the name of the segment list
    segmentListName <- mySegmentLists[i]
    
    # get the data segment list
    segmentList <- get(segmentListName, pos=1)
    
    # save the names of the stimulus segments
    segmentNames <- names(segmentList)
    
    # iterate over the data frames in each data segment list
    # j <- 1
    for (j in 1:length(segmentList)) {  
      
      segmentDF <- segmentList[[j]]
      
      # use the segmentPlot function
      z - segmentPlot(x=segmentDF)
      
      
      
      # save the plot as .Rda
      
      
      # make the graphic
      print(z)
      
      # dev.off()
      
      # termine the graphics device before the next plot 
      # dev.off()
      # and save the graphic
      
      return(z)
      
    } # end iteration over data frames in the segment list
    
  
  
  } # end iteration over name of data segment lists
  
  
  
} # end segmentPlotFn

# segmentPlotFn(x=mySegmentLists)


