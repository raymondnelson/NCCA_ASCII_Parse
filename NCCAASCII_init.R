# NCCA_ASCIIParse initialization
# 4/23/2016
# Raymond Nelson
#
# Feb 24, 2022 added PLE and RLE constraints
####



print("init parameters for parsing and processing NCCA ASCII data")



######## init parameters for workflow ######## 



{
  
  # control the output from data parsing and signal processing
  output=FALSE
  
  # print the exam name to the console during data parsing and signal processing
  showNames=TRUE
  
  # x=uniqueExams
  
  examNum <- "ALL"
  seriesNum <- "ALL"
  chartNum <- "ALL"
  segmentNum <- "ALL"
  
}



######## init parameters for the time series data ######## 



{
  
  # cps is the cycles per second or the data sampling rate
  # my digital filters are constructed for 30cps
  cps <- 30
  
  # reduceSampling will decimate the data rate to 30cps if necessary
  # will also upsample to 30cps if necessary 
  reduceSampleRate <- FALSE
  reduceSampleRate <- TRUE
  
}



######## init parameters for data parsing ######## 



{
  
  # write a .csv file as a side effect while processing the header, stimulus and time series data
  writeCSV=FALSE
  saveCSV=FALSE
  CSVName=""
  
  # write a .txt file  as a side effect while processing the header, stimulus and time series data
  saveTXT=FALSE
  
  # create a scalar or vector  as a side effect while processing the data
  makeVector=TRUE
  
  # create a data frame  as a side effect while processing the data
  makeDF=TRUE
  assignDF=FALSE
  
  # type of exam 
  type="CQT"
  
  # range to scale the data
  colRange=30000
  
}



######## init parameters for signal processing ######## 



{
  
  # EDAFilter "laf"=Lafayette, "lim"=Limestone, "leg"=Lafayette Legacy Auto, "axc"=Axciton,
  # "laf0" = other Lafaytte(.5/.01), "lafX"=Lafayatte11.2, "test"=experimental EDA
  # "none"=no filtering, "test"=for testing and development, "man"=manual eda no filtering
  # EDAFilt="laf" # Lafaytte(.2/.03)
  # EDAFilt="laf0" # Lafaytte(.5/.01)
  # EDAFilt="test" # Lafayette (.0159/.443)
  # EDAFilt="laf18" # Lafayette (.0159/.443)
  # EDAFilt="test" # Lafayette (.1/.5)
  # EDAFilt="Det2" # Lafayette Detrended-2 filter (2012)
  # EDAFilt="lafX" # Lafayette (.05/.05)
  # EDAFilt-"lafD" # Lafayette (.2/.3) experimental mode for difficult EDA
  
  
  # use the legacy EDA filter for LXCAT
  # EDAFilt="leg" # Lafayette (.04/.886)
  
  # use the "none" filter for imported data
  # comment this out to use the auto centering filters
  # EDAFilt="none"
  
  ### this is also set in the workFlow.R script
  
  # EDAHighPass filter for auto center
  EDAHighPass=TRUE
  if(EDAFilt == "man") EDAHighPass <- FALSE
  
  # EDALowPass filter for smoothing
  EDALowPass=TRUE
  
  # EDABandPass filter to evaluate the stability and quality of the EDA data
  EDABandPass=FALSE
  
  
  # additional EDA smoothing in the EDASigProc.R script
  moreEDASmooth <- FALSE # set to FALSE on Dec 2, 2022
  
  # a stronger EDA highpass filter for difficult EDA data
  strongEDAFilt <- FALSE
  
  # PneumoLowPass for smoothing to improve the diagnostic coefficient and visual appearance
  PneumoLowPass=TRUE # a low pass .886 filter
  morePnSmooth=TRUE # a 1 x .25sec moving average
  evenMorePnSmooth=FALSE # a 2 x .25sec moving average
  
  # PLE baseline 
  PLEBaseline = TRUE
  # used to call a filter to correct PLE baseline
  # not used for imported NCCA ASCII data
  
  # useFilters will turn all filters off conveniently
  useFilters=TRUE
  
  # PLEBaseline controls a filter to constraint the tracing to a stable baseline
  PLEBaseline <- FALSE
  PLEBaseline <- TRUE
  
  includePLEData <- TRUE
  # includePLEData <- FALSE
  # also check the includePLEScores parameter in the workFlow.R script
  if(!isTRUE(includePLEData)) includePLEScores <- includePLEData
}



######## init parameters for scaling and plotting ######## 



{
  
  # print("init parameters for plotting")
  
  # y axis scale for plotting 
  # names(yAxis) <- c("yMax", "yMin")
  yMax <- 1000
  yMin <- -1000
  yRange <- yMax - yMin
  
  # scale sizes for plotting each channel 
  # the data are scaled to but are not constrained to these aesthetics
  # UPneumo LPneumo EDA, Cardio, PLE, seat FC eCardio PTTPPG PTTECG PTTPTT
  # May 9, 2024 added PTT channels
  scaleVals <- c(225, 225, 300, 300, 100, 50, 300, 225, 200, 200, 200)
  # scaleVals <- c(225, 225, 200, 300, 200, 50, 300, 225)
  # scaleVals <- c(225, 225, 100, 300, 300, 50, 300, 225)
  
  # double the size of the neumos 
  # 2025Apr15
  # scaleVals[c(1,2)] <- scaleVals[c(1,2)] * 2
  # used to show thaht log(R/C) ratios for respiration data are stable at all gain levels

  
  # scaleVals <- c(400, 400, 1000, 600, 250, 50, 225, 225)
  # scaleVals <- c(600, 600, 1500, 900, 300, 150, 225, 225)
  # scaleVals <- c(200, 200, 500, 275, 120, 50, 225, 225)
  
  names(scaleVals) <- c("uPneumo", "lPneumo", "eda", "cardio", "ple", "activity", "FC", "eCardio", "PTTPPG", "PTTECG", "PTTPTT")
  # c(.09*yRange, .09*yRange, .375*yRange, .11*yRange, .06*yRange, .025*yRange, .11*yRange, .12*yRange)
  # c(.09*yRange, .09*yRange, 1250, .11*yRange, .06*yRange, .025*yRange, .11*yRange, .12*yRange)
  
  
  # Nov 17, 2023
  # range values for feature extraction
  # these are best if they are identical to the scaleVals
  # because when gainVals==100 the output will be the same as the un-gained va
  # May 9, 2024 added PTT channels
  rangeVals <- c(225, 225, 300, 300, 100, 50, 300, 225, 200, 200, 200)
  # 
  # rangeVals <- c(1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000)
  # these initialize the range of integer unit within which response features are measured
  # actual measurements can exceed these ranges 
  # May 9, 2024 added PTT names
  names(rangeVals) <- c("uPneumo", "lPneumo", "eda", "cardio", "ple", "activity", "FC", "eCardio", "PTTPPG", "PTTECG", "PTTPTT")
  
  
  # Nov 17, 2023
  # gain values used to dynamically increase or decrease the tracing size
  # not actually used in the R envir, but planning for LXEdge
  # May 9, 2024 added PTT channels
  gainVals <- c(100, 100, 100, 100, 100, 100, 100, 100, 200, 200, 200)
  # called from the global envir by the abstractScaleFn in the abstractScale.R script
  # default of 100 permits the computation of integer unit changes as percentage values
  names(gainVals) <- c("uPneumo", "lPneumo", "eda", "cardio", "ple", "activity", "FC", "eCardio", "PTTPPG", "PTTECG", "PTTPTT")
  
  # Nov 17, 1023
  # gain correction switch, 
  # to output consistent feature extraction measurements regardless of the gain or scale size
  useGainCorrection <- TRUE
  # useGainCorrection <- FALSE
  
  
  # offset values for each channel
  # yOffset <- c(130, 70, 10, -45, -110, -145, -75)
  yOffset <- c(725, 475, 0, -250, -740, -880, -400, -400, -300, -600, -900)
  # May 9, 2024 added PTT channels
  names(yOffset) <- c("uPneumo", "lPneumo", "eda", "cardio", "ple", "activity", "FC", "eCardio", "PTTPPG", "PTTECG", "PTTPTT")
  # c(yMax-(.125*yRange), yMax-(.275*yRange), yMax-(.5*yRange), yMin+(.3*yRange), yMin+(.13*yRange), yMin+(.06*yRange), yMin+(.3*yRange), yMin+(.3*yRange))75*yRange), yMax-(.5*yRange), yMin+(.3*yRange), yMin+(.13*yRange), yMin+(.06*yRange), yMin+(.3*yRange), yMin+(.3*yRange))
  # c(yMax-(.125*yRange), yMax-(.275*yRange), yMax-(.5*yRange), yMin+(.3*yRange), yMin+(.13*yRange), yMin+(.06*yRange), yMin+(.3*yRange), yMin+(.3*yRange))75*yRange), yMax-(.5*yRange), yMin+(.3*yRange), yMin+(.13*yRange), yMin+(.06*yRange), yMin+(.3*yRange), yMin+(.3*yRange))
  
  # pneumo separation value 
  pneumoSepVal <- 325
  # used in the scale offset function to 
  
  # yMaxVals <- c(1000, 700, 625, 950, 0, 950, 950, 950)
  # May 9, 2024 added PTT channels
  yMaxVals <- c(950, 950, 950, 950, 0, 0, 950, 950, 950, 950, 950, 950)
  # yMaxVals <- c(1000, 600, 900, 950, 0, 950, 950, 950)
  
  names(yMaxVals) <- c("uPneumo", "lPneumo", "eda", "cardio", "ple", "activity", "FC", "eCardio", "PTTPPG", "PTTECG", "PTTPTT")
  
  # yMinVals <- c(500, 200, -375, -950, -950, -950, -950, -950)
  # May 9, 2024 added PTT channels
  yMinVals <- c(0, 0, -950, -950, -950, -950, -950, -950, -950, -950, -950)
  # yMinVals <- c(400, 000, -400, -950, -950, -950, -950, -950)
  names(yMinVals) <- c("uPneumo", "lPneumo", "eda", "cardio", "ple", "activity", "FC", "eCardio", "PTTPPG", "PTTECG", "PTTPTT")
  
}



######## init parameters for artifact extraction ######## 



{
  
  # artifactLat is the period in seconds to wait after an artifact
  # before accepting the onset of a response 
  # artifactLat <- 3.5
  
  # number of prestimulus seconds to look for artifacts
  # artifactPrestim <- 3 * cps
  
  # number of samples to look for artifacts post response peak (EDA and cardio)
  # artifactAddSeg <- 1
  
  # artifactSeg is the period in seconds to look for artifacts 
  # within the stimulus segment
  # artifactSeg <- 15
  
  {
    # 
    processArtifacts <- FALSE
    processArtifacts <- TRUE
    
    # used by the feature extraction functions 
    artifactPneumo <- FALSE
    artifactPneumo <- TRUE
    
    artifactCardio <- TRUE
    
    artifactEDA <- TRUE
    
    artifactPLE <- FALSE
    
    artifactActivity <- TRUE
    
    artifactFC <- FALSE
    
    # these parameters are used by the artifactProc function 
    # in the artifactProc.R script
    pneumoArtifacts <- FALSE
    cardioArtifacts <- FALSE
   
    edaArtifacts <- TRUE
    
    pleArtifacts <- FALSE
    
    activityArtifacts <- TRUE
    
    FCArtifacts <- FALSE
  }
  
  {
    # Nov 3, 2023
    # share artifacts across the different sensors activity->pneumo->cardio->ple->eda
    integrateArtifacts <- TRUE
    integrateArtifacts <- FALSE
    
    integrateRespArtifacts <- TRUE
    # integrateRespArtifacts <- FALSE
    
    integrateEDAArtifacts <- TRUE
    # integrateEDAArtifacts <- FALSE
    
    integrateCardioArtifacts <- TRUE
    # integrateCardioArtifacts <- FALSE
    
    integratePLEArtifacts <- TRUE
    # integratePLEArtifacts <- FALSE
    
    integrateActivityArtifacts <- TRUE
    integrateActivityArtifacts <- FALSE
    
    integrateFCArtifacts <- TRUE
    integrateFCArtifacts <- FALSE
  }
  
  
  # slopeValue is expressed as a percentage
  # using a 1 sec moving average = rise / run * 100
  # rise is expressed as a percentage of 1/2 of the Y axis dimmension
  # Y axis is 2000 and so .5 * 2000 = 1000
  # slopeValue <- 100
  # not used 3/30/2018
  
  # tonicSlope is a value used to determine tonicity 
  # 40 = 2% of the vertical range
  # 100 = 5% of the vertical range
  # tonicValue <- 200
  
  # period in seconds to check for tonicity during question prestim segment
  # tonicPeriod <- 3
  
  # slopeWindow is a moving window, measured in seconds,
  # during which the + or - slope percentage is calculated
  # slopeWindow <- 1
  # not used 3/30/2018
  
  # nonStimRatio is the ration of prestim/stim segments
  # to determine non-stimulus artifacts
  # used by the nonStimArtifactFn() in the nonStimArtifact.R script
  # nonStimRatio <- .95
  
  # print("init parameters for artifact extraction")

  # Dec 8, 2023
  # number of prestim seconds for EDA artifact extraction
  # these are actually pre-response seconds not pre-stim
  EDAPrestim <- 4
  # was 2
  
  # Dec 8, 2023
  # number of prestim seconds for Cardio artifact extraction
  # these are actually pre-response seconds not pre-stim
  cardioPrestim <- 4
  # was 2
  
  # Dec 8, 2023
  # number of prestim seconds for Pneumo artifact extraction
  pneumoPrestim <- 5
  
}



######## init parameters for feature extraction ######## 



{
  
  # print("init parameters for feature extraction")
  
  extractPneumo <- TRUE
  extractEDA <- TRUE
  extractCardio <- TRUE
  extractPLE <- TRUE
  
  extractPneumoPatterns <- TRUE
  
  # second EDA sensor
  extractEDA2 <- TRUE
  
  
  
  # if(isTRUE(PCATFormat)) {
  # 
  #   extractPneumo <- FALSE
  #   extractCardio <- FALSE
  # 
  #   extractPneumoPatterns <- FALSE
  # 
  # }
  
  
  # finger cuff
  extractFC <- TRUE
  
  # electronic cardio
  extracteCardio <- FALSE
  
  # period in seconds to segment the pneumo data 
  # pneumoLen <- 1
  
  
  # Nov 6, 2023 switch to include or exclude respiration answer buffer
  excludePneumoAnswerBuffer <- FALSE
  excludePneumoAnswerBuffer <- TRUE
  
  
  # number of seconds before/after verbal to exclude respiration extraction
  pneumoAnsBuff <- 1.5
    # .0333
  
  # period in seconds to measure respiratory excursion
  # pneumoMeasurementBuffer <- 2.5
  # 3.75 # r=.49 # 12-9-2021
  # was 2.5 12/9/21 # this value outperforms others
  pneumoMeasurementBuffer <- 3.75
  # pneumoMeasurementBuffer <- 15
  # pneumoMeasurementBuffer <- 7.5
  # pneumoMeasurementBuffer <- 1.875
  # pneumoMeasurementBuffer <- 1
  
  # multiplier for pneumo feature extraction value
  pneumoFEFactor <- 1
  
  # prestimSeg is the number of seconds of data to include in the prestim segment
  # when slicing a stimulus segment from the chart data frame
  prestimSeg <- 10
  
  # latency is 1 sample
  PneumoLat <- .033333333
  
  # EDALat is the latency period for EDA responses, 
  # response onset is ignored if is within the latency 
  EDALat <- .5
  # EDALat <- .033333333
  
  # CardioLat is the latency period for cardio data
  CardioLat <- .5
  # CardioLat <- .033333333
  # .0333333 == 1 sample at 30cps
  
  # useROW=FALSE will set the ROW to the evaluation window 
  useROW <- TRUE
  
  # ROWStart can be "stimOn" "stimOff" "latency" or "verbalAnswer"
  ROWStart <- "latency"
  # ROWStart <- "stimOn"
  
  # ROWStop sets the reference point for the ROW End
  # can be "answer" "onset" "offset" "latency" or "EWEnd"
  ROWStop <- "answer"
  # ROWStop <- "onset"
  
  # ROWEnd is the end offset for the Response Onset Window (ROW) 
  # set as number of seconds after the ROWStop
  ROWEnd <- 5
  # ROWEnd <- 14.9
  
  # the number of seconds afer ROWStop
  
  # measuredSeg is the number of seconds in the scoring window following stimulus onset
  # to include in the feature extraction
  measuredSeg <- 15
  
  # addSeg is the number of seconds to display after ther measured segment
  # to be included in stimulus segment plots
  # and also when slicing a stimulus segment from the chart data frame
  addSeg <- 10
  
  # cardioLine defines where to make the cardio feature extraction
  # "mid" "diastolic" "systolic" or "ma" ma=slow moving average
  cardioLine <- "ma"
  
  # cardioLine defines where to make the cardio feature extraction
  # "mid" "diastolic" "systolic" or "ma" 
  # "ma" is the slower moving average
  FCLine <- cardioLine
  
  # recoveryProp is the proportion of return to onset value
  # used when evaluating response duration or 1/2 recovery time 
  # .632 = 1 time constant
  recoveryProp <- .368
  # .632 == 1 time constant == the time needed to return .632 * distance to origin
  # after 5 * Tc the data will return 99% of the distance to the origin
  # 1 - .632 == .368
  
  # strictROW = TRUE will force a strict response onset window
  # strict means that upward segments beginning after ROWEndRow are always ignored
  # FALSE will include positive slope segments that begin after ROW
  # but only if the data have not descended below the onset Y value
  # or below 50% of the max Y value if the descentRule is not 0
  # the allowed descent proportion is set by the descProp parameter
  # FALSE will also make use of the descentRule parameter
  # strictROW <- TRUE
  EDAStrictROW <- FALSE
  cardioStrictROW <- FALSE
  
  # strictWindow = TRUE will force a strict EDA and cardio evaluation window
  # strict means that measured responses end at the end of the EW 
  # even if the response continues to a peak ofter the end of the EW
  # FALSE will score to the end of reaction even if outside the EW
  # strictWindow <- TRUE
  EDAStrictWindow <- TRUE
  EDAStrictWindow <- FALSE
  cardioStrictWindow <- TRUE
  cardioStrictWindow <- FALSE
  
  # number of seconds to shorten the window when strictWindow == FALSE
  shortenEW <- 1.5
  
  # descentRule is a switch 
  # descentRule will exclude positive slope segments 
  # after the data have descended a proportion from the previous max peak
  # descentRule is used when strictROW==FALSE
  # descentRule = 0 will disable the use of the descentRule
  # and will use all upward segments that begin during the EW befor or after the ROW  
  # if the data have not descended below the onset Y value
  # descentRule = 1 will exclude positive slope segments during the EW
  # if the data have descended a proportion from the previous max peak value
  # descentRule = 2 will allow all ascending segments during ROW,
  # and exclude ascending segments during EW after ROW,
  # if the data descend more than a proportion descProp from the previous peak
  # descentRule = 1 or 2 uses the descProp parameter 
  descentRule <- 2
  
  # descProp is the proportion to use for the descentRule
  # when strictROW=FALSE
  # used by the descentRule
  # .632 = 1 time constant
  descProp <- .632
  
  # ignore is a parameter to ignore slope changes of small duration
  # when locating EDA and cardio response onset and response end x row indices
  # ignore value is the number of samples for  short slope segments that will be ignored
  # ignore=3 will exclude slope changes less than 1/10 sec in duration
  # changed to 1 1/6/2017 to improved handling of noisy EDA data
  # changed to 6 during 2018 to improve robust performance with bad data
  ignore <- 7
  # changed to 7 2020-02-17 to ignore slope changes of less than 1/4 second
  
  # inflection method
  # 0 = no inflection detection
  # 1 = select the value at 2.5 seconds
  # 2 =  statistical method
  inflection <- 2
  
  # slopechangeRule is a switch to control the use of a response onset location
  # as a function of a significant change (increase) in positive slope
  # when the slope is positive at stimulus onset 
  # slopeChangeRule = 0 will disable the use of significant changes in positive slope as a response onset
  # slopeChangeRule = 1 will enable the use of significant changes in positive slope as a response onset
  # slopeChangeRule = 2 will enable the use of significant changes in positive slope 
  # but only when there is no normal onset of a positive slope segment during the ROW
  slopeChangeRule <- 1
  cardioSlopeChangeRule <- 0
  
  # aChange is the quantile threshold to interpret a sig increase in upward slope activity
  # .9986501 = 3 standard deviations
  # .99 = 2.326348
  # 0.9937903 = 2.5 SD
  # .999 = 3.090232
  # .9999683 = 4 SD
  # .9998 = 3.5 S.D.
  aChange <- 0.999
  
  # nPre is the number of seconds to evaluate for signficant changes in postive slope energy
  nPre <- 1
  # was 1.5 2018-11-18 and 2020-03-21
  # changed from .5 to 2 sec 10-6-2018
  # changed from 3 to .5sec 5-5-2018 
  # still have a problem with tonicity and - to + slope changes during latency
  # was 1.5 change to 2sec 2-17-2018
  
  # nPost is the number of seconds to compare to nPre for sig change in pos slope activity
  nPost <- 1
  # was .5 6-21-2020
  # was 1.5 changed to .5 2-17-2018
  # 10-27-2015 was nPre=1, nPost=.5
  # 4-29-2016 changed nPost from 1 to .5
  # was .5 changed to 1 sec 10-6-2016
  # 11-7-2016 nPre=2 nPost=.5
  # 11-15-2016 nPre=1 nPost=1
  
  # tonicSec is the number of seconds for which to 
  # check for upward slope before inferring a response onset
  tonicSec <- 2 # was 3
  # was 2.5
  # was 1.5 # 20190425
  
  # ignoreTonicChange is the length in seconds for which 
  # to ignore small slope changes when evaluating upward slope
  # before inferring a response onset in EDA and cardio
  ignoreTonicChange = .25
  
  # sChangeLat is the number of seconds of latency 
  # after stimulus onset
  # before inferring a response onset via slope change
  sChangeLat <- 2.5
  # was 0.5 until 2025May14
  # was 2.5
  # was .5 20200321
  # was 1.5 20200223
  
  # sep 27, 2021 need to sort out the diff and uses of sChangeLat and tonicSec
  
  #### PLE 
  
  {
    # Vandenbosch, Verschuere, et al. 2009 FPLL method ####
    # 3 prestim seconds
    # 15 stimulus seconds
    # no latency
  }
  
  {
    # LIC method after 2015 Honts publication
    # 3 prestim seconds
    # 5 latency seconds
    # 5 stimulus seconds
  }
  
  # number of prestimulus seconds for the PLE feature extraction
  PLEPrestim <- 3
  
  # latency, in seconds after stimulus onset
  PLELat <- 5
  
  # end of the PLE stimulus segment in seconds after stimulus onset
  PLEPostEnd <- 10
  
  # select pythagorean or excursion method for PLE feature extraction
  PLEMethod <- "SDL"
  # FPLL = finger pulse line length method (Vandenbosch, Verschuere, et al. 2009 )
  # FPA = finger pulse amplitude (distance between max and min peaks)
  # SDL = systolic - diastolic line method (using interpolated sys and dyst lines)
  
  # July 27, 2023S
  # set this parameter to FALSE to prevent evaluation of the strength of RQ and CQ response
  PLESomethingVsSomethingIsNothing <- FALSE
  # PLESomethingVsSomethingIsNothing <- TRUE
  # TRUE will return a score of 0 when there is a PLE reaction at both the RQ and CQ
  
  ####
  
  # stop feature extraction for inspection
  # stopSeg <- c(FALSE, case=1, series=2, chart=3, seg="R7")
  # # stop feature extraction for inspection
  stopSeg <- c(FALSE, case=3, series=1, chart=3, seg="C4")
  
}



######## decision rules ######## 



{

  # # DRule <- "TSR"
  # DRule <- "SSR"
  # 
  # ESSMDecisionRule <- ifelse(DRule=="TSR", "eTSR", "SSR")
  # # ESSMDecisionRule <- ifelse(DRule=="TSR", "TSR", "SSR")
  # # ESSMDecisionRule <- "SSR"
  # 
  # OSS3DecisionRule <- ifelse(DRule=="TSR", "TSR", "SCR")
  # # OSS-3 can use the SCR rule which uses the KW-ANOVA
  # OSS3DecisionRule <- "SCR"
  # 
  # PADecisionRule <- "TSR"
  # 
  # OSS2DecisionRule <- "GTR"
  # 
  # ROSSDecisionRule <- ifelse(DRule=="TSR", "GTR", "SSR")
  # # ROSSDecisionRule <- "SSR"
  # 
  # PSSDecisionRule <- "GTR"
  # 
  # bootstrapDecisionRule <- "GTR"
  # 
  # PCATDecisionRule <- "pTSR"
  # # PCATDecisionRule <- "pSSR"
  # 
  # # normally comment this out
  # # if(isTRUE(PCASSFormat)) ESSMDecisionRule <- "SSR"
  # 
  # # decision rules can be "GTR" "SSR" "TSR" "FZR"
  # # also use "auto" for OSS-3, ESS-M and PCASS-2
  # # to auto-select decision rules either "SSR" or "TSR"
  # 
  # 
  # # used to stop scoring and inspect progress
  # stopScore <- FALSE

}



######## init parameters for analysis and scoring ######## 



{
  
  includePLEScores <- TRUE
  # also check the includePLEData in the workFlow_init.R script
  
  # all alphas are one-tailed unless otherwise specified
  
  essmAlphaD <- .05
  essmAlphaT <- .05
  essmAlphas <- c(aD=essmAlphaD, aT=essmAlphaT)
  essmPrior <- .5
  
  oss2AlphaT <- .07 # +6
  oss2AlphaD <- .06 # -6
  
  oss3AlphaTDiag <- .05
  oss3AlphaDDiag <- .05
  oss3AlphaTScreen <- .05
  oss3AlphaDScreen <- .05
  OSS3Alpha <- c(oss3AlphaTDiag=oss3AlphaTDiag, 
                 oss3AlphaDDiag=oss3AlphaDDiag, 
                 oss3AlphaTScreen=oss3AlphaTScreen, 
                 oss3AlphaDScreen=oss3AlphaDScreen )
  
  bootstrapCutProbT=.3
  bootstrapCutProbD=.7
  
  PSSCutProbT=.1
  PSSCutProbD=.9
  forceINC <- TRUE # attempt to resolve INC cases with extreme scores
  # PSSCutProbT=.3
  # PSSCutProbD=.7
  
  PACutProbT=.7
  PACutProbD=.3
  PAPrior=.5
  
  # ipZAlphaD <- .49
  ipZAlphaD <- .333
  
  # PCATPrior is in the form of a decimal probability of deception
  PCATPrior=.5
  # PCAT cutscores are if the form of posterior odds to 1
  PCATAlphaD <- .05
  PCATAlphaT <- .05
  PCATAlphas <- c(aD=PCATAlphaD, aT=PCATAlphaT)
  
  
  # ROSSCutScores <- c(GTDI=-14, GTNDI=14, STDI=-3, STNDI=3)
  ROSSCutScores <- c(GTDI=-6, GTNDI=6, STDI=-3, STNDI=3)
  # ROSSCutScores <- c(GTDI=-14, GTNDI=14)
  
  # coerce the analysis with non-compliant exams for OSS-2
  forced=TRUE
  
  # used in the workFlow.R script
  extractFeatures <- TRUE
  
}
  


######## constraints for R/C ratios ######## 



{
  
  # PLE constraint for min pre/post ratios else assign 0
  # PLEconstraint <- log(1.01)
  ## not sure this is used for anything 6-21-2020
  
  # minimum contraint ratio for accepting a vasomotor response
  # applied to both pre/post ratios and R/C ratios
  # Feb 24, 2022
  PLEConstraint <- log(1.1) # [1] [1] 0.09531018
  # PLEConstraint <- log(1.05) # [1] 0.04879016
  # PLEConstraint <- log(1.02) # [1] 0.01980263
  # used by the PLEExtract function and RCToESSFn
  
  # min ratio constraint for accepting a cardio R/C score to make an ESSM score
  # cardioConstraint <- log(1.33)
  cardioConstraint <- log(1.25) 
  # was 2 # oct 13, 2021
  
  # min ratio constraint for accepting an EDA score
  EDAConstraint <- log(1.1)
  # 1.2 # r=.791 12-9-2021
  
  # minimum constraint ratio for accepting a respiration response
  # applied to pre/post ratios 
  # Feb 24, 2022
  pneumoConstraint <- log(1.2) # 0.2231436
  
  # applied to the R/C Ratio
  # pneumoConstraintLow <- log(1.05) # 0.04879016
  # pneumoConstraintLow <- log(1.2) # 0.1823216
  # Feb 24, 2022
  pneumoConstraintLow <- log(1.2) # 0.2231436
  # pneumoConstraintLow <- log(1.33) # 0.2851789
  # pneumoConstraintLow <- log(1.5) # 0.4054651
  # pneumoConstraintHigh <- log(1.67) # 0.5128236
  # pneumoConstraintHigh <- log(1.75) # 0.5596158
  # Feb 24, 2022
  # pneumoConstraintHigh <- log(1.5) # 0.4054651
  pneumoConstraintHigh <- log(1.6) # 0.4700036
  # pneumoConstraintHigh <- log(2) # 0.6931472
  
  # Sep 27, 2021
  # nothingIsSomething
  # for EDA and cardio
  # TRUE returns 0 for no response onset 
  # FALSE returns NA for no response onset
  nothingIsSomething <- FALSE
  # nothingIsSomething <- TRUE
  # used in the amplitudeExtractPCFn() for EDA and Cardio feature extractio
  
}



######## init parameters for plotting ######## 



{
  
  # parameters to describe the data collection, signal processing, and feature extraction
  # cps <- 30
  # EDALat <- .5
  # CardioLat <- .5
  # ROWEnd <- 5
  # measuredSeg <- 15
  
  # prestimSeg <- 5
  # addSeg <- 5
  
  # to control the print output
  # showNames <- TRUE
  # output <- FALSE
  # showMeasurements <- TRUE
  # showArtifacts <- TRUE
  # outputSegmentFileName <- "_segmentPlot.pdf"
  # separateCharts <- FALSE
  # printPlot <- TRUE
  
  # plot a single segment instead of iterating through all exams in the global environment
  # getSegment <- FALSE
  # examNum <- 1
  # seriesNum <- 1
  # chartNum <- 1
  # segmentNum <- 8
  
  
  ###
  
  
  # to control the print output
  # showNames <- TRUE
  # output <- FALSE
  # showMeasurements <- TRUE
  # showArtifacts <- TRUE
  # outputChartFileName <- "_chartsPlot.pdf"
  # separateCharts <- FALSE
  # printPlot <- TRUE
  
  # plot a single segment instead of iterating through all exams in the global environment
  # getSegment <- FALSE
  # examNum <- 1
  # seriesNum <- 1
  # chartNum <- 3
  # segmentNum <- "ALL"
  
}



######## 




