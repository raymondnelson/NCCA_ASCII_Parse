# polygraph decision rules
# Mar 1, 2019
# Raymond Nelson
###
#
# GTR Grand Total Rule - integer or decimal scores
# SSR Subtotal Score Rule - integer or decimal scores
# TSR Two-Stage Rule (Senter Rules) - integer or decimal scores
# FZR Federal Zone Rule - integer scores only 
# TES Decision rule for Federal TES/DLST (same as FZR) - integer score only
# UT4 Utah 4 question rule - integer scores only 
# SCN Screening Rule (OSS-3) uses the KWANOVA - decimal scores only
# pSSR SSR rule for PCAT/LXCAT - integer scores
#
####

# used by the ESS-M algorithm
# source(paste0(RPath, 'autoSelectGTRTSR.R', echo=FALSE)
source(paste0(RPath, 'autoSelectTSRSSR.R'), echo=FALSE)

# used by the SCN decision rule for OSS-3
source(paste0(RPath, 'KWANOVA.R'), echo=FALSE)

##########  Grand total Rule ##########

GTRFn <- function(totalScore=.7, 
                  RQNames=NULL, 
                  cutScores=c(GTDI=-3, GTNDI=3), 
                  flip=FALSE) {
  # grand total decicion rule
  # input totalScore can be an interger or decimal value
  # GTR does not use subtotals and so subtotals are not input
  # RQNames input is a vector of RQ labels 
  # totalScore can be integer or decimal form
  # DI/SR classifications are lower side 
  # NDI/NSR classifications are upper side
  # flip is used to invert the polarity of decimal scores and probability cutscores
  # use flip=FALSE for integer cutscores
  # output is a list 
  ###
  # if(length(totalScore)==0) {
  #   # August 11, 2023 to prevent scoring when data insufficient
  #   return(list(testResult="none", 
  #               subtotalResults="none",
  #               resultUsing="none",
  #               totalScore="none",
  #               cutScore="none",
  #               lowestRQName="none",
  #               lowestSubtotalScore="none" ) )
  # }
  if(isTRUE(flip)) {
    # only for decimal scores and decimal cutscores
    # use flip <- TRUE for bootstrap and PA algorithms
    totalScore <- 1 - totalScore
    cutScores <- 1 - cutScores
  }
  # parse the test result
  testResult <- ifelse(totalScore <= cutScores['GTDI'],
                       "DI/SR",
                       ifelse(totalScore >= cutScores['GTNDI'],
                              "NDI/NSR",
                              "INC/NO" ))
  names(testResult) <- "GTR"
  outputCutscore <- ifelse(testResult == "DI/SR", 
                           cutScores['GTDI'],
                           cutScores['GTNDI'] )
  if(isTRUE(flip)) { 
    outputCutscore <- 1 - outputCutscore 
    totalScore <- 1 - totalScore
    cutScores <- 1 - cutScores
  }
  # subtotal results are inherited from the grand total result
  # all subtotals get the same result as the test result
  # subtotal results are only to satisfy people who need explicit results
  if(is.null(RQNames)) {
    # no subtotal results if no RQ names are input
    subtotalResults <- NULL
  } else {
    subtotalResults <- rep(testResult, times=length(RQNames))
    names(subtotalResults) <- RQNames
  }
  # output as a list
  list(testResult=testResult, 
       subtotalResults=subtotalResults,
       resultUsing="grand total",
       totalScore=totalScore,
       cutScore=outputCutscore,
       lowestRQName="none",
       lowestSubtotalScore="none" )
  # end GTRFn()
} 



########## Subtotal Score Rule ##############

# private function
SSRFn <- function(subtotalScores=c(-2,2,3),
                  cutScores=c(STDI=-3, STNDI=1), 
                  flip=FALSE ) {
  # subtotal score rule
  # ESS-M and OSS-3 use a statistical correction 
  # for NDI/NSR classifications with subtotal scores
  # subtotalScores input is a named vector of decimal or integer values
  # names of subtotalScore are the RQ names
  # input cutscores is a vector of 2 items ("STDI", "SDNDI")
  # the cutscores can be input with or without a statistical correction
  # DI/SR classifications are lower side 
  # NDI/NSR classifications are upper side
  # flip can be used to invert the polarity of decimal scores and probability cutscores
  # use flip=FALSE for integer cutscores
  # flip can be used to invert the polarity of the decimal input and cutscores
  # use flip=FALSE for integer cutscores
  # output is a list 
  ###
  # if(all(!is.na(subtotalScores))) {
  #   # August 11, 2023 to prevent scoring when data insufficient
  #   return(list(testResult="none", 
  #               subtotalResults="none", 
  #               resultUsing="none",
  #               lowestRQName="none",
  #               lowestSubtotalScore="none",
  #               cutScore="none" ) )
  # }
  if(!exists("flip")) flip <- FALSE
  if(isTRUE(flip)) {
    # only for decimal scores and decimal cutscores
    # use flip <- TRUE for bootstrap and PA algorithms
    totalScore <- 1 - totalScore
    cutScores <- 1 - cutScores
  }
  # fix cutscore issue, coerce use of the uncorrected subtotal cutscore
  if(!("STNDIc" %in% names(cutScores))) cutScores['STNDIc'] <- cutScores['STNDI']
  # check the names of the input cutscores
  # if(is.null(names(cutScores))) names(cutScores) <- c("GTNDI", "GTDI", "STDIc", "STDI", "STNDIc")
  # get the values needed to parse the result
  nRQ <- length(subtotalScores)
  if(is.null(names(subtotalScores))) names(subtotalScores) <- 
      paste0("R", c(1:nRQ))
  thisSubtotalScore <- which.min(subtotalScores)
  minSubtotalScore <- subtotalScores[thisSubtotalScore]
  names(minSubtotalScore) <- NULL
  # first parse the overall test result from the lowest subtotal
  testResult <- ifelse(minSubtotalScore <= cutScores['STDI'],
                       "DI/SR",
                       ifelse(minSubtotalScore >= cutScores['STNDIc'],
                              "NDI/NSR",
                              "INC/NO" ))
  # overall test result is inherited from the min subtotal 
  names(testResult) <- "SSR"
  outputCutscore <- ifelse(testResult == "DI/SR", 
                           cutScores['STDI'],
                           cutScores['STNDIc'] )
  # then parse the results for the individual questions
  # cannot have NDI/NSR and DI/SR in the same exam
  # first set everything to INC
  subtotalResults <- rep("INC/NO", times=nRQ)
  names(subtotalResults) <- names(subtotalScores)
  if(testResult == "NDI/NSR") {
    # all subtotals have the same result if the min is NDI/NSR
    subtotalResults <- rep("NDI/NSR", times=nRQ)
  } else if(testResult == "DI/SR") {
    # set the DI/SR subtotals
    # no statistical correction on DI/SR side with the SSR
    DISRSubtotals <- which(subtotalScores <= cutScores['STDI'])
    subtotalResults[DISRSubtotals] <- "DI/SR"
  } else {
    # if the test result is INC/NO then set NDI/NSR subtotals to NDI/NSR
    # SSR should use a statistical correction on the NDI/NSR side
    NDINSRSubtotals <- which(subtotalScores >= cutScores['STNDIc'])
    subtotalResults[NDINSRSubtotals] <- "NDI/NSR"
  }
  # ouput as a list
  list(testResult=testResult, 
       subtotalResults=subtotalResults, 
       resultUsing="lowest subtotal",
       lowestRQName=names(subtotalScores)[thisSubtotalScore],
       lowestSubtotalScore=minSubtotalScore,
       cutScore=outputCutscore )
  # end SSRFn()
}



########### Two-stage Rule ############

eTSRFn <- function(totalScore=6, 
                   subtotalScores=c(1,2,3),
                   cutScores=c(GTDI=-3, GTNDI=3, STDIc=-7), 
                   flip=FALSE ) {
  # two-stage rule aka Senter rule (Senter & Dollins, 2003)
  # to parse the categorical result from numerical or probability results
  # totalScore can be in integer or decimal form
  # input subtotalScores is a named vector of values for each RQ
  # subtotalScores can be in integer or decimal form
  # names of subtotalScores are the RQ names
  # cutscores is a vector of 3 named items "GTDI" "GTNDI" and "STDIc"
  # cutscores can be integer or decimal
  # ESS-M and OSS-3 use a statistical correction 
  # for DI/SR classifications with subtotal scores
  # DI/SR classifications are lower side 
  # NDI/NSR classifications are upper side
  # flip can be used to invert the polarity of decimal scores and probability cutscores
  # use flip=FALSE for integer cutscores
  # output is a list of 2 items: grand total result and subtotal results
  ###  
  # if(length(totalScore)==0 && all(!is.na(subtotalScores))) {
  #   # August 11, 2023 to prevent scoring when data insufficient
  #   return(list(testResult="none", 
  #        subtotalResults="none", 
  #        resultUsing="none", 
  #        cutScore="none",
  #        totalScore="none",
  #        lowestRQName="none",
  #        lowestSubtotalScore="none" ) )
  # }
  if(isTRUE(flip)) {
    # only for decimal scores and decimal cutscores
    # use flip <- TRUE for bootstrap and PA algorithms
    totalScore <- 1 - totalScore
    cutScores <- 1 - cutScores
  }
  # get the values needed to parse the result
  if(is.null(names(cutScores))) {
    names(cutScores) <- c("GTDI", "GTNDI", "STDIc")
  }
  nRQ <- length(subtotalScores)
  if(is.integer(totalScore) && totalScore != sum(subtotalScores)) { 
    return("TSR input error: inconsistent grand total and subtotal scores") 
  } else # if(!is.integer(totalScore) &&
    #   round(totalScore, 3) != round(pnorm(mean(qnorm(subtotalScores))), 3) ) {
    #   # return("TSR input error: inconsistent grand mean and subtotal means")
    # }
    if(is.null(names(subtotalScores))) names(subtotalScores) <- 
    paste0("R", c(1:nRQ))
  thisSubtotalScore <- which.min(subtotalScores)
  minSubtotalScore <- subtotalScores[thisSubtotalScore]
  # first parse the test result in this order
  # DI grand total, DI subtotal, NDI grand total
  # this is different than the original Senter two-stage rule
  # and may reduce FN errors
  ifelse(totalScore <= cutScores['GTDI'],
         # parse the result within the ifelse()
         # so that we can capture "using" info
         testResult <- c(result="DI/SR", using="grand total", cutScore=cutScores['GTDI']),
         ifelse(minSubtotalScore <= cutScores['STDIc'],
                testResult <- c(result="DI/SR", using="lowest subtotal", cutScore=cutScores['STDIc']),
                ifelse(totalScore >= cutScores['GTNDI'],
                       testResult <- c(result="NDI/NSR", using="grand total", cutScore=cutScores['GTNDI']),
                       testResult <- c(result="INC/NO", using="grand total", cutScore=cutScores['GTNDI']) ) ) )
  names(testResult)[1] <- "TSR"
  # all subtotals get the same result as the test result
  subtotalResults <- rep(testResult[1], times=nRQ)
  names(subtotalResults) <- names(subtotalScores)
  # output as a list
  list(testResult=testResult[1], 
       subtotalResults=subtotalResults, 
       resultUsing=testResult[2], 
       cutScore=testResult[3],
       totalScore=totalScore,
       lowestRQName=names(subtotalScores)[thisSubtotalScore],
       lowestSubtotalScore=minSubtotalScore )
} # end TSRFn()


TSRFn <- function(totalScore=6, 
                   subtotalScores=c(1,2,3),
                   cutScores=c(GTDI=-3, GTNDI=3, STDIc=-7), 
                   flip=FALSE ) {
  # two-stage rule aka Senter rule (Senter & Dollins, 2003)
  # to parse the categorical result from numerical or probability results
  # totalScore can be in integer or decimal form
  # input subtotalScores is a named vector of values for each RQ
  # subtotalScores can be in integer or decimal form
  # names of subtotalScores are the RQ names
  # cutscores is a vector of 3 named items "GTDI" "GTNDI" and "STDIc"
  # cutscores can be integer or decimal
  # ESS-M and OSS-3 use a statistical correction 
  # for DI/SR classifications with subtotal scores
  # DI/SR classifications are lower side 
  # NDI/NSR classifications are upper side
  # flip can be used to invert the polarity of decimal scores and probability cutscores
  # use flip=FALSE for integer cutscores
  # output is a list of 2 items: grand total result and subtotal results
  ###  
  # if(length(totalScore)==0 && all(!is.na(subtotalScores))) {
  #   # August 11, 2023 to prevent scoring when data insufficient
  #   return(list(testResult="none", 
  #        subtotalResults="none", 
  #        resultUsing="none", 
  #        cutScore="none",
  #        totalScore="none",
  #        lowestRQName="none",
  #        lowestSubtotalScore="none" ) )
  # }
  if(isTRUE(flip)) {
    # only for decimal scores and decimal cutscores
    # use flip <- TRUE for bootstrap and PA algorithms
    totalScore <- 1 - totalScore
    cutScores <- 1 - cutScores
  }
  # get the values needed to parse the result
  if(is.null(names(cutScores))) {
    names(cutScores) <- c("GTDI", "GTNDI", "STDIc")
  }
  nRQ <- length(subtotalScores)
  if(is.integer(totalScore) && totalScore != sum(subtotalScores)) { 
    return("TSR input error: inconsistent grand total and subtotal scores") 
  } else # if(!is.integer(totalScore) &&
    #   round(totalScore, 3) != round(pnorm(mean(qnorm(subtotalScores))), 3) ) {
    #   # return("TSR input error: inconsistent grand mean and subtotal means")
    # }
    if(is.null(names(subtotalScores))) names(subtotalScores) <- 
    paste0("R", c(1:nRQ))
  thisSubtotalScore <- which.min(subtotalScores)
  minSubtotalScore <- subtotalScores[thisSubtotalScore]
  # first parse the test result in this order
  # DI grand total, NDI grand total, DI subtotal, 
  # this is different than the original Senter two-stage rule
  # and may reduce FN errors
  ifelse(totalScore <= cutScores['GTDI'],
         # parse the result within the ifelse()
         # so that we can capture "using" info
         testResult <- c(result="DI/SR", using="grand total", cutScore=cutScores['GTDI']),
         ifelse(totalScore >= cutScores['GTNDI'],
                testResult <- c(result="NDI/NSR", using="grand total", cutScore=cutScores['GTNDI']),
                ifelse(minSubtotalScore <= cutScores['STDIc'],
                       testResult <- c(result="DI/SR", using="lowest subtotal", cutScore=cutScores['STDIc']),
                       testResult <- c(result="INC/NO", using="grand total", cutScore=cutScores['GTNDI']) ) ) )
  names(testResult)[1] <- "TSR"
  # all subtotals get the same result as the test result
  subtotalResults <- rep(testResult[1], times=nRQ)
  names(subtotalResults) <- names(subtotalScores)
  # output as a list
  list(testResult=testResult[1], 
       subtotalResults=subtotalResults, 
       resultUsing=testResult[2], 
       cutScore=testResult[3],
       totalScore=totalScore,
       lowestRQName=names(subtotalScores)[thisSubtotalScore],
       lowestSubtotalScore=minSubtotalScore )
} # end TSRFn()



############ Federal Zone Rule #############

FZRFn <- function(grandTotal=6, 
                  subtotalScores=c(1,2,3),
                  cutScores=c(GTDI=-6, GTNDI=6, STDIc=-3) ) {
  # Federal Zone Rule (FZR)
  # to parse the categorical result from numerical or probability results
  # used with the Federal ZCT and YouPhase, with 7 position and 3 position scores
  # grandTotal and subtotalScores input is in integer form only 
  # input grandTotal is a single value as an integer or decimal probability
  # input subtotalScores is a vector of values for each RQ
  # cutscores is a vector of 3 named items "GTDI" "GTNDI" and "STDIc"
  # traditional cutscores for Federal 7 position and 3 position scores
  # ZCT grand total = +6/-6 with (+ at all spots) or subtotal = -3 (wit)
  # YouPhase grand total = +4/-4 with (+ at all spots) or subtotal = -3 (wit)
  # # ESS-M and OSS-3 use a statistical correction
  # for DI/SR classifications with subtotal scores
  # output is a list of 3 items: grand total result and subtotal results
  ###  
  if(length(grandTotal)==0 || any(!is.na(subtotalScores))) {
    # August 11, 2023 to prevent scoring when data insufficient
    return(list(testResult="none", 
                subtotalResults="none", 
                resultUsing="none",
                cutScore="none",
                totalScore="none",
                lowestRQName="none", 
                lowestSubtotalScore="none" ) )
  }
  nRQ <- length(subtotalScores)
  if(is.integer(grandTotal) & grandTotal != sum(subtotalScores)) { 
    # check the input scores
    return("error") 
  } 
  if(is.null(names(subtotalScores))) names(subtotalScores) <- 
      paste0("R", c(1:nRQ))
  thisSubtotalScore <- which.min(subtotalScores)
  minSubtotalScore <- subtotalScores[thisSubtotalScore]
  # parse the test result
  ifelse(minSubtotalScore <= cutScores['STDIc'],
         testResult <- c(result="DI/SR", using="lowest subtotal", cutScore=cutScores['STDIc']),
         ifelse(grandTotal <= cutScores['GTDI'],
                testResult <- c(result="DI/SR", using="grand total", cutScore=cutScores['GTDI']),
                ifelse((grandTotal >= cutScores['GTNDI'] && minSubtotalScore > 0),
                       testResult <- c(result="NDI/NSR", using="grand total", cutScore=cutScores['GTNDI']),
                       testResult <- c(result="INC/NO", using="lowest subtotal", cutScore=cutScores['GTNDI']) ) ) )
  names(testResult) <- NULL
  # all subtotals get the same result as the test result
  subtotalResults <- rep(testResult[1], times=nRQ)
  names(subtotalResults) <- names(subtotalScores)
  # output as a list
  list(testResult=testResult[1], 
       subtotalResults=subtotalResults, 
       resultUsing=testResult[2],
       cutScore=testResult[3],
       totalScore=grandTotal,
       lowestRQName=names(subtotalScores)[thisSubtotalScore], 
       lowestSubtotalScore=minSubtotalScore )
} # end FZRFn()



############ TES Decision Rule #############

TESFn <- function(grandTotal=5, 
                  subtotalScores=c(3,2),
                  cutScores=c(GTDI=-4, GTNDI=4, STDIc=-3) ) {
  # TES Decision rule 
  # similar to the Federal Zone Rule (FZR) wih YouPhase cutscores
  # to parse the categorical result from numerical or probability results
  # used with the Federal TES exams, with 7 position and 3 position scores
  # grandTotal and subtotalScores input is in integer form only 
  # input grandTotal is a single value as an integer or decimal probability
  # input subtotalScores is a vector of values for each RQ
  # cutscores is a vector of 3 named items "GTDI" "GTNDI" and "STDIc"
  # traditional cutscores are 
  # grand total = +4/-4 with (+ at all spots) or subtotal = -3 (wit)
  # ESS-M and OSS-3 use a statistical correction 
  # for DI/SR classifications with subtotal scores
  # output is a list of 3 items: grand total result and subtotal results
  ###  
  if(length(grandTotal)==0 || any(!is.na(subtotalScores))) {
    # August 11, 2023 to prevent scoring when data insufficient
    return(list(testResult="none", 
                subtotalResults="none", 
                resultUsing="none",
                cutScore="none",
                totalScore="none",
                lowestRQName="none", 
                lowestSubtotalScore="none" ) )
  }
  nRQ <- length(subtotalScores)
  if(is.integer(grandTotal) & grandTotal != sum(subtotalScores)) { 
    # check the input scores
    return("error") 
  } 
  if(is.null(names(subtotalScores))) names(subtotalScores) <- 
      paste0("R", c(1:nRQ))
  thisSubtotalScore <- which.min(subtotalScores)
  minSubtotalScore <- subtotalScores[thisSubtotalScore]
  # parse the test result
  ifelse(minSubtotalScore <= cutScores['STDIc'],
         testResult <- c(result="DI/SR", using="lowest subtotal", cutScore=cutScores['STDIc']),
         ifelse(grandTotal <= cutScores['GTDI'],
                testResult <- c(result="DI/SR", using="grand total", cutScore=cutScores['GTDI']),
                ifelse((grandTotal >= cutScores['GTNDI'] && minSubtotalScore > 0),
                       testResult <- c(result="NDI/NSR", using="grand total", cutScore=cutScores['GTNDI']),
                       testResult <- c(result="INC/NO", using="lowest subtotal", cutScore=cutScores['GTNDI']) ) ) )
  names(testResult) <- NULL
  # all subtotals get the same result as the test result
  subtotalResults <- rep(testResult[1], times=nRQ)
  names(subtotalResults) <- names(subtotalScores)
  # output as a list
  list(testResult=testResult[1], 
       subtotalResults=subtotalResults, 
       resultUsing=testResult[2],
       cutScore=testResult[3],
       totalScore=grandTotal,
       lowestRQName=names(subtotalScores)[thisSubtotalScore], 
       lowestSubtotalScore=minSubtotalScore )
} # end TESFn()



############ Utah 4-Question Decision Rule #############

UT4Fn <- function(grandTotal=6, 
                  subtotalScores=c(3,2,1,0),
                  cutScores=c(GTDI=-6, GTNDI=6, STDIc=-3) ) {
  # Utah 4-question decision rule 
  # used only with the Utah 4-question format with Utah 7 position scores
  # grandTotal and subtotalScores input is in integer form only 
  # input grandTotal is a single value as an integer or decimal probability
  # input subtotalScores is a vector of values for each RQ
  # cutscores is a vector of 3 named items "GTDI" "GTNDI" and "STDIc"
  # traditional cutscores are 
  # grand total = +6/-6 with (+ at all spots) or subtotal = -3 (wit)
  # output is a list of 3 items: grand total result and subtotal results
  ###  
  if(length(grandTotal)==0 || any(!is.na(subtotalScores))) {
    # August 11, 2023 to prevent scoring when data insufficient
    return(list(testResult="none", 
                subtotalResults="none", 
                resultUsing="none",
                cutScore="none",
                totalScore="none",
                lowestRQName="none", 
                lowestSubtotalScore="none" ) )
  }
  nRQ <- length(subtotalScores)
  if(nR != 4) return("error")
  if(is.integer(grandTotal) & grandTotal != sum(subtotalScores)) { 
    # check the input scores
    return("error") 
  } 
  if(is.null(names(subtotalScores))) names(subtotalScores) <- 
      paste0("R", c(1:nRQ))
  thisSubtotalScore <- which.min(subtotalScores)
  minSubtotalScore <- subtotalScores[thisSubtotalScore]
  # parse the test result
  ifelse(grandTotal <= cutScores['GTDI'],
         testResult <- c(result="DI/SR", using="grand total", cutScore=cutScores['GTDI']),
         ifelse(minSubtotalScore <= cutScores['STDIc'],
                testResult <- c(result="DI/SR", using="lowest subtotal", cutScore=cutScores['STDIc']),
                ifelse(minSubtotalScore < 0,
                       testResult <- c(result="INC/NOR", using="lowest subtotal", cutScore=cutScores['STDIc']),
                       ifelse((grandTotal >= cutScores['GTNDI']),
                              testResult <- c(result="NDI/NSR", using="grand total", cutScore=cutScores['GTNDI']),
                              testResult <- c(result="INC/NO", using="grand total", cutScore=cutScores['GTNDI']) ) ) ) )
  names(testResult) <- NULL
  # all subtotals get the same result as the test result
  subtotalResults <- rep(testResult[1], times=nRQ)
  names(subtotalResults) <- names(subtotalScores)
  # output as a list
  list(testResult=testResult[1], 
       subtotalResults=subtotalResults, 
       resultUsing=testResult[2], 
       cutScore=testResult[3],
       totalScore=grandTotal,
       lowestRQName=names(subtotalScores)[thisSubtotalScore], 
       lowestSubtotalScore=minSubtotalScore )
} # end UT4Fn()



############ OSS-3 Screening Rule #############

SCNFn <- function(totalScore=inputPVal,
                  subtotalScores=list(pRQsTruthful, pRQsDeceptive), 
                  withinChartRQMeans=withinChartRQMeans,
                  cutScores=c(GTDI = .05, GTNDI=.95, STDI=.05, STNDI=.95, STDIc=.0125, STNDIc=.8145), 
                  flip=FALSE ) {
  # Screening rule for OSS-3
  #
  # decimal input only, not for integer scores
  #
  # uses the uncorrected subtotal cutscore for SR/DI classifications
  # then uses the KWANOVA to check for differences in RQs
  # use the grand total/mean if no significant difference between RQs
  # use the corrected subtotal cutscore if RQs differ significantly
  #
  # inputPVal is lower tail for SR classifications and upper tail for NSR
  # input subtotalScores is a list of 2 vectors of decimal values for each RQ
  # pRQsDeceptive and pRQsTruthful are both lower tail
  
  # input chartTotalsDF is the in chart weighted mean sensor scores for each RQ
  # withinChartRQMeans is used by the KWANOVA function to test differences in RQs
  # cutscores is a vector of 3 named items "GTDI" "GTNDI" and "STDIc"
  # requires the KWANOVA function
  #
  # use flip=TRUE for the bootstrap and PA algorithms
  #
  # output is a list of 2 items: grand total result and subtotal results
  #
  ###
  
  if(length(totalScore)==0 || any(is.na(subtotalScores))) {
    return(list(testResult="none", 
                subtotalResults="none",
                resultUsing="none", 
                cutScore="none",
                totalScore="none",
                lowestRQName="none",
                lowestSubtotalScore="none",
                KWResult="none" ) )
  }
  
  {
    if(!exists("flip")) flip <- FALSE
	if(!exists("cutScores")) cutScores <- c(STDI=.05, GTNDI=.95, STNDIc=.017)
  }
  
  if(isTRUE(flip)) {
    # only for decimal scores and decimal cutscores
    # use flip <- TRUE for bootstrap and PA algorithms
    totalScore <- 1 - totalScore
    cutScores <- 1 - cutScores
  }
  
  # get the KWANOVA result
  # source(paste0(RPath, 'KWANOVA.R'), echo=FALSE)
  # already sourced by the OSS3Score.R script
  KWResult <- KWANOVAFn(withinChartRQMeans=withinChartRQMeans, a=.1)
  # KWResult <- "sig" # result will be the same as SSR unless KW is "ns"
  # KWResult$KWResult is the value of interest
  
  # this can fix the upper tail as needed
  # get the subtotal scores from the subtotalScores list
  pRQsTruthful <- subtotalScores[[1]]
  pRQsDeceptive <- subtotalScores[[2]]
  
  # pRQsTruthful <- 1-(abs(subtotalScores[[1]] - .5) + .5)
  # pRQsDeceptive <- (abs(subtotalScores[[2]] - .5) + .5)
  # # need the pRQsDeceptive on the upper tail
  
  if(length(pRQsTruthful) != length(pRQsDeceptive)) stop()
  # get the values needed to parse the result
  
  nRQ <- length(pRQsTruthful)
  
  # fix the RQ names if necessary
  if(is.null(names(pRQsTruthful))) {
    names(pRQsTruthful) <- paste0("R", c(1:nRQ)) 
    names(pRQsDeceptive) <- paste0("R", c(1:nRQ))
    RQNames <- paste0("R", c(1:nRQ)) 
  } else {
    RQNames <- names(pRQsTruthful)
  }
  
  thisSubtotalScoreT <- which.min(pRQsTruthful)
  minSubtotalScoreT <- pRQsTruthful[thisSubtotalScoreT]
  
  thisSubtotalScoreD <- which.min(pRQsDeceptive)
  minSubtotalScoreD <- pRQsDeceptive[thisSubtotalScoreD]
  
  ##  first parse the overall test result ##
  
  # overall test result is parsed from the grand total and subtotal scores 
  # parse the result internal to the ifelse
  # in order to capture the "result Using" data
  # first check the lowest subtotal for deception with uncorrected alpha
  ifelse(minSubtotalScoreT <= cutScores['STDI'],
         testResult <- c(result="DI/SR", using="lowest subtotal", cutScore=cutScores['STDI']),
         # else check the totalScore for deception
         # most likely will never happen DI/SR this way
         ifelse(totalScore <= cutScores['GTDI'],
                testResult <- c(result="DI/SR", using="grand total", cutScore=cutScores['GTDI']),
                # if the result is not deceptive check the KWResult
                ifelse(KWResult$KWResult=="sig",
                       # if KW is sig use the min subtotal with statistical corrected alpha
                       ifelse(minSubtotalScoreD >= cutScores['STNDIc'],
                              # if the min subtotal is NDI/NSR the test is NDI/NSR 
                              testResult <- c(result="NDI/NSR", using="lowest subtotal", cutScore=cutScores['STNDIc']),
                              testResult <- c(result="INC/NO", using="lowest subtotal", cutScore=cutScores['STNDIc']) ),
                       # if KW is ns use the grand total
                       ifelse(totalScore >= cutScores['GTNDI'],
                              # KW==ns and totalScore is NDI/NSR the test is NDI/NSR
                              testResult <- c(result="NDI/NSR", using="grand total", cutScore=cutScores['GTNDI']),
                              # else check the min subtotal for NDI/NSR with statistical correction
                              ifelse(minSubtotalScoreD >= cutScores['STNDIc'],
                                     # most likely will never happen NDI/NSR this way
                                     testResult <- c(result="NDI/NSR", using="lowest subototal", cutScore=cutScores['STNDIc']),
                                     testResult <- c(result="INC/NO", using="lowest subototal", cutScore=cutScores['STNDIc']) ) ) ) ) )
  
  # print(testResult)
  
  ##  parse the results for the individual questions ##
  
  # cannot have NDI/NSR and DI/SR in the same exam
  
  # first set everything to INC
  subtotalResults <- rep("INC/NO", times=nRQ)
  names(subtotalResults) <- RQNames
  
  # then parse the results for each question
  if(testResult[1] == "DI/SR") {
    # if the test is DI/SR then set the DI/SR subtotals
    DISRSubtotals <- which(pRQsTruthful <= cutScores['STDI'])
    subtotalResults[DISRSubtotals] <- "DI/SR"
  } else if(testResult[1] == "NDI/NSR") {
    # if the KW is ns then all RQs will have the same result
    # or if the min is NDI/NSR then all RQs have the same result 
    subtotalResults <- rep("NDI/NSR", times=nRQ)
  } else {
    # if the test result is INC/NO then set NDI/NSR subtotals to NDI/NSR
    NDINSRSubtotals <- which(pRQsDeceptive >= cutScores['STNDIc'])
    subtotalResults[NDINSRSubtotals] <- "NDI/NSR"
  }
  
  # select which min subtotal to report
  thisMinRQName <- ifelse(testResult[1] == "DI/SR",
                          RQNames[thisSubtotalScoreT],
                          RQNames[thisSubtotalScoreD] )
  thisMinSubtotal <- ifelse(testResult[1] == "DI/SR",
                            minSubtotalScoreT,
                            minSubtotalScoreD )
  
  # ouput as a list
  list(testResult=testResult[1], 
       subtotalResults=subtotalResults,
       resultUsing=testResult[2], 
       cutScore=testResult[3],
       totalScore=totalScore,
       lowestRQName=thisMinRQName,
       lowestSubtotalScore=thisMinSubtotal,
       KWResult=KWResult )
} # end SCNFn()


########## PCAT Subtotal Score Rule ##############

# private function
pSSRFn <- function(subtotalScores=c(-2,2,3),
                  cutScores=c(STDI=-3, STNDI=1), 
                  flip=FALSE ) {
  # subtotal score rule
  # ESS-M and OSS-3 use a statistical correction 
  # for NDI/NSR classifications with subtotal scores
  # subtotalScores input is a named vector of decimal or integer values
  # names of subtotalScore are the RQ names
  # input cutscores is a vector of 2 items ("STDI", "SDNDI")
  # the cutscores can be input with or without a statistical correction
  # DI/SR classifications are lower side 
  # NDI/NSR classifications are upper side
  # flip can be used to invert the polarity of decimal scores and probability cutscores
  # use flip=FALSE for integer cutscores
  # flip can be used to invert the polarity of the decimal input and cutscores
  # use flip=FALSE for integer cutscores
  # output is a list 
  ###
  
  # if(all(!is.na(subtotalScores))) {
  #   list(testResult="none", 
  #        subtotalResults="none", 
  #        resultUsing="none",
  #        lowestRQName="none",
  #        lowestSubtotalScore="none",
  #        grandTotalScore="none",
  #        cutScore="none" )
  # }
  
  if(!exists("flip")) flip <- FALSE
  if(isTRUE(flip)) {
    # only for decimal scores and decimal cutscores
    # use flip <- TRUE for bootstrap and PA algorithms
    totalScore <- 1 - totalScore
    cutScores <- 1 - cutScores
  }
  
  # fix cutscore issue, coerce use of the uncorrected subtotal cutscore
  if(!("STNDIc" %in% names(cutScores))) cutScores['STNDIc'] <- cutScores['STNDI']
  # check the names of the input cutscores
  # if(is.null(names(cutScores))) names(cutScores) <- c("GTNDI", "GTDI", "STDIc", "STDI", "STNDIc")
  
  # get the values needed to parse the result
  nRQ <- length(subtotalScores)
  if(is.null(names(subtotalScores))) names(subtotalScores) <- 
    paste0("R", c(1:nRQ))
  
  # get the lowest subtotal score
  thisSubtotalScore <- which.min(subtotalScores)
  minSubtotalScore <- subtotalScores[thisSubtotalScore]
  names(minSubtotalScore) <- NULL
  
  # calculate the grand total in case it is needed
  GTScore <- sum(subtotalScores)
  
  # defaiult
  resultUsing <- "lowest subtotal score"
  
  # first parse the overall test result from the lowest subtotal
  testResult <- 
    ifelse(minSubtotalScore <= cutScores['STDI'],
           "DI/SR",
           ifelse(GTScore <= cutScores['GTDI'],
                  {
                    # use the grand total to reduce INC results
                    resultUsing <- "grand total score"
                    "DI/SR"
                  },
                  ifelse(minSubtotalScore >= cutScores['STNDIc'],
                         "NDI/NSR",
                         # ifelse(GTScore >= cutScores['GTNDI'],
                         #        # use the grand total if possible
                         #        # to reduce INC results
                         #        {
                         #          resultUsing <- "grand total score"
                         #          "NDI/NSR"
                         #        },
                         #        "INC/NO" ) 
                         # Nov 9, 2023 changed this in attempt to decrease FNs
                         "INC/NO"
                         )
                  )
           )
  
  # overall test result is inherited from the min subtotal 
  names(testResult) <- "pSSR"
  
  outputCutscore <- ifelse(resultUsing=="grand total score",
                           ifelse(testResult=="DI/SR",
                                  cutScores['GTDI'],
                                  cutScores['GTNDI']),
                           ifelse(testResult == "DI/SR", 
                                  cutScores['STDI'],
                                  cutScores['STNDIc'] ))
  
  # then parse the results for the individual questions
  # cannot have NDI/NSR and DI/SR in the same exam
  # first set everything to INC
  subtotalResults <- rep("INC/NO", times=nRQ)
  names(subtotalResults) <- names(subtotalScores)
  if(testResult == "NDI/NSR") {
    # all subtotals have the same result if the min is NDI/NSR
    subtotalResults <- rep("NDI/NSR", times=nRQ)
  } else if(testResult == "DI/SR") {
    # set the DI/SR subtotals
    # no statistical correction on DI/SR side with the SSR
    DISRSubtotals <- which(subtotalScores <= cutScores['STDI'])
    subtotalResults[DISRSubtotals] <- "DI/SR"
  } else {
    # if the test result is INC/NO then set NDI/NSR subtotals to NDI/NSR
    # SSR should use a statistical correction on the NDI/NSR side
    NDINSRSubtotals <- which(subtotalScores >= cutScores['STNDIc'])
    subtotalResults[NDINSRSubtotals] <- "NDI/NSR"
  }
  
  # Oct 25, 2023 
  # fix for results based on the grand total score
  if(resultUsing=="grand total score") {
    subtotalResults <- testResult
  }
  
  # ouput as a list
  list(testResult=testResult, 
       subtotalResults=subtotalResults, 
       resultUsing=resultUsing,
       lowestRQName=names(subtotalScores)[thisSubtotalScore],
       lowestSubtotalScore=minSubtotalScore,
       grandTotalScore=GTScore,
       cutScore=outputCutscore )
  # end pSSRFn()
}




