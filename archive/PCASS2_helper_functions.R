# PCASS helper functions
# July 4, 2020
# Raymond Nelson
#
####

# sdp() to calculate a population standard deviation

# PCASS2SamplerFn # to sample standardized RQ and CQ values

# PCASS2_GTRFn() # grand total rule

# PCASS2_SSRFn() # subtotal score rule

# PCASS2_TSRFn() # two-stage rule

# fixPCASS2RQsFn # a function to fix PCASS question labels

####

# function to calculate the population st dev
# base R includes a function for only the sample st dev
sdp <- function(x) {
  # using the sample variance function to recalc the pop st dev
  # input is a vector of numeric values
  (sqrt(var(x, na.rm=TRUE)*(length(x)-1)/length(x)))
}


# function to sample from standardized RQ and CQ scores
PCASS2SamplerFn <- function(n=3, 
                           input1=PCASSScoresRQ, 
                           input2=PCASSScoresCQ, 
                           cutProp=.5) {
  # sample from one of two input vectors
  # n input is the number of samples to obtain
  # n is the number of iterations/charts for each RQ
  # cutProp is the proportion at which to select the input distribution
  # input1 and input2 2 are vectors to sample from
  # the default vectors include zscores for all sensors
  # default seed vectors for RQs and CQs include all sensors
  # the null hypothesis is that there is no systematic effect
  # for any of the sensors
  # they are all random and interchangeable
  # output is a vector of sample values
  outputVc <- rep(NA, length=n)
  for(o in 1:length(outputVc)) {
    #  select randomly from the RQ and CQ seeds for each iteration
    if(runif(1) > cutProp) {
      outputVc[o] <- sample(input1, 1) 
    } else {
      outputVc[o] <- sample(input2, 1)
    }
  }
  return(mean(outputVc, na.rm=TRUE))
}


# grand total rule 
PCASS2_GTRFn <- function(totalScore=postZGT, 
                        RQNames=RNames, 
                        cutScores=c(GTDI=cutZD, GTNDI=cutZT) ) {
  # grand total decicion rule for PCASS
  # input totalScore is a signed integer score or z score
  # positive sign totalScore sign values are associated with truth-telling
  # negative totalScore sign values are associated with deception
  # this way totalScore >- cutZT is NDI and totalScore <= cutZD is DI
  # GTR does not use subtotals and so subtotals are not input
  # RQNames input is a vector of RQ labels 
  # RQNames is used only to parse the RQ results from the grand total result
  # cutScores is a vector of 2 z-scores for deception and truth-telling
  # output is a list 
  ###
  # get the cutscores
  cutZD <- cutScores[1]
  cutZT <- cutScores[2]
  # parse the test result
  testResult <- ifelse(totalScore <= cutZD,
                       "DI/SR",
                       ifelse(totalScore >= cutZT,
                              "NDI/NSR",
                              "INC/NO" ))
  names(testResult) <- "GTR"
  # parse the RQ results
  # all subtotals inherit the same result as the test result
  # subtotal results are only to satisfy people who need explicit results
  # and can be provided to prevent misunderstanding and manipulation 
  if(is.null(RQNames)) {
    # no subtotal results if no RQ names are input
    subtotalResults <- NULL
  } else {
    subtotalResults <- rep(testResult, times=length(RQNames))
    names(subtotalResults) <- RQNames
  }
  # calculate the posterior odds
  postOdds <- pnorm(abs(totalScore)) / (1-pnorm(abs(totalScore)))
  if(postOdds > 99) postOdds <- 99
  # output as a list
  list(testResult=testResult, 
       subtotalResults=subtotalResults,
       cutZ=ifelse(testResult=="NDI/NSR", cutZT,  cutZD),
       resultUsing="grand total",
       zScore=totalScore,
       postOdds=postOdds )
} # end PCASS_GTRFn()

# PCASS_GTRFn()

# subtotal score rule
PCASS2_SSRFn <- function(subtotalScores=postZST, 
                        RQNames=RNames, 
                        cutScores=c(STDISR=cutZD, STNDINSR=cutZT) )
  {
  # subtotal score rule
  # subtotalScores input is a named vector of signed integer values
  # or signed z scores
  # negative subtotals are associated with deception
  # positive subtotals are associated with truth-telling
  # RQNames of subtotalScore are the RQ names
  # cutScores is a vector of 2 z-scores for deception and truth-telling
  # for NDI/NSR classifications with subtotal scores
  # a statistical correction is applied subtotal z scores 
  # for truthful classifications
  # output is a list 
  ###
  # get the cutscores
  cutZD <- cutScores[1]
  cutZT <- cutScores[2]
  # get some values needed to parse the result
  nRQ <- length(subtotalScores)
  if(is.null(names(subtotalScores))) names(subtotalScores) <- 
    paste0("R", c(1:nRQ))
  # get the min subtotal z score
  thisSubtotalScore <- which.min(subtotalScores)
  minRQName <- names(subtotalScores)[thisSubtotalScore]
  minSubtotalScore <- subtotalScores[thisSubtotalScore]
  # remove the name from the min subtotal z score for cleaner output
  names(minSubtotalScore) <- NULL
  # apply the sidak correction to the minSubtotal z score
  # and also forall subtotal z scores < 0
  if(sign(minSubtotalScore) == 1) {
    # SSR uses statistical correction only for NDI/NSR subtotals
    # pnorm gives a decimal proportion for a z value
    # qnorm gives a z value for a decimal proportion
    minSubtotalScoreC <- qnorm( pnorm(minSubtotalScore)^(1/nRQ) )
    # apply the sidak correction to subtotal z  scores > 0
    subtotalScoresC <- subtotalScores
    if(length(which(subtotalScores > 0)) > 0) {
      subtotalScoresC[which(subtotalScores > 0)] <-
        qnorm( pnorm(subtotalScores[which(subtotalScores > 0)])^(1/nRQ) )
    }
  } else {
    # no correction if the min subtotal logRC values is < 0
    minSubtotalScoreC <- minSubtotalScore
    subtotalScoresC <- subtotalScores
    # recalculate subtotal logRCs > 0 with the sidak correction
    if(length(which(subtotalScores > 0)) > 0) {
      subtotalScoresC[which(subtotalScores > 0)] <-
        qnorm( pnorm(subtotalScores[which(subtotalScores > 0)])^(1/nRQ) )
    }
  }
  # first parse the overall test result from the lowest subtotal
  ifelse(minSubtotalScore <= cutZD, # min subtotal z without  correction
         testResult <- "DI/SR",
         ifelse(minSubtotalScoreC >= cutZT,  # min  sub z with correction
                testResult <- "NDI/NSR",
                testResult <- "INC/NO" ))
  # overall test result is inherited from the min subtotal 
  names(testResult) <- "SSR"
  # then parse the results for the individual questions
  # cannot have NDI/NSR and DI/SR in the same exam
  # first set everything to INC
  subtotalResults <- rep("INC/NO", times=nRQ)
  # names(subtotalResults) <- names(subtotalScores)
  if(testResult == "NDI/NSR") {
    # all subtotals have the same result if the min is NDI/NSR
    subtotalResults <- rep("NDI/NSR", times=nRQ)
  } else if(testResult == "DI/SR") {
    # set the DI/SR subtotals if they exceed the STDISR cutscore
    DISRSubtotals <- which(subtotalScores <= cutZD) # no correction
    subtotalResults[DISRSubtotals] <- "DI/SR"
  } else {
    # if the test result is INC/NO 
    # then set NDI/NSR subtotals to NDI/NSR 
    # if they exceed the STNDINSR cutscore
    NDINSRSubtotals <- which(subtotalScoresC >= cutZT) # with correction
    subtotalResults[NDINSRSubtotals] <- "NDI/NSR"
  }
  names(subtotalResults) <- RQNames
  # get the cutZScore for output
  cutZ <- ifelse(sign(minSubtotalScore)==1, cutZT, cutZD)
  # calculate the posterior odds
  # check the sign value of the min subtotal
  if(sign(minSubtotalScore) == 1) {
    # use the corrected min score to calculate post odds for truthful results
    postOdds <- pnorm(minSubtotalScoreC) / (1-pnorm(minSubtotalScoreC))
    if(postOdds > 99) postOdds <- 99
  } else {
    # calculate the post odds for deceptive resutls without correction
    # inverse from normal calculation so we have the upper tail odds
    postOdds <- (1-pnorm(minSubtotalScore)) / pnorm(minSubtotalScore)
    if(postOdds > 99) postOdds <- 99
  }
  # ouput as a list
  list(testResult=testResult, 
       subtotalResults=subtotalResults, 
       RQNames=RQNames,
       cutZ=cutZ,
       resultUsing="lowest subtotal",
       lowestRQName=names(subtotalScores)[thisSubtotalScore],
       lowestSubtotalScore=minSubtotalScore,
       zScore=ifelse(sign(minSubtotalScore)==1,minSubtotalScoreC,minSubtotalScore),
       postOdds=postOdds )
} # end PCASS_SSRFn()

# PCASS_SSRFn()

# two-stage rule
PCASS2_TSRFn <- function(totalScore=postZGT, 
                        subtotalScores=postZST, 
                        RQNames=RNames, 
                        cutScores=c(GTDI=cutZD, GTNDI=cutZT) ) {
  # two-stage rule aka Senter rule (Senter & Dollins, 2003)
  # to parse the categorical result from numerical or probability results
  # positive sign totalScore sign values are associated with truth-telling
  # negative totalScore sign values are associated with deception
  # this way scores >= cutZT are NDI and scoores <= cutZD are DI
  # subtotalScores input is a named vector of signed z scores
  # negative subtotals are associated with deception
  # positive subtotals are associated with truth-telling
  # RQNames of subtotalScore are the RQ names
  # cutScores is a vector of 2 z values for deception and truth-telling
  # cutScore z values are calculated from odds cut values
  # TSR uses the grand total at stage 1 and the lowest subtotal at stage 2
  # a statistical correction is applied 
  # to negative subtotal z-values at stage 2
  # no truthful classification is possible at stage 2
  # this TSR is slightly different than the original,
  # and makes DI classification 
  # with statistical correction to the min subtotal z value
  # before proceding to NDI classifications
  # output is a list
  ###  
  # get the cutscores
  cutZD <- cutScores[1]
  cutZT <- cutScores[2]
  # get the values needed to parse the result
  nRQ <- length(subtotalScores)
  if(is.null(names(subtotalScores))) names(subtotalScores) <- 
    paste0("R", c(1:nRQ))
  thisSubtotalScore <- which.min(subtotalScores)
  minSubtotalScore <- subtotalScores[thisSubtotalScore]
  # remove the name from the min subtotal for cleaner output
  names(minSubtotalScore) <- NULL
  # work on the statistical correction
  # first initialize a vector to hold the corrected subtotal scores
  subtotalScoresC <- subtotalScores
  # apply a statistical correction to the minSubtotal and all subtotals
  # use the sidak instead of simple bonferroni
  if(sign(minSubtotalScore) == -1) {
    # TSR uses statistical correction only for DI/SR subtotals
    # qnorm gives the z value for a decimal proportion
    # pnorm gives the decimal proportion for a z-score
    # subtotalScores are posterior z-scores
    
    minSubtotalScoreC <- qnorm( 1-(1-pnorm(minSubtotalScore))^(nRQ) )
    # minSubtotalScoreC <- qnorm( (pnorm(minSubtotalScore)^(nRQ)) )
    
    # use a loop to check for subtotal z scores with negative sign
    i=1
    for(i in 1:length(subtotalScores)) {
      if(sign(subtotalScores[i]) == -1) {
        # TSR uses no correction for + sign  subtotal z scores
        subtotalScoresC[i] <- qnorm( 1-(1-pnorm(subtotalScores[i]))^(nRQ) )
      }
    }
  } else {
    # same as without correction, to avoid errors
    minSubtotalScoreC <- minSubtotalScore
    # use the minSubtotalScoreC for all subsequent calculatioons
    # regardless of whether a statistical correction is used
  }
  # PCASS_TSR logic here
  # first parse the test result in this order
  # DI grand total, DI subtotal, NDI grand totaal
  # this is different than the original Senter two-stage rule
  # and may reduce FN errors
  # FP errors are protected by the statistical correction
  # Stage 1 - DI classifications
  ifelse(totalScore <= cutZD,
         # parse the result within the ifelse()
         # so that we can capture "using" info
         testResult <- c(TSR="DI/SR", using="grand total"),
         ifelse(minSubtotalScoreC <= cutZD,
                testResult <- c(TSR="DI/SR", using="lowest subtotal"),
                # Stage 2 - NDI classifications
                ifelse(totalScore >= cutZT,
                       testResult <- c(TSR="NDI/NSR", using="grand total"),
                       ifelse(minSubtotalScoreC >= cutZT,
                              testResult <- c(TSR="NDI/NSR", using="lowest subtotal"),
                              testResult <- c(TSR="INC/NO", using="lowest subtotal") ) ) ) )
  # names(testResult)[1] <- "TSR"
  # parse the subtotal results similar to the GTR 
  # all subtotals get the same result as the test result
  subtotalResults <- rep(testResult[1], times=nRQ)
  names(subtotalResults) <- RQNames
  # get the cutZScore for output
  cutZ <- ifelse(testResult[2]=="grand total",
                 ifelse(sign(totalScore)==1, cutZT, cutZD),
                 ifelse(sign(minSubtotalScore)==1, cutZT, cutZD))
  # get the zScore for the test
  zScore <- ifelse(testResult[2]=="grand total", totalScore, minSubtotalScoreC)
  names(zScore) <- testResult[2]
  # calculate the posterior odds
  # check the sign value of the min subtotal
  if(sign(minSubtotalScore) == 1) {
    # use the corrected min score to calculate post odds for truthful results
    postOdds <- pnorm(minSubtotalScoreC) / (1-pnorm(minSubtotalScoreC))
    
  } else {
    # if the lowest subtotal z-score is < 0
    # calculate the post odds for deceptive resutls without correction
    # inverse from normal calculation so we have the upper tail odds
    postOdds <- (1-pnorm(minSubtotalScoreC)) / pnorm(minSubtotalScoreC)
    # if(postOdds > 99) postOdds <- 99
    
  }
  # calculate the posterior probability
  postP <- postOdds / (1 + postOdds)
  # apply some constraints to the probabilities
  if(postP > .99) {
    postP <- .99 
    } else if(postP < .01) {
      postP <- .01
    }
  if(postOdds > 99) {
    postOdds <- 99
  } else if(postOdds < .01) {
    postOdds <- .01
  }
  
  # output as a list
  list(testResult=testResult[1], 
       subtotalResults=subtotalResults, 
       resultUsing=testResult[2], 
       zScore=zScore,
       postOdds=postOdds, 
       cutZ=cutZ,
       totalScore=totalScore,
       lowestSubtotalScore=minSubtotalScoreC,
       lowestRQName=names(subtotalScores)[thisSubtotalScore] )
} # end PCASS_TSRFn()

# PCASS_TSRFn()



fixPCASS2RQsFn <- function(saveQuestionLabels=saveQuestionLabels, 
                           CQRQLabels=CQRQLabels) {
  # a function to fix the PCASS question labels
  # saveQuestionLabels is a vector with the original question labels
  # RQLabels is a vector of DLST/DLDT/PCASS type RQ labels
  # output is a vector of corrected question labels
  # of the same length to replace the original question labels
  newQuestionLabels <- saveQuestionLabels
  fixThese <- which(newQuestionLabels %in% CQRQLabels)
  if(length(fixThese)==0) return(saveQuestionLabels)
  newQuestionLabels[fixThese] <- 
    # requires the stringr package
    str_sub(newQuestionLabels[fixThese], 2, -1)
  return(newQuestionLabels)
}



BFOddsSidakFn <- function(BF, signVal, n) {
  # function to calculate the Sidak correction for Bayes Factor
  # BF is the posterior odds under the equal prior
  # or the ratio of posterior odds and prior odds
  # signVal is the sign value of the z score for the min subtotal score
  # signVal is used to select the inversion for the sidak correction
  # n is the number of RQs
  pVal <- (BF / (1 + BF))
  if(signVal == 1) {
    pValC <- (pVal)^(1/n)
    (pValC / (1 - pValC))
  } else {
    pValC <- (pVal)^(n)
    (pValC / (1 - pValC))
  }
}




