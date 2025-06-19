

################## ESS-Multinomial Reference Model ###################

{
  # set the directory location for the ESS-M reference tables 
  ESSMRefDir <- "~/Dropbox/R/NCCA_ASCII_Parse/"
  
  # import the ESS-M reference tables
  ESSM_TableI_2RQs <- read.csv(paste0(ESSMRefDir, "ESSM_TableI_2RQs.csv"), 
                               stringsAsFactors=FALSE)
  # View(ESSM_TableI_2RQs)
  ESSM_TableJ_3RQs <- read.csv(paste0(ESSMRefDir, "ESSM_TableJ_3RQs.csv"), 
                               stringsAsFactors=FALSE)
  # View(ESSM_TableJ_3RQs)
  ESSM_TableK_4RQs <- read.csv(paste0(ESSMRefDir, "ESSM_TableK_4RQs.csv"), 
                               stringsAsFactors=FALSE)
  # View(ESSM_TableK_4RQs)
  ESSM_TableL_Subtotals <- read.csv(paste0(ESSMRefDir, 
                                           "ESSM_TableL_Subtotals.csv"), 
                                    stringsAsFactors=FALSE)
  # View(ESSM_TableL_Subtotals)
}


######## define some functions to parse the ESS-M results ########

ESSMBayesFn <- function(prior, likelihood) {
  # calculate Bayes Theorem for a prior and lielihood statistic
  (prior * likelihood) / ((prior * likelihood) + (1-prior * 1-likelihood))
}

# clopperPearsonFn
# source('~/Dropbox/R/NCCA_ASCII_Parse/ClopperPearsonBinomialCI.R')
clopperPearsonFn <- function(p=.8697865, n=75, a=.05) {
  # Clopper-Pearson confidence interval
  ###
  # degrees of freedom
  # truncated degrees of freedom give the same result as Excel
  v1=trunc(2*(n*(1-p)+1),0)
  v2=trunc(2*n*p,0)
  v3=trunc(2*(n*p+1),0)
  v4=trunc(2*n*(1-p),0)
  # f statistics for lower and upper limit
  FLow <- qf((1-(a)),v1,v2)
  FUp <- qf((a),v3,v4)
  # calculate the limits
  LL <- (1+FLow*((1-p)+1/n)/p)^-1
  UL <- (1 + (1-p) / (1 / n + p) * FUp)^-1
  # output and compare the lower limit to the prior
  c(lowerLimit=LL, upperLimit=UL)
}

pToOddsFn <- function(p=.5) {
  # convert a decimal probability to odds
  p / (1-p)
}

oddsToPFn <- function(odds=1) {
  # convert an odds to a decimal probability
  odds / (1+odds)
}

correctedOddsFn <- function(odds=1, n=3, val=-1) {
  # multiplicity correction for posterior odds 
  # val= 1 or -1 to calculate for deflation or inflation
  exp(log(odds)/n^val)
} 
# correctedOddsFn(odds=3.93, n=3, val=1)

ESSMLookupFn <- function(x=score, type="GT", nRQ=3, PLE=TRUE) {
  # function to lookup a likelihood statistic for an ESS Score
  
}

ESSMTableFn <- function(nRQ=3, prior=.5, aT=.05, aD=.05, PLE=TRUE) {
  # dynamically re-calculate an ESS-M table for a prior and alpha
  # requires other functions (ESSMBayesFn, clopperPearsonFn, pToOddsFn)
  # also requires ESS-M base tables
  ###
  # get the base tables
  if(isTRUE(PLE)) {
    lookupTable <- switch(as.character(nRQ),
                          "2"=ESSM_TableI_2RQs,
                          "3"=ESSM_TableJ_3RQs,
                          "4"=ESSM_TableK_4RQs)
  } else {
    lookupTable <- switch(as.character(nRQ),
                          "2"=ESSM_TableI_2RQs,
                          "3"=ESSM_TableJ_3RQs,
                          "4"=ESSM_TableK_4RQs)
  }
  lookupTableSub <- ESSM_TableL_Subtotals
  #
  
}



ESSMCutscoreFn <- function(prior=.5, aT=.05, aD=.05, nRQ=3, PLE=TRUE) {
  # calculate ESS-M cutscores
  
}

#### ESS-M decision rules compare the CI a/2 lower limit to the prior 

# 

TSRFn <- function(totalScore,
                  minSubtotalScore,
                  nRQ,
                  
                  postP=postP, 
                  postPSub=postPSub, 
                  priorP=priorP,
                  aT=.05,
                  aD=.05,
                  nRQ=3,
                  PLE=TRUE) {
  # two-stage rules (Senter)
  # for grand total and subtotal scores
  # uses p instead of odds because of more convenient directionality
  # requires a function to calculate the Clopper-Pearson interval
  # use a statistical correction for subtotals for DR/SR classification 
  ###
  # calculate the size
  n <- ifelse(isTRUE(PLE), 25, 20)
  N <- nRQ * n
  # get the lower limits 
  LL <- clopperPearsonFn(p=postP, n=N, a=aT)[1]
  LLSub <- clopperPearsonFn(p=postPSub, n=nRQ, a=aD)[1] 
  
  if(postOddsGT > priorOdds) {}
  
  
  
  catResult <- 
    
    correctedOddsFn(odds=postOddsST, n=3, val=-1)
  
  
  if()
    
}

SSRFn <- function(postOddsST=postOddsST, 
                  priorOdds=priorOdds,
                  n=3) {
  # use a statistical correction for subtotals for DI/SR classification 
  
  
}

GTRFn <- function(postOddsST=postOddsST, 
                  priorOdds=priorOdds) {
  # no statistical correction
  
  
}











# FZRFn
#
# UT4Fn
#
# TESFn

# pairwiseRQFn # pairwise comparison of RQ scores
source('~/Dropbox/R/NCCA_ASCII_Parse/autoSelectTSRSSR.R')


######################## ESS-M numerical Cutscores #######################

{
  priorProb <- .5
  priorOdds <- priorProb / (1 - priorProb)
  
  ESSMCutscores <- as.data.frame(matrix(ncol=6, nrow=3))
  row.names(ESSMCutscores) <- c("RQ2", "RQ3", "RQ4")
  names(ESSMCutscores) <- c("GTDI", 
                            "GTNDI", 
                            "STDI", 
                            "STNDI", 
                            "STDIc", 
                            "STNDIc" )
  # View(ESSMCutscores)
  
  zero2 <- which(ESSM_TableI_2RQs$score == 0)
  zero3 <- which(ESSM_TableJ_3RQs$score == 0)
  zero4 <- which(ESSM_TableK_4RQs$score == 0)
  
  cutRowD <- max(which(ESSM_TableI_2RQs$oddsLL05[1:zero2] > priorOdds))
  cutScoreD <- ESSM_TableI_2RQs$score[1:zero2][cutRowD]
  
  cutRowT <- 
    min(which(ESSM_TableI_2RQs$oddsLL05[zero2:nrow(ESSM_TableI_2RQs)] > 
                priorOdds))
  cutScoreT <- 
    ESSM_TableI_2RQs$score[zero2:nrow(ESSM_TableI_2RQs)][cutRowT]
  
  #### hard-coded cutscores instead of the dynamic lookup.
  
  
  
  ESSMCutscores$GTDI <- -3
  ESSMCutscores$GTNDI <- 3
  ESSMCutscores$STDI <- -3
  ESSMCutscores$STNDI <- 3
  ESSMCutscores$STDIc <- c(-5, -7, -9)
  ESSMCutscores$STNDIc <- c(2, 1, 1)
  
  
}


ESSMCutscoreFn <- function(prior=.5, aT=.05, aD=.05, nRQ=3, PLE=TRUE) {
  # calculate ESS-M cutscores
  
}


# pairwiseRQFn # pairwise comparison of RQ scores
source('~/Dropbox/R/NCCA_ASCII_Parse/autoSelectTSRSSR.R')






