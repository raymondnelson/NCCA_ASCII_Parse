nRQ <- 20 # number of relevant questions
nCQ <- NULL # number of comparision questions
aCQ <-3 # number of artifacts at comparison questions
aRQ <- 0 # number of artifacts at relevant questions
alpha <- .05 # probabilty cut-score

testOfProportions <- function(nCQ = nCQ, nRQ = nRQ, aCQ = aCQ, aRQ = aRQ, alpha = .05) {
    # function to calculate the the test of proportions
    # 
    # nCQ <- 9 # number of comparision questions
    # nRQ <- 9 # number of relevant questions
    # aCQ <- 4 # number of artifacts at comparison questions
    # aRQ <- 2 # number of artifacts at relevant questions
    # alpha <- .05 # probabilty cut-score
    #
    # for ESS-M the number of CQs presentations
    # is always equal to the number of RQ presentations
    # this is an approximation that is necessary 
    # because test formats vary in th number of RQs and CQs
    # and examiners continue to make ad hoc modifications
    # for which the variety cannot be completely anticipated
    #
    ####
   
    if(is.null(nCQ)) nCQ <- nRQ
  
    # proportion of CQ artifacts
    CQprop <- aCQ / nCQ
    # proportion of RQ artifacts
    RQprop <- aRQ / nRQ
    # proportion of total artifacts
    totProp <-  (aCQ + aRQ) / (nCQ + nRQ)
    # ratio 
    prop <- (1-totProp) * totProp
    # prop / nCQ
    propCQ <- prop / nCQ
    # prop / nRQ
    propRQ <- prop / nRQ
    # calculate the denominator
    denom <- sqrt(abs(propCQ + propRQ))
    # calculate the numerator
    numer <- CQprop - RQprop
    # calculate the z value
    z <- numer / denom
    # step 10 compute the quantile
    pval <- pnorm(z, lower.tail = TRUE) # using normal approximation to the binomial distribution
    # result
    if(pval > .5) pval <- 1 - pval
    
    result <- ifelse(pval <= alpha, "Artifacts differ significantly from random.", "Artifacts are not signficantly different from random.")
    print(pval)
    return(result)
} # end testOfProportions


# testOfProportions(nCQ, nRQ, aCQ, aRQ, alpha)

# ############ vermont state police
# 
# # deceptive results
# nCQ <- 156 # number of DLST exams 2017
# nRQ <- 278 # number of RI exams 2013
# aCQ <- 11 # number of artifacts at comparison questions
# aRQ <- 4 # number of artifacts at relevant questions
# alpha <- .05 # probabilty cut-score
# testOfProportions(nCQ, nRQ, aCQ, aRQ, alpha)
# # [1] 0.001065497
# 
# # inconclusive results
# nCQ <- 156 # number of DLST exams 2017
# nRQ <- 278 # number of RI exams 2013
# aCQ <- 11 # number of artifacts at comparison questions
# aRQ <- 46 # number of artifacts at relevant questions
# alpha <- .05 # probabilty cut-score
# testOfProportions(nCQ, nRQ, aCQ, aRQ, alpha)
# # [1] 0.002475478
# 
# # truthful results
# nCQ <- 156 # number of DLST exams 2017
# nRQ <- 278 # number of RI exams 2013
# aCQ <- 73 # number of artifacts at comparison questions
# aRQ <- 81 # number of artifacts at relevant questions
# alpha <- .05 # probabilty cut-score
# testOfProportions(nCQ, nRQ, aCQ, aRQ, alpha)
# # [1] 0.0001124653
# 
# 
# # uninterpretable data quality
# nCQ <- 156 # number of DLST exams 2017
# nRQ <- 278 # number of RI exams 2013
# aCQ <- 31 # number of artifacts at comparison questions
# aRQ <- 62 # number of artifacts at relevant questions
# alpha <- .05 # probabilty cut-score
# testOfProportions(nCQ, nRQ, aCQ, aRQ, alpha)
# # [1] 0.2768977
# 
# 
# # conclusive results
# nCQ <- 156 # number of DLST exams 2017
# nRQ <- 278 # number of RI exams 2013
# aCQ <- 84 # number of artifacts at comparison questions
# aRQ <- 85 # number of artifacts at relevant questions
# alpha <- .05 # probabilty cut-score
# testOfProportions(nCQ, nRQ, aCQ, aRQ, alpha)
# # [1] 9.184842e-07
# 
# 
# 
# 
# 
# 
