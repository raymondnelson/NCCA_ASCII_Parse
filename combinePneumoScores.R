# refer to Nelson & Krapohl (2018)

combinePn <- function(x) {
  # x input is a vector of P1 and P1 scores
  # can be log(R/C), ESS, 3position or 7 position scores
  # as long as the scores have + and - sign value
  if(prod(x) < 0) return(0)
  x[which.max(abs(x))]
}




