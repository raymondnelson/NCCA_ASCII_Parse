trimEventList <- function(List) {
  if("No" %in% List) stimulusEventList <- List[-match("No", List)]
  if("Yes" %in% List) List <- List[-match("Yes", stimulusEventList)]
  if("" %in% List) List <- List[-match("", stimulusEventList)]
  if("xx" %in% List) List <- List[-match("xx", stimulusEventList)]
  if("x" %in% List) List <- List[-match("x", stimulusEventList)]
  if("XX" %in% List) List <- List[-match("XX", stimulusEventList)]
  if("X" %in% List) List <- List[-match("X", stimulusEventList)]
}
