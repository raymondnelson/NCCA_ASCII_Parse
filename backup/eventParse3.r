#####################################
# R script to to process events
#
#
#
#
######################################
#
eventsDF1 <- read.csv("~/Documents/R programming/Recursion/D&-PF090316_Cleaned_Copy-1.01A_events.csv")
eventsDF2 <- read.csv("~/Documents/R programming/Recursion/D&-PF090316_Cleaned_Copy-1.02A_events.csv")
eventsDF3 <- read.csv("~/Documents/R programming/Recursion/D&-PF090316_Cleaned_Copy-1.03A_events.csv")
#
eventLists <- ls(pattern = "DF")
#
# make a vector of event tags for each chart
eventTags <- function(x = eventLists) {
  for (i in 1:length(eventLists)) {
    tempEventMatrix <- get(eventLists[i])
    tempEventTags <- as.vector(tempEventMatrix[,2])
    # tempVectorName <- paste("eventTags", i, sep = "")
    assign(paste("eventTags", i, sep = ""), tempEventTags, pos = 1)
  }
}
#
eventTags()
#
# get the max length of eventTags
eventTagVectorNames <- ls(pattern = "eventTags")

# function 
nrowEventLists <- function(x, xL) {
  # requires a vector name (x) for a list of event vectors, and
  # a the length of the vector of (x) of event vectors
  y <- NULL
  for (i in 1:length(x)) {
    y <- c(y, nrow(get(x[i], pos = 1)))
  }
  return(y)
}
maxEvents <- nrowEventLists("eventLists")
#
eventVectors <- function(x = "eventLists") {
  for (i in 1:length(x)) {
    
  }
}
#
fixEventLists <- function(x = "eventlists", y = maxEvents) {
  for (i  in 1:length(eventLists)) {
    assign(eventLists[i], c(eventLists[i], y - length(eventLists[i])))
  }
}
fixEventLists()
grep("R", eventsDF2)