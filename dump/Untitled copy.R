
mySegmentLists <- ls(pattern="*_dataSegmentList$")
myEventLists <- ls(pattern="*_eventList$")

# mySegmentLists <- "PF090316_1.01A_dataSegmentList_backup"

# mySegmentLists <- "PF090316_1.01A_dataSegmentList"

dataSegmentList_test <- "PF090316_1.01A_dataSegmentList[2]"
# mySegmentLists <- "dataSegmentList_test"

mySegmentLists <- mySegmentLists[2]
myEventLists <- myEventLists[2]

myCardioData <- PF090316_1_Charts[[1]][,c(1:6,10)]

