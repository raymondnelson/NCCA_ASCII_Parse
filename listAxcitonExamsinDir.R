examNames <- unique(str_sub(list.files(), 1, -5))

DAT <- cbind.data.frame(examName=examNames, criterionState=1)

write_csv(DAT, "criterionState_NDI.csv")
