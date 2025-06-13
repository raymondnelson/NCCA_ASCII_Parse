# ouput algorithm results to pdf

###

thisOutput <- unlist(DX0I6GD_X_OSS3OutputList)

library(gridExtra)
library(grid)
pdf(file=paste(examName, "OSS-3_Output.pdf", sep="_"), width=8.5, height=11)
print(thisOutput)
print(DX0I6GD_X_bootstrapOutputList)# dev.new()
dev.off()


