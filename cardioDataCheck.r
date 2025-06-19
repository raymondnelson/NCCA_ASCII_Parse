# wrapper function to check for unresponsive cardio data
# 4-28-2017
# Raymond Nelson
#
#########################

pneumoDataCheckfn <- function(xUP=chartDF$c_UPneumo, 
											 xLP=chartDF$c_LPneumo
                       sec=15, 
                       times=20, 
                       omit=0, 
                       firstRow=NULL, 
                       lastRow=NULL, 
                       sVal=200,
                       columnName="UPneumoUnresponse_a",
                       output="dataframe") {
                       
                       
	# check the upper pneumo data
	outputDF <- dataCheckFn(x=x, 
												 sec=sec, 
												 times=times, 
												 omit=omit, 
												 firstRow=firstRow, 
												 lastRow=lastRow, 
												 sVal=sVal,
												 columnName=columnName,
												 output=output)
	        
	return(outputDF)

} # end 