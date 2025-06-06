# secondsVC <- round(seq(from=0.0, to=190, by = .033), 2)



toMinSecFn <- function(x=secondsVc, vct=TRUE) {
  # format time from seconds 000.000 to mm:ss.00
  # input is a vector of seconds with 2 decimals
  # modified May 10, 2020
  # output is a vector of time values 
  # in the format minutes and seconds (mm:ss.00)
  # modified Mar 8, 2021
  # to include only the last value in the output
  # vct=TRUE will output a vector
  # vct=FALSE WILL output the last value of the time vector
  
  if(!exists("vct")) vct <- TRUE
  
  # x <- x[!is.na(x)]
  
  x <- as.numeric(x)
  
  outputTime <- rep(NA, times=length(x))
  
  # get the minutes
  xMin <- str_pad(trunc(x/60), width=2, side="left", pad="0")
  
  # use the modulus operator to get the seconds
  xSec <- str_sub((str_pad(trunc(x) %% 60, width=2, side="left", pad="0")), 1, 2)
  
  # use the modulus again to get sec/100 to 2 decimals and format it
  xSec100 <- as.character(format(round(x %% 1, 2), trim=TRUE))
  
  xSec100s <- str_sub(sub("^(-?)0.", "\\1.", sprintf("%s", xSec100)), -3, -1)
  
  # iterate and concatenate the minutes and seconds into a character vector
  for(i in 1:length(outputTime)) {
    outputTime[i] <- paste0(xMin[i], ":", xSec[i], xSec100s[i])
  }
  
  if(isTRUE(vct)){
    return(outputTime)
  } else {
    return(outputTime[length(outputTime)])
  }
  
  # tail(outputTime)
  
}

# head(toMinSecFn(secondsVC))
# tail(toMinSecFn(secondsVC))

fromMinSecFn <- function(x=chartDF$Time, vct=TRUE) {
  # calculate the number of seconds from MM:SS.00 input
  # input is a character vector of NCCA ASCII time increments
  # transformed to 000.00 numeric output
  # indicating the number of seconds rounded to 2 decimals
  # may 10, 2020
  
  # convert the minutes to seconds
  xMinSec <- as.numeric(str_sub(x, -8, -7)) * 60
  
  # get the seconds
  xSec <- as.numeric(str_sub(x, -5, -4))
  
  # keep the decimal with the sec proportion 
  xSecD <- as.numeric(str_sub(x, -3, -1))
  
  outputTime <- xMinSec + xSec + xSecD
  
  if(isTRUE(vct)){
    return(outputTime)
  } else {
    if(length(x > 1)) {
      return( outputTime[length(outputTime)] - outputTime[1] ) 
    } else {
      return(outputTime)
    }
  }
  
  # return(outputTime)
  
}

# fromMinSecFn()

# heaad(toMinSecFn(fromMinSecFn()))
# tail(toMinSecFn(fromMinSecFn()))

# timeScale <- toMinSec(secondsVC)
# str(secondsVC)
# str(timeScale)
