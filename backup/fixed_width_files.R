cNames <- read.fwf("PF090316_CleanedCopy_1_01A_data.txt", 
                               c(6, 9, 9, 11, 11, 11, 11, 11, 11, 11), 
                               header = FALSE, 
                               skip = 0,
                               n = 1)
                  
testDF <- read.fwf(textConnection(chart1data, open = "r"), 
                   c(6, 9, 9, 11, 11, 11, 11, 11, 11, 11), 
                   header = FALSE, 
                   skip = 1,
                   col.names = as.vector(str_trim(t(cNames), side = "both")),
                   n = 10)

rName <- "chart1data"
read.fwf(textConnection(get(rName), open = "r"), 
         c(6, 9, 9, 11, 11, 11, 11, 11, 11, 11), 
         header = FALSE, 
         skip = 1,
         col.names = as.vector(str_trim(t(cNames), side = "both")),
         n = 10)