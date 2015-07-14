# R script to create .csv files from NCCA ASCII output.
# 
# require first running the parseFilesCSV.r script, which will source this script
# 
#################################
#
# change the format of the data file to .csv
# 
# start by trimming leading and trailing spaces from all rows in the character vector "NCCAASCIIdata"
NCCAASCIIdata <- str_trim(NCCAASCIIdata, side = "both")
# first fix the -9.9 filler
NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "-9.9", "9.9")
# then remove dash characters
# uses "ZZZ" as a placeholder so the field is not lost
# when the spaces are substituted with commas at a later stage
NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "--------", "ZZZ") #8
NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "-------", "ZZZ") #7
NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "------", "ZZZ") #6
NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "-----", "ZZZ") #5
NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "----", "ZZZ") #4
NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "---", "ZZZ") #3
NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "--", "ZZZ") #2
NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "-", "ZZZ") #1
# remove excess spaces
NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "        ", " ") #8
NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "       ", " ") #7
NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "      ", " ") #6
NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "     ", " ") #5
NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "    ", " ") #4
NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "   ", " ") #3
NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "  ", " ") #2
# replace all spaces with comma
NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, " ", ",")
# replace the ZZZ placeholders
NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "ZZZ", "")
# save the result
cat(NCCAASCIIdata, file = "trimdata.txt", sep = "\n")
# set the name of the data file
NCCAASCIIdataCSV <- paste(fileName, "_data.csv", sep = "")
# finally save the data file
cat(NCCAASCIIdata, file = NCCAASCIIdataCSV, sep = "\n")
#
# read the .csv 
# dataCSV <- read.csv("D&-20131002 DLSC YANGO ARCIA-1.01A_data.csv", nrows = -1)
# dataCSVnames <- names(dataCSV)
# dataCSVnames


