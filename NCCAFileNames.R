library(stringr)



NCCAASCIIFileNames <- list.files(path = ".", 
                                  pattern = "^D&+", 
                                  all.files = FALSE,
                                  full.names = FALSE, 
                                  recursive = FALSE,
                                  ignore.case = FALSE, 
                                  include.dirs = FALSE,
                                  no.. = FALSE)


uniqueExams <- unique(str_sub(NCCAASCIIFileNames, 4, -7))


