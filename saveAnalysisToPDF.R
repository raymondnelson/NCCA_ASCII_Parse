# R script to save .txt analysis output to .pdf
# July 29, 2020
# Raymond Nelson

####



require(rmarkdown)
require(stringr)



# initialize a function
txtToPDF <- function(x=analysisOutputFiles) {
  # render a .txt file to a .pdf document
  # requires RMarkdown
  # also requires stringr package
  # x can be a vector of text file names
  i=1
  for (i in 1:length(x)) {
    thisName <- str_sub(x[i], 1, -5)
    thisText <- readLines(x[i])
    # append some characters to force the text to fixed width font
    thisText <- c("```", thisText,"```")
    # save the vector as an R Markdown file
    markDownName <- paste0(thisName, " .Rmd")
    cat(thisText, sep=" \n", file=markDownName)
    # render the pdf 
    render(markDownName, pdf_document())
    # clean up
    file.remove(markDownName)
  }
  outputMsg <- 
    paste("rendered", length(x), "analysis files  to pdf")
  return(outputMsg)
}



