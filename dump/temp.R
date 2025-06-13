# this script with removed whitespace from the beginning of wrapped text lines
# will unwrap the lines and 
library(stringr)
v1 <- c("This is a line of text. ", "This is another line.", "      And another, with whitespace. ", "And yet another line of text.")
v3 <- NULL
v2 <- NULL
# v1 <- c(v1, "Another line.", "      And another line of text.", "And one more line of text.", "      Before we end the lines of text.", "With one more line.")
for (i in 1:length(v1)) {
  if (strtrim(v1[i], 6) == "      ") v3 <- c(v3, paste(v1[i -1], str_sub(v1[i], 7, nchar(v1[i])), sep = " "))
  if (strtrim(v1[i], 6) != "      ") v3 <- c(v3, v1[i])
}
print(v3)