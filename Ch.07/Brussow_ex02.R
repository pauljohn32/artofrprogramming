x <- c("douglas-kansas-2016-10-21", "johnson-kansas-2016-10-33", "sedgwick-kansas-2016-04-12") 

cleanup <- function(x){
  x <- gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(x), perl=TRUE)
  x <- gsub("Kansas", "KS", x)
  x <- strsplit(x, "-")
  x <- sapply(x, function(x){x <- paste(paste(y[2], y[1], sep = "-"), paste0(y[3:length(y)], collapse = ""), sep ="-")})
}

x2 <- cleanup(x)
