#' rearrange string
#'
#' @param x string with format county-state-year-month-day
#'
#' @return rearrange string to ST-County-yearmonthday
#' @export
#'
#' @examples 
#' x <- c("douglas-kansas-2016-10-21", "johnson-kansas-2016-10-33", "sedgwick-kansas-2016-04-12")
#' x2 <- c("KS-Douglas-20161021", "KS-Johnson-20161033", "KS-sedgwick-20160412")
#' pjprob(x) 
pjprob <- function(x){
  require(stringi)
  x1 <- gsub("-", " ", x)
  x3 <- stri_trans_totitle(x1)
  df <- read.table(textConnection(x3), stringsAsFactors = FALSE)
  df$V2 <- state.abb[match(df$V2,state.name)]
  x5 <- paste0(df$V6, paste0(df$V3, df$V4, df$V5))
  x6 <- paste(paste(df$V2, df$V1, x5, sep = "-"))
  x6
}