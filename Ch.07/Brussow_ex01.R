x <- rnorm(100)
xlog <- log(x)

log_x <- function(input, missing = 0){
  output <- log(input)
  output[is.nan(output)] <- missing
}
