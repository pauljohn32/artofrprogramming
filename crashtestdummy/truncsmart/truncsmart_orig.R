##' Cuts a string at a specified linewidth, trying to align cut with a separator
##' 
##' Some strings are simply too long. Let's shorten them and attempt to cut at a
##' specified separator. Use vectorized function, shortenv, to apply to non-scalar
##' @param textstring target string to shorten
##' @param linewidth approximate length to shorten taking into account tolerance
##' @param tol number of characters forward/back to check; if single value then only backwards checking
##' @param capwidth integer specifying the width of capital letters
##' @param separator accepts 1- or 2-element vector to separate string; default = c(" ", "_")
##' @return shortened character string
##' @author Brent Kaplan, Ben Kite, Paul Johnson
##' @examples
##' test <- "123_567_9"
##' (truncsmart(test, 5, tol = c(1,2)))
##'
truncsmart <- function(textstring, linewidth, tol = c(5, 5), capwidth = 1.2, separator = c(" ", "_")) {
    if(length(tol) > 2) stop("Please specify 1 or 2 values for tol.")
    lets <- unlist(strsplit(textstring, split = ""))
    caps <- sapply(lets, function(x) x %in% LETTERS)
    widthval <- ifelse(caps == TRUE, capwidth, 1)
    linelength <- cumsum(widthval)
    if(linelength[length(linelength)] <= linewidth) return(textstring)
    withinlength <- linelength  <= linewidth
    ifelse(length(tol) == 2, withintol <- linelength <= (linewidth + tol[2]),
           withintol <- linelength <= (linewidth + tol))
    letswl <- lets[withinlength]
    letso <- letswl
    letswt <- lets[withintol]
    if(length(tol) == 2) {
        for (j in 0:tol[1]){
            if(identical(letswl[length(letswl)], separator[1]) ||
               identical(letswl[length(letswl)], separator[2])) {
                letso1 <- letswl[-length(letswl)]
                break()
            }
            letswl <- letswl[-length(letswl)]
        }
        for (j in 0:tol[2]) {
            if(identical(letswt[length(letswt)], separator[1]) ||
               identical(letswt[length(letswt)], separator[2])) {
                letso2 <- letswt[-length(letswt)]
                break()
            }
            letswt <- letswt[-length(letswt)]
        }
        ifelse(!exists("letso1") && !exists("letso2"), letso,
        ifelse(exists("letso1") && !exists("letso2"), letso <- letso1,
        ifelse(!exists("letso1") && exists("letso2"), letso <- letso2,
        ifelse(abs((length(letso1) - length(lets[withinlength]))) <= (length(letso2) - length(lets[withintol])), letso <- letso1, letso <- letso2))))
    } else {
        for (j in 0:tol){
            if(identical(letswl[length(letswl)], separator[1]) ||
               identical(letswl[length(letswl)], separator[2])) {
                letso <- letswl[-length(letswl)]
                 break()
            }
            letswl <- letswl[-length(letswl)]
        }
    }
    out <- paste0(letso, collapse  = "")
    out
}

truncsmart <- Vectorize(truncsmart, USE.NAMES=FALSE)

