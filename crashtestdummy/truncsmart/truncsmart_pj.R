
##' Cuts a string at a specified linewidth, trying to align cut with a
##' separator
##'
##' Some strings are simply too long. We don't want to chop them
##' exactly at, say, 40 characters, if we could allow 42 and chop on a
##' space or other separator. We'd rather chop at 37 if there is a
##' separator, rather than terminate a word exactly at 40. This function
##' shortens them and attempt to cut at a separator, allowing for a
##' user specified fudge-factor (the tol parameter).
##'
##' The default capwidth value is 1, so the calculations treat all
##' letters equally. In practice, we notice trouble when some strings
##' are written in ALL CAPS and they are longer than the same
##' information in lower case letters.  We have decided to allow a
##' user-specified penalty for capital letters. If each capital
##' counts for, say 1.2 ordinary letters, then we may end up truncating
##' the string on an earlier separator.
##'
##' There's some approximation here. The capital-penalized widths are
##' calculated for all characters and then we left-shift the target
##' value so that it is equal to the last penalized value that is
##' under the target length. Then the "look to the left" and "look
##' to the right" logic begins.  That looking logic ignores the
##' capital letter penalty, it is treating all letters the same. 
##' 
##' @param x character or vector of characters
##' @param target Goal for result length, in characters
##' @param tol number of characters forward/back to check; if single value then only backwards checking
##' @param separators characters at which truncation is preferred, such as space or underscore.
##' @param capwidth penalty for capital characters
##' @return shorter string truncated at acceptable separators when found
##' @author Paul Johnson
##' @examples
##' x <- "Aasdf asdIasdf fW_asd asd aasjdf_as fasdasdfasdfasd"
##' truncsmart(x, target = 10, tol = c(5, 2))
##' truncsmart(x, target = 10, tol = c(1, 4))
##' truncsmart(x, target = 10, tol = c(5, 2), capwidth = 1.2)
##' truncsmart(x, target = 20, tol = c(5, 2))
##' truncsmart(x, target = 20, tol = c(10,10), capwidth = 2)
##' truncsmart(x, target = 20, tol = c(10,10), capwidth = 3)
##' truncsmart(x, target = 20, tol = c(10,10), capwidth = 4)
##' truncsmart(x, target = 20, tol = c(10,10), capwidth = 6)
truncsmart <- function(x, target = 20, tol = c(5, 3), separators = c(" ", "_", ";", ","), capwidth = 1.0){
    ## How many steps forward does the first TRUE occur?
    ## Subtract 1 so that return is the position of the one before the
    ## first separator.
    nextTrue <- function(y) {
        which(cumsum(y) > 0)[1] #- 1 Removing this made the function
                                #find the limit correctly
    }
    ## Start at the end and count backwards till there is a TRUE.
    ## Then go one more so we omit the separator itself. Returns the
    ## "steps backward" to land on the last value we want to keep in the truncated
    ## string.
    previousTrue <- function(y) {
        which(cumsum(rev(y)) > 0)[1] #+ 1 Removing this made the function
                                #find the limit correctly
    }
    
    xchars <- unlist(strsplit(x, ""))
    ## cumulative length after applying penalty for capital width.
    xcharwidth <- cumsum(ifelse(sapply(xchars,
                                       function(x) x %in% LETTERS), capwidth, 1))

    ## Recalibrate target value. new target that is in vicinity of
    ## capital adjusted length
    target.orig <- target
    target <- max(which(xcharwidth <= target)) ## Ben Kite added the equal sign on 8/19

    ## Input is under the target, so return the original thing
    if (xcharwidth[length(xcharwidth)] <= target) return(x)

    xseps <- xchars %in% separators
    ## If target is separator, we are finished
    if(xseps[target]) return(paste0(xchars[1:(target -1)], collapse = ""))

    ## Otherwise have to look left and right, so check tol input
    if (length(tol) > 2){
        stop("tol must not have more than 2 values")
    } else {
        if (length(tol) < 2){
            tol <- c(tol, 0)
        }
    }

    ## How many to left and right do I need to go?
    ## previousTrue should return number of steps to left to find last character
    ## we keep.  nextTrue should return number of steps to right to find last one to keep.
    ## Have to be sure ranges stay in bounds
    ## for xseps. A little tricky if target is low or very near end of sequence
    steps <- c(previousTrue(xseps[max(1, (target - tol[1])): (target - 1)] ),
               nextTrue(xseps[(target + 1): min(length(xseps), (target + tol[2] + 1))]))
    
    ## Get the smallest number of steps and use that.
    ## which.min breaks ties how? Chooses the first, the left side, which we like.
    direction <- which.min(steps)

    ## no safe trunc exists, just chop at target.
    if (all(is.na(direction))) return (paste0(xchars[1:target], collapse = ""))
    
    ## steps[1] is backwards, so multiply by -1
    if (direction == 1) steps[1] <- -1 * steps[1]
    ## Calculate result at proper truncation.
    paste0(xchars[1:(target + steps[direction] - 1)], collapse = "")
}
