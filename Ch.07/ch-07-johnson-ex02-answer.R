x <- c("douglas-kansas-2016-10-21", "johnson-kansas-2016-10-33",
       "sedgwick-kansas-2016-04-12", "cowboy-oklahoma-2016-11-14",
       "oklahoma-oklahoma-2016-01-01")

## Approaching this as a sting manipulation exercise, practicing
## usage of regular expressions.

## Here are the steps I would go through in testing

## 1. eliminate dashes in date
x1 <- gsub("([0-9]{4})-([0-9]{2})-([0-9]{2})$", "\\1\\2\\3", x)
## 2. Capitalize the district name with perl-style regular expression
## see ?sub. FYI: We found other capitalization functions in packages
## Hmisc and stringi
x2 <- sub("(\\w)", "\\U\\1", x1, perl = TRUE)
## 3. Swap state and district name
x3 <- sub("(.*)-(.*)-", "\\2\\-\\1-", x2)
## 4. Abbreviate the state name. Make sure this
## does not change counties which are named same as state.
library(kutils)
(x4 <- mgsub(paste0("^", tolower(state.name), "-"),
             paste0(state.abb,"-"), x3))


## Now write that as a function

##' Change values "district-State-YYYY-MM-DD" to "ST-Dist-YYYYMMDD"
##'
##' Uses regular expressions. Note this ONLY capitalizes the first
##' word of the district name.  If the district name is
##' "wilma_by_the_river" we would need to ask the what should be done.
##' @param x A character vector
##' @return Re-arranged character vector
##' @author Paul Johnson <pauljohn@@ku.edu>
##' @examples
##' x <- c("douglas-kansas-2016-10-21", "johnson-kansas-2016-10-33",
##'       "sedgwick-kansas-2016-04-12", "cowboy-oklahoma-2016-11-14",
##'       "oklahoma-oklahoma-2016-01-01")
##' fixer(x)
fixer <- function(x){
    y <- gsub("([0-9]{4})-([0-9]{2})-([0-9]{2})$", "\\1\\2\\3", x)
    y <- sub("(\\w)", "\\U\\1", y, perl = TRUE)
    y <- sub("(.*)-(.*)-", "\\2\\-\\1-", y)
    require(kutils)
    kutils::mgsub(paste0("^", tolower(state.name), "-"),
                  paste0(state.abb,"-"), x3)
}


    
