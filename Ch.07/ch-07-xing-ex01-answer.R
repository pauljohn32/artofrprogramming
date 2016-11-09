##---------------------##
## Chong Xing 11092016 ##
## ch-07-johnson-ex01  ##
## Answer              ##
##---------------------##

## a) A function for receiving x and returning positive xlog values
##    and 0 for the other ones

# Step 1: A function for calculating xlog
posReturn <- function(x) {
    xlog <- log(x)
    xlog
}

x <- rnorm(100)
posReturn(x)

# Step 2: Adding an argument for changing NaN to 0
posReturn <- function(x) {
    xlog <- log(x)
    xlog <- ifelse(xlog == "NaN", 0, xlog)
    xlog
}

x <- rnorm(100)
posReturn(x)

# Step 3: Adding an argument for changing negative xlog to 0
posReturn <- function(x) {
    xlog <- log(x)
    xlog <- ifelse(xlog == "NaN", 0, xlog)
    xlog <- ifelse(xlog <= 0, 0, xlog)
    xlog
}

x <- rnorm(100)
posReturn(x)

## b) A function for assigning alternative values for the negative values
posReturnNegReplace <- function(x, negReplace) {
    xlog <- log(x)
    xlog <- ifelse(xlog == "NaN", 0, xlog)
    xlog <- ifelse(xlog <= 0, negReplace, xlog)
    xlog
}

x <- rnorm(100)
posReturnNegReplace(x, -9999)

## c) Stress testing both functions with missing values

x[sample(1:100, 20)] <- NA
x

posReturn(x)
posReturnNegReplace(x, -9999)
