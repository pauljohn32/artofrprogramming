# Getting started

## Introduction to functions

Running the code below will result in what value of *x*?

```{r}
l <- list()
for (i in 1:length(l)) {
  x <- i
}
x
```

> 0

Given the code below, what is the final value of *x*?


```{r}
x <- 5L
super.assigner <- function(n = 8) {
  x <<- 10L * n
}
super.assigner(x)
x
```

> 50

What about now? What would you expect *x* to be?

```{r}
x <- 5
super.assigner <- function(n = 8) {
  n * 1
}
x <- super.assigner()
x
```

> 8



## Getting help

A student would like to use the `rnorm()` function.
Provide at least two ways the student can seek help within the R environment.

> ?rnorm

> help(rnorm)

> example(rnorm)

> help.search("rnorm")

