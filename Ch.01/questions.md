# Ch.01
Given the code below, what is the final value of *x*?
```{r}
x <- 5L
super.assigner <- function(n = 8) {
  x <<- 10L * n
}
super.assigner(x)
x
```

What about now? What would you expect *x* to be?
```{r}
x <- 5
super.assigner <- function(n = 8) {
  n * 1
}
x <- super.assigner()
x
```

A student would like to use the `rnorm()` function. Provide code for at least two ways the student can seek help within the R environment.

What is the scope of a variable defined outside of a function?
What is the scope of a variable defined within a function?
Name three types of data structures in R.
What function would you use to find your current directory?


