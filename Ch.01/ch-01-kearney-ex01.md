Given the code below, what is the final value of *x*?
```{r}
x <- 5L
super.assigner <- function(n = 8) {
  x <<- 10L * n
}
super.assigner(x)
x
```
