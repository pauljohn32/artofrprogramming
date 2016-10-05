Running the code below will result in what value of *x*?

```{r}
l <- list()
for (i in 1:length(l)) {
  x <- i
}
x
```

*A: 0*

Given the code below, what is the final value of *x*?


```{r}
x <- 5L
super.assigner <- function(n = 8) {
  x <<- 10L * n
}
super.assigner(x)
x
```

*A: 50*

What about now? What would you expect *x* to be?

```{r}
x <- 5
super.assigner <- function(n = 8) {
  n * 1
}
x <- super.assigner()
x
```

*A: 8*

