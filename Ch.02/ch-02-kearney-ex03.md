Given the code below, what would you expect the output to be?
```{r}
x <- 1:10
foo <- function(x) x %% 2 == 0
any(foo(x))
```
