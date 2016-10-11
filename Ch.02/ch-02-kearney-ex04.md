Examine the code allow. What do you expect the returned value to be?
```{r}
x <- 1:10
foo <- function(x) x %% 2 == 0
all(foo(x))
```
