According to the chapter, which of the functions below is best? **Extra credit**: Which of the above functions is fastest?
```{r}
a <- function(x) {
  k <- 0
  for (n in x) {
    if (n %% 2 == 1) k <- k + 1
  }
  k
}

b <- function(x) {
  k <- 0
  for (i in 1:length(x)) {
    if (x[i] %% 2 == 1) k <- k + 1
  }
  k
}

c <- function(x) {
  sum(x %% 2 == 1)
}
```
