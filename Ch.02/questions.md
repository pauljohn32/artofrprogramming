# Ch.02
How do you find out how many elements are in an R vector?

Did you mean length()?


Do I need to run "vector("double", 10)" or "double(10)". Why do they have 2 ways to create a real valued vector?

Which of the following is not a vector? a. array b. matrix c. data.frame c. data.frame

Running the code below will result in what value of *x*?
```{r}
l <- list()
for (i in 1:length(l)) {
  x <- i
}
x
```

Given the code below, what would you expect the output to be?
```{r}
x <- 1:10
foo <- function(x) x %% 2 == 0
any(foo(x))
```

Examine the code allow. What do you expect the returned value to be?
```{r}
x <- 1:10
foo <- function(x) x %% 2 == 0
all(foo(x))
```

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

How many data types may be stored in a vector?
What does R do when applying an operation to two vectors that requires them to be the same length?
How would you extract indices 3, 4, and 7 from vector x?
How would you write the equivalent of for(i in 1:length(x)) using seq()?
What does it mean for a function to be vectorized?


