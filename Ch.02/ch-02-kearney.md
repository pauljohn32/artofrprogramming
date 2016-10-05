# Vectors

## 2.1

Which of the following is not a vector?

a. array

b. matrix

c. data.frame

> c. data.frame


## 2.4

Running the code below will result in what value of *x*?

```{r}
l <- list()
for (i in 1:length(l)) {
  x <- i
}
x
```

> 0


## 2.5

Given the code below, what would you expect the output to be?

```{r}
x <- 1:10
foo <- function(x) x %% 2 == 0
any(foo(x))
```

> TRUE

What about now? What will the be returned?

```{r}
x <- 1:10
foo <- function(x) x %% 2 == 0
all(foo(x))
```

> FALSE

## 2.6

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

According to the chapter, which of the above functions is best? **Extra credit**: Which of the above functions is fastest?


> c

> Extra credit: c

