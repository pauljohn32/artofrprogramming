Create x and xlog like this:

```
x <- rnorm(100)
xlog <- log(x)
```

I don't want NaN, but you see there are some. Yes?

a) write a function that receives x and gives back the log for positive values of x and returns 0 for the other ones.

b) write a function that allows me to specify what alternative value
should be assigned for the negative values.

c) stress test both of your functions for presence of
missing values

```
x[sample(1:100, 20)] <- NA
```
