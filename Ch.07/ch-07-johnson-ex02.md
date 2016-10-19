I have strings like this:

```
x <- c("douglas-kansas-2016-10-21", "johnson-kansas-2016-10-33",
       "sedgwick-kansas-2016-04-12") 
```

But what I really need is

x2 <- c("KS-Douglas-20161021", "KS-Johnson-20161033",
       "KS-sedgwick-20160412")

Wrige a function that receives x and gets the job done.

Hints: 1) the R gsub function is for changing strings.
       2) If you get up to the point where your result is
          "Douglas-KS-20161021", you are very close to being
           finished.  The last bit may be too difficult if
           I don't give you another hint.  In the regular
           expressions, \2 \1 will help.  I'll show you 
           what I mean if you get to that point.

