## Paul Johnson
## 20161012

I got pretty confused about how to best design functions.
Lets look at some mistakes, see what we can learn.

plot-drawHist-01.R is a function that draws a histogram
and provides some decoration. Its not very flexible. It
does have some "..." work in it.

plot-plotmath-03-Normal.R is as "script" (not a function) that makes a
nice looking drawing. plot-plotmath-04-diagnosis.R wraps that up and
adds some features.

plotDist_revised.R

Recently, I needed to use that code for a project and
I found it was not flexible enough. I ended up
revising those functions.

I see the flaw in my ways, I *think* I know how
to fix it. But lets see if you can find the flaws too.


The drawHist function in plotDist_revised is helpful for
discussing "..." management.  I can walk you through that.
