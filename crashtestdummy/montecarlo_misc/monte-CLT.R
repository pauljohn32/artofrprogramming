##Paul Johnson <pauljohn@ku.edu>
## 2010-06-15

## Quick simulation to demonstrate central limit theorem.
## Uses "replicate"

## Writes 2 files if SAVEPLOTS == TRUE, otherwise
## shows on screen

set.seed(10000)
simmean <- function(N=100, mean = 7, std = 10){
  x <- rnorm(N, m = mean, sd = std)
  mean(x)
}



## In a simple job like this, we can let replicate do
## all of the work
mymeans1 <- replicate(10000, simmean(100, 13, 23 ))


## Explore that with some histograms
rangex <- range(mymeans1)

hist(mymeans1, breaks=20,
     xlab="Means drawn from N(12,23) with sample=100", prob=TRUE,
     xlim=rangex)

dev.new()
mymeans2 <- replicate(10000, simmean( 1000, 13, 23 ))

hist(mymeans2, breaks=20,
     xlab = "Means drawn from N(13,23) with sample=1000", prob = TRUE,
     xlim = rangex)

lines(density(mymeans2), lty = 2,col = "red", lwd = 3)


plotPDF <- function(a = NULL, b = NULL){
    x <- seq(0.001, 0.999, length=500)
    xpdf <- dbeta(x, a, b) #x probability density
    bev <- round(a/(a+b), 3) #expected value
    bvar <- round(a*b/((a+b)^2 * (a+b+1)), 3) #variance
    plot(x, xpdf, type="l", ylab="Probability Density",
         ylim=c(0,1.2*(max(xpdf))), main="")
    polygon(c(0,x,1,1),c(0,xpdf,max(xpdf),0), col="gray96")
    l0 <- bquote(x ~ Beta(.(a), .(b)))
    l1 <- bquote(E[x] == .(bev))
    l2 <- bquote(Var[x] == .(bvar))
    legend("topleft", legend=c(l0,l1,l2))
}

plotSimMeans <- function(mymeans, a, b){
    den1 <- density(mymeans)

    hist(mymeans, prob=T, ylim=c(0, 1.3*max(den1$y)), col="gray98",
         xlab=expression("Estimated means"), main="")

    x2 <- range(mymeans)
    x2seq <-  seq(x2[1],x2[2], length.out=100)

    bev <- round(a/(a+b), 3) #expected value
    bvar <- round(a*b/((a+b)^2 * (a+b+1)), 3) #variance
    ##> bev
    ##[1] 0.5566802
    ##> bvar
    ##[1] 0.08292586

    ## get pdf from Normal with EV and Variance set to observed values
    pdfnorm <- dnorm(x2seq, mean=bev, sd = sqrt(bvar)/sqrt(500))

    lines(x2seq, pdfnorm, lty=1)
    lines(den1$x, den1$y, lty=2)
    legend("topleft", c("Theoretical Density (CLT)","Observed Density"),
           lty=c(1,2))
}

## Now consider the beta distribution (1.1, 0.876)

a <- 1.1
b <- 0.876



SAVEPLOTS <- FALSE
if(SAVEPLOTS == TRUE ){
    fileNameBase <- paste("beta-",a,"-",b,"-%03d.pdf", sep="")
    pdf(file=fileNameBase, width=6, height=6, onefile = FALSE,
        family="Times", pointsize=12, paper="special")
}
## else: show plots on screen

plotPDF(a, b)

## Draw 10000 samples, size 500, get mean
mybetas <- replicate(10000, mean(rbeta(500, a, b)))


mean(mybetas)
var(mybetas)

plotSimMeans(mybetas, a, b)

if(SAVEPLOTS == TRUE ){
    dev.off()
}
