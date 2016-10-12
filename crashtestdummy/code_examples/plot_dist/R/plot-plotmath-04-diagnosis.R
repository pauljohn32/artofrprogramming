## Filename: plotmath-diagnosis.R
## Paul Johnson June 25, 2010
## 20161008: Some cleanup because we needed to use this in a project.
### email me <pauljohn@ku.edu>
### DKJ wondered if I could create a figure like the one on Wikipedia.


##' Creates a plot similar to a diagnostic plot we found in Wikipedia
##'
##' Given user specified values, it draws a normal curve and marks
##' some regions
##' @param mu A single real-numbered value for the expected value
##' @param sigma A real-number for the standard deviation
##' @param file A character string for the file name of the output. If
##'     NULL, draw on screen
##' @param N Default FALSE: Show Normal-based diagnostic axis
##' @param Z Default TRUE: Show Z-score-based (standardized N)
##'     diagnostic axis
##' @param t Default FALSE: Show t-score based diagnostic axis
##' @return NULL
##' @author Paul Johnson <pauljohn@@ku.edu>
plot.diagnostic <- function(mu = 10.03, sigma = 12.57, file = NULL,
                            N = FALSE, Z = TRUE, t = FALSE){
    sigma <- round(sigma, 2)
    mu <- round(mu, 2)
    myx <- seq(mu - 3.5 * sigma,  mu + 3.5 * sigma, length.out = 500)
    myDensity <- dnorm(myx, mean = mu, sd = sigma)

    myTitle1 <- bquote (paste("x ~ Normal(", mu== .(round(mu,2)), ',', sigma== .(round(sigma,2)),")") )

    ## add half inch cor each desired axis
    height <- 4.5 + 0.5*N + 0.5*Z + 0.5*T
    if (!is.null(file)){
        pdf(file = file, height = height, width = 6.5,
            onefile = FALSE, paper = "special", pointsize=10)
    } else {
        ## dev.new(height = height, width=6.5,pointsize=9)
    }
    
    ## xpd needed to allow writing outside strict box of graph
    ## Expand margin below to make room for  each requested axis
    par(xpd=TRUE, ps=10, mar=c(4.5 + 4 *(0.25 + N + Z + T), 2, 3, 2))

    plot(myx, myDensity, type = "l", xlab = "", ylab = "Probability Density ",
         main = myTitle1, axes = FALSE)
    axis(2, pos = mu - 3.6*sigma)
    axis(1, pos = 0)
    lines(c(myx[1], myx[length(myx)]), c(0,0)) ### closes off axes

    ## A nested function
    addInteriorLine <- function(x, m, sd){
        for (i in 1:(length(x))){
            lines( c(x[i],x[i]), c(0, dnorm(x[i],m=m,sd=sd)), lty= 14, lwd=.2)
        }
    }

    dividers <- c(qnorm(0.025), -1, 0, 1, qnorm(0.975))
    addInteriorLine(mu + sigma * dividers, mu, sigma)

    addInteriorLabel <- function(pos1, pos2, m, s){
        area <- abs(100 * (pnorm(m + pos1 * s, m, s)- pnorm(m + pos2 * s, m, s)))
        mid <- m + 0.5 * (pos1 + pos2) * s
        text(mid, 0.5 * dnorm(mid, m, s), label = paste(round(area,2), "%"))
    }


    addInteriorLabel(dividers[1], dividers[2], mu, sigma)
    addInteriorLabel(dividers[2], dividers[3], mu, sigma)
    addInteriorLabel(dividers[3], dividers[4], mu, sigma)
    addInteriorLabel(dividers[4], dividers[5], mu, sigma)


    ## Create a formula for the Normal
    normalFormula <- expression (f(x) == frac (1, sigma* sqrt(2*pi)) * e^{~~ - ~~ frac(1,2)~~ bgroup("(", frac(x-mu,sigma),")")^2})
    ## Draw the Normal formula
    text(mu + 0.5 * sigma, max(myDensity)- 0.10 * max(myDensity),  normalFormula, pos=4) 


    criticalValue <- qnorm(0.025, mu, sigma)
    ## Then grab all myx smaller than that
    specialX <-  myx[myx <= criticalValue]

    ## mark the critical value in the graph
    ## mtext(paste(round(criticalValue, 2)), 1,  at = criticalValue, line=1.5)
    ## Take sequence parallel to values of myx inside critical region
    specialY <- myDensity[myx < criticalValue]
    ##  Polygon makes a nice shaded illustration
    polygon(c(specialX[1], specialX, specialX[length(specialX )]),
            c(0, specialY, 0), density=c(-110),
            col="gray97" )

    shadedArea <- round(pnorm(criticalValue, mean = mu, sd = sigma), 2)
    text(criticalValue, 0.25 * dnorm(criticalValue, mu, sigma), label = "2.5%",pos = 2)

    criticalValue <- qnorm(0.025, mu, sigma, lower.tail = FALSE)
    ## Then grab all myx smaller than that
    specialX <-  myx[myx >= criticalValue]

    ## mark the critical value in the graph
    ## text ( criticalValue, 0 , label= paste(round(criticalValue,2)), pos=1)
    ## Take sequence parallel to values of myx inside critical region
    specialY <- myDensity[myx > criticalValue]
                                        #  Polygon makes a nice shaded illustration
    polygon(c(specialX[1], specialX, specialX[length(specialX )]),
            c(0, specialY, 0), density = c(-110),
            col = "gray97" )
    
    shadedArea <- round(pnorm(criticalValue, mean = mu, sd = sigma),2)
    text(criticalValue, 0.25*dnorm(criticalValue, mu, sigma), label="2.5%",pos=4)

    b1 <- expression( mu - 1.96*sigma )
    b2 <- expression( mu - sigma )
    b3 <- expression( mu )
    b4 <- expression( mu + sigma )
    b5 <- expression( mu + 1.96*sigma )

    ## bquote creates an expression that text plotters can use
    t1 <- bquote(mu -1.96*sigma== .(eval(b1)))
    t3 <-  bquote(mu== .(mu))
    t5 <- bquote(mu +1.96*sigma== .(eval(b5)))
    
    mtext(as.expression(c(t1, t3, t5)), 1, at = c(eval(b1), eval(b3), eval(b5)), line=1.5)
      
    line = 3
    if (N) {
        axis(1, line= line, at=mu+dividers*sigma, labels=c(b1,b2,b3,b4,b5), padj=0)
        mtext("N", 1, line= line +0.25, at=mu-sigma*2.5,cex=1.5)
        line <- line + 3
    }
    if (Z) {
        axis(1, line= line, at=mu+dividers*sigma, labels=c(-1.96,-1,0,1,1.96), padj=-1)
        mtext("Z", 1, line= line +0.25, at=mu-sigma*2.5,cex=1.5)
        line <- line + 3
    }
    if (t) {
        tdividers <- qt(pnorm(dividers), df=100)
        axis(1, line = line, at=mu+sigma*tdividers, labels=round(tdividers,2), padj=-1)
        mtext("t", 1, line= line +0.25, at=mu-sigma*2.5, cex=1.5) 
    }
    
    if (!is.null(file))  dev.off()
}

plot.diagnostic(mu=3, sigma=7)
plot.diagnostic(mu=3, sigma=7, N = TRUE, t = TRUE)
plot.diagnostic(mu=600, sigma=120)


## 20160823
## This next part seems unnecessary to me, but here we go.


##' Draw a red dotted line for a given observed value
##'
##' This decorates the previously created output of plot.diagnostic
##' @param x The point at which the marker is to be inserted
##' @param mu The expected value
##' @param sigma The standard deviation
##' @return NULL
##' @author Paul Johnson <pauljohn@@ku.edu>
markValue <- function(x=0, mu=0, sigma=10) {
    normProb <- dnorm(x, m=mu, sd=sigma)
    lines( c(x,x), c(-0.025, normProb ), col="gray90",lty=1,lwd=10)
    lines( c(x,x), c(-0.025, normProb ), col="red",lty=2,lwd=2)
    S <- ifelse(x < mu, -1, 1)
    text (x+S*0.05*sigma, 1.1*normProb, label=round(x,2))
    arrows(S*1.01*x, 1.01*normProb, x+S*0.04*sigma, 0.97*1.1*normProb, code=1, length=0.1)
}

plot.diagnostic(700, 120)
markValue(600, mu = 700, sigma = 120)



##' Puts plot.diagnostic and markValue Together
##'
##' .. content for \details{} ..
##' @title 
##' @param x 
##' @param mu 
##' @param sigma 
##' @return 
##' @author Paul Johnson
diagnostic <- function(x=0, mu=0, sigma=1){
  plot.diagnostic(mu=mu, sigma=sigma)
  markValue(x=x, mu=mu, sigma=sigma)
}

diagnostic(88, mu=70, sigma=10)
diagnostic(48, mu=24, sigma=10)
diagnostic(9, mu=24, sigma=10)
