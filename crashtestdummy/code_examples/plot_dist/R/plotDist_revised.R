## Paul Johnson
## 20161009

## library(kutils)

wdir <- "../workingdata/"
ddir <- "../data/"
odir <- "../output/"
tdir <- "../tmp/"

pdf.options(onefile = FALSE, family="Times", paper="special", height=5, width=6, pointsize=10)
ps.options(horizontal = FALSE, onefile = FALSE, family="Times", paper="special", height=5, width=6)
options(papersize="special")

if(SAVEME) pdf(paste0(odir, "part1-%03d.pdf"), onefile = FALSE)

dat <- readRDS(file = paste0(wdir, "fake.rds"))


plot(y.prop ~ age, data = dat)

dat$y.count <- dat$y.prop * 20
plot(y.count ~ age, data = dat)
## Will need n, or log(n) later, n = 20
dat$nlog <- log(20)



library(rockchalk)
summarize(dat$y.prop)

summarize(dat$y.count)

m0 <- lm(y.prop ~ age, data = dat)
summary(m0)
plotSlopes(m0, plotx= "age", interval = "predict")


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
plot.diagnostic <- function(mu = 0, sigma = 1, file = NULL,
                            N = FALSE, Z = TRUE, t = FALSE){
    sigma <- round(sigma, 2)
    mu <- round(mu, 2)
    myx <- seq(mu - 3.5 * sigma,  mu + 3.5 * sigma, length.out = 500)
    myDensity <- dnorm(myx, mean = mu, sd = sigma)

    myTitle1 <- bquote (paste("x ~ Normal(", mu== .(round(mu,2)), ',', sigma^2== .(round(sigma,2))^2,")") )

    ## add half inch cor each desired axis
    height <- 5.5 + 0.6*N + 0.6*Z + 0.6*T
    width <- 6.5
    ## if (!is.null(file)){
    ##     pdf(file = file, height = height, width = 6.5,
    ##         onefile = FALSE, paper = "special", pointsize=10)
    ## } else {
    ##     # dev.new(height = height, width=6.5,pointsize=9)
    ## }

    dev.new(height = height, width = width, pointsize = 9)
    
    ## xpd needed to allow writing outside strict box of graph
    ## Expand margin below to make room for  each requested axis
    par.orig <- par(no.readonly = TRUE)
    par(xpd=TRUE, ps=10, mar=0.1 + c(5 + 2.5 * (N + Z + T), 5, 3, 2))

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
      
    line = 4
    if (N) {
        axis(1, line = line, at=mu+dividers*sigma, labels=c(b1,b2,b3,b4,b5), padj=0)
        mtext("N", 1, line= line +0.25, at=mu-sigma*2.5,cex=1.5)
        line <- line + 3
    }
    if (Z) {
        axis(1, line= line, at=mu+dividers*sigma, labels=c(-1.96,-1,0,1,1.96))#, padj=-1)
        mtext("Z", 1, line= line +0.25, at=mu-sigma*2.5, cex=1.5)
        line <- line + 3
    }
    if (t) {
        tdividers <- qt(pnorm(dividers), df=100)
        axis(1, line = line, at=mu+sigma*tdividers, labels=round(tdividers,2))
        mtext("t", 1, line= line +0.25, at=mu-sigma*2.5, cex=1.5) 
    }
 
    if (!is.null(file))  dev.off()
    par <- par.orig
    c(height = height, width = width)
}


pd1 <-  plot.diagnostic(mu = 0, sigma = 1, Z = TRUE)
if(SAVEME) {
    dev.print(pdf, height = pd1["height"], width = pd1["width"],
             file = paste0(odir, "part1-normal_0_1.pdf"))
    ##dev.off()
}

pd1 <- plot.diagnostic(mu = 3.271, sigma = 2.328, Z = TRUE)
if(SAVEME) {
    dev.copy(pdf, height = pd1["height"], width = pd1["width"],
             file = paste0(odir, "part1-normal_3.2_2.3.pdf"))
    dev.off()
}


dev.new(height=5, width=6)
x <- 0:20
xprob <- dpois(x, lambda = mean(dat$y.count))
plot(xprob ~ x, type = "h", xlab = "y.count", ylab = "Probability")
points(xprob ~ x, cex = 0.7)

if(SAVEME) {
    dev.copy(pdf, height = 5, width = 6,
             file = paste0(odir, "part1-poisson_3.2.pdf"))
    dev.off()
}
## pd1 <- plot.diagnostic(mu = 3.271, sigma = 2.328, Z = TRUE, N = TRUE, t = TRUE)
## if(SAVEME) {
##     dev.copy(pdf, height = pd1["height"], width = pd1["width"],
##              file = paste0(odir, "part1-normal_3.2_2.3_3scales.pdf"))
##     dev.off()
## }

## Quick check: y does not look log normal
hist(log(dat$y.prop), main = "Its Not Log Normal",
     xlab = "log(y)")
if(SAVEME) dev.off()


## From WorkingExamples/plot-drawHist-01.R
## So, in the following, the legal arguments are ALL of the ones that are valid
## for hist (see ?hist). And, as usual, I'm imposed my tastes on color and space.
drawHist <- function(x, normal = TRUE, pois = FALSE, kde = TRUE, nbinom = FALSE, ...){
    dots <- list(...)
    histDefaults <- list(right = FALSE, prob = TRUE, border = gray(.80),
                         include.lowest = TRUE, plot = FALSE)
    
    ## These are legal argument names we need to take from ...
    ## if the user supplied them.
    ## Run: dput(names(formals(hist.default)))
    histNames <- c("breaks", "freq", "probability", "include.lowest", "right", 
                   "density", "angle", "col", "border", "main", "xlim", "ylim", 
                   "xlab", "ylab", "axes", "plot", "labels", "nclass")
    argsForHist <- dots[ names(dots) [names(dots) %in% histNames]]

    ## place the dot args for host on top of histDefaults
    argsForHist <- modifyList(histDefaults, argsForHist)
    ## x now loose in function's environment:
    assign(deparse(substitute(x)), x)
    argsForHist[["x"]] <- as.name(deparse(substitute(x)))
  
    if ((length(dots$breaks) == 1) & is.numeric(dots$breaks)){
        argsForHist$breaks <- min(dots$breaks, length(unique(x))+1)
    }

    ## Remove prob argument to silence warning
    argsForHistSubset <- argsForHist
    argsForHistSubset[["prob"]] <- NULL
    argsForHistSubset[["xlim"]] <- NULL
    argsForHistSubset[["ylim"]] <- NULL
    argsForHistSubset[["border"]] <- NULL
    h1 <- do.call("hist", argsForHistSubset)

    ## Now prepare to draw that
   
    if (is.null(dots$xlim)){
        cr <- range(x)
    } else {
        cr <- dots$xlim
    }
    
    argsForHist[["plot"]] <- TRUE
    ## Unless the ... or "h1" arguments say otherwise, use these:
    if(is.null(argsForHist[["main"]])) argsForHist[["main"]] <- ""
    if(is.null(argsForHist[["xlim"]])) argsForHist[["xlim"]] <- extendrange(cr, f = 0.10)
    if(is.null(argsForHist[["ylim"]])) argsForHist[["ylim"]] <- rockchalk::magRange(h1$density, c(1, 1.2))
    if(is.null(argsForHist[["ylab"]])) argsForHist[["ylab"]] <- "density"
    if(is.null(argsForHist[["xlab"]])) argsForHist[["xlab"]] <- gsub(".*\\$", "", deparse(substitute(x)))
    ## argsForHist[["x"]] <- as.name(deparse(substitute(x)))

    ## ## Problem: Must take notice of breaks calculated by h1.
    ## defaults <- modifyList(defaults, list("breaks" = h1$breaks,
    ##                                         "include.lowest" = h1$include.lowest,
    ##                                         "right" = h1$right,
    ##                                         "nclass" = h1$nclass))


    
    callhist <- do.call("hist", argsForHist)

    ## Will need mean and standard deviation, rounded
    xm1 <- round(mean(x), 3)
    xsd1 <- round(sd(x), 3)
    
    legends <- c()
    ltys <- c()
    cols <- c()
    lwds <- c()

    if(normal) {
        ind <- seq(cr[1], cr[2], length.out=100)
        nprob <- dnorm(ind, m = xm1, s = xsd1)
        lines(ind, nprob, lty = 1, col = "black")
        nlab <-   bquote(paste("Normal(", .(round(xm1,3)),", ", .(round(xsd1,3))^2,")"))
        legends <- c(legends, as.expression(nlab))
        ltys <- c(ltys, 1)
        cols  <- c(cols, "black")
        lwds  <- c(lwds, 1)
    }
   
    if (pois) {
        indp <- seq(floor(cr[1]), ceiling(cr[2]))
        pprob <- rep(0, times = length(indp))
        pprob[indp >= 0] <- dpois(indp[indp >= 0], lambda = xm1)
        colp <- "purple"
        ltyp <- 5
        lwdp <- 2
        lines(pprob ~ indp, type = "s", col = colp, lty = ltyp, lwd = lwdp)
        plab <-   bquote(paste("Poisson(", .(round(xm1,3)),")"))
        legends[1] <- as.expression(plab)
        ltys[1] <- ltyp
        cols[1] <- colp
        lwds[1] <- lwdp
    }
    
    if(kde) {
        lines(density(x), col = "red", lty = 2, lwd = 2)
        legends <- c(legends, "Kernel Density")
        ltys <- c(ltys, 2)
        cols <- c(cols, "red")
        lwds <- c(lwds, 2)
    }
    
    if (nbinom) {
        indp <- seq(floor(cr[1]), ceiling(cr[2]))
        pprob <- rep(0, times = length(indp))
        pprob[indp >= 0] <- dnbinom(indp[indp >= 0], mu = xm1, size = dots$theta)
        colnb <- "orange"
        ltynb <- 5
        lwdnb <- 2
        lines(pprob ~ indp, type = "s", col = colnb, lty = ltynb, lwd = lwdnb)
        planb <-   bquote(paste("NegBin(mu=", .(round(xm1,3)),", size=", .(round(dots$theta, 3)), ")"))
        legends[1] <- as.expression(planb)
        ltys[1] <- ltynb
        cols[1] <- colnb
        lwds[1] <- lwdnb
    }
    
    
    legend("topright", legend=c(paste("Obs. mean=", xm1), paste("Obs. s.d.=",xsd1)))

    if(length(legends) > 0){
        legend("topleft", legend = legends, lty = ltys, col = cols, lwd = lwds)
    }
    invisible(callhist)
}

if(SAVEME) pdf(paste0(odir, "part2-2016110-%03d.pdf"), width = 6.5, height = 5.5)

drawHist(dat$y.count, border = "gray60", xlim = c(0,20),
         ylim = c(0, .30), kde = FALSE, normal = FALSE, right = FALSE, breaks = 40)

drawHist(dat$y.count, border = "gray60", xlim = c(-3,20),
         ylim = c(0, .30), kde = FALSE, normal = TRUE, breaks = 40)

drawHist(dat$y.count, border = "gray50", xlim = c(-3,20),
         ylim = c(0, .30), kde = FALSE, normal = FALSE, pois = TRUE, breaks = 40)

drawHist(dat$y.count, border = "gray50", xlim = c(-3,20),
         ylim = c(0, .30), kde = FALSE, normal = FALSE, pois = FALSE,
         nbinom= TRUE, theta = 5, breaks = 40)


drawHist(dat$y.count, border = "gray60", xlim = c(0,20),
         ylim = c(0, .30), kde = TRUE, normal = FALSE)

y.count.p <- c(0.1, 0.3, 0.5, 0.7, 0.9, 0.975)
y.count.q <- quantile(dat$y.count, probs = y.count.p)
names(y.count.q) <- as.character(y.count.p)
names(y.count.p) <- as.character(y.count.q)
par(xpd = FALSE)
for(j in seq_along(y.count.q)){
    lines(rep(y.count.q[j], 2), c(0, 0.25), col = "green")
    text(y.count.q[j], .27, srt = 45, labels = 100 * y.count.p[j])
    mtext(y.count.q, 1, at = y.count.q)
}
text(y.count.q[length(y.count.q)] + 0.1 * (max(dat$y.count) - y.count.q[length(y.count.q)]), .27,
    label = "Percentile markers", pos = 4)


## Now draw markers with Normal approach 
drawHist(dat$y.count, border = "gray60", xlim = c(0,20),
         ylim = c(0, .30), kde = FALSE, normal = TRUE)
y.count.p <- c(0.1, 0.3, 0.5, 0.7, 0.9, 0.975)
y.count.q <- qnorm(y.count.p, mean = 3.271, sd = 2.328)
names(y.count.q) <- as.character(y.count.p)
names(y.count.p) <- as.character(y.count.q)
for(j in seq_along(y.count.q)){
    lines(rep(y.count.q[j], 2), c(0, 0.25), col = "green")
    text(y.count.q[j], .27, srt = 45, labels = 100 * y.count.p[j])
    mtext(round(y.count.q, 1), 1, at = y.count.q)
}
text(y.count.q[length(y.count.q)] + 0.1 * (max(dat$y.count) - y.count.q[length(y.count.q)]), .27,
    label = "Percentile markers", pos = 4)


if(SAVEME) dev.off()

