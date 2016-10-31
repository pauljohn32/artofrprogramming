### Paul Johnson <pauljohn@ku.edu>
### 2010-06-16

set.seed(45452434)
N <- 1000
A <- -1
B <- 0.3
x <- 1 + 10 * rnorm(N)

simLogit <- function(eta){
   simpi <- exp(eta)/(1+exp(eta))
   simunif <- runif(length(eta))
   y <- ifelse(simunif < simpi, 1, 0)
 }

simUnbalanced <- function(iter = 0, C = 0, PrFem = 0.5){
  sex <- ifelse(runif(N) < PrFem,0,1)
  eta <- A + B * x + C * sex
  sex <- factor(sex, levels=c(0,1), labels=c("M","F"))
  y <- simLogit(eta)
  myglm <- glm ( y ~ x + sex, family=binomial(link="logit") )
  myglmsum <- coef(summary(myglm))
  est <- myglmsum[3,]
  est
}

createFigs <- function(result, C = 0, PrFem = 0){
  betas <- result[1,]
  pvals <- result[4,]
  hypos <- ifelse( (pvals <= 0.05 ) , 1, 0)

  bhist <- hist(betas, breaks=50, plot = FALSE)

  breakMember <- cut(betas, bhist$breaks)

  df <- data.frame(betas, pvals, hypos, breakMember)

  propsig <- by(df$hypos, INDICES=list(df$breakMember), mean, simplify = TRUE)

  gc <- c("gray98","gray70","gray40")

  catpropsig <- ifelse(propsig == 0, 1, ifelse(propsig == 1, 3, 2))

  barplot(bhist$density, col=gc[catpropsig], names=bhist$mids, xlab=paste("Estimates of Gender Effect (true = ", C,")"))

  legend("topleft", legend=c(paste("Mean=",round(mean(betas),2)), paste("Std.Dev.=",round(sd(betas),2) )), bty="n")

  legend("left", legend=c(expression(all ~~ p > .05), expression(mixed), expression(all ~~ p <= .05)),fill=gc[1:3])
}

SAVE <- F
if (SAVE == T) pdf(file="unbalancedTesting%03d.pdf", height=6, width=6, paper="special", family="Times", onefile=F)


C <- 0.4
PrFem <- 0.5
result45 <- sapply(1:1000, simUnbalanced, C = C, PrFem = PrFem)
createFigs(result45, C, PrFem)

PrFem <- 0.9
result49 <- sapply(1:1000, simUnbalanced, C = C, PrFem = PrFem)

createFigs(result49, C, PrFem)

dev.off()

