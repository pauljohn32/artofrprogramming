###     A programme to obtain the power of parameters in 2 level
#       balanced model  with Poisson response
#                    generated on 09/11/16
###~~~~~~~~~~~~~~~~~    Required packages  ~~~~~~~~~~~~~~~~~~~~~###
    library(MASS)
    library(lme4)
###~~~~~~~~~~~~~~~~~~~     Initial inputs    ~~~~~~~~~~~~~~~~~~~~###

set.seed(666)
siglevel<-0.025
z1score<-abs(qnorm(siglevel))
simus<-1000
n1low<-5
n1high<-7
n1step<-1
n2low<-30
n2high<-50
n2step<-5
npred<-3
randsize<-1
beta<-c(3.000000,6.950000,0.150000,0.150000)
betasize<-length(beta)
effectbeta<-abs(beta)
sgnbeta<-sign(beta)
randcolumn<-0
meanpred<-c(0,6.130000,5.730000,5.210000)
varpred<-c(0,9.760000,6.230000,1.210000)
varpred2<-c(0,0.100000,0.100000,0.100000)
sigma2u<-matrix(c(0.200000),randsize,randsize)
n1range<-seq(n1low,n1high,n1step)
n2range<-seq(n2low,n2high,n2step)
n1size<-length(n1range)
n2size<-length(n2range)
totalsize<-n1size*n2size
finaloutput<-matrix(0,totalsize,2*betasize)
rowcount<-1
##-----------------        Inputs for model fitting       -----------------##

fixname<-c("x0","x1","x2","x3")
fixform<-"1+x1+x2+x3"
randform<-"(1|l2id)"
expression<-paste(c(fixform,randform),collapse="+")
modelformula<-formula(paste("y ~",expression))
data<-vector("list",2+length(fixname))
names(data)<-c("l2id","y",fixname)

#####--------- Initial input for power in two approaches ----------------#####

   powaprox<-vector("list",betasize)
    names(powaprox)<-c("b0","b1","b2","b3")
     powsde<-powaprox

cat("               The programme was executed at", date(),"\n")
cat("   --------------------------------------------------------------------\n")

 for(n2 in seq(n2low,n2high,n2step)){
  for(n1 in seq(n1low,n1high,n1step)){

                                             length=n1*n2
                                            x<-matrix(1,length,betasize)
                                           z<-matrix(1,length,randsize)
                                          l2id<-rep(c(1:n2),each=n1)
                                       powaprox[1:betasize]<-rep(0,betasize)
                                      powsde<-powaprox

cat(" Start of simulation for sample sizes of ",n1," micro and ",n2,"macro units\n")
  for(iter in 1:simus){

                       if(iter/10==floor(iter/10)){
                                                   cat(" Iteration remain=",simus-iter,"\n")
                                                  }
#######------------       To set up X matrix          --------------########

            micpred<-rnorm(length,meanpred[2],sqrt(varpred[2]))
             macpred<-rnorm(n2,0,sqrt(varpred2[2]))
              macpred<-rep(macpred,each=n1)
               x[,2]<-micpred+macpred
            micpred<-rnorm(length,meanpred[3],sqrt(varpred[3]))
             macpred<-rnorm(n2,0,sqrt(varpred2[3]))
              macpred<-rep(macpred,each=n1)
               x[,3]<-micpred+macpred
            micpred<-rnorm(length,meanpred[4],sqrt(varpred[4]))
             macpred<-rnorm(n2,0,sqrt(varpred2[4]))
              macpred<-rep(macpred,each=n1)
               x[,4]<-micpred+macpred
#####-----------------------------------------------------------------------#####
                   u<-mvrnorm(n2,rep(0,randsize),sigma2u)
                    fixpart<-x%*%beta
                     randpart<-rowSums(z*u[l2id,])
                      y<-rpois(length,exp(fixpart+randpart))
##-------------------        Inputs for model fitting       ---------------##

  data$l2id<-as.factor(l2id)
  data$y<-y
    data$x0<-x[,1]
    data$x1<-x[,2]
    data$x2<-x[,3]
    data$x3<-x[,4]
###~~~~~~~~~~      Fitting the model using lmer funtion    ~~~~~~~~~~###

(fitmodel <- lmer(modelformula,data,family=poisson,method="Laplace"))

######~~~~~~~~~~   To obtain the power of parameter(s) ~~~~~~~~~~######

estbeta<-fixef(fitmodel)
 sdebeta<-sqrt(diag(vcov(fitmodel)))
  for(l in 1:betasize)
  {
   cibeta<-estbeta[l]-sgnbeta[l]*z1score*sdebeta[l]
    if(beta[l]*cibeta>0)              powaprox[[l]]<-powaprox[[l]]+1
     powsde[[l]]<-powsde[[l]]+sdebeta[l]
  }
##------------------------------------------------------------------------##
        } ##  iteration end here

 ###---------                Powers of parameter(s)         ---------###

for(l in 1:betasize)
{
      powaprox[[l]]<-unlist(powaprox[[l]]/simus)
     powsde[[l]]<-powsde[[l]]/simus
    powsde[[l]]<-pnorm(effectbeta[l]/powsde[[l]]-z1score)
   finaloutput[rowcount,(2*l-1):(2*l)]<-c(powaprox[[l]],powsde[[l]])
}
###~~~~~~~~~~            Set out the results in a data frame    ~~~~~~~~~~###

rowcount<-rowcount+1
cat("--------------------------------------------------------------------\n")
                               } ## end of the loop  over the first level
                           } ## end of the loop  over the second level

 ###---------         Export output in a file                      ---------###

finaloutput<-as.data.frame(round(finaloutput,3))
 output<-data.frame(cbind(rep(n2range,each=n1size),rep(n1range,n2size),finaloutput))
  names(output)<-c("N","n","zpb0","spb0","zpb1","spb1","zpb2","spb2","zpb3","spb3")

write.table(output,"powerout.txt",sep="\t ",quote=F,eol="\n",dec=".",col.names=T,row.names=F,qmethod="double")
