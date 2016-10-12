
#------------------Clear All---------------------------#
rm(list=ls())
#------------------------------------------------------#
#-------------TECH SPECIFICATIONS----------------------#
library(simsem)
iter <- 10       #how many iterations per condition
set.seed(7913025)   #set random seed
#------------------------------------------------------#
#-------------POPULATION SPECIFICATIONS----------------#
perclust <- 20000    #Simulation POPULATION data set size 
    nmin <- 100      #MINIMUM Sample Size
    nmax <- 100    #MAXIMUM Sample Size
    nby <- 100       #INCREASE BY Sample Size
nvar <- 10           #number of manifest variables
nLvar <- 10          #number of latent variables
LYload <- 0.70      #loadings of constructs on vars 
basecor <- 0.30     #<----####base population correlation
XYcor <- 0.70       #specific population correlation 
TYmean <- 0         #manifest intercepts 
ALmean <- 0         #latent variable means 
deg <- 2            #degree of polynomials and interactions 1, 2, or 3 
    nmissmin <- 3   #MINIMUM missing data percent (only whole number)
	nmissmax <- 3   #MAXIMUM missing data percent (only whole number)
	nmissby <- 3    #INCREASE BY missing data percent (only whole number)
#------------------------------------------------------#
#---------------GENERATE SIM MODEL---------------------#
if(!exists("CFA.Model"))  { 
#---------------GENERATE LOADINGS----------------------#
if(nLvar == nvar){
loading <- matrix(0, nvar, nvar)
diag(loading) <- 1
} else {
VarperL <- (nvar/nLvar) 
loading <- matrix(0, nvar, nLvar)
loading[1:VarperL, 1] <- NA
j<-2
 if ((j) == nLvar) {
for (i in seq(VarperL,(VarperL),VarperL))   
	{
loading[(i+1):(VarperL+i), j] <- NA  
    }
     }	else {
	  for (i in seq(VarperL,(VarperL*VarperL),VarperL))   
	       {
      loading[(i+1):(VarperL+i), j] <- NA
	   j<-j+1
           }
	}
}	
if(nLvar == nvar){
loadingValues <- matrix(0, nvar, nvar)
diag(loadingValues) <- 1
} else {
VarperL <- (nvar/nLvar) 
loadingValues <- matrix(0, nvar, nLvar)
loadingValues[1:VarperL, 1] <- LYload
j<-2
 if ((j) == nLvar) {
for (i in seq(VarperL,(VarperL),VarperL))   
	{
loadingValues[(i+1):(VarperL+i), j] <- LYload  
    }
     }	else {
	  for (i in seq(VarperL,(VarperL*VarperL),VarperL))   
	       {
      loadingValues[(i+1):(VarperL+i), j] <- LYload
	   j<-j+1
           }
	}
}
LY <- simMatrix(loading, loadingValues)	
#------------------------------------------------------#
#---------------CORRELATIONS---------------------------#
latent.cor <- matrix(0, nLvar, nLvar)
diag(latent.cor) <- 1
j<-1
for (i in seq(2,(nLvar),1)) {
latent.cor[(i):(nLvar), j] <- NA
j<-j+1 }
j<-nLvar
for (i in seq(1,(nLvar-1),1)) {
latent.cor[(1):(nLvar-i), j] <- NA
j<-j-1
    }	
if(basecor == XYcor){
latent.corValues <- matrix(0, nLvar, nLvar)
diag(latent.corValues) <- 1
j<-1
for (i in seq(2,(nLvar),1)) 
   {
latent.corValues[(i):(nLvar), j] <- basecor
j<-j+1
    }
j<-nLvar
for (i in seq(1,(nLvar-1),1))   
	{
latent.corValues[(1):(nLvar-i), j] <- basecor
j<-j-1
    }	
PS <- symMatrix(latent.cor, latent.corValues)	
} else { 
latent.corValues <- matrix(0, nLvar, nLvar)
diag(latent.corValues) <- 1
j<-1
for (i in seq(2,(nLvar),1)) 
   {
latent.corValues[(i):(nLvar), j] <- basecor
j<-j+1
    }
j<-nLvar
for (i in seq(1,(nLvar-1),1))   
	{
latent.corValues[(1):(nLvar-i), j] <- basecor
j<-j-1
    }	
latent.corValues[(2):(2), 1] <- XYcor
latent.corValues[(1):(1), 2] <- XYcor
PS <- symMatrix(latent.cor, latent.corValues)
    }
#------------------------------------------------------#
#------------ERROR VAR-COVARIANCES---------------------#
if(nLvar == nvar){
error.cor <- matrix(0, nvar, nvar)
diag(error.cor) <- 0	
TE <- symMatrix(error.cor)
} else { 
error.cor <- matrix(0, nvar, nvar)
diag(error.cor) <- 1	
TE <- symMatrix(error.cor) }
#------------------------------------------------------#
#---------------INTERCEPTS AND MEANS-------------------#
intercept <- rep(NA, nvar)
TY <- simVector(intercept, TYmean)
factor.mean <- rep(ALmean, nLvar)
AL <- simVector(factor.mean)
#------------------------------------------------------#
#------------CREATE SIMULATION MODEL-------------------#
CFA.Model <- simSetCFA(LY = LY, PS = PS, TE = TE, TY = TY, AL = AL) 
#summary(CFA.Model)
SimData <- simData(perclust, CFA.Model)
PopData <- run(SimData)

#---------Set Population Continious Conditions---------#
iteration.data.list1 <- vector("list", iter) 
iteration.data.list2 <- vector("list", iter) 
iteration.data.list3 <- vector("list", iter) 
iteration.data.list4 <- vector("list", iter) 
iteration.data.list5 <- vector("list", iter) 
iteration.data.list6 <- vector("list", iter) 
iteration.data.list7 <- vector("list", iter) 
iteration.data.list8 <- vector("list", iter) 
iteration.data.list9 <- vector("list", iter) 
iteration.data.list10 <- vector("list", iter) 
missingpercent.data.list1 <- vector("list", 10) 
missingpercent.data.list2 <- vector("list", 10) 
missingpercent.data.list3 <- vector("list", 10) 
missingpercent.data.list4 <- vector("list", 10) 
missingpercent.data.list5 <- vector("list", 10) 
missingpercent.data.list6 <- vector("list", 10) 
missingpercent.data.list7 <- vector("list", 10) 
missingpercent.data.list8 <- vector("list", 10) 
missingpercent.data.list9 <- vector("list", 10) 
missingpercent.data.list10 <- vector("list", 10) 
samplesize.data.list1 <- vector("list", nmax) 
samplesize.data.list2 <- vector("list", nmax) 
samplesize.data.list3 <- vector("list", nmax) 
samplesize.data.list4 <- vector("list", nmax) 
samplesize.data.list5 <- vector("list", nmax) 
samplesize.data.list6 <- vector("list", nmax) 
samplesize.data.list7 <- vector("list", nmax) 
samplesize.data.list8 <- vector("list", nmax) 
samplesize.data.list9 <- vector("list", nmax) 
samplesize.data.list10 <- vector("list", nmax) 
#------------------------------------------------------#
} else { print("To save over the previous CFA.Model run rm(list=ls())")} 
#------------------------------------------------------#

for (a in 1:iter) { # set iterations to code -> TIER1
#------------Draw random sample of size n--------------# 
for (n in seq(nmin,nmax,nby)) { #make sample size continious -> TIER2
SimSample <- PopData[sample(1:nrow(PopData),n,replace=TRUE),]
#-------GENERATE INTERACTION & SQUARED TERMS-----------#
if(deg == 3){
SimSample2 <- t(apply(SimSample, 1, combn, (deg-1), prod)) 
colnames(SimSample2) <- paste("Inter.y", combn(1:nvar, (deg-1), paste, collapse="y"), sep="")
SimSample3 <- t(apply(SimSample, 1, combn, deg, prod)) 
colnames(SimSample3) <- paste("Inter.y", combn(1:nvar, deg, paste, collapse="y"), sep="")
SimSampleSQ <- numeric(0)
SimSampleSQ <- cbind(SimSampleSQ, SimSample^2)
colnames(SimSampleSQ) <- paste("SQ.y", combn(1:nvar, 1, paste, collapse="y"), sep="")
SimSampleCU <- numeric(0)
SimSampleCU <- cbind(SimSampleCU, SimSample^3)
colnames(SimSampleCU) <- paste("CU.y", combn(1:nvar, 1, paste, collapse="y"), sep="")
SimSample2 <- cbind(SimSample, SimSample2, SimSample3, SimSampleSQ, SimSampleCU)
} 
if(deg == 2){
SimSample2 <- t(apply(SimSample, 1, combn, deg, prod)) 
colnames(SimSample2) <- paste("Inter.y", combn(1:nvar, deg, paste, collapse="y"), sep="")
SimSampleSQ <- numeric(0)
SimSampleSQ <- cbind(SimSampleSQ, SimSample^2)
colnames(SimSampleSQ) <- paste("SQ.y", combn(1:nvar, 1, paste, collapse="y"), sep="")
SimSample2 <- cbind(SimSample, SimSample2, SimSampleSQ)
} else {
SimSample2 <- SimSample }
#------------------------------------------------------# 
#------------------------------------------------------#
    for (m in seq(nmissmin,nmissmax,nmissby)) { #make percent missing continious -> TIER3
	
#--------SIMULATE MISSING DATA SPECIFICATIONS----------#
pm <- (m/10)  #--Total percentage of data missing pm<-.20-----# 
MARpm  <- 1  #--Percent of Total Missing that is MAR-#
MCARpm <- 0  #--Percent of Total Missing that is MCAR#
FocusVars <- c("y1","y2")   
               #--"FocusVars" allows for auxiliary vars#
			   #--list non-auxiliary variables---------# 
			   #--use "ncol(SimSample2)" for no aux.---#
VarsMiss <- c("y1")
               #--"VarsMiss" is used to identify the---# 
			   #--variables that have missing values---#
VarsNOmiss <- c("y2")
myMARvars <- c("y3")
			   #--"myMARvars" is used to identify up to#  
			   #--3 variables responsible for creating-# 
			   #--missing data - can be non-linear-----#  
useAUX <- 0 
AUXvars <- c("y3")
usePCA <- 0    #--use pca aux. vars: 0 = no, 1 = yes---#
PCkeep <- c("Comp.1","Comp.2")	
MARvar1dec <- 3  
MARvar2dec <- 3  
MARvar3dec <- 3  
			   #--"MARvar1dec"-"MARvar3dec" are the----#
			   #--deciles that reflect the distribution#
			   #--of MARvar1-MARvar3 that cause missing#
Attrition <- 1 #--generate attrition: 0 = no, 1 = yes--#
nimp <- 20      #--number of multiple imputations-------#

#------------------------------------------------------#	  
#-------------GENERATE MISSING DATA--------------------#
#--MAR MISSING-----------------------------------------# 
if(MARpm > 0){ 
MARvars <- length(myMARvars)
MARvar1 <- (SimSample2[myMARvars])
ModelVars <- matrix(SimSample2[,c(FocusVars)])
myrandomdata <- matrix(sample(1:1000,100000,rep=TRUE,prob=(rep(.001,1000)))) #1:20 prob=(rep(.05,20)
dimnames(myrandomdata) = list(NULL,c("Student"))
myrandsample <- matrix(myrandomdata[sample(1:nrow(myrandomdata),n,replace=TRUE),])
for (i in MARvars) {
if(i == 1) { 
dec1 <- matrix((MARvar1[,i]))	
dimnames(dec1)<-list(NULL,paste("MARvar",i,sep=""))
mydec1 <- cbind(dec1, Decile1 = cut(dec1, 10, labels = 1:10)) 
vec1 <- as.data.frame(mydec1)
myMARdec <- cbind(SimSample2, myrandsample, vec1$Decile1)
if(Attrition == 0) { #prevents missing on the same cases for the selected vars 
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myrandomdata <- matrix(sample(1:1000,100000,rep=TRUE,prob=(rep(.001,1000)))) #1:20 prob=(rep(.05,20)
dimnames(myrandomdata) = list(NULL,c("Student"))
myrandsample <- matrix(myrandomdata[sample(1:nrow(myrandomdata),n,replace=TRUE),])
myMARdec[myMARdec$"vec1$Decile1" <= MARvar1dec & (myrandsample) <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)),j] <- NA
} } else {
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myMARdec[myMARdec$"vec1$Decile1" <= MARvar1dec & myMARdec$"myrandsample" <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)),j] <- NA
} }
missvar <- myMARdec[,c(VarsMiss)]
d <- ((sum(is.na(missvar)))/(ncol(ModelVars)*n))
print(d)
for(i in 1:1000) { 		
if(d < ((pm*MARpm))){
if(Attrition == 0) { #prevents missing on the same cases for the selected vars 
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myrandomdata <- matrix(sample(1:1000,100000,rep=TRUE,prob=(rep(.001,1000)))) #1:20 prob=(rep(.05,20)
dimnames(myrandomdata) = list(NULL,c("Student"))
myrandsample <- matrix(myrandomdata[sample(1:nrow(myrandomdata),n,replace=TRUE),])
myMARdec[myMARdec$"vec1$Decile1" <= MARvar1dec & (myrandsample) <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)+i),j] <- NA
} } else {
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myMARdec[myMARdec$"vec1$Decile1" <= MARvar1dec & myMARdec$"myrandsample" <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)+i),j] <- NA
} }
missvar <- myMARdec[,c(VarsMiss)]
ModelVars <- matrix(SimSample2[,c(FocusVars)])
d <- ((sum(is.na(missvar)))/(ncol(ModelVars)*n))
print(d)
} else { print("desired missingness reached")
} }

if(d < ((pm*MARpm))){
print("Caution: desired MAR Missing Data Percent not reached, increasing the value of MARdec")
MARvar1dec <- MARvar1dec+1
for(i in 1:1000) { 		
if(d < ((pm*MARpm))){
if(Attrition == 0) { #prevents missing on the same cases for the selected vars 
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myrandomdata <- matrix(sample(1:1000,100000,rep=TRUE,prob=(rep(.001,1000)))) #1:20 prob=(rep(.05,20)
dimnames(myrandomdata) = list(NULL,c("Student"))
myrandsample <- matrix(myrandomdata[sample(1:nrow(myrandomdata),n,replace=TRUE),])
myMARdec[myMARdec$"vec1$Decile1" <= MARvar1dec & (myrandsample) <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)+i),j] <- NA
} } else {
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myMARdec[myMARdec$"vec1$Decile1" <= MARvar1dec & myMARdec$"myrandsample" <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)+i),j] <- NA
} }
missvar <- myMARdec[,c(VarsMiss)]
ModelVars <- matrix(SimSample2[,c(FocusVars)])
d <- ((sum(is.na(missvar)))/(ncol(ModelVars)*n))
print(d)
} else { print("desired missingness reached")
} }
} else {
print("MAR missingness was generated from 1 variable")
print("Requested MAR Missing Data Percentage was")
print(MARpm)
print("Which is the following percentage of the Total Missing Data Rate")
print((pm*MARpm))
print("The observed MAR Rate of the Total Missing Data Percentage is")
print(d)  
}

if(d < ((pm*MARpm))){
print("Caution: desired MAR Missing Data Percent not reached, increasing the value of MARdec")
MARvar1dec <- MARvar1dec+2
for(i in 1:1000) { 		
if(d < ((pm*MARpm))){
if(Attrition == 0) { #prevents missing on the same cases for the selected vars 
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myrandomdata <- matrix(sample(1:1000,100000,rep=TRUE,prob=(rep(.001,1000)))) #1:20 prob=(rep(.05,20)
dimnames(myrandomdata) = list(NULL,c("Student"))
myrandsample <- matrix(myrandomdata[sample(1:nrow(myrandomdata),n,replace=TRUE),])
myMARdec[myMARdec$"vec1$Decile1" <= MARvar1dec & (myrandsample) <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)+i),j] <- NA
} } else {
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myMARdec[myMARdec$"vec1$Decile1" <= MARvar1dec & myMARdec$"myrandsample" <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)+i),j] <- NA
} }
missvar <- myMARdec[,c(VarsMiss)]
ModelVars <- matrix(SimSample2[,c(FocusVars)])
d <- ((sum(is.na(missvar)))/(ncol(ModelVars)*n))
print(d)
} else { print("desired missingness reached")
} } 
} else {
print("MAR missingness was generated from 1 variable")
print("Requested MAR Missing Data Percentage was")
print(MARpm)
print("Which is the following percentage of the Total Missing Data Rate")
print((pm*MARpm))
print("The observed MAR Rate of the Total Missing Data Percentage is")
print(d)  
}  
} 
else if(i == 2) { 
dec1 <- matrix((MARvar1[,1]))	
dimnames(dec1)<-list(NULL,paste("MARvar",1,sep=""))
mydec1 <- cbind(dec1, Decile1 = cut(dec1, 10, labels = 1:10)) 
vec1 <- as.data.frame(mydec1)
dec2 <- matrix((MARvar1[,i]))	
dimnames(dec2)<-list(NULL,paste("MARvar",i,sep=""))
mydec2 <- cbind(dec2, Decile2 = cut(dec2, 10, labels = 1:10))
vec2 <- as.data.frame(mydec2)
myMARdec <- cbind(SimSample2, myrandsample, vec1$Decile1, vec2$Decile2)
if(Attrition == 0) { #prevents missing on the same cases for the selected vars 
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myrandomdata <- matrix(sample(1:1000,100000,rep=TRUE,prob=(rep(.001,1000)))) #1:20 prob=(rep(.05,20)
dimnames(myrandomdata) = list(NULL,c("Student"))
myrandsample <- matrix(myrandomdata[sample(1:nrow(myrandomdata),n,replace=TRUE),])
myMARdec[myMARdec$"vec2$Decile2" <= MARvar2dec & myMARdec$"vec1$Decile1" <= MARvar1dec & (myrandsample) <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)),j] <- NA
} } else {
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myMARdec[myMARdec$"vec2$Decile2" <= MARvar2dec & myMARdec$"vec1$Decile1" <= MARvar1dec & myMARdec$"myrandsample" <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)),j] <- NA
} }
missvar <- myMARdec[,c(VarsMiss)]
ModelVars <- matrix(SimSample2[,c(FocusVars)])
d <- ((sum(is.na(missvar)))/(ncol(ModelVars)*n))
print(d)
for(i in 1:1000) { 		
if(d < ((pm*MARpm))){
if(Attrition == 0) { #prevents missing on the same cases for the selected vars 
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myrandomdata <- matrix(sample(1:1000,100000,rep=TRUE,prob=(rep(.001,1000)))) #1:20 prob=(rep(.05,20)
dimnames(myrandomdata) = list(NULL,c("Student"))
myrandsample <- matrix(myrandomdata[sample(1:nrow(myrandomdata),n,replace=TRUE),])
myMARdec[myMARdec$"vec2$Decile2" <= MARvar2dec & myMARdec$"vec1$Decile1" <= MARvar1dec & (myrandsample) <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)+i),j] <- NA
} } else {
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myMARdec[myMARdec$"vec2$Decile2" <= MARvar2dec & myMARdec$"vec1$Decile1" <= MARvar1dec & myMARdec$"myrandsample" <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)+i),j] <- NA
} }
missvar <- myMARdec[,c(VarsMiss)]
ModelVars <- matrix(SimSample2[,c(FocusVars)])
d <- ((sum(is.na(missvar)))/(ncol(ModelVars)*n))
print(d)
} else { print("desired missingness reached")
} }
if(d < ((pm*MARpm))){
print("Caution: desired MAR Missing Data Percent not reached, increase the value of MARdec")
MARvar1dec <- MARvar1dec+1
MARvar2dec <- MARvar2dec+1
for(i in 1:1000) { 		
if(d < ((pm*MARpm))){
if(Attrition == 0) { #prevents missing on the same cases for the selected vars 
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myrandomdata <- matrix(sample(1:1000,100000,rep=TRUE,prob=(rep(.001,1000)))) #1:20 prob=(rep(.05,20)
dimnames(myrandomdata) = list(NULL,c("Student"))
myrandsample <- matrix(myrandomdata[sample(1:nrow(myrandomdata),n,replace=TRUE),])
myMARdec[myMARdec$"vec2$Decile2" <= MARvar2dec & myMARdec$"vec1$Decile1" <= MARvar1dec & (myrandsample) <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)+i),j] <- NA
} } else {
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myMARdec[myMARdec$"vec2$Decile2" <= MARvar2dec & myMARdec$"vec1$Decile1" <= MARvar1dec & myMARdec$"myrandsample" <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)+i),j] <- NA
} }
missvar <- myMARdec[,c(VarsMiss)]
ModelVars <- matrix(SimSample2[,c(FocusVars)])
d <- ((sum(is.na(missvar)))/(ncol(ModelVars)*n))
print(d)
} else { print("desired missingness reached")
} }
} else {
print("MAR missingness was generated from 1 variable")
print("Requested MAR Missing Data Percentage was")
print(MARpm)
print("Which is the following percentage of the Total Missing Data Rate")
print((pm*MARpm))
print("The observed MAR Rate of the Total Missing Data Percentage is")
print(d)  
}  
if(d < ((pm*MARpm))){
print("Caution: desired MAR Missing Data Percent not reached, increase the value of MARdec")
MARvar1dec <- MARvar1dec+2
MARvar2dec <- MARvar2dec+2
for(i in 1:1000) { 		
if(d < ((pm*MARpm))){
if(Attrition == 0) { #prevents missing on the same cases for the selected vars 
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myrandomdata <- matrix(sample(1:1000,100000,rep=TRUE,prob=(rep(.001,1000)))) #1:20 prob=(rep(.05,20)
dimnames(myrandomdata) = list(NULL,c("Student"))
myrandsample <- matrix(myrandomdata[sample(1:nrow(myrandomdata),n,replace=TRUE),])
myMARdec[myMARdec$"vec2$Decile2" <= MARvar2dec & myMARdec$"vec1$Decile1" <= MARvar1dec & (myrandsample) <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)+i),j] <- NA
} } else {
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myMARdec[myMARdec$"vec2$Decile2" <= MARvar2dec & myMARdec$"vec1$Decile1" <= MARvar1dec & myMARdec$"myrandsample" <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)+i),j] <- NA
} }
missvar <- myMARdec[,c(VarsMiss)]
ModelVars <- matrix(SimSample2[,c(FocusVars)])
d <- ((sum(is.na(missvar)))/(ncol(ModelVars)*n))
print(d)
} else { print("desired missingness reached")
} }
} else {
print("MAR missingness was generated from 1 variable")
print("Requested MAR Missing Data Percentage was")
print(MARpm)
print("Which is the following percentage of the Total Missing Data Rate")
print((pm*MARpm))
print("The observed MAR Rate of the Total Missing Data Percentage is")
print(d)  
}  
} 
else if(i == 3) { 
dec1 <- matrix((MARvar1[,1]))	
dimnames(dec1)<-list(NULL,paste("MARvar",1,sep=""))
mydec1 <- cbind(dec1, Decile1 = cut(dec1, 10, labels = 1:10)) 
vec1 <- as.data.frame(mydec1)
dec2 <- matrix((MARvar1[,2]))	
dimnames(dec2)<-list(NULL,paste("MARvar",2,sep=""))
mydec2 <- cbind(dec2, Decile2 = cut(dec2, 10, labels = 1:10))
vec2 <- as.data.frame(mydec2)
dec3 <- matrix((MARvar1[,i]))	
dimnames(dec3)<-list(NULL,paste("MARvar",i,sep=""))
mydec3 <- cbind(dec3, Decile3 = cut(dec3, 10, labels = 1:10))
vec3 <- as.data.frame(mydec3)
myMARdec <- cbind(SimSample2, myrandsample, vec1$Decile1, vec2$Decile2, vec3$Decile3)
if(Attrition == 0) { #prevents missing on the same cases for the selected vars 
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myrandomdata <- matrix(sample(1:1000,100000,rep=TRUE,prob=(rep(.001,1000)))) #1:20 prob=(rep(.05,20)
dimnames(myrandomdata) = list(NULL,c("Student"))
myrandsample <- matrix(myrandomdata[sample(1:nrow(myrandomdata),n,replace=TRUE),])
myMARdec[myMARdec$"vec3$Decile3" <= MARvar3dec & myMARdec$"vec2$Decile2" <= MARvar2dec & myMARdec$"vec1$Decile1" <= MARvar1dec & (myrandsample) <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)),j] <- NA
} } else {
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myMARdec[myMARdec$"vec3$Decile3" <= MARvar3dec & myMARdec$"vec2$Decile2" <= MARvar2dec & myMARdec$"vec1$Decile1" <= MARvar1dec & myMARdec$"myrandsample" <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)),j] <- NA
} }
missvar <- myMARdec[,c(VarsMiss)]
ModelVars <- matrix(SimSample2[,c(FocusVars)])
d <- ((sum(is.na(missvar)))/(ncol(ModelVars)*n))
print(d)
for(i in 1:1000) { 		
if(d < ((pm*MARpm))){
if(Attrition == 0) { #prevents missing on the same cases for the selected vars 
myrandomdata <- matrix(sample(1:100,100000,rep=TRUE,prob=(rep(.01,100)))) #1:20 prob=(rep(.05,20)
dimnames(myrandomdata) = list(NULL,c("Student"))
myrandsample <- matrix(myrandomdata[sample(1:nrow(myrandomdata),n,replace=TRUE),])
myMARdec[myMARdec$"vec3$Decile3" <= MARvar3dec & myMARdec$"vec2$Decile2" <= MARvar2dec & myMARdec$"vec1$Decile1" <= MARvar1dec & (myrandsample) <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)+i),VarsMiss] <- NA
} else {
myMARdec[myMARdec$"vec3$Decile3" <= MARvar3dec & myMARdec$"vec2$Decile2" <= MARvar2dec & myMARdec$"vec1$Decile1" <= MARvar1dec & myMARdec$"myrandsample" <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)+i),VarsMiss] <- NA
}
missvar <- myMARdec[,c(VarsMiss)]
ModelVars <- matrix(SimSample2[,c(FocusVars)])
d <- ((sum(is.na(missvar)))/(ncol(ModelVars)*n))
print(d)
} else { print("desired missingness reached")
} } 
if(d < ((pm*MARpm))){
print("Caution: desired MAR Missing Data Percent not reached, increase the value of MARdec")
MARvar1dec <- MARvar1dec+1
MARvar2dec <- MARvar2dec+1
MARvar3dec <- MARvar3dec+1
for(i in 1:1000) { 		
if(d < ((pm*MARpm))){
if(Attrition == 0) { #prevents missing on the same cases for the selected vars 
myrandomdata <- matrix(sample(1:100,100000,rep=TRUE,prob=(rep(.01,100)))) #1:20 prob=(rep(.05,20)
dimnames(myrandomdata) = list(NULL,c("Student"))
myrandsample <- matrix(myrandomdata[sample(1:nrow(myrandomdata),n,replace=TRUE),])
myMARdec[myMARdec$"vec3$Decile3" <= MARvar3dec & myMARdec$"vec2$Decile2" <= MARvar2dec & myMARdec$"vec1$Decile1" <= MARvar1dec & (myrandsample) <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)+i),VarsMiss] <- NA
} else {
myMARdec[myMARdec$"vec3$Decile3" <= MARvar3dec & myMARdec$"vec2$Decile2" <= MARvar2dec & myMARdec$"vec1$Decile1" <= MARvar1dec & myMARdec$"myrandsample" <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)+i),VarsMiss] <- NA
}
missvar <- myMARdec[,c(VarsMiss)]
ModelVars <- matrix(SimSample2[,c(FocusVars)])
d <- ((sum(is.na(missvar)))/(ncol(ModelVars)*n))
print(d)
} else { print("desired missingness reached")
} } 
} else {
print("MAR missingness was generated from 1 variable")
print("Requested MAR Missing Data Percentage was")
print(MARpm)
print("Which is the following percentage of the Total Missing Data Rate")
print((pm*MARpm))
print("The observed MAR Rate of the Total Missing Data Percentage is")
print(d)  
}  
if(d < ((pm*MARpm))){
print("Caution: desired MAR Missing Data Percent not reached, increasing the value of MARdec")
MARvar1dec <- MARvar1dec+2
MARvar2dec <- MARvar2dec+2
MARvar3dec <- MARvar3dec+2
for(i in 1:1000) { 		
if(d < ((pm*MARpm))){
if(Attrition == 0) { #prevents missing on the same cases for the selected vars 
myrandomdata <- matrix(sample(1:100,100000,rep=TRUE,prob=(rep(.01,100)))) #1:20 prob=(rep(.05,20)
dimnames(myrandomdata) = list(NULL,c("Student"))
myrandsample <- matrix(myrandomdata[sample(1:nrow(myrandomdata),n,replace=TRUE),])
myMARdec[myMARdec$"vec3$Decile3" <= MARvar3dec & myMARdec$"vec2$Decile2" <= MARvar2dec & myMARdec$"vec1$Decile1" <= MARvar1dec & (myrandsample) <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)+i),VarsMiss] <- NA
} else {
myMARdec[myMARdec$"vec3$Decile3" <= MARvar3dec & myMARdec$"vec2$Decile2" <= MARvar2dec & myMARdec$"vec1$Decile1" <= MARvar1dec & myMARdec$"myrandsample" <= ((((pm*MARpm)/(ncol(ModelVars)))+((pm*MARpm)/(ncol(ModelVars)))*10)+i),VarsMiss] <- NA
}
missvar <- myMARdec[,c(VarsMiss)]
ModelVars <- matrix(SimSample2[,c(FocusVars)])
d <- ((sum(is.na(missvar)))/(ncol(ModelVars)*n))
print(d)
} else { print("desired missingness reached")
} } 
} else {
print("MAR missingness was generated from 1 variable")
print("Requested MAR Missing Data Percentage was")
print(MARpm)
print("Which is the following percentage of the Total Missing Data Rate")
print((pm*MARpm))
print("The observed MAR Rate of the Total Missing Data Percentage is")
print(d)   
} } 
} }
else { print("No MAR data requested.")}
#------MCAR MISSING------------------------------------#
if(MCARpm > 0){
if((length(VarsMiss) > 1)) { 
ModelVars <- matrix(SimSample2[,c(FocusVars)])
myrandomdata <- matrix(sample(1:1000,100000,rep=TRUE,prob=(rep(.001,1000)))) #1:20 prob=(rep(.05,20)
dimnames(myrandomdata) = list(NULL,c("Student"))
myrandsample <- matrix(myrandomdata[sample(1:nrow(myrandomdata),n,replace=TRUE),])
myMCAR <- cbind(SimSample2, myrandsample)
if(MARpm == 0) {
myMARdec <- myMCAR } else {}
if(Attrition == 0) { #prevents missing on the same cases for the selected vars 
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myrandomdata <- matrix(sample(1:1000,100000,rep=TRUE,prob=(rep(.001,1000)))) #1:20 prob=(rep(.05,20)
dimnames(myrandomdata) = list(NULL,c("Student"))
myrandsample <- matrix(myrandomdata[sample(1:nrow(myrandomdata),n,replace=TRUE),])
missvar <- myMARdec[,c(VarsMiss)]
myMCAR[(!is.na(missvar[j])) & (myrandsample) <= ((((pm*MCARpm)/(ncol(ModelVars)))+((pm*MCARpm)/(ncol(ModelVars)))*10)),j] <- NA 
myMARdec[(!is.na(missvar[j])) & (myrandsample) <= ((((pm*MCARpm)/(ncol(ModelVars)))+((pm*MCARpm)/(ncol(ModelVars)))*10)),j] <- NA 
} } else {
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myMCAR[(!is.na(missvar[j])) & myMCAR$"myrandsample" <= ((((pm*MCARpm)/(ncol(ModelVars)))+((pm*MCARpm)/(ncol(ModelVars)))*10)),j] <- NA 
myMARdec[(!is.na(missvar[j])) & myMCAR$"myrandsample" <= ((((pm*MCARpm)/(ncol(ModelVars)))+((pm*MCARpm)/(ncol(ModelVars)))*10)),j] <- NA 
} }
missvar2 <- myMCAR[,c(VarsMiss)]
ModelVars <- matrix(SimSample2[,c(FocusVars)])
d <- ((sum(is.na(missvar2)))/(ncol(ModelVars)*n))
print(d)
for(i in 1:1000) { 		
if(d < (pm*MCARpm)){
if(Attrition == 0) { #prevents missing on the same cases for the selected vars 
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myrandomdata <- matrix(sample(1:1000,100000,rep=TRUE,prob=(rep(.001,1000)))) #1:20 prob=(rep(.05,20)
dimnames(myrandomdata) = list(NULL,c("Student"))
myrandsample <- matrix(myrandomdata[sample(1:nrow(myrandomdata),n,replace=TRUE),])
missvar <- myMARdec[,c(VarsMiss)]
myMCAR[(!is.na(missvar[j])) & (myrandsample) <= ((((pm*MCARpm)/(ncol(ModelVars)))+((pm*MCARpm)/(ncol(ModelVars)))*10)+i),j] <- NA 
myMARdec[(!is.na(missvar[j])) & (myrandsample) <= ((((pm*MCARpm)/(ncol(ModelVars)))+((pm*MCARpm)/(ncol(ModelVars)))*10)+i),j] <- NA 
} } else {
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myMCAR[(!is.na(missvar[j])) & myMCAR$"myrandsample" <= ((((pm*MCARpm)/(ncol(ModelVars)))+((pm*MCARpm)/(ncol(ModelVars)))*10)+i),j] <- NA 
myMARdec[(!is.na(missvar[j])) & myMCAR$"myrandsample" <= ((((pm*MCARpm)/(ncol(ModelVars)))+((pm*MCARpm)/(ncol(ModelVars)))*10)+i),j] <- NA 
} }
missvar2 <- myMCAR[,c(VarsMiss)]
ModelVars <- matrix(SimSample2[,c(FocusVars)])
d <- ((sum(is.na(missvar2)))/(ncol(ModelVars)*n))
    print(d)
}  else { print("desired missingness reached")
} }
if(d < (pm*MCARpm)){
print("Caution: desired MCAR Missing Data Percent not reached")
} else {
print("Requested MCAR Missing Data Percentage was")
print(MCARpm)
print("Which is the following percentage of the Total Missing Data Rate")
print(pm*MCARpm)
print("The observed MCAR Rate of the Total Missing Data Percentage is")
print(d)  
} } else {
ModelVars <- matrix(SimSample2[,c(FocusVars)])
myrandomdata <- matrix(sample(1:1000,100000,rep=TRUE,prob=(rep(.001,1000)))) #1:20 prob=(rep(.05,20)
dimnames(myrandomdata) = list(NULL,c("Student"))
myrandsample <- matrix(myrandomdata[sample(1:nrow(myrandomdata),n,replace=TRUE),])
myMCAR <- cbind(SimSample2, myrandsample)
if(MARpm == 0) {
myMARdec <- myMCAR } else {}
if(Attrition == 0) { #prevents missing on the same cases for the selected vars 
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myrandomdata <- matrix(sample(1:1000,100000,rep=TRUE,prob=(rep(.001,1000)))) #1:20 prob=(rep(.05,20)
dimnames(myrandomdata) = list(NULL,c("Student"))
myrandsample <- matrix(myrandomdata[sample(1:nrow(myrandomdata),n,replace=TRUE),])
missvar <- myMARdec[,c(VarsMiss)]
myMCAR[(!is.na(missvar)) & (myrandsample) <= ((((pm*MCARpm)/(ncol(ModelVars)))+((pm*MCARpm)/(ncol(ModelVars)))*10)),j] <- NA 
myMARdec[(!is.na(missvar)) & (myrandsample) <= ((((pm*MCARpm)/(ncol(ModelVars)))+((pm*MCARpm)/(ncol(ModelVars)))*10)),j] <- NA 
} } else {
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myMCAR[(!is.na(missvar)) & myMCAR$"myrandsample" <= ((((pm*MCARpm)/(ncol(ModelVars)))+((pm*MCARpm)/(ncol(ModelVars)))*10)),j] <- NA 
myMARdec[(!is.na(missvar)) & myMCAR$"myrandsample" <= ((((pm*MCARpm)/(ncol(ModelVars)))+((pm*MCARpm)/(ncol(ModelVars)))*10)),j] <- NA 
} }
missvar2 <- myMCAR[,c(VarsMiss)]
ModelVars <- matrix(SimSample2[,c(FocusVars)])
d <- ((sum(is.na(missvar2)))/(ncol(ModelVars)*n))
print(d)
for(i in 1:1000) { 		
if(d < (pm*MCARpm)){
if(Attrition == 0) { #prevents missing on the same cases for the selected vars 
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myrandomdata <- matrix(sample(1:1000,100000,rep=TRUE,prob=(rep(.001,1000)))) #1:20 prob=(rep(.05,20)
dimnames(myrandomdata) = list(NULL,c("Student"))
myrandsample <- matrix(myrandomdata[sample(1:nrow(myrandomdata),n,replace=TRUE),])
missvar <- myMARdec[,c(VarsMiss)]
myMCAR[(!is.na(missvar)) & (myrandsample) <= ((((pm*MCARpm)/(ncol(ModelVars)))+((pm*MCARpm)/(ncol(ModelVars)))*10)+i),j] <- NA 
myMARdec[(!is.na(missvar)) & (myrandsample) <= ((((pm*MCARpm)/(ncol(ModelVars)))+((pm*MCARpm)/(ncol(ModelVars)))*10)+i),j] <- NA 
} } else {
for(j in (VarsMiss)) { #works through variable list of variables with missing data
myMCAR[(!is.na(missvar)) & myMCAR$"myrandsample" <= ((((pm*MCARpm)/(ncol(ModelVars)))+((pm*MCARpm)/(ncol(ModelVars)))*10)+i),j] <- NA 
myMARdec[(!is.na(missvar)) & myMCAR$"myrandsample" <= ((((pm*MCARpm)/(ncol(ModelVars)))+((pm*MCARpm)/(ncol(ModelVars)))*10)+i),j] <- NA 
} }
missvar2 <- myMCAR[,c(VarsMiss)]
ModelVars <- matrix(SimSample2[,c(FocusVars)])
d <- ((sum(is.na(missvar2)))/(ncol(ModelVars)*n))
    print(d)
}  else { print("desired missingness reached")
} }
if(d < (pm*MCARpm)){
print("Caution: desired MCAR Missing Data Percent not reached")
} else {
print("Requested MCAR Missing Data Percentage was")
print(MCARpm)
print("Which is the following percentage of the Total Missing Data Rate")
print(pm*MCARpm)
print("The observed MCAR Rate of the Total Missing Data Percentage is")
print(d)  
} }
} else { print("No MCAR data requested.") }
#--------------MISSING DATA DIAGNOSTICS----------------#
if (pm > 0){
missvar <- myMARdec[,c(VarsMiss)]
ModelVars <- matrix(SimSample2[,c(FocusVars)])
d <- ((sum(is.na(missvar)))/(ncol(ModelVars)*n))
Vmiss <- length(VarsMiss)
Cmiss <- sum(!complete.cases(missvar))
print("Requested TOTAL Missing Data Percentage was")
print(pm)
print("The observed TOTAL Missing Data Percentage is")
print(d) 
print("The number of variables with at least one missing value is")
print(Vmiss) 
print("The number of cases with at least one missing value is")
print(Cmiss) 
} else {
print("No missing data requested.")}
#------------------------------------------------------#	  
#----------GENERATE PRINCIPAL COMPONENTS---------------#
if (useAUX == 1) {
   if (usePCA == 1) { #----use PCA aux method----#
PCAmodel <- princomp(subset(myMARdec, select = c(VarsNOmiss,AUXvars)), cor=TRUE)
PCs <- cbind(myMARdec, PCAmodel$scores)


#------------------------------------------------------#
#----SELECT VARIABLES TO USE IN THE IMPUTATION MODEL---#
myvars <- c(FocusVars, PCkeep)
newdata <- PCs[myvars]
print("Note: PCA auxiliary variable method used") 
                     } else { 
myvars <- c(FocusVars, AUXvars)
newdata <- myMARdec[myvars]
print("Note: Regular auxiliary variable method used") 
                            }
                } 
else { 
newdata <- myMARdec[FocusVars]
print("Note: no auxiliary variables used") 
     }
#------------------------------------------------------#
#-------MULTIPLE IMPUTATION ROUTINE WITH AMELIA--------#
if (pm > 0) { #add in possibility for no missing data     
library(Amelia)
SEED <- round(runif(1)*10000000)
Sample.amelia <- amelia (newdata, m=nimp, emburn=c(1000,1000), p2s=F, seed=SEED)
amelia.data.list1 <- vector("list", nimp) 
amelia.data.list2 <- vector("list", nimp) 
amelia.data.list3 <- vector("list", nimp) 
amelia.data.list4 <- vector("list", nimp) 
for (i in 1:nimp) { # THE ANALYSIS PHASE
  m.data <- Sample.amelia$imputations[[i]] 
  y <- m.data[, 1] # first variable
  x <- m.data[, 2] # second variable
amelia.data.list1[[i]] <- cor(x,y) 
amelia.data.list2[[i]] <- mean(y) 
amelia.data.list3[[i]] <- sd(y)
amelia.data.list4[[i]] <- var(y)
} 
#------------------------------------------------------#
#--COMBINING PARAMETER ESTIAMTES IN THE POOLING PHASE--#
XY.theta <- unlist(amelia.data.list1) 
Z.XY.theta <- 0.5*(log(1+XY.theta)-log(1-XY.theta)) 
               #--TRANSFORM PARAMETER ESTIMATES PRIOR--#
               #--TO COMBINING ESTIMTES USING FISHER'S-#
               #--Z TRANSFORMATION (SEE FISHER, 1915)--#
Z.XY.mean <- mean(Z.XY.theta)
			   #--"Z.XY.mean" AVERAGE CORRELATION ON A-#
               #------------Z SCORE METRIC-------------#
invZ.XY=(exp(2*Z.XY.mean)-1)/(exp(2*Z.XY.mean)+1)
               #--"invZ.XY" TRANSFORMS THE POOLED EST.-#
               #------BACK TO A CORRELATION METRIC-----#
Y.mean <- unlist(amelia.data.list2)
Y.sd <- unlist(amelia.data.list3)
Y.var <- unlist(amelia.data.list4)
Y.MEAN <- mean(unlist(amelia.data.list2))
Y.SD <- mean(unlist(amelia.data.list3)) 
Y.VAR <- mean(unlist(amelia.data.list4)) 			   
#--------------POOLING STANDARD ERRORS-----------------#

XY.corSE <- ((sqrt(1 - (XY.theta^2)))/(sqrt(n-2)))
Y.MEANSE <- ((sqrt(1 - (Y.MEAN^2)))/(sqrt(n-2))) #
#Y.SDSE <- ((sqrt(1 - (Y.SD^2)))/(sqrt(n-2)))     #
#Y.VARSE <- ((sqrt(1 - (Y.VAR^2)))/(sqrt(n-2)))   #

XY.SampVar <- (XY.corSE)^2
			   #---"XY.SampVar" CORRELATION VARIANCE---#
Y.MEAN.SampVar <- (Y.MEANSE)^2   #
#Y.SD.SampVar <- (Y.SDSE)^2       #
#Y.VAR.SampVar <- (Y.VARSE)^2     #

XY.Vw <- mean(XY.SampVar) 
               #-"XY.Vw" IS THE WITHIN IMPUTATION VAR--#
			   #-THIS IS AN ESTIMATE OF SAMPLING VAR---#
			   #-WOULD HAVE RESULTED WITH COMPLETE DATA#		   
Y.MEAN.Vw <- mean(Y.MEAN.SampVar) #
#Y.SD.Vw <- mean(Y.SD.SampVar)     #
#Y.VAR.Vw <- mean(Y.VAR.SampVar)   #			   
			   			   
XY.SampErr <- (XY.theta - invZ.XY)^2
XY.Vb <- sum((XY.SampErr)/((nimp)-1)) 
               #-"XY.Vb" IS THE BETWEEN IMPUTAITON VAR-#
			   #-THIS IS AN ESTIMATE OF MISS DATA SAMP-#
			   #-ERROR FROM FLUXUATING POINT ESTIMATES-#
Y.MEAN.SampErr <- (Y.mean - Y.MEAN)^2           #
Y.MEAN.Vb <- sum((Y.MEAN.SampErr)/((nimp)-1))   #
#Y.SD.SampErr <- (Y.sd - Y.SD)^2                 #
#Y.SD.Vb <- sum((Y.SD.SampErr)/((nimp)-1))       #
#Y.VAR.SampErr <- (Y.var - Y.VAR)^2              # 
#Y.VAR.Vb <- sum((Y.VAR.SampErr)/((nimp)-1))     #
		   
XY.Vt <- (XY.Vw + XY.Vb + (XY.Vb / nimp))
               #-"XY.Vt" IS THE TOTAL SAMPLING VARIANCE#
			   #-THIS IS AN ESTIMATE OF TOTAL VAR------#
Y.MEAN.Vt <- (Y.MEAN.Vw + Y.MEAN.Vb + (Y.MEAN.Vb / nimp)) #
#Y.SD.Vt <- (Y.SD.Vw + Y.SD.Vb + (Y.SD.Vb / nimp))         # 
#Y.VAR.Vt <- (Y.VAR.Vw + Y.VAR.Vb + (Y.VAR.Vb / nimp))	  #		   
			   
XY.SE <- sqrt(XY.Vt)
               #-"XY.SE" ESTIMATE OF THE POOLED STD ERR#
Y.MEAN.SE <- sqrt(Y.MEAN.Vt) #
#Y.SD.SE <- sqrt(Y.SD.Vt)     #
#Y.VAR.SE <- sqrt(Y.VAR.Vt)   #		 	 
			   
#-----------MISSING DATA DIAGNOSTIC TOOLS--------------#		   
FMI <- (((XY.Vb) + (XY.Vb/nimp)) / XY.Vt)
#VDF <- ((nimp-1)*(1+(XY.Vw/(XY.Vb+(XY.Vb/nimp))))^2)
#Vtd <- (1-FMI)*((((n-1)+1))/(((n-1)+3)))*(n-1)
#v1 <- ginv((1/VDF)+(1/Vtd)) 
#FMI1 <- ((XY.Vb+XY.Vb/nimp+2/(VDF+3))/ XY.Vt)
#FMI2 <- (((XY.Vb) + (XY.Vb/nimp) + (2/(v1+3))) / XY.Vt)
               #-"FMI" FRACTION OF MISSING INFORMATION-#
RIV <- (((XY.Vb) + (XY.Vb/nimp)) / XY.Vw)
               #--"RIV" RELATIVE INCREASE IN VARIANCE--#
} #missing data 
   else {print("No missing data to impute.")} 
#------------------------------------------------------#
#----------------DETERMINE THE BIAS--------------------#
XYRawB <- XYcor - invZ.XY 
XYStdB <- ((XYRawB) / (XY.SE*sqrt(n)))
Y.MEANRawB <- 0 - Y.MEAN
Y.MEANStdB <- ((Y.MEANRawB) / (Y.MEAN.SE*sqrt(n)))
#Y.VARRawB <- 1 - Y.VAR
#Y.VARStdB <- ((Y.VARRawB) / (Y.VAR.SE*sqrt(n)))
#------------------------------------------------------#

missingpercent.data.list1[[m]] <- invZ.XY
missingpercent.data.list2[[m]] <- Y.MEAN
missingpercent.data.list3[[m]] <- Y.VAR
missingpercent.data.list4[[m]] <- XYRawB
missingpercent.data.list5[[m]] <- XYStdB
missingpercent.data.list6[[m]] <- Y.MEANRawB
missingpercent.data.list7[[m]] <- Y.MEANStdB
missingpercent.data.list8[[m]] <- XY.SE
#missingpercent.data.list9[[m]] <- Y.VARStdB
#missingpercent.data.list10[[m]] <- 

  } #percent missing <- TIER 3

samplesize.data.list1[[n]] <- matrix(missingpercent.data.list1) 
samplesize.data.list2[[n]] <- matrix(missingpercent.data.list2)
samplesize.data.list3[[n]] <- matrix(missingpercent.data.list3)
samplesize.data.list4[[n]] <- matrix(missingpercent.data.list4)
samplesize.data.list5[[n]] <- matrix(missingpercent.data.list5)
samplesize.data.list6[[n]] <- matrix(missingpercent.data.list6)
samplesize.data.list7[[n]] <- matrix(missingpercent.data.list7)
samplesize.data.list8[[n]] <- matrix(missingpercent.data.list8)
#samplesize.data.list9[[n]] <- matrix(missingpercent.data.list9)
#samplesize.data.list10[[n]] <- matrix(missingpercent.data.list10)  
  
    }      #samplesize <- TIER 2

iteration.data.list1[[a]] <- matrix(samplesize.data.list1)
iteration.data.list2[[a]] <- matrix(samplesize.data.list2)
iteration.data.list3[[a]] <- matrix(samplesize.data.list3)
iteration.data.list4[[a]] <- matrix(samplesize.data.list4)
iteration.data.list5[[a]] <- matrix(samplesize.data.list5)
iteration.data.list6[[a]] <- matrix(samplesize.data.list6) 
iteration.data.list7[[a]] <- matrix(samplesize.data.list7) 
iteration.data.list8[[a]] <- matrix(samplesize.data.list8) 
#iteration.data.list9[[a]] <- matrix(samplesize.data.list9) 
#iteration.data.list10[[a]] <- matrix(samplesize.data.list10)  
 
     }        #iterations <- TIER 1















#------------------------------------------------------#
#----------------GATHER THE RESULTS--------------------#	  

XwithY <- (unlist(as.matrix(iteration.data.list1))) 
MEAN.Y <- (unlist(as.matrix(iteration.data.list2)))
VAR.Y <- (unlist(as.matrix(iteration.data.list3)))
XwithYRbias <- (unlist(as.matrix(iteration.data.list4)))
XwithYSbias <- (unlist(as.matrix(iteration.data.list5)))
MEAN.YRbias <- (unlist(as.matrix(iteration.data.list6)))
MEAN.YSbias <- (unlist(as.matrix(iteration.data.list7)))
XY.SE <- (unlist(as.matrix(iteration.data.list8)))
#VAR.YSbias <- (unlist(as.matrix(iteration.data.list9)))
#XwithY <- (unlist(as.matrix(iteration.data.list10)))

	  
niteration <- seq(1,iter,1)
nSEQ <- seq(nmin,nmax,nby)
missSEQ <- seq(nmissmin,nmissmax,nmissby)

sampmatid <- cbind(rep(c(nSEQ), each=((length(missSEQ))),times=(length(niteration))))
missmatid <- cbind(rep(c(missSEQ),times=(length(niteration)*length(nSEQ))))
itermatid <- cbind(rep(c(niteration), each=(length(missSEQ)*(length(nSEQ)))))

results <- cbind(itermatid, sampmatid, missmatid, XwithY, MEAN.Y, VAR.Y, XwithYRbias, XwithYSbias)
colnames(results) <- c("iter","N","Miss","correl", "Ymean", "Yvar", "Rbias", "Sbias") 	  


test<-colMeans(results)


newdataCORR <- results[ which(results[,3]==3 & results[,2] ==400), select=("Sbias")]
myCORR<-mean(newdataCORR)

############################
newdataCORR <- results[ which(results[,3]==3 & results[,2] ==400), select=("correl")]

myMEAN <- mean(newdataCORR)

write.table(SimSample2,"C:\\Users\\Student\\Desktop\\simsample2.xls", row.names=F,sep="\t") 	
write.table(myCORR,"C:\\Users\\Student\\Desktop\\corrtable.xls", row.names=F,sep="\t") 	 
	 
write.table(results,"C:\\Users\\Student.HOME\\Desktop\\test.xls", row.names=F,sep="\t") 
	 
	 
cor(SimSample2$y1, SimSample2$y2)
	  

