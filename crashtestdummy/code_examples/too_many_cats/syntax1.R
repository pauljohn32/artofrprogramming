## Writes Mplus code to generate the data ##
## then runs the Mplus code it generates  ##
#------------------Clear All--------------------------#
rm(list=ls())
#---------------SPECIFICATIONS------------------------#
# Specify root directory and location of mplus; use double slashes; leave end open
dirroot <- "D:\\Users\\username\\Desktop\\simulation"   #where to place data,etc.
iter=1000                      #how many iterations per condition
set.seed(7913025)                  # set random seed
#--------------END SPECIFICATIONS---------------------#
#n per cluster sample size
for (perclust in c(100)) {
#number of clusters - later for MLM application
	for (nclust in c(1) ){
#common correlation
		for (setcorr in c(1:8) ){
			if(setcorr == 1){
				corr.10 <-1
				corr.20 <-0
				corr.30 <-0
				corr.40 <-0
				corr.50 <-0
				corr.60 <-0
				corr.70 <-0
				corr.80 <-0
				}
			if(setcorr == 2){
				corr.10 <-1
				corr.20 <-1
				corr.30 <-0
				corr.40 <-0
				corr.50 <-0
				corr.60 <-0
				corr.70 <-0
				corr.80 <-0
				}
			if(setcorr == 3){
				corr.10 <-1
				corr.20 <-1
				corr.30 <-1
				corr.40 <-0
				corr.50 <-0
				corr.60 <-0
				corr.70 <-0
				corr.80 <-0
				}
			if(setcorr == 4){
				corr.10 <-1
				corr.20 <-1
				corr.30 <-1
				corr.40 <-1
				corr.50 <-0
				corr.60 <-0
				corr.70 <-0
				corr.80 <-0
				}
			if(setcorr == 5){
				corr.10 <-1
				corr.20 <-1
				corr.30 <-1
				corr.40 <-1
				corr.50 <-1
				corr.60 <-0
				corr.70 <-0
				corr.80 <-0
				}
			if(setcorr == 6){
				corr.10 <-1
				corr.20 <-1
				corr.30 <-1
				corr.40 <-1
				corr.50 <-1
				corr.60 <-1
				corr.70 <-0
				corr.80 <-0
				}
			if(setcorr == 7){
				corr.10 <-1
				corr.20 <-1
				corr.30 <-1
				corr.40 <-1
				corr.50 <-1
				corr.60 <-1
				corr.70 <-1
				corr.80 <-0
				}
			if(setcorr == 8){
				corr.10 <-1
				corr.20 <-1
				corr.30 <-1
				corr.40 <-1
				corr.50 <-1
				corr.60 <-1
				corr.70 <-1
				corr.80 <-1
				}

#missing pattern
		for (setpattern in c(1:4) ){
			if(setpattern == 1){
				mcar <-1
				mar  <-0
				mnar <-0
				}
			if(setpattern == 2){
				mcar <-0
				mar  <-1
				mnar <-0
				}
			if(setpattern == 3){
				mcar <-0
				mar  <-0
				mnar <-1
				}
			if(setpattern == 4){
				mcar <-0
				mar  <-1
				mnar <-1
				}

#percent missing
		for (percentmiss in c(1:6) ){
			if(percentmiss == 1){
				miss.10 <-1
				miss.20 <-0
				miss.30 <-0
				miss.40 <-0
				miss.50 <-0
				miss.60 <-0
				miss.70 <-0
				}
			if(percentmiss == 2){
				miss.10 <-1
				miss.20 <-1
				miss.30 <-0
				miss.40 <-0
				miss.50 <-0
				miss.60 <-0
				miss.70 <-0
				}
			if(percentmiss == 3){
				miss.10 <-1
				miss.20 <-1
				miss.30 <-1
				miss.40 <-0
				miss.50 <-0
				miss.60 <-0
				miss.70 <-0
				}
			if(percentmiss == 4){
				miss.10 <-1
				miss.20 <-1
				miss.30 <-1
				miss.40 <-1
				miss.50 <-0
				miss.60 <-0
				miss.70 <-0
				}
			if(percentmiss == 5){
				miss.10 <-1
				miss.20 <-1
				miss.30 <-1
				miss.40 <-1
				miss.50 <-1
				miss.60 <-0
				miss.70 <-0
				}
			if(percentmiss == 6){
				miss.10 <-1
				miss.20 <-1
				miss.30 <-1
				miss.40 <-1
				miss.50 <-1
				miss.60 <-1
				miss.70 <-0
				}

#aux method
		for (aux in c(1:3) ){
			if(aux == 1){
				var <-1
				pca <-0
				}
			if(aux == 2){
				var <-0
				pca <-1
				}
            if(aux == 3){
				var <-0
				pca <-0
				}

#number of auxiliary variables
		for (auxnumber in c(1:7) ){
			if(auxnumber == 1){
				aux.1 <-1
				aux.2 <-0
				aux.3 <-0
				aux.4 <-0
				aux.5 <-0
				aux.6 <-0
				aux.7 <-0
				}
			if(auxnumber == 2){
				aux.1 <-1
				aux.2 <-1
				aux.3 <-0
				aux.4 <-0
				aux.5 <-0
				aux.6 <-0
				aux.7 <-0
				}
			if(auxnumber == 3){
				aux.1 <-1
				aux.2 <-1
				aux.3 <-1
				aux.4 <-0
				aux.5 <-0
				aux.6 <-0
				aux.7 <-0
				}
			if(auxnumber == 4){
				aux.1 <-1
				aux.2 <-1
				aux.3 <-1
				aux.4 <-1
				aux.5 <-0
				aux.6 <-0
				aux.7 <-0
				}
			if(auxnumber == 5){
				aux.1 <-1
				aux.2 <-1
				aux.3 <-1
				aux.4 <-1
				aux.5 <-1
				aux.6 <-0
				aux.7 <-0
				}
			if(auxnumber == 6){
				aux.1 <-1
				aux.2 <-1
				aux.3 <-1
				aux.4 <-1
				aux.5 <-1
				aux.6 <-1
				aux.7 <-0
				}
			if(auxnumber == 7){
				aux.1 <-1
				aux.2 <-1
				aux.3 <-1
				aux.4 <-1
				aux.5 <-1
				aux.6 <-1
				aux.7 <-1
				}

# CREATE DIRECTORIES
path <- paste(dirroot,perclust,nclust,setcorr,setpattern,percentmiss,aux,auxnumber,sep="\\")
shell (paste("mkdir", path, sep=" "))
#--------------seperate missing data mechanisms in SAS--------------------------------#
##########################################
#       WRITE GENERATION CODE            #
##########################################

pathGEN <- paste(dirroot,perclust,nclust,setcorr,sep="\\")
gen <- paste(pathGEN,"generatecorrdata.inp",sep="\\")
testdata1 <- paste(pathGEN,"data1.dat",sep="\\")

cat('MONTECARLO: \n', file=gen)
cat('NAMES ARE x y q1-q8; \n', file=gen, append=T)
cat('NOBSERVATIONS = 1000 ; \n', file=gen, append=T)
cat(' NREPS = ',iter,'; \n', file=gen, append=T)
cat(' SEED = ',round(runif(1)*10000000), '; \n', file=gen, append=T)
cat(' REPSAVE=ALL; \n', file=gen, append=T)
cat(' SAVE=\n', pathGEN,'\\data*.dat; \n', file=gen, append=T, sep="")
cat('MODEL POPULATION: \n', file=gen, append=T)
cat('[q1-q8*0 x*0 y*0]; \n', file=gen, append=T)
cat('q1-q8*1; x*1; y*1;  \n', file=gen, append=T)
cat('q1-q8 with q1-q8*.50;  \n', file=gen, append=T)
cat('x with y*.50; \n', file=gen, append=T)
cat('x with q1-q8*.50; \n', file=gen, append=T)
cat('y with q1-q8*', .0+(corr.10*(.10+corr.20*.10+corr.30*.10+corr.40*+corr.40*.10+corr.50*.10+corr.60*.10+corr.70*.10+corr.80*.10)), '; \n', file=gen, append=T, sep="")

if(file.exists(testdata1)){
} else {
shell (paste("mplus.exe",gen, paste(pathGEN,"save1.out", sep="\\"), sep=" "))
       }

#----------------------------------------------------------------------------------#

##########################################
#          GENERATE SAS CODE             #
##########################################
pathSAS <- paste(dirroot,perclust,nclust,setcorr,setpattern,percentmiss,aux,auxnumber,sep="\\")
pathSASdata <- paste(dirroot,perclust,nclust,setcorr,setpattern,percentmiss,sep="\\")
smcarmiss <- paste(pathSAS,"modifydata.sas",sep="\\")
testdata2 <- paste(pathSASdata,"data1.dat",sep="\\")

#-------------------------------------------------------#
#------------- import SIM data into SAS ----------------#
#-------------------------------------------------------#

if(file.exists(testdata2)){
} else {

cat('proc printto \n', file=smcarmiss, append=T)
cat('log="R:\\users\\username\\data\\simLOG2\\LOGLOG.log" \n', file=smcarmiss, append=T)
cat('print="R:\\users\\username\\data\\simLOG2\\LSTLST.lst" \n', file=smcarmiss, append=T)
cat('new; \n', file=smcarmiss, append=T)
cat('run; \n', file=smcarmiss, append=T)
cat(' \n', file=smcarmiss, append=T)

cat('%macro importMPLUS; \n', file=smcarmiss, append=T)
cat('%do i=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('data work.data&i; \n', file=smcarmiss, append=T)
cat('infile ', file=smcarmiss, append=T)
cat('"', file=smcarmiss, append=T)
cat(paste(pathGEN,"data&i..dat",sep="\\") , file=smcarmiss, append=T)
cat('" ; \n', file=smcarmiss, append=T)
cat('INPUT x y q1 q2 q3 q4 q5 q6 q7 q8; /*<---insert variables here*/ \n', file=smcarmiss, append=T)
cat('RUN; \n', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%importMPLUS \n', file=smcarmiss, append=T)
cat(' \n', file=smcarmiss, append=T)

##########################################
#     Draw random sample of size N       #
##########################################
cat('%macro samplesize; \n', file=smcarmiss, append=T)
cat('%do i=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('Proc surveyselect data=work.data&i out=work.sampledata&i method=SRS \n', file=smcarmiss, append=T)
if(perclust == 50) {
cat('sampsize=50 \n', file=smcarmiss, append=T)
}
else if(perclust == 75) {
cat('sampsize=75 \n', file=smcarmiss, append=T)
}
else if(perclust == 100) {
cat('sampsize=100 \n', file=smcarmiss, append=T)
}
else if(perclust == 200) {
cat('sampsize=200 \n', file=smcarmiss, append=T)
}
else if(perclust == 400) {
cat('sampsize=400 \n', file=smcarmiss, append=T)
}
else if(perclust == 800) {
cat('sampsize=800 \n', file=smcarmiss, append=T)
}
else if(perclust == 1000) {
cat('sampsize=1000 \n', file=smcarmiss, append=T)
}
cat('SEED = ',round(runif(1)*10000000), '; \n', file=smcarmiss, append=T)
cat('RUN; \n', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%samplesize \n', file=smcarmiss, append=T)
cat(' \n', file=smcarmiss, append=T)

#------------------------------------------------------------#
#---------Export sample data for later use-------------------#
#------------------------------------------------------------#
#cat('LIBNAME mysim "R:\\users\\username\\data\\mysim" ;  \n', file=smcarmiss, append=T)
#cat('%macro samplesave; \n', file=smcarmiss, append=T)
#cat('%do i=1 %to ', file=smcarmiss, append=T)
#cat(paste(iter) , file=smcarmiss, append=T)
#cat('; \n', file=smcarmiss, append=T)
#cat('data mysim.sampledata&i ; set work.sampledata&i; \n', file=smcarmiss, append=T)
#cat('RUN; \n', file=smcarmiss, append=T)
#cat('%end; \n', file=smcarmiss, append=T)
#cat('%mend; \n', file=smcarmiss, append=T)
#cat('%samplesave \n', file=smcarmiss, append=T)


##################################################
#              interaction  MACRO                #
##################################################

cat(' \n', file=smcarmiss, append=T)
cat('%MACRO interact(vars, quadr = 1, prefix = INT); \n', file=smcarmiss, append=T)
cat('%LET c=1; \n ', file=smcarmiss, append=T)
cat('%DO %WHILE(%SCAN(&vars,&c) NE); \n' , file=smcarmiss, append=T)
cat('%LET c=%EVAL(&c+1); \n', file=smcarmiss, append=T)
cat('%END; \n', file=smcarmiss, append=T)
cat('%LET nvars=%EVAL(&c-1); \n', file=smcarmiss, append=T)
cat('%DO i = 1 %TO &nvars; \n', file=smcarmiss, append=T)
cat('%DO j = %EVAL(&i+1-&quadr) %TO &nvars; \n', file=smcarmiss, append=T)
cat('&prefix._%SCAN(&vars,&i)_%SCAN(&vars,&j) =  \n', file=smcarmiss, append=T)
cat('%SCAN(&vars,&i) * %SCAN(&vars,&j); \n', file=smcarmiss, append=T)
cat('%END; \n', file=smcarmiss, append=T)
cat('%END; \n', file=smcarmiss, append=T)
cat('%MEND; \n', file=smcarmiss, append=T)
cat(' \n', file=smcarmiss, append=T)

#------------------------------------------------------------#
#-------------------call MACRO-------------------------------#
#------------------------------------------------------------#
cat('%macro inter; \n', file=smcarmiss, append=T)
cat('%do k=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('data interdata&k; SET work.sampledata&k; \n', file=smcarmiss, append=T)
cat('%INTERACT(q1 q2,quadr=1); \n', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('RUN; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%inter \n', file=smcarmiss, append=T)

#--------------------------------------------------------------#
#---------------------SET P(miss)------------------------------#
#--------------------------------------------------------------#
cat(' \n', file=smcarmiss, append=T)
cat('proc format; \n', file=smcarmiss, append=T)
cat('value username 0.00-0.10 = 1 \n', file=smcarmiss, append=T)
cat('0.10<-0.20 = 2 \n', file=smcarmiss, append=T)
cat('0.20<-0.30 = 3 \n', file=smcarmiss, append=T)
cat('0.30<-0.40 = 4 \n', file=smcarmiss, append=T)
cat('0.40<-0.50 = 5 \n', file=smcarmiss, append=T)
cat('0.50<-0.60 = 6 \n', file=smcarmiss, append=T)
cat('0.60<-0.70 = 7 \n', file=smcarmiss, append=T)
cat('0.70<-0.80 = 8 \n', file=smcarmiss, append=T)
cat('0.80<-0.90 = 9 \n', file=smcarmiss, append=T)
cat('0.90<-1.00 = 10 \n', file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat(' \n', file=smcarmiss, append=T)
cat('data myrandomdata(drop=_:); \n', file=smcarmiss, append=T)
cat('do _i=1 to 1e5; \n', file=smcarmiss, append=T)
cat('random = rantbl(999,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10,0.10); \n', file=smcarmiss, append=T)
cat('output;  \n', file=smcarmiss, append=T)
cat('end; \n', file=smcarmiss, append=T)
cat('run; \n', file=smcarmiss, append=T)

#--------------------------------------------------------------------#
#----------------------MCAR MECHANISM--------------------------------#
#--------------------------------------------------------------------#
if(setpattern == 1) {
cat(' \n', file=smcarmiss, append=T)
cat('%macro MCAR; \n', file=smcarmiss, append=T)
cat('%do i=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('/*MCAR*/ \n', file=smcarmiss, append=T)
cat('Data MCAR&i; set interdata&i;  \n', file=smcarmiss, append=T)
cat('array vars(10) x y q1 q2 q3 q4 q5 q6 q7 q8; \n', file=smcarmiss, append=T)
cat('random=put(ranuni(12345&i),username.); \n', file=smcarmiss, append=T)
cat('drop random; \n', file=smcarmiss, append=T)
cat('if random=', 0+((miss.10*1)), ' then do; vars(2) = .; end; else \n', file=smcarmiss, append=T, sep="")
cat('if random=', 0+((miss.10*1+miss.20*1)), ' then do; vars(2) = .; end; else \n', file=smcarmiss, append=T, sep="")
cat('if random=', 0+((miss.10*1+miss.20*1+miss.30*1)), ' then do; vars(2) = .; end; else \n', file=smcarmiss, append=T, sep="")
cat('if random=', 0+((miss.10*1+miss.20*1+miss.30*1+miss.40*1)), ' then do; vars(2) = .; end; else \n', file=smcarmiss, append=T, sep="")
cat('if random=', 0+((miss.10*1+miss.20*1+miss.30*1+miss.40*1+miss.50*1)), ' then do; vars(2) = .; end; else \n', file=smcarmiss, append=T, sep="")
cat('if random=', 0+((miss.10*1+miss.20*1+miss.30*1+miss.40*1+miss.50*1+miss.60*1)), ' then do; vars(2) = .; end;  \n', file=smcarmiss, append=T, sep="")
cat('RUN; \n', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%MCAR \n', file=smcarmiss, append=T)
##################################################
#           generate PCAs regular                #
##################################################
cat(' \n', file=smcarmiss, append=T)
cat('%macro PCA; \n', file=smcarmiss, append=T)
cat('%do i=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('proc princomp data=MCAR&i out=work.prin&i N=7; \n', file=smcarmiss, append=T)
cat('var q1 q2 q3 q4 q5 q6 q7 q8 ; \n', file=smcarmiss, append=T)
cat('run; \n', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%PCA \n', file=smcarmiss, append=T)
cat(' \n', file=smcarmiss, append=T)
#------------------------------------------------------------#
#-----------------Create datalist file-----------------------#
#------------------------------------------------------------#
cat('%let folder =  ', file=smcarmiss, append=T)
cat(paste(pathSASdata,sep="\\") , file=smcarmiss, append=T)
cat('\\; \n', file=smcarmiss, append=T)
cat('%macro importlist; \n', file=smcarmiss, append=T)
cat('proc iml; \n', file=smcarmiss, append=T)
cat('file ', file=smcarmiss, append=T)
cat('"', file=smcarmiss, append=T)
cat('&folder.datalist.dat', file=smcarmiss, append=T)
cat('" ; \n', file=smcarmiss, append=T)
cat('%do i=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('put ("data&i..dat"); \n', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('closefile ', file=smcarmiss, append=T)
cat('"', file=smcarmiss, append=T)
cat('&folder.datalist.dat', file=smcarmiss, append=T)
cat('" ; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%importlist \n', file=smcarmiss, append=T)
cat('quit; \n', file=smcarmiss, append=T)
cat(' \n', file=smcarmiss, append=T)
#------------------------------------------------------------#
#-------------Convert . to 999 for MPLUS---------------------#
#------------------------------------------------------------#
cat('%macro codemissing; \n', file=smcarmiss, append=T)
cat('%do j=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('/*recode missing number to 999*/  \n', file=smcarmiss, append=T)
cat('data prin&j; set prin&j; \n', file=smcarmiss, append=T)
cat('array vars{*}_numeric_; \n', file=smcarmiss, append=T)
cat('do i = 1 to dim(vars); \n', file=smcarmiss, append=T)
cat('if vars{i} = . then vars{i} = 999; \n', file=smcarmiss, append=T)
cat('end; \n', file=smcarmiss, append=T)
cat('drop i; \n', file=smcarmiss, append=T)
cat('run; \n', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%codemissing \n', file=smcarmiss, append=T)
cat(' \n', file=smcarmiss, append=T)
#------------------------------------------------------------#
#---------Export data with Principal Components--------------#
#------------------------------------------------------------#
cat('%macro exportpca; \n', file=smcarmiss, append=T)
cat('%do i=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('PROC EXPORT DATA=work.prin&i OUTFILE = ', file=smcarmiss, append=T)
cat('"', file=smcarmiss, append=T)
cat(paste(pathSASdata,"data&i..dat",sep="\\") , file=smcarmiss, append=T)
cat('"  \n', file=smcarmiss, append=T)
cat('DBMS= dlm replace;  \n', file=smcarmiss, append=T)
cat('putnames=no; \n', file=smcarmiss, append=T)
cat('RUN; \n', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%exportpca \n', file=smcarmiss, append=T)
}

#--------------------------------------------------------------------#
#------------------------MAR MECHANISM-------------------------------#
#--------------------------------------------------------------------#
else if(setpattern == 2) {
cat(' \n', file=smcarmiss, append=T)
cat('%macro MAR; \n', file=smcarmiss, append=T)
cat('%do i=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('/*MAR*/ \n', file=smcarmiss, append=T)
cat('proc univariate ', file=smcarmiss, append=T)
cat('Data=interdata&i;  \n', file=smcarmiss, append=T)
cat('var q1; \n', file=smcarmiss, append=T)
cat('output out=deciles&i pctlpts=10 20 30 40 50 60 70 80 90 pctlpre=pct; \n', file=smcarmiss, append=T)
cat('run; \n', file=smcarmiss, append=T)
cat(' \n', file=smcarmiss, append=T)
cat('/* write the cutpoints to macro variables */ \n', file=smcarmiss, append=T)
cat('data _null_; \n', file=smcarmiss, append=T)
cat('set deciles&i; \n', file=smcarmiss, append=T)
cat('call symput("qu1" ,pct10) ; \n', file=smcarmiss, append=T)
cat('call symput("qu2" ,pct20) ; \n', file=smcarmiss, append=T)
cat('call symput("qu3" ,pct30) ; \n', file=smcarmiss, append=T)
cat('call symput("qu4" ,pct40) ; \n', file=smcarmiss, append=T)
cat('call symput("qu5" ,pct50) ; \n', file=smcarmiss, append=T)
cat('call symput("qu6" ,pct60) ; \n', file=smcarmiss, append=T)
cat('call symput("qu7" ,pct70) ; \n', file=smcarmiss, append=T)
cat('call symput("qu8" ,pct80) ; \n', file=smcarmiss, append=T)
cat('call symput("qu9" ,pct90) ; \n', file=smcarmiss, append=T)
cat('run; \n', file=smcarmiss, append=T)
cat('Data MAR&i; set interdata&i;  \n', file=smcarmiss, append=T)
cat('array vars(10) x y q1 q2 q3 q4 q5 q6 q7 q8; \n', file=smcarmiss, append=T)
cat('if &qu', 0+((miss.10*1+miss.20*1+miss.30*1+miss.40*1+miss.50*1+miss.60*1)),' >= Q1 then do; vars(2) = .; end; ', file=smcarmiss, append=T, sep="")
cat('RUN;', file=smcarmiss, append=T)
cat('\n ', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%MAR \n', file=smcarmiss, append=T)
##################################################
#           generate PCAs regular                #
##################################################
cat(' \n', file=smcarmiss, append=T)
cat('%macro PCA; \n', file=smcarmiss, append=T)
cat('%do i=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('proc princomp data=MAR&i out=work.prin&i N=7; \n', file=smcarmiss, append=T)
cat('var q1 q2 q3 q4 q5 q6 q7 q8 ; \n', file=smcarmiss, append=T)
cat('run; \n', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%PCA \n', file=smcarmiss, append=T)
cat(' \n', file=smcarmiss, append=T)
#------------------------------------------------------------#
#-----------------Create datalist file-----------------------#
#------------------------------------------------------------#
cat('%let folder =  ', file=smcarmiss, append=T)
cat(paste(pathSASdata,sep="\\") , file=smcarmiss, append=T)
cat('\\; \n', file=smcarmiss, append=T)
cat('%macro importlist; \n', file=smcarmiss, append=T)
cat('proc iml; \n', file=smcarmiss, append=T)
cat('file ', file=smcarmiss, append=T)
cat('"', file=smcarmiss, append=T)
cat('&folder.datalist.dat', file=smcarmiss, append=T)
cat('" ; \n', file=smcarmiss, append=T)
cat('%do i=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('put ("data&i..dat"); \n', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('closefile ', file=smcarmiss, append=T)
cat('"', file=smcarmiss, append=T)
cat('&folder.datalist.dat', file=smcarmiss, append=T)
cat('" ; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%importlist \n', file=smcarmiss, append=T)
cat('quit; \n', file=smcarmiss, append=T)
cat(' \n', file=smcarmiss, append=T)
#------------------------------------------------------------#
#-------------Convert . to 999 for MPLUS---------------------#
#------------------------------------------------------------#
cat('%macro codemissing; \n', file=smcarmiss, append=T)
cat('%do j=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('/*recode missing number to 999*/  \n', file=smcarmiss, append=T)
cat('data prin&j; set prin&j; \n', file=smcarmiss, append=T)
cat('array vars{*}_numeric_; \n', file=smcarmiss, append=T)
cat('do i = 1 to dim(vars); \n', file=smcarmiss, append=T)
cat('if vars{i} = . then vars{i} = 999; \n', file=smcarmiss, append=T)
cat('end; \n', file=smcarmiss, append=T)
cat('drop i; \n', file=smcarmiss, append=T)
cat('run; \n', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%codemissing \n', file=smcarmiss, append=T)
cat(' \n', file=smcarmiss, append=T)
#------------------------------------------------------------#
#---------Export data with Principal Components--------------#
#------------------------------------------------------------#
cat('%macro exportpca; \n', file=smcarmiss, append=T)
cat('%do i=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('PROC EXPORT DATA=work.prin&i OUTFILE = ', file=smcarmiss, append=T)
cat('"', file=smcarmiss, append=T)
cat(paste(pathSASdata,"data&i..dat",sep="\\") , file=smcarmiss, append=T)
cat('"  \n', file=smcarmiss, append=T)
cat('DBMS= dlm replace;  \n', file=smcarmiss, append=T)
cat('putnames=no; \n', file=smcarmiss, append=T)
cat('RUN; \n', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%exportpca \n', file=smcarmiss, append=T)
}

#--------------------------------------------------------------------#
#------------------------MNAR MECHANISM------------------------------#
#--------------------------------------------------------------------#
else if(setpattern == 3) {
cat(' \n', file=smcarmiss, append=T)
cat('%macro MNAR; \n', file=smcarmiss, append=T)
cat('%do i=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('/*MAR*/ \n', file=smcarmiss, append=T)
cat('proc univariate ', file=smcarmiss, append=T)
cat('Data=interdata&i;  \n', file=smcarmiss, append=T)
cat('var y; \n', file=smcarmiss, append=T)
cat('output out=deciles&i pctlpts=10 20 30 40 50 60 70 80 90 pctlpre=pct; \n', file=smcarmiss, append=T)
cat('run; \n', file=smcarmiss, append=T)
cat(' \n', file=smcarmiss, append=T)
cat('/* write the cutpoints to macro variables */ \n', file=smcarmiss, append=T)
cat('data _null_; \n', file=smcarmiss, append=T)
cat('set deciles&i; \n', file=smcarmiss, append=T)
cat('call symput("qu1" ,pct10) ; \n', file=smcarmiss, append=T)
cat('call symput("qu2" ,pct20) ; \n', file=smcarmiss, append=T)
cat('call symput("qu3" ,pct30) ; \n', file=smcarmiss, append=T)
cat('call symput("qu4" ,pct40) ; \n', file=smcarmiss, append=T)
cat('call symput("qu5" ,pct50) ; \n', file=smcarmiss, append=T)
cat('call symput("qu6" ,pct60) ; \n', file=smcarmiss, append=T)
cat('call symput("qu7" ,pct70) ; \n', file=smcarmiss, append=T)
cat('call symput("qu8" ,pct80) ; \n', file=smcarmiss, append=T)
cat('call symput("qu9" ,pct90) ; \n', file=smcarmiss, append=T)
cat('run; \n', file=smcarmiss, append=T)
cat('Data MNAR&i; set interdata&i;  \n', file=smcarmiss, append=T)
cat('array vars(10) x y q1 q2 q3 q4 q5 q6 q7 q8; \n', file=smcarmiss, append=T)
cat('if &qu', 0+((miss.10*1+miss.20*1+miss.30*1+miss.40*1+miss.50*1+miss.60*1)),' >= y then do; vars(2) = .; end; ', file=smcarmiss, append=T, sep="")
cat('RUN;', file=smcarmiss, append=T)
cat('\n ', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%MNAR \n', file=smcarmiss, append=T)
##################################################
#           generate PCAs regular                #
##################################################
cat(' \n', file=smcarmiss, append=T)
cat('%macro PCA; \n', file=smcarmiss, append=T)
cat('%do i=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('proc princomp data=MNAR&i out=work.prin&i N=7; \n', file=smcarmiss, append=T)
cat('var q1 q2 q3 q4 q5 q6 q7 q8 ; \n', file=smcarmiss, append=T)
cat('run; \n', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%PCA \n', file=smcarmiss, append=T)
cat(' \n', file=smcarmiss, append=T)
#------------------------------------------------------------#
#-----------------Create datalist file-----------------------#
#------------------------------------------------------------#
cat('%let folder =  ', file=smcarmiss, append=T)
cat(paste(pathSASdata,sep="\\") , file=smcarmiss, append=T)
cat('\\; \n', file=smcarmiss, append=T)
cat('%macro importlist; \n', file=smcarmiss, append=T)
cat('proc iml; \n', file=smcarmiss, append=T)
cat('file ', file=smcarmiss, append=T)
cat('"', file=smcarmiss, append=T)
cat('&folder.datalist.dat', file=smcarmiss, append=T)
cat('" ; \n', file=smcarmiss, append=T)
cat('%do i=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('put ("data&i..dat"); \n', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('closefile ', file=smcarmiss, append=T)
cat('"', file=smcarmiss, append=T)
cat('&folder.datalist.dat', file=smcarmiss, append=T)
cat('" ; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%importlist \n', file=smcarmiss, append=T)
cat('quit; \n', file=smcarmiss, append=T)
cat(' \n', file=smcarmiss, append=T)
#------------------------------------------------------------#
#-------------Convert . to 999 for MPLUS---------------------#
#------------------------------------------------------------#
cat('%macro codemissing; \n', file=smcarmiss, append=T)
cat('%do j=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('/*recode missing number to 999*/  \n', file=smcarmiss, append=T)
cat('data prin&j; set prin&j; \n', file=smcarmiss, append=T)
cat('array vars{*}_numeric_; \n', file=smcarmiss, append=T)
cat('do i = 1 to dim(vars); \n', file=smcarmiss, append=T)
cat('if vars{i} = . then vars{i} = 999; \n', file=smcarmiss, append=T)
cat('end; \n', file=smcarmiss, append=T)
cat('drop i; \n', file=smcarmiss, append=T)
cat('run; \n', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%codemissing \n', file=smcarmiss, append=T)
cat(' \n', file=smcarmiss, append=T)
#------------------------------------------------------------#
#---------Export data with Principal Components--------------#
#------------------------------------------------------------#
cat('%macro exportpca; \n', file=smcarmiss, append=T)
cat('%do i=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('PROC EXPORT DATA=work.prin&i OUTFILE = ', file=smcarmiss, append=T)
cat('"', file=smcarmiss, append=T)
cat(paste(pathSASdata,"data&i..dat",sep="\\") , file=smcarmiss, append=T)
cat('"  \n', file=smcarmiss, append=T)
cat('DBMS= dlm replace;  \n', file=smcarmiss, append=T)
cat('putnames=no; \n', file=smcarmiss, append=T)
cat('RUN; \n', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%exportpca \n', file=smcarmiss, append=T)
}

#--------------------------------------------------------------------#
#-----------------nonlinear MAR MECHANISM----------------------------#
#--------------------------------------------------------------------#
else if(setpattern == 4) {
cat(' \n', file=smcarmiss, append=T)
cat('%macro nonlinearMAR; \n', file=smcarmiss, append=T)
cat('%do i=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('/*MAR*/ \n', file=smcarmiss, append=T)
cat('proc univariate ', file=smcarmiss, append=T)
cat('Data=interdata&i;  \n', file=smcarmiss, append=T)
cat('var INT_q1_q2; \n', file=smcarmiss, append=T)
cat('output out=deciles&i pctlpts=10 20 30 40 50 60 70 80 90 pctlpre=pct; \n', file=smcarmiss, append=T)
cat('run; \n', file=smcarmiss, append=T)
cat(' \n', file=smcarmiss, append=T)
cat('/* write the cutpoints to macro variables */ \n', file=smcarmiss, append=T)
cat('data _null_; \n', file=smcarmiss, append=T)
cat('set deciles&i; \n', file=smcarmiss, append=T)
cat('call symput("qu1" ,pct10) ; \n', file=smcarmiss, append=T)
cat('call symput("qu2" ,pct20) ; \n', file=smcarmiss, append=T)
cat('call symput("qu3" ,pct30) ; \n', file=smcarmiss, append=T)
cat('call symput("qu4" ,pct40) ; \n', file=smcarmiss, append=T)
cat('call symput("qu5" ,pct50) ; \n', file=smcarmiss, append=T)
cat('call symput("qu6" ,pct60) ; \n', file=smcarmiss, append=T)
cat('call symput("qu7" ,pct70) ; \n', file=smcarmiss, append=T)
cat('call symput("qu8" ,pct80) ; \n', file=smcarmiss, append=T)
cat('call symput("qu9" ,pct90) ; \n', file=smcarmiss, append=T)
cat('run; \n', file=smcarmiss, append=T)
cat('Data MARnon&i; set interdata&i;  \n', file=smcarmiss, append=T)
cat('array vars(10) x y q1 q2 q3 q4 q5 q6 q7 q8; \n', file=smcarmiss, append=T)
cat('if &qu', 0+((miss.10*1+miss.20*1+miss.30*1+miss.40*1+miss.50*1+miss.60*1)),' >= INT_q1_q2 then do; vars(2) = .; end; ', file=smcarmiss, append=T, sep="")
cat('RUN;', file=smcarmiss, append=T)
cat('\n ', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%nonlinearMAR \n', file=smcarmiss, append=T)
##################################################
############ generate PCAs interaction ###########
##################################################
cat(' \n', file=smcarmiss, append=T)
cat('%macro PCA; \n', file=smcarmiss, append=T)
cat('%do i=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('proc princomp data=MARnon&i out=prin&i N=7; \n', file=smcarmiss, append=T)
cat('var q1 q2 q3 q4 q5 q6 q7 q8
INT_q1_q1
INT_q1_q2
INT_q2_q2
; \n', file=smcarmiss, append=T)
cat('run; \n', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%PCA \n', file=smcarmiss, append=T)
#------------------------------------------------------------#
#-----------------Create datalist file-----------------------#
#------------------------------------------------------------#
cat('%let folder =  ', file=smcarmiss, append=T)
cat(paste(pathSASdata,sep="\\") , file=smcarmiss, append=T)
cat('\\; \n', file=smcarmiss, append=T)
cat('%macro importlist; \n', file=smcarmiss, append=T)
cat('proc iml; \n', file=smcarmiss, append=T)
cat('file ', file=smcarmiss, append=T)
cat('"', file=smcarmiss, append=T)
cat('&folder.datalist.dat', file=smcarmiss, append=T)
cat('" ; \n', file=smcarmiss, append=T)
cat('%do i=1 %to ', file=smcarmiss, append=T)
cat(paste(iter), file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('put ("data&i..dat"); \n', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('closefile ', file=smcarmiss, append=T)
cat('"', file=smcarmiss, append=T)
cat('&folder.datalist.dat', file=smcarmiss, append=T)
cat('" ; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%importlist \n', file=smcarmiss, append=T)
cat('quit; \n', file=smcarmiss, append=T)
cat(' \n', file=smcarmiss, append=T)
#------------------------------------------------------------#
#-------------Convert . to 999 for MPLUS---------------------#
#------------------------------------------------------------#
cat('%macro codemissing; \n', file=smcarmiss, append=T)
cat('%do j=1 %to ', file=smcarmiss, append=T)
cat(paste(iter), file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('/*recode missing number to 999*/  \n', file=smcarmiss, append=T)
cat('data prin&j; set prin&j; \n', file=smcarmiss, append=T)
cat('array vars{*}_numeric_; \n', file=smcarmiss, append=T)
cat('do i = 1 to dim(vars); \n', file=smcarmiss, append=T)
cat('if vars{i} = . then vars{i} = 999; \n', file=smcarmiss, append=T)
cat('end; \n', file=smcarmiss, append=T)
cat('drop i; \n', file=smcarmiss, append=T)
cat('run; \n', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%codemissing \n', file=smcarmiss, append=T)
cat(' \n', file=smcarmiss, append=T)
#------------------------------------------------------------#
#---------Export data with Principal Components--------------#
#------------------------------------------------------------#
cat('%macro exportpca; \n', file=smcarmiss, append=T)
cat('%do i=1 %to ', file=smcarmiss, append=T)
cat(paste(iter) , file=smcarmiss, append=T)
cat('; \n', file=smcarmiss, append=T)
cat('PROC EXPORT DATA=work.prin&i OUTFILE = ', file=smcarmiss, append=T)
cat('"', file=smcarmiss, append=T)
cat(paste(pathSASdata,"data&i..dat",sep="\\") , file=smcarmiss, append=T)
cat('"  \n', file=smcarmiss, append=T)
cat('DBMS= dlm replace;  \n', file=smcarmiss, append=T)
cat('putnames=no; \n', file=smcarmiss, append=T)
cat('RUN; \n', file=smcarmiss, append=T)
cat('%end; \n', file=smcarmiss, append=T)
cat('%mend; \n', file=smcarmiss, append=T)
cat('%exportpca \n', file=smcarmiss, append=T)
}

##########################################
#        RUN SAS code in all files       #
##########################################
#shell (paste("sas.exe",smcarmiss))

SASexe <- paste('"C:\\Program Files\\SASHome\\SASFoundation\\9.3\\sas.exe"')
shell (paste(SASexe,smcarmiss))

}

#----------------------------------------------------------------------------------#
##########################################
#       WRITE analysis CODE              #
##########################################
#----------------------------------------------------------------------#
#------code writes MPLUS Monte Carlo code to "Path" directory----------#
#----------------------------------------------------------------------#
#-------------data drawn from "PERCENTMISS" folder---------------------#
#----------------------------------------------------------------------#
pathsample <- paste(dirroot,perclust,nclust,setcorr,setpattern,percentmiss,sep="\\")

ana <- paste(path,"analyze.inp",sep="\\")
cat('DATA: FILE = ', file=ana, append=T)
cat(paste(pathsample,"datalist.dat ; ",sep="\\"), file=ana, append=T)
cat(' \n', file=ana, append=T)
cat('TYPE = MONTECARLO; \n', file=ana, append=T)
cat('VARIABLE: \n', file=ana, append=T)
cat('NAMES = x y q1-q8 inter1 inter2 inter3 prin1-prin7; \n', file=ana, append=T)
cat('USEVARIABLES = x y; \n', file=ana, append=T)
cat('MISSING = ALL (999); \n', file=ana, append=T)

##################################################
######## CODE IN AUXILIARY VARIABLES #############
##################################################

if(pca == 1){cat('auxiliary = (m) prin1-prin', 0+(aux.1*(1+aux.2*1+aux.3*1+aux.4* +aux.4*1+aux.5*1+aux.6*1+aux.7*1)), '; \n',file=ana, append=T, sep="")
	    }
if(var == 1){cat('auxiliary = (m) q1-q', 0+(aux.1*(1+aux.2*1+aux.3*1+aux.4* +aux.4*1+aux.5*1+aux.6*1+aux.7*1)), '; \n', file=ana, append=T, sep="")
	    }
#------------------------------------------------#
cat('savedata: \n ', file=ana, append=T)
cat('results are ', file=ana, append=T)
cat(paste(path,"results1.txt",sep="\\"), file=ana, append=T)
cat(' ; \n', file=ana, append=T)
cat('MODEL: \n', file=ana, append=T)
cat('[x*0 y*0]; \n', file=ana, append=T)
cat('x*1; y*1;  \n', file=ana, append=T)
cat('x with y*.50; \n', file=ana, append=T)
cat('OUTPUT:  \n', file=ana, append=T)
##########################################
#         RUN ANALYSIS CODE              #
##########################################
shell (paste("mplus.exe",ana, paste(path,"saveout.out", sep="\\"), sep=" "))
#----------------------------------------------------------------------------------#

##########################################
#        Zip original data FILES         #
##########################################

#setwd(path)
#exe <- paste('"C:\\Program\ Files\\7-Zip\\7z.exe"')
#shell (paste(exe," a ",path,"\\data.zip ",path,"\\*.dat",sep=""))
#shell (paste("del ",path,"\\*.dat",sep=''))

            } #number of auxiliary variables
          } #aux method
	    } #percent missing
      } #missing pattern
    } #common correlation
  } #number of clusters
} #n per cluster sample size


