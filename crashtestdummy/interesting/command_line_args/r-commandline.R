## Paul Johnson
## 2012-10-22

## I returned to this after a long hiatus. I decided to clean up the
## code so it serves as a good, general example of processing command
## line arguments.

## To get the idea of what this does, run this in the shell:

## R  -f r-commandline.R --args myMeanN=88.1231 runI=31 whateverC="hell" nofcasesI=232

## R  -f r-commandline.R --args myMeanN=88.1231 runI=17  whateverC="hell" nofcasesI=2432


## Those values after --args are the program-specific command line
## arguments that are put to use. The program has default values
## that will be used, unless various alternatives are offered
## in the command line.

## How does this help with high performance computing? We want to
## write an R program that runs with default values, but also make it
## possible to run the program over and over again with slightly
## different arguments.  So, for example, we can re-set the random
## generator's seed, the run number or various parameters.

## To make all this work we use the R function commandArgs().  That
## function scans the command line and recognizes arguments. In this
## case, I use the argument trailingOnly = TRUE, so that commandArgs
## only grabs the arguments that are on the command line after the
## keyword --args.

## After scanning the command line arguments, I have to make sense out
## of them. This program shows one example of how it can be
## done. It takes the command arguments, and then tries to tell
## if the values are supposed to be integers, real numbers, or
## a character string.

## This tries to make the parsing of arguments a little smarter,
## and it may come in handy because there can be subtle problems when
## variables that we intend to be integers are seen as others.
##
## You can name variables however you want, but if you add and I, N,
## or C on the end of the variable name, then the program will try to
## do the right thing.

## It will accept variables specified in the ordinary way, as in
## run=17, but it also allows the specification of an integer valued
## variable, as runI=17.

## The function below scans those arguments and turns them into
## variables. So, for example, if we run

## R -f r-commandline.R runI=33 mymeanN=4824.1314 outdirC="hell"
##
## then any of those arguments will be scanned and created as variables
## within the R session. The ones that have N, I and C on the end
## of the variable names will get some special treatment
## to make sure they are set as the correct variable type.

## The suffixes, I, N, and C, are OPTIONAL markers that are used to improve
## value recognition. We want integer arguments to be integers.
## So the arguments should have suffixes I, N and C, for
## I: integer values.
## N: numeric variables (floating point variable).
## C: character variables. should be in quotes on command line.
##
## To show that the suffix is not required, try this:
## Run program like
## R -f r-commandline.R run=33 runI=21
## To see why you might be wise to use it, try this:
## R -f r-commandline.R run=234.131 runI=234.132
## and then check to see what values are received in the variables run and runI


## The "trailingOnly" argument means that we are using only the
## command line arguments that follow the keyword --args.
commArgs <- commandArgs(trailingOnly = TRUE)

print(commArgs)
## For each commandArg, check the variable type, and assign that
## variable. Checks variables named suffixed with I, N and C.
## If an argument is specified with no equal sign and no value,
## then that variable is created and set equal to TRUE.
for (e in commArgs) {
  ta = strsplit(e,"=",fixed=TRUE)
  vname <- ta[[1]][1] ## the name on the left hand side
  if(! is.na(ta[[1]][2])) {
      temp <- ta[[1]][2] ## the value on the right hand side
      if(substr(vname, nchar(vname), nchar(vname)) == "I") {
          temp = as.integer(temp)
      }
      if(substr(vname, nchar(vname), nchar(vname)) == "N") {
      temp = as.numeric(temp)
    }
    if(substr(vname, nchar(vname), nchar(vname)) == "C") {
      temp = as.character(temp)
    }
    assign(vname,temp)
    cat("assigned ", vname," the value of :", temp,".\n")
  } else {
    assign(vname,TRUE)
    cat("assigned ", vname," the value of TRUE\n")
  }
}

## Double-check the command line arguments by retrieving
## the values from the session
for (i in commArgs) {
    ta = strsplit(i, "=", fixed=TRUE)
    print(paste(ta[[1]][1], "is",  get(ta[[1]][1])))
}

## If outdirC was not a command line arg, create it.
if (!exists("outdirC")) outdirC <- "output"
if (!dir.exists(outdirC)) dir.create(outdirC)

## Specify default values if no command line argument is supplied
nofcases <- if(exists("nofcasesI")) nofcasesI else 1000
mymean <- if (exists("mymeanN"))  mymeanN else 33
run <- if (exists("runI")) runI else 001

x <- rnorm(nofcases, mean = mymean, sd = 43)

mean(x)

saveRDS(x, file = file.path(outdirC, paste0("testsave", run,".rds")))

pdf(file = file.path(outdirC, paste0("testGraph", run, ".pdf")), height = 6, width = 6,
    onefile = FALSE, family="Times", paper = "special")
hist(x, main = paste("Run", run, "of Many Stupid Histograms"))
dev.off()

