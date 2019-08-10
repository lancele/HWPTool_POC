#step 1: create the gbmdata object

#load in GBM package (NOTE: must be the included gbm package and R version 2.15.2; the newer versions do not work with provided code), splitsets function, and BRT functions
library(gbm)
#splitsets() function
source("splitsets.R")
#brt.functions
source("brt.functions.R")

#read in metadata table and CAdata, define explanatory variables (EXP) and response variables (response)
vars.meta <- read.csv("CA_vars_meta.csv",header=TRUE,stringsAsFactors=FALSE)
EXP <- vars.meta[vars.meta[,"vartype"]=="exp","shortname"]
responses <- vars.meta[vars.meta[,"vartype"]=="res","shortname"]
varnames <- vars.meta[vars.meta[,"vartype"]!="","shortname"]
resvars <- responses
CAdata <- read.csv("CAdata_full.csv",header=TRUE)

#remove desert modoc regions
CAdata <- CAdata[CAdata$PSARegion!="Deserts Modoc",]

#create gbmdata object
gbmdata <- vector(mode="list")

#go through each response and find a transformation to make it normally distributed (change resvar.num and look at histograms)
resvar.num <- 1
tempdat <- CAdata[is.na(CAdata[,resvars[resvar.num]])==FALSE,]
summary(tempdat[,resvars[resvar.num]])
par(mfrow=c(1,1))
hist(tempdat[,resvars[resvar.num]],main=resvars[resvar.num])
boxplot(tempdat[,resvars[resvar.num]],main=resvars[resvar.num])

par(mfrow=c(3,2))
hist(log10(tempdat[,resvars[resvar.num]]),main=paste(resvars[resvar.num]," - log10",sep=""))
hist(log1p(tempdat[,resvars[resvar.num]]),main=paste(resvars[resvar.num]," - log1p",sep=""))
hist(log(tempdat[,resvars[resvar.num]]+5,base=500),main=paste(resvars[resvar.num]," - log",sep=""))
hist(sqrt(tempdat[,resvars[resvar.num]]),main=paste(resvars[resvar.num]," - sqrt",sep=""))
hist(tempdat[,resvars[resvar.num]]^2,main=paste(resvars[resvar.num]," - x^2",sep=""))
hist(tempdat[,resvars[resvar.num]]^(1/4),main=paste(resvars[resvar.num]," - x^1/4",sep=""))


#use this line to transform the response variable once a transformation has been chosen
tempdat[,resvars[resvar.num]] <- log1p(tempdat[,resvars[resvar.num]])
#check summary for transformation and make sure no infinite values were created
summary(tempdat[,resvars[resvar.num]])
sum(is.infinite(tempdat[,resvars[resvar.num]]))
#use the splitsets function to randomly subset the data into training and validation datasets
gbmdata[[resvars[resvar.num]]] <- splitsets(tempdat,prop=c(.75,.25,0))

#save the gbmdata object (a list with all the split datasets for each response variable)
save(gbmdata,file="gbmdata.RData")
