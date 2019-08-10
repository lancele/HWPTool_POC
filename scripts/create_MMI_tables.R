#step 4: create the MMI tables

#load in GBM package (NOTE: must be the included gbm package and R version 2.15.2; the newer versions do not work with provided code), splitsets function, and BRT functions
library(gbm)
#brt.functions
source("brt.functions.R")

#read in metadata table and predicted CAdata, define explanatory variables (EXP) and response variables (response)
#make sure to back-transform response variables after prediction (or change pH predictions to distance from ideal [range of 6.5-8.0])
#make sure climate variables are absolute values before MMI creation
vars.meta <- read.csv("CA_vars_meta.csv",header=TRUE,stringsAsFactors=FALSE)
EXP <- vars.meta[vars.meta[,"vartype"]=="exp","shortname"]
responses <- vars.meta[vars.meta[,"vartype"]=="res","shortname"]
varnames <- c("CATCHID",EXP,responses)
resvars <- responses
CAdata <- read.csv("CAdata_full.csv",header=TRUE)

#read in .csv with single column: catchments outside of the Desert/Modoc region
noMod.catchments <- read.csv("CAcatchments_noModoc.csv",header=T)

#load in results.summary and results.gbm objects
load("results_summary.RData")
load("results_gbm.RData")

#source the createMMI_func_CA file to get the createMMI function
source("createMMI_func_CA.R")
CAdata.MMI <- createMMI(results.summary.all,vars.meta,CAdata,noMod.catchments,"CATCHID")

#export tables relevant to map and graph creation
#relative influence table
write.csv(CAdata.MMI[["mmi.table"]],"RelInf_table.csv",row.names=F)
#normalized indicator data: all catchments
write.csv(CAdata.MMI[["normdata.wMod"]],"CA_NormIndicators.csv",row.names=F)
#normalized indicator data: non-Desert Modoc catchments
write.csv(CAdata.MMI[["normdata.noMod"]],"CA_NormIndicators_noMod.csv",row.names=F)
#directionally-aligned normalized indicator data: all catchments
write.csv(CAdata.MMI[["DAnormdata.wMod"]],"CA_DirAlignedNormIndicators.csv",row.names=F)
#directionally-aligned normalized indicator data: non-Desert Modoc catchments
write.csv(CAdata.MMI[["DAnormdata.noMod"]],"CA_DirAlignedNormIndicators_noMod.csv",row.names=F)
#MMI scores: all catchments
write.csv(CAdata.MMI[["MMIs.wMod"]],"CA_MMIs_wMod.csv",row.names=F)
#MMI scores: non-Desert Modoc catchments
write.csv(CAdata.MMI[["MMIs.noMod"]],"CA_MMIs_noMod.csv",row.names=F)