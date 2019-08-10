#step 3: use models to predict all catchments

#load in GBM package (NOTE: must be the included gbm package and R version 2.15.2; the newer versions do not work with provided code), splitsets function, and BRT functions
library(gbm)
#brt.functions
source("brt.functions.R")

#read in CAdata
CAdata <- read.csv("CAdata_full.csv",header=TRUE)
#read in the gbm model object list
load("results_gbm.RData")

#create the allmod.predict function: runs through each model and each catchment, predicting a value for each response variable based on the data
allmod.predict <- function(gbmresults,  #results.gbm object list
                           rawdata,  #the catchment table
                           IDfield,  #name of the ID field in the catchment table
                           savefolder,  #save location
                           startpoint=1)  {  #where to begin predicting (used if predictions must be stopped midway through)
  
  modfin.list <- names(gbmresults)
  pred.tab <- matrix(NA, nrow=nrow(rawdata), ncol=length(modfin.list)+1)		#+1 for IDfield
  colnames(pred.tab) <- c(IDfield, modfin.list)
  pred.tab[,IDfield] <- rawdata[,IDfield]
  savepoints <- floor(quantile(1:nrow(rawdata),probs=seq(0,1,by=.2)))
  
  pb <- txtProgressBar(min = 0, max = nrow(rawdata), style = 3)
  for (j in modfin.list)  {
    for (k in startpoint:nrow(rawdata))  {
      #for (k in 1:100)  {
      modfin <- gbmresults[[j]]
      pred.tab[k,j] <- predict.gbm(modfin, rawdata[k,], n.trees=modfin$gbm.call$best.trees, type="response")
      
      if (k %in% savepoints)  {
        write.csv(pred.tab,paste(savefolder,"predsave",k,".csv",sep=""),row.names=FALSE)  #save the table periodically through the process so data is not lost
      }
      if (k == savepoints[length(savepoints)])  {
        write.csv(pred.tab,paste(savefolder,"predsave_",j,".csv",sep=""),row.names=FALSE)  #save the table at the end of each response variable in separate files so data is not overwritten and lost
      }
      setTxtProgressBar(pb, k)
    }
    print(j)
  }
  close(pb)
  pred.tab
}

#run allmod.predict on catchments
CApreds <- allmod.predict(results.gbm,CAdata,"CATCHID","C:/PredResults/")
#save final result
write.csv(CApreds, "CApredictions.csv",row.names=FALSE)

#be sure to back-transform all predicted values if they were transformed into normal distributions previously
#merge the back-transformed values into the original dataset to get a table that is suitable for MMI creation
