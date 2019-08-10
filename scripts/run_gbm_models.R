#step 2: run the models, export the model objects

#load in GBM package (NOTE: must be the included gbm package and R version 2.15.2; the newer versions do not work with provided code), splitsets function, and BRT functions
library(gbm)
#brt.functions
source("brt.functions.r")

#read in metadata table and CAdata, define explanatory variables (EXP) and response variables (response)
vars.meta <- read.csv("CA_vars_meta.csv",header=TRUE,stringsAsFactors=FALSE)
EXP <- vars.meta[vars.meta[,"vartype"]=="exp","shortname"]
responses <- vars.meta[vars.meta[,"vartype"]=="res","shortname"]
varnames <- c("CATCHID",EXP,responses)
resvars <- responses
CAdata <- read.csv("CAdata_full.csv",header=TRUE)

#load in created gbmdata object
load("gbmdata.RData")

#create results.gbm, results.pred, and results.summary objects
results.gbm <- vector(mode="list")
results.preds <- vector(mode="list")
results.summary <- vector(mode="list")

#create BRT models by going through each response variable by changing resvar.num, use the first section for
#responses with normal distributions, abnd the second section for binary variables (like the hydrology metrics)
resvar.num <- 1
resvars[resvar.num]

#for normal distributions
results.gbm[[resvars[resvar.num]]] <- gbm.step(data=gbmdata[[resvars[resvar.num]]][[1]], 
                                                      gbm.x = which(colnames(gbmdata[[resvars[resvar.num]]][[1]]) %in% EXP),
                                                      gbm.y = which(colnames(gbmdata[[resvars[resvar.num]]][[1]])==resvars[resvar.num]),
                                                      family = "gaussian",
                                                      tree.complexity = 5, #change tree complexity
                                                      learning.rate = .004, #and learning rate to find the best fit model
                                                      bag.fraction = .5)
#aim for a number of trees between 1000 and 2000: if it is outside this range, change learning rate and complexity

#training correlation
results.gbm[[resvars[resvar.num]]]$self.statistics$correlation
#cross-validation correlation
results.gbm[[resvars[resvar.num]]]$cv.statistics$correlation.mean


#when you find a satisfactory model, test the training dataset against the validation dataset and calculate the independent data correlation
results.preds[[resvars[resvar.num]]] <- predict.gbm(results.gbm[[resvars[resvar.num]]],
                                                           gbmdata[[resvars[resvar.num]]][[2]],
                                                           n.trees=results.gbm[[resvars[resvar.num]]]$gbm.call$best.trees,
                                                           type="response")

#independent correlation
cor(gbmdata[[resvars[resvar.num]]][[2]][,resvars[resvar.num]],results.preds[[resvars[resvar.num]]])

#read final model into the results.summary object
results.summary[[resvars[resvar.num]]] <- summary(results.gbm[[resvars[resvar.num]]])


#####
#for binomial distributions
resvar.num <- 1
resvars[resvar.num]

results.gbm[[resvars[resvar.num]]] <- gbm.step(data=gbmdata[[resvars[resvar.num]]][[1]], 
                                                      gbm.x = which(colnames(gbmdata[[resvars[resvar.num]]][[1]]) %in% EXP),
                                                      gbm.y = which(colnames(gbmdata[[resvars[resvar.num]]][[1]])==resvars[resvar.num]),
                                                      family = "bernoulli",
                                                      tree.complexity = 5, #change tree complexity
                                                      learning.rate = .001, #and learning rate to find the best fit model
                                                      bag.fraction = .5)
#aim for a number of trees between 1000 and 2000: if it is outside this range, change learning rate and complexity

#training ROC values
#requires reading in the allmod.predict function (from allmod_predict.R) and predicting training dataset values to calculate ROC values
bitrain <- allmod.predict(results.gbm.all,gbmdata[[resvars[resvar.num]]][[1]],"CATCHID","C:/PredResults/",1,resvar.num)
roc(gbmdata[[resvars[resvar.num]]][[1]][,resvars[resvar.num]],bitrain[,resvars[resvar.num]])

#validation ROC
mean(results.gbm.all[[resvars[resvar.num]]]$cv.roc.matrix)

#when you find a satisfactory model, test the training dataset against the validation dataset and calculate the independent data ROC
results.preds[[resvars[resvar.num]]] <- predict.gbm(results.gbm[[resvars[resvar.num]]],
                                                           gbmdata[[resvars[resvar.num]]][[2]],
                                                           n.trees=results.gbm[[resvars[resvar.num]]]$gbm.call$best.trees,
                                                           type="response")

#independent ROC
roc(gbmdata[[resvars[resvar.num]]][[2]][,resvars[resvar.num]],results.preds.all[[resvars[resvar.num]]])

#read final model into the results.summary object
results.summary[[resvars[resvar.num]]] <- summary(results.gbm[[resvars[resvar.num]]])



#when models are created for each response variable, export the results.gbm, results.preds, and results.summary objects
save(results.gbm,file="results_gbm.RData")
save(results.preds,file="results_preds.RData")
save(results.summary,file="results_summary.RData")