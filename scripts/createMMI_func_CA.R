#filepath <- "C:/Users/Marc/Documents/Cadmus/CA_Redo/RedoModels/"
#source(paste(filepath,"ModelOutput/OutputMMI/createMMI_func.R",sep=""))

#function to create normalized tables for each MMI

createMMI <- function(gbmsummary,    #results.summary list object
                      vartable,  	#XX_vars_meta.csv table
                      rawdata,		#full raw data
                      nomods,    #.csv with single column: the noModoc catchments
                      IDfield)  {		#name of ID field (ex. "COMID" or "CATCHID")
  #####
  #####
  #create MMIlist: a list object of tables defining all indicators that go into each MMI and their directions of influence
  MMIlist <- vector(mode="list")
  
  #some indicators are used in multiple MMIs, requiring the use of three different MMIclass columns
  #go through the columns and define the MMIs
  MMItitles <- unique(vartable[,"MMIclass"])[unique(vartable[,"MMIclass"]) != ""]
  MMItitles2 <- unique(vartable[,"MMIclass2"])[unique(vartable[,"MMIclass2"]) != ""]
  MMItitles3 <- unique(vartable[,"MMIclass3"])[unique(vartable[,"MMIclass3"]) != ""]
  for (i in MMItitles)  {
    MMIlist[[i]]$MMIvars <- vartable[vartable[,"MMIclass"]==i,"shortname"]
    MMIlist[[i]]$MMItype <- vartable[vartable[,"MMIclass"]==i,"IndDirection"]
  }
  for (r in MMItitles2)  {
    MMIlist[[r]]$MMIvars <- vartable[vartable[,"MMIclass2"]==r,"shortname"]
    MMIlist[[r]]$MMItype <- vartable[vartable[,"MMIclass2"]==r,"IndDirection"]
  }
  for (s in MMItitles3)  {
    MMIlist[[s]]$MMIvars <- vartable[vartable[,"MMIclass3"]==s,"shortname"]
    MMIlist[[s]]$MMItype <- vartable[vartable[,"MMIclass3"]==s,"IndDirection"]
  }
  print(MMIlist)
  
  #####
  #####
  #create two sets of data: all catchments, and only catchments that are outside of the Desert Modoc region
  #two sets of MMIs will be created with these data, this ensure normalization in each of these datasets will be conducted properly
  noModoc.catch <- nomods[,IDfield]
  wMod.data <- rawdata
  noMod.data <- rawdata[rawdata$CATCHID %in% noModoc.catch,]
  
  
  #####
  #####
  #define model names and MMI names
  modnames <- names(gbmsummary)
  MMInames <- names(MMIlist)
  print(MMInames)
  nomod.MMInames <- c("PhysBio","InstreamBio","WaterQuality")
  wmod.MMInames <- c("Landscape","ClimateChange","LandCover","Fire","WaterUse","Natural","Anthropogenic","Watershed")
  all.MMInames <- c(nomod.MMInames,wmod.MMInames)
  IND <- vartable[vartable[,"Indicator"]=="ind","shortname"]
  EXP <- vartable[vartable[,"vartype"]=="exp","shortname"]
  numfields <- names(sapply(rawdata,class)[sapply(rawdata,class)!="factor"])
  
  #####
  #####
  #check if any indicator field is a non-numeric field, if so, stop and warn the user
  if (length(IND[!(IND %in% numfields)]) > 1) {
    print(IND[!(IND %in% numfields)])
    stop("Non-numeric indicator fields included.")
  }
  
  
  #####
  #####
  #create a relative influence table: all of the variable names, truncated and full, with their relative influences
  #in every model
  
  #set up a table that will hold all of the relative influence values from each model
  mmi.table <- matrix(NA,nrow=length(EXP),ncol=length(modnames)+2)  #+2 is for "FullName" and "ShortName" columns
  colnames(mmi.table) <- c("FullName","ShortName",modnames)
  mmi.table <- as.data.frame(mmi.table)
  
  #populate the full name and short name columns
  mmi.table[,"FullName"] <- vartable[,"fullnames"][vartable[,"shortname"] %in% EXP][order(vartable[,"shortname"][vartable[,"shortname"] %in% EXP])]
  mmi.table[,"ShortName"] <- vartable[,"shortname"][vartable[,"shortname"] %in% EXP][order(vartable[,"shortname"][vartable[,"shortname"] %in% EXP])]
  
  #populate relative influence for each model
  for (i in modnames)  {
    tempsum <- gbmsummary[[i]][gbmsummary[[i]][,"var"] %in% EXP,]
    tempmerge <- merge(mmi.table,tempsum,by.x="ShortName",by.y="var",all.x=TRUE,all.y=TRUE)
    #print(nrow(tempmerge))
    mmi.table[,i] <- tempmerge[,"rel.inf"]
  }
  print("Relative influence table created.")

  
  #####
  #####
  #create two tables holding rank-normalized indicator data: one for all catchments, and one for catchments outside of Desert
  #Modoc regions
  
  #set up a table that will hold the rank-normalized indicator data for all catchments
  normdata.wMod <- matrix(NA,nrow=nrow(wMod.data),ncol=length(c(IDfield,IND)))
  colnames(normdata.wMod) <- c(IDfield,IND)
  normdata.wMod <- as.data.frame(normdata.wMod)
  normdata.wMod[,IDfield] <- wMod.data[,IDfield]
  
  #create and populate table with normalized data for all catchments
  for (u in IND)  {
    temprank <- rank(wMod.data[,u],na.last="keep",ties.method="max")
    normdata.wMod[,u] <- (temprank - min(temprank,na.rm=TRUE))/(max(temprank,na.rm=TRUE) - min(temprank,na.rm=TRUE))
  }
  print("Rank-normalized data for all catchments have been created.")
  
  #set up a table that will hold the rank-normalized indicator data for non-Desert Modoc catchments
  normdata.noMod <- matrix(NA,nrow=nrow(noMod.data),ncol=length(c(IDfield,IND)))
  colnames(normdata.noMod) <- c(IDfield,IND)
  normdata.noMod <- as.data.frame(normdata.noMod)
  normdata.noMod[,IDfield] <- noMod.data[,IDfield]
  
  #create and populate table with normalized data for non-Desert Modoc catchments
  for (v in IND)  {
    temprank <- rank(noMod.data[,v],na.last="keep",ties.method="max")
    normdata.noMod[,v] <- (temprank - min(temprank,na.rm=TRUE))/(max(temprank,na.rm=TRUE) - min(temprank,na.rm=TRUE))
  }
  print("Rank-normalized data for non-Desert Modoc catchments have been created.")
  
  #####
  #####
  #create two tables holding directionally-aligned rank-normalized indicator data (if high indicator scores result
  #in a lower index value, the inverse rank-normalized values are used): one for all catchments, and one for
  #catchments outside of Desert Modoc regions
  
  #set up a table that will hold the directionally-aligned rank-normalized indicator data for all catchments
  DAnormdata.wMod <- matrix(NA,nrow=nrow(wMod.data),ncol=length(c(IDfield,IND)))
  colnames(DAnormdata.wMod) <- c(IDfield,IND)
  DAnormdata.wMod <- as.data.frame(DAnormdata.wMod)
  DAnormdata.wMod[,IDfield] <- wMod.data[,IDfield]
  
  #create and populate table with directionally-aligned normalized data for all catchments
  for (j in IND)  {
    #print(vartable[vartable[,"shortname"]==j,"shortname"])
    if (vartable[vartable[,"shortname"]==j,"IndDirection"]=="good")  {
      temprank <- rank(wMod.data[,j],na.last="keep",ties.method="max")
      DAnormdata.wMod[,j] <- (temprank - min(temprank,na.rm=TRUE))/(max(temprank,na.rm=TRUE) - min(temprank,na.rm=TRUE))
    }
    if (vartable[vartable[,"shortname"]==j,"IndDirection"]=="bad")  {
      temprank <- rank(wMod.data[,j],na.last="keep",ties.method="max")
      DAnormdata.wMod[,j] <- 1-((temprank - min(temprank,na.rm=TRUE))/(max(temprank,na.rm=TRUE) - min(temprank,na.rm=TRUE)))
    }
  }
  print("Directionally-aligned rank-normalized data for all catchments have been created.")
  
  #set up a table that will hold the directionally-aligned rank-normalized indicator data for catchments outside
  #of Deserts Modoc region
  DAnormdata.noMod <- matrix(NA,nrow=nrow(noMod.data),ncol=length(c(IDfield,IND)))
  colnames(DAnormdata.noMod) <- c(IDfield,IND)
  DAnormdata.noMod <- as.data.frame(DAnormdata.noMod)
  DAnormdata.noMod[,IDfield] <- noMod.data[,IDfield]
  
  #create and populate table with directionally-aligned normalized data for all catchments
  for (j in IND)  {
    #print(vartable[vartable[,"shortname"]==j,"shortname"])
    if (vartable[vartable[,"shortname"]==j,"IndDirection"]=="good")  {
      temprank <- rank(noMod.data[,j],na.last="keep",ties.method="max")
      DAnormdata.noMod[,j] <- (temprank - min(temprank,na.rm=TRUE))/(max(temprank,na.rm=TRUE) - min(temprank,na.rm=TRUE))
    }
    if (vartable[vartable[,"shortname"]==j,"IndDirection"]=="bad")  {
      temprank <- rank(noMod.data[,j],na.last="keep",ties.method="max")
      DAnormdata.noMod[,j] <- 1-((temprank - min(temprank,na.rm=TRUE))/(max(temprank,na.rm=TRUE) - min(temprank,na.rm=TRUE)))
    }
  }
  print("Directionally-aligned rank-normalized data for non-Desert Modoc catchments have been created.")
  
  
  #####
  #####
  #create all index scores (mean of the directionally-aligned normalized data in each index category) for each
  #catchment for all catchment data as well as the non-Desert Modoc data
  
  #create table to hold the index scores for all catchments
  indexdata.wMod <- matrix(NA,nrow=nrow(wMod.data),ncol=length(c(IDfield,MMInames)))
  colnames(indexdata.wMod) <- c(IDfield,MMInames)
  indexdata.wMod <- as.data.frame(indexdata.wMod)
  indexdata.wMod[,IDfield] <- wMod.data[,IDfield]
  
  #calculate and populate index data table for all catchments
  for (n in MMInames)  {
    tempvarlist <- MMIlist[[n]]$MMIvars
    #print(tempvarlist)
    #print(colnames(DAnormdata.wMod))
    if (length(tempvarlist) == 1)  {  #if the MMI only has one indicator, the index score is just the normalized data scores
      indexdata.wMod[,n] <- DAnormdata.wMod[,tempvarlist]
    }
    else  {  #otherwise, it is the mean of all of the normalized values that are in that indicator
      indexdata.wMod[,n] <- apply(DAnormdata.wMod[,tempvarlist],1,mean,na.rm=TRUE)
    }
  }
  print("Indicator scores for all catchments have been calculated.")
  
  #create table to hold the index scores for non-Desert Modoc catchments
  indexdata.noMod <- matrix(NA,nrow=nrow(noMod.data),ncol=length(c(IDfield,MMInames)))
  colnames(indexdata.noMod) <- c(IDfield,MMInames)
  indexdata.noMod <- as.data.frame(indexdata.noMod)
  indexdata.noMod[,IDfield] <- noMod.data[,IDfield]
  
  #calculate and populate index data table for non-Desert Modoc catchments
  for (n in MMInames)  {
    tempvarlist <- MMIlist[[n]]$MMIvars
    #print(tempvarlist)
    #print(colnames(DAnormdata.noMod))
    if (length(tempvarlist) == 1)  {  #if the MMI only has one indicator, the index score is just the normalized data scores
      indexdata.noMod[,n] <- DAnormdata.noMod[,tempvarlist]
    }
    else  {  #otherwise, it is the mean of all of the normalized values that are in that indicator
      indexdata.noMod[,n] <- apply(DAnormdata.noMod[,tempvarlist],1,mean,na.rm=TRUE)
    }
  }
  print("Indicator scores for non-Desert Modoc catchments have been calculated.")
  
  
  #####
  #####
  #create two tables that re-normalize the catchment index scores after the index scores were calculated in the
  #previous step
  
  #create table to hold all catchment rank-normalized indicator scores
  norminddata.wMod <- matrix(NA,nrow=nrow(wMod.data),ncol=length(c(IDfield,MMInames)))
  colnames(norminddata.wMod) <- c(IDfield,MMInames)
  norminddata.wMod <- as.data.frame(norminddata.wMod)
  norminddata.wMod[,IDfield] <- wMod.data[,IDfield]
  #print(MMInames)
  for (o in MMInames)  {
    temprank2 <- rank(indexdata.wMod[,o],na.last="keep",ties.method="max")
    norminddata.wMod[,o] <- (temprank2 - min(temprank2,na.rm=TRUE))/(max(temprank2,na.rm=TRUE) - min(temprank2,na.rm=TRUE))
  }
  print("Indicator scores have been rank-normalized for all catchments.")
  
  #create table to hold non-Desert Modoc catchment rank-normalized indicator scores
  norminddata.noMod <- matrix(NA,nrow=nrow(noMod.data),ncol=length(c(IDfield,MMInames)))
  colnames(norminddata.noMod) <- c(IDfield,MMInames)
  norminddata.noMod <- as.data.frame(norminddata.noMod)
  norminddata.noMod[,IDfield] <- noMod.data[,IDfield]
  #print(MMInames)
  for (o in MMInames)  {
    temprank2 <- rank(indexdata.noMod[,o],na.last="keep",ties.method="max")
    norminddata.noMod[,o] <- (temprank2 - min(temprank2,na.rm=TRUE))/(max(temprank2,na.rm=TRUE) - min(temprank2,na.rm=TRUE))
  }
  print("Indicator scores have been rank-normalized for non-Desert Modoc catchments.")
  
  
  #####
  #####
  #calculate watershed condition and watershed vulnerability MMIs for all catchments and non-Desert Modoc catchments
  
  #create table to hold MMI values for all catchments
  hltvln.wMod <- matrix(NA,nrow=nrow(wMod.data),ncol=10)
  colnames(hltvln.wMod) <- c(IDfield,"NormWatershedHealth","NormWatershedVulnerability","Natural","Anthropogenic","Landscape","ClimateChange","LandCover","WaterUse","Fire")
  hltvln.wMod <- as.data.frame(hltvln.wMod)
  hltvln.wMod[,IDfield] <- wMod.data[,IDfield]
  
  #calculate watershed health for all catchments
  hlt.wMod <- norminddata.wMod[,c("Watershed")]
  #calculate vatershed vulnerability for all catchments
  vln.wMod <- apply(norminddata.wMod[,c("ClimateChange","LandCover","WaterUse","Fire")],1,mean,na.rm=TRUE)
  
  #rank-normalize watershed health and populate table
  healthranks.wMod <- rank(hlt.wMod,na.last="keep",ties.method="max")
  hltvln.wMod[,"NormWatershedHealth"] <- (healthranks.wMod - min(healthranks.wMod,na.rm=TRUE))/(max(healthranks.wMod,na.rm=TRUE) - min(healthranks.wMod,na.rm=TRUE))
  #rank-normalize watershed vulnerability and populate table
  wsvuln.wMod <- rank(vln.wMod,na.last="keep",ties.method="max")
  hltvln.wMod[,"NormWatershedVulnerability"] <- (wsvuln.wMod - min(wsvuln.wMod,na.rm=TRUE))/(max(wsvuln.wMod,na.rm=TRUE) - min(wsvuln.wMod,na.rm=TRUE))
  
  #populate remaining MMI columns
  hltvln.wMod[,"Natural"] <- norminddata.wMod[,"Natural"]
  hltvln.wMod[,"Anthropogenic"] <- norminddata.wMod[,"Anthropogenic"]
  hltvln.wMod[,"Landscape"] <- norminddata.wMod[,"Landscape"]
  hltvln.wMod[,"ClimateChange"] <- norminddata.wMod[,"ClimateChange"]
  hltvln.wMod[,"LandCover"] <- norminddata.wMod[,"LandCover"]
  hltvln.wMod[,"WaterUse"] <- norminddata.wMod[,"WaterUse"]
  hltvln.wMod[,"Fire"] <- norminddata.wMod[,"Fire"]
  print("MMI scores have been calculated for all catchments")
  
  #create table to hold MMI values for non-Desert Modoc catchments
  hltvln.noMod <- matrix(NA,nrow=nrow(noMod.data),ncol=9)
  colnames(hltvln.noMod) <- c(IDfield,"NormWatershedHealth","NormWatershedVulnerability","Natural","Anthropogenic","NormStreamHealth","WaterQuality","PhysBio","InstreamBio")
  hltvln.noMod <- as.data.frame(hltvln.noMod)
  hltvln.noMod[,IDfield] <- noMod.data[,IDfield]
  
  #calculate watershed health for non-Desert Modoc catchments
  hlt.noMod <- norminddata.noMod[,c("Watershed")]
  #calculate vatershed vulnerability for all catchments
  vln.noMod <- apply(norminddata.noMod[,c("ClimateChange","LandCover","WaterUse","Fire")],1,mean,na.rm=TRUE)
  #calculate stream health for non-Desert Modoc catchments
  stream.noMod <- apply(norminddata.noMod[,c("PhysBio","WaterQuality","InstreamBio")],1,mean,na.rm=TRUE)
  
  #rank-normalize watershed health and populate table
  healthranks.noMod <- rank(hlt.noMod,na.last="keep",ties.method="max")
  hltvln.noMod[,"NormWatershedHealth"] <- (healthranks.noMod - min(healthranks.noMod,na.rm=TRUE))/(max(healthranks.noMod,na.rm=TRUE) - min(healthranks.noMod,na.rm=TRUE))
  #rank-normalize watershed vulnerability and populate table
  wsvuln.noMod <- rank(vln.noMod,na.last="keep",ties.method="max")
  hltvln.noMod[,"NormWatershedVulnerability"] <- (wsvuln.noMod - min(wsvuln.noMod,na.rm=TRUE))/(max(wsvuln.noMod,na.rm=TRUE) - min(wsvuln.noMod,na.rm=TRUE))
  #rank-normalize stream health and populate table
  streamranks.noMod <- rank(stream.noMod,na.last="keep",ties.method="max")
  hltvln.noMod[,"NormStreamHealth"] <- (streamranks.noMod - min(streamranks.noMod,na.rm=TRUE))/(max(streamranks.noMod,na.rm=TRUE) - min(streamranks.noMod,na.rm=TRUE))
  
  #populate remaining MMI columns
  hltvln.noMod[,"Natural"] <- norminddata.noMod[,"Natural"]
  hltvln.noMod[,"Anthropogenic"] <- norminddata.noMod[,"Anthropogenic"]
  hltvln.noMod[,"WaterQuality"] <- norminddata.noMod[,"WaterQuality"]
  hltvln.noMod[,"PhysBio"] <- norminddata.noMod[,"PhysBio"]
  hltvln.noMod[,"InstreamBio"] <- norminddata.noMod[,"InstreamBio"]
  print("MMI scores have been calculated for non-Desert Modoc catchments")

  
  #####
  #####
  #create a list object to store all important tables:
  #mmi.table = Relative influence table
  #normdata.wMod = Normalized indicator values for all catchments
  #normdata.noMod = Normalized indicator values for non-Desert Modoc catchments
  #DAnormdata.wMod = Directionally-aligned normalized indicator values for all catchments
  #DAnormdata.noMod = Directionally-aligned normalized indicator values for non-Desert Modoc catchments
  #norminddata.wMod = Normalized MMI component values for all catchments
  #norminddata.noMod = Normalized MMI component values for non-Desert Modoc catchments
  #hltvln.wMod = Final MMI scores for all catchments
  #hltvln.noMod = Final MMI scores for non-Desert Modoc catchments
  
  result.MMI <- vector(mode="list")
  result.MMI[["mmi.table"]] <- mmi.table
  result.MMI[["normdata.wMod"]] <- normdata.wMod
  result.MMI[["normdata.noMod"]] <- normdata.noMod
  result.MMI[["DAnormdata.wMod"]] <- DAnormdata.wMod
  result.MMI[["DAnormdata.noMod"]] <- DAnormdata.noMod
  result.MMI[["norminddata.wMod"]] <- norminddata.wMod
  result.MMI[["norminddata.noMod"]] <- norminddata.noMod
  result.MMI[["MMIs.wMod"]] <- hltvln.wMod
  result.MMI[["MMIs.noMod"]] <- hltvln.noMod
  result.MMI
}