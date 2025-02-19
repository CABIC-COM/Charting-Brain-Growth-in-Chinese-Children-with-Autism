rm(list=ls())
library(readr)
library(dplyr)
library(tidyr)
library(writexl)
library(tidyverse)
library(reticulate)
source("100.common-variables.r")
source("101.common-functions.r")
source("200.variables.r")
source("201.functions.r")
source("300.variables.r")
source("301.functions.r")

DATA_PATH = './DATA'
mask_data <- readRDS(file.path( DATA_PATH, "MASK_DATA.rds" ) )
real_data_PATH <- file.path(DATA_PATH,"CABIC_Subjects_info.csv")
real_data <-  read.csv(real_data_PATH)
real_data_DIM = dim(real_data)
if (file.exists(file.path(DATA_PATH,"DATA.rds"))){
  file.remove(file.path(DATA_PATH,"DATA.rds"))
}

SUBSET <- mask_data |> head(n=real_data_DIM[1]) 
SUBSET |> select(1:6,25:27) -> SUBSET
real_data[,"SITE"] |> factor() -> SUBSET[,"Study"]
real_data[,"SEX"] |> factor() -> SUBSET[,"Grp"] 
real_data[,"GROUP"] |> factor() ->SUBSET[,"Type"]
real_data[,"SUBID"] |> factor() ->SUBSET[,"ID"]
real_data[,"HANDEDNESS"] |> factor() -> SUBSET[,"HANDEDNESS"]
SUBSET[,"TimeTransformed"] = real_data[,"AGE"]

Out_data_PATH <- file.path(DATA_PATH,"GMV_subregions.csv")
data_original <- read.csv(Out_data_PATH)
## delet null and zeros
data_training <- data_original[complete.cases(data_original),]
colnames(data_training)[2] <- "ID"
merge_data <- merge(SUBSET,data_training)


##Harmonize multi-site training data
source_python("./combatGAM_Python4R.py")
covars_temp = merge_data[c("Study","Grp","TimeTransformed")]
covars_temp_group = covars_temp
covars_temp$Grp |> as.factor() |> as.numeric() -> covars_temp$Grp
colnames(covars_temp)[1] <- "SITE"
colnames(covars_temp)[3] <- "age"
data_temp = data_training[,c(3:ncol(data_training))]
write.csv(covars_temp[,1:3],"./Curve/covars_temp.csv", row.names = FALSE) # save temporary covariates
write.csv(data_temp,"./Curve/data_temp.csv", row.names = FALSE) # save temporary data
adjustedData_model <- combatGAM_Python4R_function("./Curve./") # please cite the path of the saved temporary data and covariates
data_harmonized <- data.frame(adjustedData_model[[1]])
data_training[,c(3:ncol(data_original))] = data_harmonized
SUBSET <- merge(SUBSET,data_training)
SUBSET[,"ID"] |> factor() ->SUBSET[,"ID"]
SUBSET$INDEX.ID <- factor( paste(SUBSET$Study,SUBSET$ID,sep="_") )
SUBSET$INDEX.TYPE <- SUBSET$Type
regionname <- names(SUBSET)
ALLSUBSET <- SUBSET

for (region in 12:ncol(SUBSET)){
  if (file.exists(file.path(DATA_PATH,"DATA.rds"))){
    file.remove(file.path(DATA_PATH,"DATA.rds"))
  }
  IndexName <- regionname[region]
  ALLSUBSET[,IndexName]/1000 ->ALLSUBSET[,"Wand"]
  ALLSUBSET |> drop_na(Wand) -> SUBSET
  SUBSET |> filter(Type=="ASD") -> SUBSET
  saveRDS(object=SUBSET, file=file.path( DATA_PATH, "DATA.rds"))
  write.csv(SUBSET, file=file.path( DATA_PATH, "DATA.csv"))
  
  MAINPATH = "./DATA"
  DATAPATH = file.path(MAINPATH,"DATA.rds")
  MODELPATH = file.path(MAINPATH,"MODEL")
  if (file.exists(file.path(MAINPATH,"SUBSET.rds"))){
    file.remove(file.path(MAINPATH,"SUBSET.rds"))
  }
  if (dir.exists(file.path(MAINPATH,"omega-Wand__.n0000"))){
    unlink(file.path(MAINPATH,"omega-Wand__.n0000"),recursive = T)
  }
  
  
  PATHS <- ll.Create.Folders( "omega-Wand__.n0000",MAINPATH)
  file.copy(DATAPATH,file.path(PATHS,"SUBSET.rds"))
  model_name <- "base220.GGalt.bfpNA.rds" ######################################################################
  for (dirname in dir(MODELPATH)){  
    mydir <- paste0(getwd(),MODELPATH,"/",dirname)
    file.copy(mydir,PATHS$MODEL)  
  }
  HOLDER <- Load.Subset.Wrapper( Tag="omega-Wand__.n0000", LSubset=TRUE )
  HOLDER$MODEL <- readRDS( file.path( PATHS$MODEL, "base220.GGalt.fp.rds" ) ) 
  EXTRACT <- Extract.Wrapper( HOLDER, Store.Full=TRUE ) ## store full fitting object to use as initial point of bfpNA() re-fit [expect 12 iterations]
  summary(EXTRACT$FIT.FULL) ## standard summary of a GAMLSS fit
  if (is.null(EXTRACT$FIT.FULL)){
    next
  } else {

  getSmo(EXTRACT$FIT.FULL,what="mu") 
  getSmo(EXTRACT$FIT.FULL,what="mu")$power ##
  
 
  HOLDER$MODEL <- Make.bfpNA.model.from.extract( EXTRACT$param )
  saveRDS( HOLDER$MODEL, file.path( PATHS$MODEL,model_name) )
  EXTRACT.bfp <- Extract.Wrapper( HOLDER, Fit.Full=TRUE, start.from=EXTRACT$FIT ) ## helpful to start.from, improves convergence speed [expect 5ish iterations]
  Save.Extracted( EXTRACT.bfp, PATHS, model_name, Save.Full=TRUE )
  
  ## 320-script
  
  EXTRACT.bfp$param$BIC ## compare BIC on all fitted models
  
  
  file.copy(from=file.path(PATHS$MODEL,model_name),to=file.path(PATHS$PATH,"MODEL.rds"))
  file.copy(from=file.path(PATHS$FIT.EXTRACT,model_name),to=file.path(PATHS$PATH,"FIT.EXTRACT.rds"))
  
  
  
  ## 330-script (and 340-script)
  HOLDER <- Load.Subset.Wrapper( Tag="omega-Wand__.n0000", LSubset=TRUE, LModel=TRUE, LFit=TRUE )
  
  BOOT <- list()
  BOOT[[1]] <- Boot.Function(n=1,Base.Seed=12345,Holder=HOLDER)
  BOOT[[2]] <- Boot.Function(n=2,Base.Seed=12345,Holder=HOLDER)
  BOOT[[3]] <- Boot.Function(n=3,Base.Seed=12345,Holder=HOLDER)
  for( NUM in 4:100 ) { ## 100s of bootstrap replicates required
    cat("Bootstrap replicate: ",NUM,"\n")
    BOOT[[NUM]] <- Boot.Function(n=NUM,Base.Seed=12345,Holder=HOLDER)
  }
  
  Reduce(rbind,lapply(BOOT,function(X){X$param$mu$fixef}))
  Reduce(rbind,lapply(BOOT,function(X){X$param$sigma$fixef}))
  
  apply( Reduce(rbind,lapply(BOOT,function(X){X$param$mu$fixef})), 2, quantile, probs=c(0.05,0.95), na.rm=TRUE )
  apply( Reduce(rbind,lapply(BOOT,function(X){X$param$sigma$fixef})), 2, quantile, probs=c(0.05,0.95), na.rm=TRUE )
  apply( Reduce(rbind,lapply(BOOT,function(X){X$param$nu$fixef})), 2, quantile, probs=c(0.05,0.95), na.rm=TRUE )
  
  saveRDS(object=BOOT,file=file.path(PATHS$PATH,"BOOT.EXTRACT.rds"))
  
  DirName <- paste(MAINPATH,"/omega-Wand__.n0000",sep = "")
  Rename <- paste(MAINPATH,"/ASD","_",IndexName,sep = "")
  file.rename(DirName,Rename)
  }
  #####################################################################################################################
  if (file.exists(file.path(DATA_PATH,"DATA.rds"))){
    file.remove(file.path(DATA_PATH,"DATA.rds"))
  }
  
  ALLSUBSET |> drop_na(Wand) -> SUBSET
  SUBSET |> filter(Type=="TDC") -> SUBSET
  saveRDS(object=SUBSET, file=file.path( DATA_PATH, "DATA.rds"))
  write.csv(SUBSET, file=file.path( DATA_PATH, "DATA.csv"))
  
  MAINPATH = "./DATA"
  DATAPATH = file.path(MAINPATH,"DATA.rds")
  MODELPATH = file.path(MAINPATH,"MODEL")
  if (file.exists(file.path(MAINPATH,"SUBSET.rds"))){
    file.remove(file.path(MAINPATH,"SUBSET.rds"))
  }
  if (dir.exists(file.path(MAINPATH,"omega-Wand__.n0000"))){
    unlink(file.path(MAINPATH,"omega-Wand__.n0000"),recursive = T)
  }
  
  
  PATHS <- ll.Create.Folders( "omega-Wand__.n0000",MAINPATH)
  file.copy(DATAPATH,file.path(PATHS,"SUBSET.rds"))
  model_name <- "base220.GGalt.bfpNA.rds" ######################################################################
  for (dirname in dir(MODELPATH)){  
    mydir <- paste0(getwd(),MODELPATH,"/",dirname)
    file.copy(mydir,PATHS$MODEL)  
  }
  HOLDER <- Load.Subset.Wrapper( Tag="omega-Wand__.n0000", LSubset=TRUE )
  HOLDER$MODEL <- readRDS( file.path( PATHS$MODEL, "base220.GGalt.fp.rds" ) )
  EXTRACT <- Extract.Wrapper( HOLDER, Store.Full=TRUE ) ## store full fitting object to use as initial point of bfpNA() re-fit [expect 12 iterations]
  summary(EXTRACT$FIT.FULL) ## standard summary of a GAMLSS fit
  if (is.null(EXTRACT$FIT.FULL)){
    next
  } else {

  getSmo(EXTRACT$FIT.FULL,what="mu") 
  getSmo(EXTRACT$FIT.FULL,what="mu")$power ##
  
  
  HOLDER$MODEL <- Make.bfpNA.model.from.extract( EXTRACT$param )
  saveRDS( HOLDER$MODEL, file.path( PATHS$MODEL,model_name) )
  EXTRACT.bfp <- Extract.Wrapper( HOLDER, Fit.Full=TRUE, start.from=EXTRACT$FIT ) ## helpful to start.from, improves convergence speed [expect 5ish iterations]
  Save.Extracted( EXTRACT.bfp, PATHS, model_name, Save.Full=TRUE )
  
  ## 320-script
  
  EXTRACT.bfp$param$BIC ## compare BIC on all fitted models
  
  
  file.copy(from=file.path(PATHS$MODEL,model_name),to=file.path(PATHS$PATH,"MODEL.rds"))
  file.copy(from=file.path(PATHS$FIT.EXTRACT,model_name),to=file.path(PATHS$PATH,"FIT.EXTRACT.rds"))
  
  
  
  ## 330-script (and 340-script)
  HOLDER <- Load.Subset.Wrapper( Tag="omega-Wand__.n0000", LSubset=TRUE, LModel=TRUE, LFit=TRUE )
  
  BOOT <- list()
  BOOT[[1]] <- Boot.Function(n=1,Base.Seed=12345,Holder=HOLDER)
  BOOT[[2]] <- Boot.Function(n=2,Base.Seed=12345,Holder=HOLDER)
  BOOT[[3]] <- Boot.Function(n=3,Base.Seed=12345,Holder=HOLDER)
  for( NUM in 4:100 ) { ## 100s of bootstrap replicates required
    cat("Bootstrap replicate: ",NUM,"\n")
    BOOT[[NUM]] <- Boot.Function(n=NUM,Base.Seed=12345,Holder=HOLDER)
  }
  
  Reduce(rbind,lapply(BOOT,function(X){X$param$mu$fixef}))
  Reduce(rbind,lapply(BOOT,function(X){X$param$sigma$fixef}))
  
  apply( Reduce(rbind,lapply(BOOT,function(X){X$param$mu$fixef})), 2, quantile, probs=c(0.05,0.95), na.rm=TRUE )
  apply( Reduce(rbind,lapply(BOOT,function(X){X$param$sigma$fixef})), 2, quantile, probs=c(0.05,0.95), na.rm=TRUE )
  apply( Reduce(rbind,lapply(BOOT,function(X){X$param$nu$fixef})), 2, quantile, probs=c(0.05,0.95), na.rm=TRUE )
  
  saveRDS(object=BOOT,file=file.path(PATHS$PATH,"BOOT.EXTRACT.rds"))
  
  DirName <- paste(MAINPATH,"/omega-Wand__.n0000",sep = "")
  Rename <- paste(MAINPATH,"/TDC","_",IndexName,sep = "")
  file.rename(DirName,Rename)  
  }
}




