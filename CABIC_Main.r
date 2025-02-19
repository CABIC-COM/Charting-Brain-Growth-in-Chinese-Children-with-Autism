rm(list=ls())
source("100.common-variables.r")
source("101.common-functions.r")
source("300.variables.r")
source("301.functions.r")
library(readr)
library(dplyr)
library(tidyr)
library(writexl)
library(tidyverse)
library(reticulate)

## 310-script
IndexName = "" ########################################
GROUP <- "ASD" #####################
MAINPATH = "./DATA"
DATAPATH = file.path(MAINPATH,"DATA.rds")
MODELPATH = file.path(MAINPATH,"MODEL")
if (file.exists(file.path(MAINPATH,"SUBSET.rds"))){
  file.remove(file.path(MAINPATH,"SUBSET.rds"))
}
if (dir.exists(file.path(MAINPATH,"omega-Wand__.n0000"))){
  unlink(file.path(MAINPATH,"omega-Wand__.n0000"),recursive = T)
}


PATHS <- ll.Create.Folders( "omega-Wand__.n0000",MAINPATH )
file.copy(DATAPATH,file.path(PATHS,"SUBSET.rds"))
model_name <- "base220.GGalt.bfpNA.rds" ######################################################################
for (dirname in dir(MODELPATH)){  
  mydir <- paste0(getwd(),MODELPATH,"/",dirname)
  file.copy(mydir,PATHS$MODEL)  
}
HOLDER <- Load.Subset.Wrapper( Tag="omega-Wand__.n0000", LSubset=TRUE )
HOLDER$MODEL <- readRDS( file.path( PATHS$MODEL, "base220.GGalt.fp.rds" ) )##The model can be replaced here for retesting, and the optimal model can be filtered out by comparing the BIC
## The generalized gamma outcome distribution (GG) is used here
## The original gamlss package was fitted using fp
EXTRACT <- Extract.Wrapper( HOLDER, Store.Full=TRUE ) ## store full fitting object to use as initial point of bfpNA() re-fit [expect 12 iterations]
summary(EXTRACT$FIT.FULL) ## standard summary of a GAMLSS fit

##Since the fractional polynomial is added as a smoothing term, we can access the fitted smoothing term using getSmo()
##Optimal second-order fractional polynomials have powers of 1 and 3 (i.e., linear and cubic terms with respect to age)
getSmo(EXTRACT$FIT.FULL,what="mu") 
getSmo(EXTRACT$FIT.FULL,what="mu")$power ##


##Refit using bfp, removing the constant intercept term makes model interpretation simpler
HOLDER$MODEL <- Make.bfpNA.model.from.extract( EXTRACT$param )
saveRDS( HOLDER$MODEL, file.path( PATHS$MODEL,model_name) )
EXTRACT.bfp <- Extract.Wrapper( HOLDER, Fit.Full=TRUE, start.from=EXTRACT$FIT ) ## helpful to start.from, improves convergence speed [expect 5ish iterations]
Save.Extracted( EXTRACT.bfp, PATHS, model_name, Save.Full=TRUE )

## 320-script

EXTRACT.bfp$param$BIC ## compare BIC on all fitted models


file.copy(from=file.path(PATHS$MODEL,model_name),to=file.path(PATHS$PATH,"MODEL.rds"))
file.copy(from=file.path(PATHS$FIT.EXTRACT,model_name),to=file.path(PATHS$PATH,"FIT.EXTRACT.rds"))



## 330-script (and 340-script)Confidence intervals were determined by Bootsrap
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


###################
##Change folder name
DirName <- paste(MAINPATH,"/omega-Wand__.n0000",sep = "")
Rename <- paste(MAINPATH,"/",GROUP,"_",IndexName,sep = "")
file.rename(DirName,Rename)
