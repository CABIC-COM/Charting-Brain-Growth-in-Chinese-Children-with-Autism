rm(list=ls())
library(readr)
library(dplyr)
library(MASS)
library(tidyr)
library(writexl)
library(tidyverse)
library(reticulate)
source("100.common-variables.r")
source("101.common-functions.r")
source("200.variables.r")
source("201.functions.r")
library(ggplot2)
library(ggsci)
##Import pre-defined templates and data
IndexName <- "TotalCorticalGMV" ########## 修改IndexName和GROUP
GROUP <- "ASD" #####################
DATA_PATH = './DATA'
mask_data <- readRDS(file.path( DATA_PATH, "MASK_DATA.rds" ) )
real_data_PATH <- file.path(DATA_PATH,"CABIC_Subjects_info.csv")
real_data <-  read.csv(real_data_PATH)
real_data_DIM = dim(real_data)
if (file.exists(file.path(DATA_PATH,"DATA.rds"))){
  file.remove(file.path(DATA_PATH,"DATA.rds"))
}
## Replacement data
SUBSET <- mask_data |> head(n=real_data_DIM[1]) 
SUBSET |> dplyr::select(1:6,25:27) -> SUBSET
real_data[,"STUDY"] |> factor() -> SUBSET[,"Study"]
real_data[,"SEX"] |> factor() -> SUBSET[,"Grp"] 
real_data[,"GROUP"] |> factor() ->SUBSET[,"Type"]
real_data[,"SUBID"] |> factor() ->SUBSET[,"ID"]
real_data[,"HANDEDNESS"] |> factor() -> SUBSET[,"HANDEDNESS"]
SUBSET[,"TimeTransformed"] = real_data[,"AGE"]


## Replacement output
Out_data_PATH <- file.path(DATA_PATH,"CABIC_Subjects_output_WholeBrain.csv")
data_original <- read.csv(Out_data_PATH)
## delet null and zeros
data_training <- data_original[complete.cases(data_original),]
# data_original |> drop_na(TotalWMV) -> data_training

#Remove outliers,Here use 1.5xIQR rule to remove outliers.
# outlierIndexList = NULL
# indexList = NULL
# for (region in 3:ncol(data_training)){
#   Q1 = quantile(data_training[,region], .25)
#   Q3 = quantile(data_training[,region], .75)
#   IQR = IQR(data_training[,region])
#   lower_outliers = which(data_training[,region]< (Q1 - 1.5*IQR))
#   upper_outliers = which(data_training[,region]> (Q3 + 1.5*IQR))
#   all_outliers = c(lower_outliers,upper_outliers)
#   outlierIndexList = c(outlierIndexList,all_outliers)
# }
# outlierIndexList = unique(outlierIndexList)
# data_training = data_training[-outlierIndexList,]

colnames(data_training)[2] <- "ID"
merge_data <- merge(data_training,SUBSET,by = "ID")


##Harmonize multi-site training data
source_python("./combatGAM_Python4R.py")
covars_temp = merge_data[c("Study","Grp","TimeTransformed")]
covars_temp_group = covars_temp
covars_temp$Grp |> as.factor() |> as.numeric() -> covars_temp$Grp
colnames(covars_temp)[1] <- "SITE"
colnames(covars_temp)[3] <- "age"
data_temp = merge_data[,c(3:12)]
write.csv(covars_temp[,1:3],"./Curve/covars_temp.csv", row.names = FALSE) # save temporary covariates
write.csv(data_temp,"./Curve/data_temp.csv", row.names = FALSE) # save temporary data
adjustedData_model <- combatGAM_Python4R_function("./Curve./") # please cite the path of the saved temporary data and covariates
data_harmonized <- data.frame(adjustedData_model[[1]])
data_training[,c(3:ncol(data_original))] = data_harmonized
SUBSET <- merge(SUBSET,data_training,by = "ID")


SUBSET[,IndexName]/1000 ->SUBSET[,"Wand"]
SUBSET |> drop_na(Wand) -> SUBSET
SUBSET |> filter(Type==GROUP) -> SUBSET
##
## Add special columns (INDEX.ID,INDEX.OB,HANDNESS) used by later scripts
##
SUBSET[,"ID"] |> factor() ->SUBSET[,"ID"]
SUBSET$INDEX.ID <- factor( paste(SUBSET$Study,SUBSET$ID,sep="_") )
SUBSET$INDEX.TYPE <- SUBSET$Type

##

##
## Save dataset in RDS format for use in later scripts
saveRDS(object=SUBSET, file=file.path( DATA_PATH, "DATA.rds"))
write.csv(SUBSET, file=file.path( DATA_PATH, "DATA.csv"))

## Population variance

##
## Generate model sets
## For all the derived subsets, as extra scenarios, we will use the "best" model selected
## NOTE: There will be some code in the fitting script to link/copy the relevant model, then fit these scenario-subsets
##



## NOTE: FAMILY.SET allows us to explore multiple gamlss outcome distributions, later scripts will select the 'best' (via AIC/BIC/etc)
PATH_MODEL <- file.path(DATA_PATH,"MODEL") 
FAMILY.SET <- c("GGalt") ## c("GGalt","BCCG","GIG",文章中最佳输出为Wand,因此选GG)
OUTCOME <- "Wand" 
FP.SET <- matrix(c(1,0,0,
                   1,1,0,
                   1,0,1,
                   2,0,0,
                   2,1,0,
                   2,0,1,
                   2,1,1,
                   2,2,0,
                   2,2,1,
                   3,0,0,
                   3,1,0,
                   3,0,1,
                   3,1,1,
                   3,2,0,
                   3,2,1,
                   3,3,0,
                   3,3,1
),
byrow=TRUE,ncol=3,dimnames=list(NULL,c("mu","sigma","nu")))


for( lFAM in FAMILY.SET ) { ## loop to search multiple outcome distributions
  
  for( iFP in 1:NROW(FP.SET) ) {
    
    MODEL.NAME <- paste0("base",paste0(FP.SET[iFP,],collapse=""))
    
    MODEL <- list(covariates=list("Y"=OUTCOME,
                                  "X"="TimeTransformed",
                                  "ID"="ID",
                                  "BY"="Grp",
                                  "OTHER"=NULL,
                                  "COND"="Type", ## should be all equal to base case in fitted SUBSET
                                  "RANEF"="Study"),
                  family=lFAM,
                  contrasts=list("Grp"="contr.sum"), ## (*1*)
                  stratify=c("Study","Grp"),
                  mu   =if(FP.SET[iFP,"mu"]>0){
                    sprintf("%s ~ 1 + fp(TimeTransformed,npoly=%i) + Grp + random(Study)",OUTCOME,FP.SET[iFP,"mu"])
                  } else {
                    sprintf("%s ~ 1 + Grp + random(Study)",OUTCOME)
                  },
                  sigma=if(FP.SET[iFP,"sigma"]>0){
                    sprintf("%s ~ 1 + fp(TimeTransformed,npoly=%i) + Grp",OUTCOME,FP.SET[iFP,"sigma"])
                  } else {
                    sprintf("%s ~ 1 + Grp",OUTCOME)
                  },
                  nu   =if(FP.SET[iFP,"nu"]>0){
                    sprintf("%s ~ 1 + fp(TimeTransformed,npoly=%i)",OUTCOME,FP.SET[iFP,"nu"])
                  } else {
                    sprintf("%s ~ 1",OUTCOME)
                  },
                  inc.fp=TRUE)
    saveRDS(object=MODEL,file=file.path(PATH_MODEL,sprintf("%s.%s.fp.rds",MODEL.NAME,lFAM)))
  }
  
}
