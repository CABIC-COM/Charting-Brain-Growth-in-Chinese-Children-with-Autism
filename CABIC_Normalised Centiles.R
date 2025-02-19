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
library(ggplot2)
library(ggpubr)
library(ggsci)
library(foreign)
library(rstatix)

IndexName <- "" ########## 
GROUP <- "ASD" #####################
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


## output
Out_data_PATH <- file.path(DATA_PATH,"CABIC_Subjects_output_WholeBrain.csv")
data_original <- read.csv(Out_data_PATH)
## delet null and zeros
data_training <- data_original[complete.cases(data_original),]

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

## 310-script
IndexName = "" ########################################
ASD_Curve_data <- paste("ASD_",IndexName,sep = "")
TDC_Curve_data <- paste("TDC_",IndexName,sep = "")
MAINPATH = "./DATA"

###################################################################################################################
PRIMARY <- Load.Subset.Wrapper( Tag=TDC_Curve_data, LSubset=TRUE, LModel=TRUE, LFit=TRUE, LBoot=TRUE, LData=TRUE )
PRIMARY$DATA.PRED <- Apply.Param(NEWData=PRIMARY$DATA, Reference.Holder=PRIMARY, FITParam=PRIMARY$FIT.EXTRACT$param,
                                 Pred.Set=c("l025"=0.025,"l250"=0.250,"m500"=0.5,"u750"=0.750,"u975"=0.975),
                                 Add.Moments=FALSE, Add.Normalise=TRUE, Add.Derivative=FALSE,
                                 MissingToZero=TRUE, NAToZero=TRUE )
## obtain the centiles (Wand.q.wre) and normalised values (Wand.normalised)


