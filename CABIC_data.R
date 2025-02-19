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
##导入预设好的模板和数据
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

## 智商、疾病 分类型
real_data |>
  mutate(
    intellgence_type = {
      x <- vector()
      x[!is.na(real_data$PPVT) & real_data$PPVT >= 100] <- "High-level"
      x[!is.na(real_data$PPVT) & real_data$PPVT <= 100 & real_data$PPVT >= 88] <- "Medium-level"
      x[!is.na(real_data$PPVT) &  real_data$PPVT <= 87 & real_data$PPVT >= 73] <- "LOW-level"
      x[!is.na(real_data$PPVT) &  real_data$PPVT <= 72] <- "Extremely LOW-level"
      x[!is.na(real_data$PPVT) & real_data$PPVT >= 100] <- "High-level"
      x[!is.na(real_data$DQ) & real_data$DQ >= 109] <- "High-level"
      x[!is.na(real_data$DQ) & real_data$DQ <= 79 & real_data$DQ >= 70] <- "LOW-level"
      x[!is.na(real_data$DQ) & real_data$DQ <= 69] <- "Extremely LOW-level"
      x[!is.na(real_data$DQ) & real_data$DQ <= 108 & real_data$DQ >= 80] <- "Medium-level"
      x[!is.na(real_data$AQ) & real_data$AQ >= 135] <- "High-level"
      x[!is.na(real_data$AQ) & real_data$AQ <= 94 & real_data$AQ >= 60] <- "LOW-level"
      x[!is.na(real_data$AQ) & real_data$AQ <= 59] <- "Extremely LOW-level"
      x[!is.na(real_data$AQ) & real_data$AQ <= 134 & real_data$AQ >= 95] <- "Medium-level"
      x[!is.na(real_data$FIQ) & real_data$FIQ >= 110] <- "High-level"
      x[!is.na(real_data$FIQ) & real_data$FIQ <= 89 & real_data$FIQ >= 70] <- "LOW-level"
      x[!is.na(real_data$FIQ) & real_data$FIQ <= 69] <- "Extremely LOW-level"
      x[!is.na(real_data$FIQ) & real_data$FIQ <= 90 & real_data$FIQ >= 109] <- "Medium-level"
      x
    }
  ) -> real_data

real_data |>
  mutate(
    autism_type = {
      x <- vector()
      x[!is.na(real_data$ABC_TOTAL) & real_data$ABC_TOTAL >= 67] <- "Severe autism"
      x[!is.na(real_data$ABC_TOTAL) & real_data$ABC_TOTAL <= 66 & real_data$ABC_TOTAL >= 53] <- "Mild to moderate autism"
      x[!is.na(real_data$ABC_TOTAL) &  real_data$ABC_TOTAL <= 52 ] <- "Non-autism"
      x[!is.na(real_data$CARS) & real_data$CARS >= 37] <- "Severe autism"
      x[!is.na(real_data$CARS) & real_data$CARS <= 36 & real_data$CARS >= 30] <- "Mild to moderate autism"
      x[!is.na(real_data$CARS) &  real_data$CARS <= 29 ] <- "Non-autism"
      x[!is.na(real_data$ADOS_SA) &real_data$ADOS ==1 & real_data$ADOS_SA >= 12] <- "Severe autism"
      x[!is.na(real_data$ADOS_SA) &real_data$ADOS ==1 & real_data$AGE < 3 & real_data$ADOS_SA < 12 & real_data$ADOS_SA >= 7] <- "Mild to moderate autism"
      x[!is.na(real_data$ADOS_SA) &real_data$ADOS ==1 & real_data$AGE >= 3 & real_data$ADOS_SA < 12 & real_data$ADOS_SA >= 8] <- "Mild to moderate autism"
      x[!is.na(real_data$ADOS_SA) &real_data$ADOS ==1 & real_data$AGE < 3 & real_data$ADOS_SA < 7] <- "Non-autism"
      x[!is.na(real_data$ADOS_SA) &real_data$ADOS ==1 & real_data$AGE >= 3 & real_data$ADOS_SA < 8] <- "Non-autism"
      x[!is.na(real_data$ADOS_SA) &real_data$ADOS ==2 & real_data$AGE < 3 & real_data$ADOS_SA+real_data$ADOS_RRB >= 16] <- "Severe autism"
      x[!is.na(real_data$ADOS_SA) &real_data$ADOS ==2 & real_data$AGE >= 3 & real_data$ADOS_SA+real_data$ADOS_RRB >= 9] <- "Severe autism"
      x[!is.na(real_data$ADOS_SA) &real_data$ADOS ==2 & real_data$AGE < 3 & real_data$ADOS_SA+real_data$ADOS_RRB >= 11 & real_data$ADOS_SA+real_data$ADOS_RRB <= 15] <- "Mild to moderate autism"
      x[!is.na(real_data$ADOS_SA) &real_data$ADOS ==2 & real_data$AGE >= 3 & real_data$ADOS_SA+real_data$ADOS_RRB >= 7 & real_data$ADOS_SA+real_data$ADOS_RRB <= 8] <- "Mild to moderate autism"
      x[!is.na(real_data$ADOS_SA) &real_data$ADOS ==2 & real_data$AGE < 3 & real_data$ADOS_SA+real_data$ADOS_RRB <= 10] <- "Non-autism"
      x[!is.na(real_data$ADOS_SA) &real_data$ADOS ==2 & real_data$AGE >= 3 & real_data$ADOS_SA+real_data$ADOS_RRB <= 6] <- "Non-autism"
      x
    }
  ) -> real_data
## social deficts type
real_data |>
  mutate(
    social_type = {
      x <- vector()
      x[!is.na(real_data$SRS_TOTAL) & real_data$SRS_TOTAL >= 91] <- "Severe social deficits"
      x[!is.na(real_data$SRS_TOTAL) & real_data$SRS_TOTAL >= 76 & real_data$SRS_TOTAL<= 90] <- "Moderate social deficits"
      x[!is.na(real_data$SRS_TOTAL) & real_data$SRS_TOTAL >= 61 & real_data$SRS_TOTAL<= 75] <- "Mild social deficits"
      x[!is.na(real_data$SRS_TOTAL) & real_data$SRS_TOTAL <= 60] <- "No social deficits"
      x
    }
  ) -> real_data
write.csv(real_data,"./DATA/CABIC_Subjects_info_SUBTYPE.csv", row.names = FALSE) 


## 替换数据
SUBSET <- mask_data |> head(n=real_data_DIM[1]) 
SUBSET |> dplyr::select(1:6,25:27) -> SUBSET
real_data[,"STUDY"] |> factor() -> SUBSET[,"Study"]
real_data[,"SEX"] |> factor() -> SUBSET[,"Grp"] 
real_data[,"GROUP"] |> factor() ->SUBSET[,"Type"]
real_data[,"SUBID"] |> factor() ->SUBSET[,"ID"]
real_data[,"HANDEDNESS"] |> factor() -> SUBSET[,"HANDEDNESS"]
SUBSET[,"TimeTransformed"] = real_data[,"AGE"]


## 替换output
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