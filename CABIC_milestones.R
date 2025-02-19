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
library(ggsci)


###############################################################
IndexName = "" ########################################
IndexName.x <- "" ##################################################
ASD_Curve_data <- paste("ASD_",IndexName,sep = "")
TDC_Curve_data <- paste("TDC_",IndexName,sep = "")
MAINPATH = "./DATA"
DATA_PATH = "./DATA"
###################################################################################################################
##ASD Curve


GROUP <- "ASD" #####################
mask_data <- readRDS(file.path( DATA_PATH, "MASK_DATA.rds" ) )
real_data_PATH <- file.path(DATA_PATH,"CABIC_Subjects_info.csv")
real_data <-  read.csv(real_data_PATH)
real_data_DIM = dim(real_data)
if (file.exists(file.path(DATA_PATH,"DATA.rds"))){
  file.remove(file.path(DATA_PATH,"DATA.rds"))
}
## data
SUBSET <- mask_data |> head(n=real_data_DIM[1]) 
SUBSET |> dplyr::select(1:6,25:27) -> SUBSET
real_data[,"STUDY"] |> factor() -> SUBSET[,"Study"]
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
SUBSET <- merge(SUBSET,data_training)
SUBSET[,IndexName.x]/1000 ->SUBSET[,"Wand"]
SUBSET |> drop_na(Wand) -> SUBSET
SUBSET |> filter(Type==GROUP) -> SUBSET
SUBSET[,"ID"] |> factor() ->SUBSET[,"ID"]
SUBSET$INDEX.ID <- factor( paste(SUBSET$Study,SUBSET$ID,sep="_") )
SUBSET$INDEX.TYPE <- SUBSET$Type
saveRDS(object=SUBSET, file=file.path( DATA_PATH, "DATA.rds"))
write.csv(SUBSET, file=file.path( DATA_PATH, "DATA.csv"))




PRIMARY <- Load.Subset.Wrapper( Tag=ASD_Curve_data, LSubset=TRUE, LModel=TRUE, LFit=TRUE, LBoot=TRUE, LData=TRUE )
PRIMARY$DATA.PRED <- Apply.Param(NEWData=PRIMARY$DATA, Reference.Holder=PRIMARY, FITParam=PRIMARY$FIT.EXTRACT$param,
                                 Pred.Set=c("l025"=0.025,"l250"=0.250,"m500"=0.5,"u750"=0.750,"u975"=0.975),
                                 Add.Moments=FALSE, Add.Normalise=TRUE, Add.Derivative=FALSE,
                                 MissingToZero=TRUE, NAToZero=TRUE )


head(PRIMARY$DATA.PRED)
tail(PRIMARY$DATA.PRED)
range(PRIMARY$DATA[,"TimeTransformed"]) ## whole dataset
PRIMARY$CURVE <- Apply.Param(NEWData=expand.grid(list(
  TimeTransformed=seq(1.5,12.5,length.out=2^9),
  Grp=c("Female","Male")
)),
FITParam=PRIMARY$FIT.EXTRACT$param )

ASD_male=subset(PRIMARY$CURVE,Grp=="Male")
ASD_male$Grp <- "ASD"
ASD_female=subset(PRIMARY$CURVE,Grp=="Female")
ASD_female$Grp <- "ASD"

###################################################################################################################
##TDC Curve
GROUP <- "TDC" #####################
mask_data <- readRDS(file.path( DATA_PATH, "MASK_DATA.rds" ) )
real_data_PATH <- file.path(DATA_PATH,"CABIC_Subjects_info.csv")
real_data <-  read.csv(real_data_PATH)
real_data_DIM = dim(real_data)
if (file.exists(file.path(DATA_PATH,"DATA.rds"))){
  file.remove(file.path(DATA_PATH,"DATA.rds"))
}

SUBSET <- mask_data |> head(n=real_data_DIM[1]) 
SUBSET |> dplyr::select(1:6,25:27) -> SUBSET
real_data[,"STUDY"] |> factor() -> SUBSET[,"Study"]
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
SUBSET <- merge(SUBSET,data_training)
SUBSET[,IndexName.x]/1000 ->SUBSET[,"Wand"]
SUBSET |> drop_na(Wand) -> SUBSET
SUBSET |> filter(Type==GROUP) -> SUBSET
SUBSET[,"ID"] |> factor() ->SUBSET[,"ID"]
SUBSET$INDEX.ID <- factor( paste(SUBSET$Study,SUBSET$ID,sep="_") )
SUBSET$INDEX.TYPE <- SUBSET$Type
saveRDS(object=SUBSET, file=file.path( DATA_PATH, "DATA.rds"))
write.csv(SUBSET, file=file.path( DATA_PATH, "DATA.csv"))

PRIMARY <- Load.Subset.Wrapper( Tag=TDC_Curve_data, LSubset=TRUE, LModel=TRUE, LFit=TRUE, LBoot=TRUE, LData=TRUE )
PRIMARY$DATA.PRED <- Apply.Param(NEWData=PRIMARY$DATA, Reference.Holder=PRIMARY, FITParam=PRIMARY$FIT.EXTRACT$param,
                                 Pred.Set=c("l025"=0.025,"l250"=0.250,"m500"=0.5,"u750"=0.750,"u975"=0.975),
                                 Add.Moments=FALSE, Add.Normalise=TRUE, Add.Derivative=FALSE,
                                 MissingToZero=TRUE, NAToZero=TRUE )


head(PRIMARY$DATA.PRED)
tail(PRIMARY$DATA.PRED)
range(PRIMARY$DATA[,"TimeTransformed"]) ## whole dataset
PRIMARY$CURVE <- Apply.Param(NEWData=expand.grid(list(
  TimeTransformed=seq(1.5,12.5,length.out=2^9),  Grp=c("Female","Male")
)),
FITParam=PRIMARY$FIT.EXTRACT$param )

TDC_male=subset(PRIMARY$CURVE,Grp=="Male")
TDC_male$Grp <- "TDC"
TDC_female=subset(PRIMARY$CURVE,Grp=="Female")
TDC_female$Grp <- "TDC"

TDC_male <- TDC_male[,c("TimeTransformed","Grp","PRED.m500.pop","PRED.l025.pop","PRED.u975.pop","PRED.l250.pop","PRED.u750.pop")]
ASD_male <- ASD_male[,c("TimeTransformed","Grp","PRED.m500.pop","PRED.l025.pop","PRED.u975.pop","PRED.l250.pop","PRED.u750.pop")]
TDC_female <- TDC_female[,c("TimeTransformed","Grp","PRED.m500.pop","PRED.l025.pop","PRED.u975.pop","PRED.l250.pop","PRED.u750.pop")]
TimeTransformed <- ASD_female[,c("TimeTransformed","Grp","PRED.m500.pop","PRED.l025.pop","PRED.u975.pop","PRED.l250.pop","PRED.u750.pop")]

Group_difference <- data.frame(matrix(nrow = nrow(ASD_male), ncol = 0))
Group_difference$TimeTransformed <- ASD_male$TimeTransformed
Group_difference$Grp <- IndexName
Group_difference$diff <- ASD_male$PRED.m500.pop - TDC_male$PRED.m500.pop
Group_difference$diff_Z <- Group_difference$diff/max(abs(Group_difference$diff))
Group_Alldifference <-Group_difference

TDC_male=subset(PRIMARY$CURVE,Grp=="Male")
TDC_male$Grp <- "TDC"
TDC_female=subset(PRIMARY$CURVE,Grp=="Female")
TDC_female$Grp <- "TDC"

TDC_male <- TDC_male[,c("TimeTransformed","Grp","PRED.m500.pop","PRED.l025.pop","PRED.u975.pop","PRED.l250.pop","PRED.u750.pop")]
ASD_male <- ASD_male[,c("TimeTransformed","Grp","PRED.m500.pop","PRED.l025.pop","PRED.u975.pop","PRED.l250.pop","PRED.u750.pop")]
TDC_female <- TDC_female[,c("TimeTransformed","Grp","PRED.m500.pop","PRED.l025.pop","PRED.u975.pop","PRED.l250.pop","PRED.u750.pop")]
TimeTransformed <- ASD_female[,c("TimeTransformed","Grp","PRED.m500.pop","PRED.l025.pop","PRED.u975.pop","PRED.l250.pop","PRED.u750.pop")]

Group_difference <- data.frame(matrix(nrow = nrow(ASD_male), ncol = 0))
Group_difference$TimeTransformed <- ASD_male$TimeTransformed
Group_difference$Grp <- IndexName
Group_difference$diff <- ASD_male$PRED.m500.pop - TDC_male$PRED.m500.pop
Group_difference$diff_Z <- Group_difference$diff/max(abs(Group_difference$diff))
Group_Alldifference <- rbind(Group_Alldifference,Group_difference)

####################################################################

colnames(Group_Alldifference)[2] <- "MRIPhenotype"
p1 = ggplot(Group_Alldifference,aes(x = TimeTransformed,group = MRIPhenotype, color=MRIPhenotype))+
    labs(x = "Age", y = "Group Difference Z_scores (ASD-TDC)", size =30)+
    geom_smooth(aes(y = diff_Z),
                method = 'loess',
                linetype = "solid",
                size = 3,
                se = FALSE,
                alpha=0.6)+
    theme_bw ( )+
    theme(axis.text = element_text (size = 15, colour = "black"),
             axis.title = element_text (size =20, colour = "black"),
            legend.position = c (0.9, 0.8), 
            legend.background = element_blank (),
            legend.text = element_text (size = 15))
p1_npg = p1 + scale_color_npg(alpha=0.6) 
save_path <- paste("",".png",sep = "")
ggsave(save_path, width = 15, height = 8,dpi = 300)  

