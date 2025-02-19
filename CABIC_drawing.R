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
library(ggplot2)
library(ggsci)
## 310-script
IndexName = "" ########################################
ASD_Curve_data <- paste("ASD_",IndexName,sep = "")
TDC_Curve_data <- paste("TDC_",IndexName,sep = "")
MAINPATH = "./DATA"

###################################################################################################################
##ASD Curve
ASD_DATA_PATH <- paste(MAINPATH,"/",ASD_Curve_data,"/SUBSET.rds",sep = "")
file.copy(ASD_DATA_PATH,MAINPATH) 
file_old_name <- paste(MAINPATH,"/SUBSET.rds",sep = "")
file_new_name <- paste(MAINPATH,"/DATA.rds",sep = "")
file.rename(file_old_name,file_new_name)

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
TDC_DATA_PATH <- paste(MAINPATH,"/",TDC_Curve_data,"/SUBSET.rds",sep = "")
file.copy(TDC_DATA_PATH,MAINPATH) 
file_old_name <- paste(MAINPATH,"/SUBSET.rds",sep = "")
file_new_name <- paste(MAINPATH,"/DATA.rds",sep = "")
file.rename(file_old_name,file_new_name)

PRIMARY <- Load.Subset.Wrapper( Tag=TDC_Curve_data, LSubset=TRUE, LModel=TRUE, LFit=TRUE, LBoot=TRUE, LData=TRUE )
PRIMARY$DATA.PRED <- Apply.Param(NEWData=PRIMARY$DATA, Reference.Holder=PRIMARY, FITParam=PRIMARY$FIT.EXTRACT$param,
                                 Pred.Set=c("l025"=0.025,"l250"=0.250,"m500"=0.5,"u750"=0.750,"u975"=0.975),
                                 Add.Moments=FALSE, Add.Normalise=TRUE, Add.Derivative=FALSE,
                                 MissingToZero=TRUE, NAToZero=TRUE )


head(PRIMARY$DATA.PRED)
tail(PRIMARY$DATA.PRED)
range(PRIMARY$DATA[,"TimeTransformed"]) ## whole dataset
PRIMARY$CURVE <- Apply.Param(NEWData=expand.grid(list(
  TimeTransformed=seq(1.5,12.5,length.out=2^9),   Grp=c("Female","Male")
)),
FITParam=PRIMARY$FIT.EXTRACT$param )

TDC_male=subset(PRIMARY$CURVE,Grp=="Male")
TDC_male$Grp <- "TDC"
TDC_female=subset(PRIMARY$CURVE,Grp=="Female")
TDC_female$Grp <- "TDC"


TDC_male <- TDC_male[,c("TimeTransformed","Grp","PRED.m500.pop","PRED.l025.pop","PRED.u975.pop","PRED.l250.pop","PRED.u750.pop","PRED.variance.pop")]
ASD_male <- ASD_male[,c("TimeTransformed","Grp","PRED.m500.pop","PRED.l025.pop","PRED.u975.pop","PRED.l250.pop","PRED.u750.pop","PRED.variance.pop")]
TDC_female <- TDC_female[,c("TimeTransformed","Grp","PRED.m500.pop","PRED.l025.pop","PRED.u975.pop","PRED.l250.pop","PRED.u750.pop","PRED.variance.pop")]
ASD_female <- ASD_female[,c("TimeTransformed","Grp","PRED.m500.pop","PRED.l025.pop","PRED.u975.pop","PRED.l250.pop","PRED.u750.pop","PRED.variance.pop")]



p <- ggplot(Male_Curve, aes(x = TimeTransformed, group = Grp, color = Grp)) +
  labs(x = "Age", y = IndexName, size = 15) +
  geom_smooth(aes(y = PRED.l250.pop), method = 'loess', linetype = 'dotted', size = 5, se = FALSE, fullrange = TRUE) +
  geom_smooth(aes(y = PRED.u750.pop), method = 'loess', linetype = 'dotted', size = 5, se = FALSE, fullrange = TRUE) +
  geom_smooth(aes(y = PRED.m500.pop), method = 'loess', linetype = 'solid', size = 6, se = FALSE, fullrange = TRUE) +
  theme_bw() +
  theme(
    axis.text = element_text(size = 25, colour = "black"),
    axis.text.x = element_text(size = 45, face = "bold"),  
    axis.text.y = element_text(size = 45, face = "bold"),   
    axis.line = element_line(size = 2, color = "black"),
    axis.ticks = element_line(size = 1.5, color = "black"),
    axis.ticks.length = unit(15, "pt"),
    plot.title = element_text(hjust = 0.5, size = 15),
    legend.position = "none",    legend.title = element_blank(),
    legend.key.size = unit(40, "point"),
    legend.text = element_text(size = 18),
    panel.border = element_rect(color = "black", size = 2, fill = NA)) + # 设置上、右边框
  coord_cartesian(clip = "off")+  
  scale_color_nejm(alpha = 0.6) 



##Filtering only a particular study and plotting study-specific population curves
# range(PRIMARY$DATA[PRIMARY$DATA$Study=="SYSU","TimeTransformed"]) ## only study SYSU
# PRIMARY$CURVE.E <- Apply.Param(NEWData=expand.grid(list(
#   TimeTransformed=seq(0,9,length.out=2^8),
#   Grp=c("Female","Male"),
#   Study="SYSU"
# )),
# FITParam=PRIMARY$FIT.EXTRACT$param )
# ##plot
# RANGE <- range(PRIMARY$DATA[PRIMARY$DATA$Study=="SYSU","TimeTransformed"])



