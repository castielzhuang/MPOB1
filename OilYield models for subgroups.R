
# LOAD PACKAGES
library(haven)
library(ggplot2)
library(splines)
library(dlnm)
library(mgcv)
library(tidyr)
library(Hmisc)
library(rms) 
library(ggsci) #调色板
library(MASS);
library(nlme)
library(plyr)
library(splines)
library(dplyr)


###Temperature 1----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
outcome<-transform(outcome, temp_average=(temp_imputed+temp_lag1_imputed+temp_lag2_imputed+temp_lag3_imputed+temp_lag4_imputed
                                          +temp_lag5_imputed+temp_lag6_imputed+temp_lag7_imputed+temp_lag8_imputed+temp_lag9_imputed
                                          +temp_lag10_imputed+temp_lag11_imputed+temp_lag12_imputed+temp_lag13_imputed
                                          +temp_lag14_imputed+temp_lag15_imputed+temp_lag16_imputed+temp_lag17_imputed
                                          +temp_lag18_imputed+temp_lag19_imputed+temp_lag20_imputed+temp_lag21_imputed
                                          +temp_lag22_imputed+temp_lag23_imputed)/24)
quantile(outcome$temp_average, c(0:3/3))
index=which(outcome$temp_average<=299.2415)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 1/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of temperature 1----


###Temperature 2----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
outcome<-transform(outcome, temp_average=(temp_imputed+temp_lag1_imputed+temp_lag2_imputed+temp_lag3_imputed+temp_lag4_imputed
                                          +temp_lag5_imputed+temp_lag6_imputed+temp_lag7_imputed+temp_lag8_imputed+temp_lag9_imputed
                                          +temp_lag10_imputed+temp_lag11_imputed+temp_lag12_imputed+temp_lag13_imputed
                                          +temp_lag14_imputed+temp_lag15_imputed+temp_lag16_imputed+temp_lag17_imputed
                                          +temp_lag18_imputed+temp_lag19_imputed+temp_lag20_imputed+temp_lag21_imputed
                                          +temp_lag22_imputed+temp_lag23_imputed)/24)
quantile(outcome$temp_average, c(0:3/3))
index=which(outcome$temp_average>299.2415 & outcome$temp_average<=299.5595)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 2/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of temperature 2----


###Temperature 3----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
outcome<-transform(outcome, temp_average=(temp_imputed+temp_lag1_imputed+temp_lag2_imputed+temp_lag3_imputed+temp_lag4_imputed
                                          +temp_lag5_imputed+temp_lag6_imputed+temp_lag7_imputed+temp_lag8_imputed+temp_lag9_imputed
                                          +temp_lag10_imputed+temp_lag11_imputed+temp_lag12_imputed+temp_lag13_imputed
                                          +temp_lag14_imputed+temp_lag15_imputed+temp_lag16_imputed+temp_lag17_imputed
                                          +temp_lag18_imputed+temp_lag19_imputed+temp_lag20_imputed+temp_lag21_imputed
                                          +temp_lag22_imputed+temp_lag23_imputed)/24)
quantile(outcome$temp_average, c(0:3/3))
index=which(outcome$temp_average>299.5595)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/temp 3/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of temperature 3----


###Soil temperature 1----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
outcome<-transform(outcome, soil_average=(soil_imputed+soil_lag1_imputed+soil_lag2_imputed+soil_lag3_imputed+soil_lag4_imputed
                                          +soil_lag5_imputed+soil_lag6_imputed+soil_lag7_imputed+soil_lag8_imputed+soil_lag9_imputed
                                          +soil_lag10_imputed+soil_lag11_imputed+soil_lag12_imputed+soil_lag13_imputed
                                          +soil_lag14_imputed+soil_lag15_imputed+soil_lag16_imputed+soil_lag17_imputed
                                          +soil_lag18_imputed+soil_lag19_imputed+soil_lag20_imputed+soil_lag21_imputed
                                          +soil_lag22_imputed+soil_lag23_imputed)/24)
quantile(outcome$soil_average, c(0:3/3))
index=which(outcome$soil_average<=299.9611)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 1/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of soil temperature 1----


###Soil temperature 2----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
outcome<-transform(outcome, soil_average=(soil_imputed+soil_lag1_imputed+soil_lag2_imputed+soil_lag3_imputed+soil_lag4_imputed
                                          +soil_lag5_imputed+soil_lag6_imputed+soil_lag7_imputed+soil_lag8_imputed+soil_lag9_imputed
                                          +soil_lag10_imputed+soil_lag11_imputed+soil_lag12_imputed+soil_lag13_imputed
                                          +soil_lag14_imputed+soil_lag15_imputed+soil_lag16_imputed+soil_lag17_imputed
                                          +soil_lag18_imputed+soil_lag19_imputed+soil_lag20_imputed+soil_lag21_imputed
                                          +soil_lag22_imputed+soil_lag23_imputed)/24)
quantile(outcome$soil_average, c(0:3/3))
index=which(outcome$soil_average>299.9611 & outcome$soil_average<=300.4897)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 2/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of soil temperature 2----


###Soil temperature 3----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
outcome<-transform(outcome, soil_average=(soil_imputed+soil_lag1_imputed+soil_lag2_imputed+soil_lag3_imputed+soil_lag4_imputed
                                          +soil_lag5_imputed+soil_lag6_imputed+soil_lag7_imputed+soil_lag8_imputed+soil_lag9_imputed
                                          +soil_lag10_imputed+soil_lag11_imputed+soil_lag12_imputed+soil_lag13_imputed
                                          +soil_lag14_imputed+soil_lag15_imputed+soil_lag16_imputed+soil_lag17_imputed
                                          +soil_lag18_imputed+soil_lag19_imputed+soil_lag20_imputed+soil_lag21_imputed
                                          +soil_lag22_imputed+soil_lag23_imputed)/24)
quantile(outcome$soil_average, c(0:3/3))
index=which(outcome$soil_average>300.4897)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil temp 3/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of soil temperature 3----


###Precipitation 1----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
outcome<-transform(outcome, precip_average=(precip_imputed+precip_lag1_imputed+precip_lag2_imputed+precip_lag3_imputed+precip_lag4_imputed
                                            +precip_lag5_imputed+precip_lag6_imputed+precip_lag7_imputed+precip_lag8_imputed+precip_lag9_imputed
                                            +precip_lag10_imputed+precip_lag11_imputed+precip_lag12_imputed+precip_lag13_imputed
                                            +precip_lag14_imputed+precip_lag15_imputed+precip_lag16_imputed+precip_lag17_imputed
                                            +precip_lag18_imputed+precip_lag19_imputed+precip_lag20_imputed+precip_lag21_imputed
                                            +precip_lag22_imputed+precip_lag23_imputed)/24)
quantile(outcome$precip_average, c(0:3/3))
index=which(outcome$precip_average<=0.006399136)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 1/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of precipitation 1----


###Precipitation 2----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
outcome<-transform(outcome, precip_average=(precip_imputed+precip_lag1_imputed+precip_lag2_imputed+precip_lag3_imputed+precip_lag4_imputed
                                            +precip_lag5_imputed+precip_lag6_imputed+precip_lag7_imputed+precip_lag8_imputed+precip_lag9_imputed
                                            +precip_lag10_imputed+precip_lag11_imputed+precip_lag12_imputed+precip_lag13_imputed
                                            +precip_lag14_imputed+precip_lag15_imputed+precip_lag16_imputed+precip_lag17_imputed
                                            +precip_lag18_imputed+precip_lag19_imputed+precip_lag20_imputed+precip_lag21_imputed
                                            +precip_lag22_imputed+precip_lag23_imputed)/24)
quantile(outcome$precip_average, c(0:3/3))
index=which(outcome$precip_average>0.006399136 & outcome$precip_average<=0.007428345)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 2/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of precipitation 2----


###Precipitation 3----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
outcome<-transform(outcome, precip_average=(precip_imputed+precip_lag1_imputed+precip_lag2_imputed+precip_lag3_imputed+precip_lag4_imputed
                                            +precip_lag5_imputed+precip_lag6_imputed+precip_lag7_imputed+precip_lag8_imputed+precip_lag9_imputed
                                            +precip_lag10_imputed+precip_lag11_imputed+precip_lag12_imputed+precip_lag13_imputed
                                            +precip_lag14_imputed+precip_lag15_imputed+precip_lag16_imputed+precip_lag17_imputed
                                            +precip_lag18_imputed+precip_lag19_imputed+precip_lag20_imputed+precip_lag21_imputed
                                            +precip_lag22_imputed+precip_lag23_imputed)/24)
quantile(outcome$precip_average, c(0:3/3))
index=which(outcome$precip_average>0.007428345)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/precip 3/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of precipitation 3----


###West Malaysia----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$State=="SARAWAK" | outcome$State=="SABAH")
outcome<-outcome[-index,]
data_GIS1<-data_GIS1[-index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/west/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of West Malaysia----


###East Malaysia----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$State=="SARAWAK" | outcome$State=="SABAH")
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/east/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of East Malaysia----


###Low labor intensity----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$intensity_cat==1)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity low/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of low labor intensity----


###Medium-low labor intensity----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$intensity_cat==2)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med low/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of medium-low labor intensity----


###Medium-high labor intensity----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$intensity_cat==3)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity med high/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of medium-high labor intensity----


###High labor intensity----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$intensity_cat==4)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/labor intensity high/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of high labor intensity----


###Cost 0 or missing----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$cost_category==1)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost 0 or missing/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of cost 0 or missing----


###Cost low----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$cost_category==2)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost low/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of cost low----


###Cost medium-low----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$cost_category==3)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med low/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of cost medium-low----


###Cost medium-high----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$cost_category==4)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost med high/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of cost medium-high----


###Cost high----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$cost_category==5)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/cost high/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of cost high----


###Age profile immature----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$AAP_lag1yr_cat==1)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 1 immature/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of age profile immature----


###Age profile young----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$AAP_lag1yr_cat==2)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 2 young/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of age profile young----


###Age profile prime----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$AAP_lag1yr_cat==3)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 3 prime/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of age profile prime----


###Age profile ageing----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$AAP_lag1yr_cat==4)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 4 ageing/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of age profile ageing----


###Age profile old----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$AAP_lag1yr_cat==5)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/age 5 old/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of age profile old----


###Soil missing ----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$soil_category2021==1)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 0 missing/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of soil missing----


###Soil worst----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$soil_category2021==2)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 1 worst/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of soil worst----


###Soil fair----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$soil_category2021==3)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 2 fair/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of soil fair----


###Soil good----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$soil_category2021==4)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 3 good/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of soil good----


###Soil best----
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)
index=which(outcome$soil_category2021==5)
outcome<-outcome[index,]
data_GIS1<-data_GIS1[index,]

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
lk = logknots(c(0,2),23)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbMEI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/MEI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/MEI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/MEI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/MEI_cumse.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(lk)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
lk = logknots(c(0,2),23)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk))

m.all1 <- gam(OilYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/abNino1_2_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/abNino1_2_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/abNino1_2_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/abNino1_2_cumse.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(lk)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
lk = logknots(c(0,2),23)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbabNino34+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/abNino34_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/abNino34_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/abNino34_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/abNino34_cumse.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(lk)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
lk = logknots(c(0,2),23)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbONI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/ONI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/ONI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/ONI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/ONI_cumse.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(lk)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
lk = logknots(c(0,2),23)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbSOI+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/SOI_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/SOI_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/SOI_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/SOI_cumse.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(lk)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
lk = logknots(c(0,2),23)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(knots=lk)) 

m.all1 <- gam(OilYield ~ cbBEST+ PHOSROCK_year+DAP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                CPO_PriceUSD_year+
                NER_year+cases+factor(lockdown)+CPI_Overall_year+
                AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                labor_intensity, 
              data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)
#plot(pred,"slices",ci="bars",var=1,type="p",pch=16,ci.level=0.95,cex=1.5)

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/BEST_lagest.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/BEST_lagse.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/BEST_cumest.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/OilYield/soil 4 best/BEST_cumse.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(lk)
remove(m.all1)
###end of soil best----

