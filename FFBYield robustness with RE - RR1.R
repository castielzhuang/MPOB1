
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData")

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
library(MASS)
library(nlme)
library(plyr)
library(splines)
library(dplyr)

colnames(data_GIS1)
outcome<-data_GIS1[,c(1:253,493:535)]
colnames(outcome)

### Choose lags to be included based on MEI----
# for (i in 3:24){
#   colnames(data_GIS1)
#   MEI<-data_GIS1[,c(243,253:(253+i-1))]
#   colnames(MEI)
#   colnames(MEI)[1]=c("MEI_lag0")
#   colnames(MEI)
#   cbMEI <- crossbasis(MEI,lag=c(0,i),argvar=list(fun="bs",degree=2,df=3), arglag=list(fun="bs",degree=2,df=3))
# 
#   m.all1 <- glmmPQL(FFBYield ~ cbMEI+ PHOSROCK_year+DAP_year+TSP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
#                       CPO_PriceUSD_year+
#                       NER_year+cases+factor(lockdown)+CPI_Overall_year+
#                       soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
#                       AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
#                       cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
#                       labor_intensity,random=~1|ID,
#                     data=outcome, family=gaussian(link = "identity"))
# 
#   print(AIC(m.all1))
# }
### We found including 0 to 23-month lags the optimal----

colnames(data_GIS1)
MEI<-data_GIS1[,c(243,253:275)]
colnames(MEI)
colnames(MEI)[1]=c("MEI_lag0")
colnames(MEI)
cbMEI <- crossbasis(MEI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(fun="bs",degree=2,df=3))

m.all1 <- glmmPQL(FFBYield ~ cbMEI+ PHOSROCK_year+DAP_year+TSP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                    CPO_PriceUSD_year+
                    NER_year+cases+factor(lockdown)+CPI_Overall_year+
                    soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                    AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                    cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                    labor_intensity,random=~1|ID,
                  data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbMEI,m.all1,cumul=TRUE,cen=0)

#####overall 
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/Overall MEIv2RR1.tiff", width = 3000, height = 2500, res = 300)
redcum_1 <- crossreduce(cbMEI, m.all1, type="overall", lag=23,cen=0)
par(mai=c(1,1,0.25,1),lwd=5)
plot<-plot(redcum_1,col=1,lty=1,ci.arg=list(col="Gainsboro"),lwd=10,xlab="MEI",ylab="",cex.lab=2.5,cex.axis=2.5)
ind1 <- pred$predvar<=0
ind2 <- pred$predvar>=0
lines(pred$predvar[ind1],pred$allfit[ind1],col="DeepSkyBlue4",lwd=10)
lines(pred$predvar[ind2],pred$allfit[ind2],col="Red3",lwd=10)
#abline(v=quantile(pred$predvar,c(50)/100,na.rm=T), col = "gray", lwd = 2)
abline(v=-0.5, col = "DeepSkyBlue4", lwd = 5, lty = 2) #La Nina
abline(v=0.5, col = "Red3", lwd = 5, lty = 2) #weak El Nino
abline(v=1, col = "Red3", lwd = 5, lty = 2) #moderate El Nino

par(new=T)
par(mai=c(1,1,5.5,1),lwd=5)
hist(data_GIS1$MEI,col = rgb(240, 128, 128, 50, maxColorValue=255),breaks=10,axes=F,ann=F,xaxt="n",cex.axis=2.5,freq = F,border="Black")
axis(side=4,cex.axis=2.5,yaxt="n")
axis(side=4,at=c(0.0,0.3,0.6),labels=c(0.0,0.3,0.6),cex.lab=2.5,cex.axis=2.5,lwd=5) 
dev.off()

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/MEI_lagestRR1.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/MEI_lagseRR1.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/MEI_cumestRR1.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/MEI_cumseRR1.csv")

remove(MEI)
remove(cbMEI)
remove(pred)
remove(redcum_1)
remove(ind2)
remove(ind1)
remove(plot)
remove(m.all1)


###abNino1_2
colnames(data_GIS1)
abNino1_2<-data_GIS1[,c(244,277:299)]
colnames(abNino1_2)
colnames(abNino1_2)[1]=c("abNino1_2_lag0")
colnames(abNino1_2)
cbabNino1_2 <- crossbasis(abNino1_2,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(fun="bs",degree=2,df=3))

m.all1 <- glmmPQL(FFBYield ~ cbabNino1_2+ PHOSROCK_year+DAP_year+TSP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                    CPO_PriceUSD_year+
                    NER_year+cases+factor(lockdown)+CPI_Overall_year+
                    soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                    AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                    cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                    labor_intensity,random=~1|ID,
                  data=outcome, family=gaussian(link = "identity"))


pred <- crosspred(cbabNino1_2,m.all1,cumul=TRUE,cen=0)

#####overall 
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/Overall abNino1_2v2RR1.tiff", width = 3000, height = 2500, res = 300)
redcum_1 <- crossreduce(cbabNino1_2, m.all1, type="overall", lag=23,cen=0)
par(mai=c(1,1,0.25,1),lwd=5)
plot<-plot(redcum_1,col=1,lty=1,ci.arg=list(col="Gainsboro"),lwd=10,xlab="Nino 1+2",ylab="",cex.lab=2.5,cex.axis=2.5)
ind1 <- pred$predvar<=0
ind2 <- pred$predvar>=0
lines(pred$predvar[ind1],pred$allfit[ind1],col="DeepSkyBlue4",lwd=10)
lines(pred$predvar[ind2],pred$allfit[ind2],col="Red3",lwd=10)
#abline(v=quantile(pred$predvar,c(50)/100,na.rm=T), col = "gray", lwd = 2)
abline(v=-0.5, col = "DeepSkyBlue4", lwd = 5, lty = 2) #La Nina
abline(v=0.5, col = "Red3", lwd = 5, lty = 2) #weak El Nino
abline(v=1, col = "Red3", lwd = 5, lty = 2) #moderate El Nino

par(new=T)
par(mai=c(1,1,5.5,1),lwd=5)
hist(data_GIS1$abNino1_2,col = rgb(240, 128, 128, 50, maxColorValue=255),breaks=10,axes=F,ann=F,xaxt="n",cex.axis=2.5,freq = F,border="Black")
axis(side=4,cex.axis=2.5,yaxt="n")
axis(side=4,at=c(0.0,0.3,0.6),labels=c(0.0,0.3,0.6),cex.lab=2.5,cex.axis=2.5,lwd=5) 
dev.off()

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/abNino1_2_lagestRR1.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/abNino1_2_lagseRR1.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/abNino1_2_cumestRR1.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/abNino1_2_cumseRR1.csv")

remove(abNino1_2)
remove(cbabNino1_2)
remove(pred)
remove(redcum_1)
remove(ind2)
remove(ind1)
remove(plot)
remove(m.all1)


###abNino34
colnames(data_GIS1)
abNino34<-data_GIS1[,c(247,349:371)]
colnames(abNino34)
colnames(abNino34)[1]=c("abNino34_lag0")
colnames(abNino34)
cbabNino34 <- crossbasis(abNino34,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(fun="bs",degree=2,df=3)) 

m.all1 <- glmmPQL(FFBYield ~ cbabNino34+ PHOSROCK_year+DAP_year+TSP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                    CPO_PriceUSD_year+
                    NER_year+cases+factor(lockdown)+CPI_Overall_year+
                    soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                    AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                    cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                    labor_intensity,random=~1|ID,
                  data=outcome, family=gaussian(link = "identity"), control=lmeControl(opt = "optim"))

pred <- crosspred(cbabNino34,m.all1,cumul=TRUE,cen=0)

#####overall 
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/Overall abNino34v2RR1.tiff", width = 3000, height = 2500, res = 300)
redcum_1 <- crossreduce(cbabNino34, m.all1, type="overall", lag=23,cen=0)
par(mai=c(1,1,0.25,1),lwd=5)
plot<-plot(redcum_1,col=1,lty=1,ci.arg=list(col="Gainsboro"),lwd=10,xlab="Nino 3.4",ylab="",cex.lab=2.5,cex.axis=2.5)
ind1 <- pred$predvar<=0
ind2 <- pred$predvar>=0
lines(pred$predvar[ind1],pred$allfit[ind1],col="DeepSkyBlue4",lwd=10)
lines(pred$predvar[ind2],pred$allfit[ind2],col="Red3",lwd=10)
#abline(v=quantile(pred$predvar,c(50)/100,na.rm=T), col = "gray", lwd = 2)
abline(v=-0.5, col = "DeepSkyBlue4", lwd = 5, lty = 2) #La Nina
abline(v=0.5, col = "Red3", lwd = 5, lty = 2) #weak El Nino
abline(v=1, col = "Red3", lwd = 5, lty = 2) #moderate El Nino

par(new=T)
par(mai=c(1,1,5.5,1),lwd=5)
hist(data_GIS1$abNino34,col = rgb(240, 128, 128, 50, maxColorValue=255),breaks=10,axes=F,ann=F,xaxt="n",cex.axis=2.5,freq = F,border="Black")
axis(side=4,cex.axis=2.5,yaxt="n")
axis(side=4,at=c(0.0,0.3,0.6),labels=c(0.0,0.3,0.6),cex.lab=2.5,cex.axis=2.5,lwd=5) 
dev.off()

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/abNino34_lagestRR1.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/abNino34_lagseRR1.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/abNino34_cumestRR1.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/abNino34_cumseRR1.csv")

remove(abNino34)
remove(cbabNino34)
remove(pred)
remove(redcum_1)
remove(ind2)
remove(ind1)
remove(plot)
remove(m.all1)


###ONI
colnames(data_GIS1)
ONI<-data_GIS1[,c(250,421:443)]
colnames(ONI)
colnames(ONI)[1]=c("ONI_lag0")
colnames(ONI)
cbONI <- crossbasis(ONI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(fun="bs",degree=2,df=3)) 

m.all1 <- glmmPQL(FFBYield ~ cbONI+ PHOSROCK_year+DAP_year+TSP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                    CPO_PriceUSD_year+
                    NER_year+cases+factor(lockdown)+CPI_Overall_year+
                    soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                    AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                    cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                    labor_intensity,random=~1|ID,
                  data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbONI,m.all1,cumul=TRUE,cen=0)

#####overall 
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/Overall ONIv2RR1.tiff", width = 3000, height = 2500, res = 300)
redcum_1 <- crossreduce(cbONI, m.all1, type="overall", lag=23,cen=0)
par(mai=c(1,1,0.25,1),lwd=5)
plot<-plot(redcum_1,col=1,lty=1,ci.arg=list(col="Gainsboro"),lwd=10,xlab="ONI",ylab="",cex.lab=2.5,cex.axis=2.5)
ind1 <- pred$predvar<=0
ind2 <- pred$predvar>=0
lines(pred$predvar[ind1],pred$allfit[ind1],col="DeepSkyBlue4",lwd=10)
lines(pred$predvar[ind2],pred$allfit[ind2],col="Red3",lwd=10)
#abline(v=quantile(pred$predvar,c(50)/100,na.rm=T), col = "gray", lwd = 2)
abline(v=-0.5, col = "DeepSkyBlue4", lwd = 5, lty = 2) #La Nina
abline(v=0.5, col = "Red3", lwd = 5, lty = 2) #weak El Nino
abline(v=1, col = "Red3", lwd = 5, lty = 2) #moderate El Nino

par(new=T)
par(mai=c(1,1,5.5,1),lwd=5)
hist(data_GIS1$ONI,col = rgb(240, 128, 128, 50, maxColorValue=255),breaks=10,axes=F,ann=F,xaxt="n",cex.axis=2.5,freq = F,border="Black")
axis(side=4,cex.axis=2.5,yaxt="n")
axis(side=4,at=c(0.0,0.3,0.6),labels=c(0.0,0.3,0.6),cex.lab=2.5,cex.axis=2.5,lwd=5) 
dev.off()

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/ONI_lagestRR1.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/ONI_lagseRR1.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/ONI_cumestRR1.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/ONI_cumseRR1.csv")

remove(ONI)
remove(cbONI)
remove(pred)
remove(redcum_1)
remove(ind2)
remove(ind1)
remove(plot)
remove(m.all1)


###SOI
colnames(data_GIS1)
SOI<-data_GIS1[,c(251,445:467)]*(-1)
colnames(SOI)
colnames(SOI)[1]=c("SOI_lag0")
colnames(SOI)
cbSOI <- crossbasis(SOI,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(fun="bs",degree=2,df=3)) 

m.all1 <- glmmPQL(FFBYield ~ cbSOI+ PHOSROCK_year+DAP_year+TSP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                    CPO_PriceUSD_year+
                    NER_year+cases+factor(lockdown)+CPI_Overall_year+
                    soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                    AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                    cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                    labor_intensity,random=~1|ID,
                  data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbSOI,m.all1,cumul=TRUE,cen=0)

#####overall 
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/Overall SOIv2RR1.tiff", width = 3000, height = 2500, res = 300)
redcum_1 <- crossreduce(cbSOI, m.all1, type="overall", lag=23,cen=0)
par(mai=c(1,1,0.25,1),lwd=5)
plot<-plot(redcum_1,col=1,lty=1,ci.arg=list(col="Gainsboro"),lwd=10,xlab="Negative SOI",ylab="",cex.lab=2.5,cex.axis=2.5)
ind1 <- pred$predvar<=0
ind2 <- pred$predvar>=0
lines(pred$predvar[ind1],pred$allfit[ind1],col="DeepSkyBlue4",lwd=10)
lines(pred$predvar[ind2],pred$allfit[ind2],col="Red3",lwd=10)
#abline(v=quantile(pred$predvar,c(50)/100,na.rm=T), col = "gray", lwd = 2)
abline(v=-0.5, col = "DeepSkyBlue4", lwd = 5, lty = 2) #La Nina
abline(v=0.5, col = "Red3", lwd = 5, lty = 2) #weak El Nino
abline(v=1, col = "Red3", lwd = 5, lty = 2) #moderate El Nino

par(new=T)
par(mai=c(1,1,5.5,1),lwd=5)
hist(data_GIS1$SOI,col = rgb(240, 128, 128, 50, maxColorValue=255),breaks=10,axes=F,ann=F,xaxt="n",cex.axis=2.5,freq = F,border="Black")
axis(side=4,cex.axis=2.5,yaxt="n")
axis(side=4,at=c(0.0,0.3,0.6),labels=c(0.0,0.3,0.6),cex.lab=2.5,cex.axis=2.5,lwd=5) 
dev.off()

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/SOI_lagestRR1.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/SOI_lagseRR1.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/SOI_cumestRR1.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/SOI_cumseRR1.csv")

remove(SOI)
remove(cbSOI)
remove(pred)
remove(redcum_1)
remove(ind2)
remove(ind1)
remove(plot)
remove(m.all1)


###BEST
colnames(data_GIS1)
BEST<-data_GIS1[,c(252,469:491)]
colnames(BEST)
colnames(BEST)[1]=c("BEST_lag0")
colnames(BEST)
cbBEST <- crossbasis(BEST,lag=c(0,23),argvar=list(fun="bs",degree=2,df=3), arglag=list(fun="bs",degree=2,df=3)) 

m.all1 <- glmmPQL(FFBYield ~ cbBEST+ PHOSROCK_year+DAP_year+TSP_year+UREA_EE_BULK_year+POTASH_year+factor(State)+
                    CPO_PriceUSD_year+
                    NER_year+cases+factor(lockdown)+CPI_Overall_year+
                    soil_category2021new2+soil_category2021new3+soil_category2021new4+soil_category2021new5+
                    AAP_lag1yr_catnew2+AAP_lag1yr_catnew3+AAP_lag1yr_catnew4+AAP_lag1yr_catnew5+
                    cost_categorynew2+cost_categorynew3+cost_categorynew4+cost_categorynew5+factor(month)+
                    labor_intensity,random=~1|ID,
                  data=outcome, family=gaussian(link = "identity"))

pred <- crosspred(cbBEST,m.all1,cumul=TRUE,cen=0)

#####overall 
tiff(file = "~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/Overall BESTv2RR1.tiff", width = 3000, height = 2500, res = 300)
redcum_1 <- crossreduce(cbBEST, m.all1, type="overall", lag=23,cen=0)
par(mai=c(1,1,0.25,1),lwd=5)
plot<-plot(redcum_1,col=1,lty=1,ci.arg=list(col="Gainsboro"),lwd=10,xlab="BEST",ylab="",cex.lab=2.5,cex.axis=2.5)
ind1 <- pred$predvar<=0
ind2 <- pred$predvar>=0
lines(pred$predvar[ind1],pred$allfit[ind1],col="DeepSkyBlue4",lwd=10)
lines(pred$predvar[ind2],pred$allfit[ind2],col="Red3",lwd=10)
#abline(v=quantile(pred$predvar,c(50)/100,na.rm=T), col = "gray", lwd = 2)
abline(v=-0.5, col = "DeepSkyBlue4", lwd = 5, lty = 2) #La Nina
abline(v=0.5, col = "Red3", lwd = 5, lty = 2) #weak El Nino
abline(v=1, col = "Red3", lwd = 5, lty = 2) #moderate El Nino

par(new=T)
par(mai=c(1,1,5.5,1),lwd=5)
hist(data_GIS1$BEST,col = rgb(240, 128, 128, 50, maxColorValue=255),breaks=10,axes=F,ann=F,xaxt="n",cex.axis=2.5,freq = F,border="Black")
axis(side=4,cex.axis=2.5,yaxt="n")
axis(side=4,at=c(0.0,0.3,0.6),labels=c(0.0,0.3,0.6),cex.lab=2.5,cex.axis=2.5,lwd=5) 
dev.off()

write.csv(pred$matfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/BEST_lagestRR1.csv")
write.csv(pred$matse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/BEST_lagseRR1.csv")
write.csv(pred$cumfit,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/BEST_cumestRR1.csv")
write.csv(pred$cumse,file="~/Downloads/dataset 20231014/Study Sample/dataset/results/FFBYield/main/BEST_cumseRR1.csv")

remove(BEST)
remove(cbBEST)
remove(pred)
remove(redcum_1)
remove(ind2)
remove(ind1)
remove(plot)
remove(m.all1)

