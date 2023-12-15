
#########lasso 构建
load("~/Downloads/dataset 20231014/Study Sample/dataset/20231015 bind.RData")

# LOAD PACKAGES
library(haven)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(rms) 
library(ggsci) #调色板
index=which(data$AverageOER>30 | data$FFBYield>6)
data<-data[-index,]
data<-transform(data,OilYield=FFBYield*AverageOER/100)

###标化----
#outcome
summary(data$FFBYield)
class(data$FFBYield)
data$FFBYield_s<-scale(data$FFBYield, center = TRUE, scale = TRUE)
summary(data$FFBYield_s)

summary(data$AverageOER)
class(data$AverageOER)
data$AverageOER_s<-scale(data$AverageOER, center = TRUE, scale = TRUE)
summary(data$AverageOER_s)

summary(data$OilYield)
class(data$OilYield)
data$OilYield_s<-scale(data$OilYield, center = TRUE, scale = TRUE)
summary(data$OilYield_s)

#协变量
summary(data$labor_intensity)
class(data$labor_intensity)  #连续
data$labor_intensity_s<-scale(data$labor_intensity, center = TRUE, scale = TRUE)
summary(data$labor_intensity_s)

summary(data$intensity_cat)
table(data$intensity_cat)  #分类

summary(data$PHOSROCK)
class(data$PHOSROCK)  #连续
colnames(data)
data$PHOSROCK_year<-(data$PHOSROCK_lag1+ data$PHOSROCK_lag2+ data$PHOSROCK_lag3+
                       data$PHOSROCK_lag4+data$PHOSROCK_lag5+data$PHOSROCK_lag6+data$PHOSROCK_lag7+
                       data$PHOSROCK_lag8+data$PHOSROCK_lag9+data$PHOSROCK_lag10+data$PHOSROCK_lag11+
                       data$PHOSROCK_lag12)/12
summary(data$PHOSROCK_year)
data$PHOSROCK_year_s<-scale(data$PHOSROCK_year, center = TRUE, scale = TRUE)
summary(data$PHOSROCK_year_s)

summary(data$DAP)
class(data$DAP)   #连续
data$DAP_year<-(data$DAP_lag1+ data$DAP_lag2+ data$DAP_lag3+
                  data$DAP_lag4+data$DAP_lag5+data$DAP_lag6+data$DAP_lag7+
                  data$DAP_lag8+data$DAP_lag9+data$DAP_lag10+data$DAP_lag11+
                  data$DAP_lag12)/12
summary(data$DAP_year)
data$DAP_year_s<-scale(data$DAP_year, center = TRUE, scale = TRUE)
summary(data$DAP_year_s)

summary(data$TSP)
class(data$TSP)  #连续
data$TSP_year<-(data$TSP_lag1+ data$TSP_lag2+ data$TSP_lag3+
                  data$TSP_lag4+data$TSP_lag5+data$TSP_lag6+data$TSP_lag7+
                  data$TSP_lag8+data$TSP_lag9+data$TSP_lag10+data$TSP_lag11+
                  data$TSP_lag12)/12
summary(data$TSP_year)
data$TSP_year_s<-scale(data$TSP_year, center = TRUE, scale = TRUE)
summary(data$TSP_year_s)

summary(data$UREA_EE_BULK)
class(data$UREA_EE_BULK)   #连续
data$UREA_EE_BULK_year<-(data$UREA_EE_BULK_lag1+ data$UREA_EE_BULK_lag2+ data$UREA_EE_BULK_lag3+
                           data$UREA_EE_BULK_lag4+data$UREA_EE_BULK_lag5+data$UREA_EE_BULK_lag6+data$UREA_EE_BULK_lag7+
                           data$UREA_EE_BULK_lag8+data$UREA_EE_BULK_lag9+data$UREA_EE_BULK_lag10+data$UREA_EE_BULK_lag11+
                           data$UREA_EE_BULK_lag12)/12
summary(data$UREA_EE_BULK_year)
data$UREA_EE_BULK_year_s<-scale(data$UREA_EE_BULK_year, center = TRUE, scale = TRUE)
summary(data$UREA_EE_BULK_year_s)

summary(data$POTASH)
class(data$POTASH)  #连续
data$POTASH_year<-(data$POTASH_lag1+ data$POTASH_lag2+ data$POTASH_lag3+
                     data$POTASH_lag4+data$POTASH_lag5+data$POTASH_lag6+data$POTASH_lag7+
                     data$POTASH_lag8+data$POTASH_lag9+data$POTASH_lag10+data$POTASH_lag11+
                     data$POTASH_lag12)/12
summary(data$POTASH_year)
data$POTASH_year_s<-scale(data$POTASH_year, center = TRUE, scale = TRUE)
summary(data$POTASH_year_s)

summary(data$CPO_PriceUSD)
class(data$CPO_PriceUSD)  #连续
data$CPO_PriceUSD_year<-(data$CPO_PriceUSD_lag1+ data$CPO_PriceUSD_lag2+ data$CPO_PriceUSD_lag3+
                           data$CPO_PriceUSD_lag4+data$CPO_PriceUSD_lag5+data$CPO_PriceUSD_lag6+data$CPO_PriceUSD_lag7+
                           data$CPO_PriceUSD_lag8+data$CPO_PriceUSD_lag9+data$CPO_PriceUSD_lag10+data$CPO_PriceUSD_lag11+
                           data$CPO_PriceUSD_lag12)/12
summary(data$CPO_PriceUSD_year)
data$CPO_PriceUSD_year_s<-scale(data$CPO_PriceUSD_year, center = TRUE, scale = TRUE)
summary(data$CPO_PriceUSD_year_s)

summary(data$NER)
class(data$NER)   #连续
data$NER_year<-(data$NER_lag1+ data$NER_lag2+ data$NER_lag3+
                  data$NER_lag4+data$NER_lag5+data$NER_lag6+data$NER_lag7+
                  data$NER_lag8+data$NER_lag9+data$NER_lag10+data$NER_lag11+
                  data$NER_lag12)/12
summary(data$NER_year)
data$NER_year_s<-scale(data$NER_year, center = TRUE, scale = TRUE)
summary(data$NER_year_s)

summary(data$CPI_Overall)
class(data$CPI_Overall)  #连续
data$CPI_Overall_year<-(data$CPI_Overall_lag1+ data$CPI_Overall_lag2+ data$CPI_Overall_lag3+
                          data$CPI_Overall_lag4+data$CPI_Overall_lag5+data$CPI_Overall_lag6+data$CPI_Overall_lag7+
                          data$CPI_Overall_lag8+data$CPI_Overall_lag9+data$CPI_Overall_lag10+data$CPI_Overall_lag11+
                          data$CPI_Overall_lag12)/12
summary(data$CPI_Overall_year)
data$CPI_Overall_year_s<-scale(data$CPI_Overall_year, center = TRUE, scale = TRUE)
summary(data$CPI_Overall_year_s)

summary(data$CPI_Fertilizers)
class(data$CPI_Fertilizers)  #连续
data$CPI_Fertilizers_year<-(data$CPI_Fertilizers_lag1+ data$CPI_Fertilizers_lag2+ data$CPI_Fertilizers_lag3+
                              data$CPI_Fertilizers_lag4+data$CPI_Fertilizers_lag5+data$CPI_Fertilizers_lag6+data$CPI_Fertilizers_lag7+
                              data$CPI_Fertilizers_lag8+data$CPI_Fertilizers_lag9+data$CPI_Fertilizers_lag10+data$CPI_Fertilizers_lag11+
                              data$CPI_Fertilizers_lag12)/12
summary(data$CPI_Fertilizers_year)
data$CPI_Fertilizers_year_s<-scale(data$CPI_Fertilizers_year, center = TRUE, scale = TRUE)
summary(data$CPI_Fertilizers_year_s)

summary(data$soil_category2021)
class(data$soil_category2021)
table(data$soil_category2021)  #分类

summary(data$AAP_lag1yr_cat)
class(data$AAP_lag1yr_cat)
table(data$AAP_lag1yr_cat)   #分类

summary(data$cost_category)
class(data$cost_category)
table(data$cost_category)  #分类

summary(data$cases)
class(data$cases)
table(data$cases)   #连续
data$cases_s<-scale(data$cases, center = TRUE, scale = TRUE)
summary(data$cases_s)

summary(data$lockdown)
class(data$lockdown)
table(data$lockdown)  #二分类
data$lockdown_s<-scale(data$lockdown, center = TRUE, scale = TRUE)
summary(data$lockdown_s)

data$soil_category2021new<-as.factor(data$soil_category2021)
data$AAP_lag1yr_catnew<-as.factor(data$AAP_lag1yr_cat)  
data$cost_categorynew<-as.factor(data$cost_category) 

#三分类变量设置成哑变量----
library(neuralnet)
a1 <- model.matrix(~soil_category2021new, data)
a2 <- model.matrix(~AAP_lag1yr_catnew, data)
a3 <- model.matrix(~cost_categorynew, data)

data_GIS1<-cbind(data,a1,a2,a3)

colnames(data_GIS1)

remove(a1)
remove(a2)
remove(a3)
remove(data)
save(data_GIS1,file ="~/Downloads/dataset 20231014/Study Sample/dataset/20231016 finaldataset.RData" )

colnames(data_GIS1)
########## individual level  为主
X<-data.frame(data_GIS1$labor_intensity_s,data_GIS1$PHOSROCK_year_s,data_GIS1$DAP_year_s,
              data_GIS1$TSP_year_s,data_GIS1$UREA_EE_BULK_year_s,
              data_GIS1$POTASH_year_s,data_GIS1$CPO_PriceUSD_year_s,data_GIS1$NER_year_s,
              data_GIS1$CPI_Overall_year_s,data_GIS1$cases_s,
              data_GIS1$lockdown_s,data_GIS1$cost_categorynew2,data_GIS1$cost_categorynew3,
              data_GIS1$cost_categorynew4,data_GIS1$cost_categorynew5,data_GIS1$AAP_lag1yr_catnew2,
              data_GIS1$AAP_lag1yr_catnew3,data_GIS1$AAP_lag1yr_catnew4,data_GIS1$AAP_lag1yr_catnew5,
              data_GIS1$soil_category2021new2,data_GIS1$soil_category2021new3,data_GIS1$soil_category2021new4,
              data_GIS1$soil_category2021new5,stringsAsFactors = FALSE)

y<-data_GIS1$AverageOER_s

library(glmnet)
library(Matrix)


### 3. LASSO 模型建立

# Model
# lasso
la.eq <- glmnet(X, y, 
                family='gaussian', 
                intercept = F, alpha=1)   #对于具有中心回归量的 LASSO，估计截距为 0
# 当alpha设置为0则为ridge回归，将alpha设置为0和1之间则为elastic net     

print(la.eq ) #### 最优模型系数结果 为lambda=0.000012

# Lasso筛选变量动态过程图 
la.eq <- glmnet(X, y, family="gaussian", 
                intercept = F, alpha=1) 

tiff(file = "~/Downloads/dataset 20231014/lasso/AverageOER_s plot.tiff",width = 3500, height = 2000, res = 400)
# plot
plot(la.eq,xvar = "lambda", label = F)
dev.off()

la.eq <- glmnet(X, y,lambda=0.000012, family="gaussian", 
                intercept = F, alpha=1) 
cof<-coef(la.eq)
cof
# 也可以用下面的方法绘制
#matplot(log(la.eq$lambda), t(la.eq$beta),
#               type="l", main="Lasso", lwd=2)

##5. 必须对X就行矩阵转化，否则无法往下进行,以及变量标化才可以看得到
X <- as.matrix(X)
Y <- y


### 6. 变量筛选

# Run cross-validation & select lambda
#————————————————
mod_cv <- cv.glmnet(x=X, y=Y, family="gaussian", nfolds = 10,
                    intercept = F, alpha=1)

plot(mod_cv) 

tiff(file = "~/Downloads/dataset 20231014/lasso/AverageOER_s plotcvfit.tiff",width = 3500, height = 2000, res = 400)
plot(mod_cv)
dev.off()

# lambda.min : the λ at which the minimal MSE is achieved.

# lambda.1se : the largest λ at which the MSE is within one standard error of the minimal MSE.
print(paste(mod_cv$lambda.min,
            log(mod_cv$lambda.min)))
print(paste(mod_cv$lambda.1se,
            log(mod_cv$lambda.1se)))

# 这里我们以lambda.min为最优 λ
best_lambda <- mod_cv$lambda.min
best_lambda


# 最终模型的系数估计
#find coefficients of best model
best_model <- glmnet(X, y, lambda = best_lambda, family="gaussian", 
                     intercept = F, alpha=1)
coef(best_model)


###如变量没有显示系数，即lasso回归收缩系数为零。这意味着它完全被排除在模型之外，因为它的影响力不够。系数非0的变量即为我们筛选的重要特征。

