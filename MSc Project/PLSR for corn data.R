library("R.matlab")
library(pls) 
rawdata <- readMat("/Users/hongwei/Downloads/corn.mat")
m5data <- rawdata$m5spec$data
mp5data <- rawdata$mp5spec$data
propvals <- rawdata$propvals$data

corn.pls <- plsr(m5data, propvals[,1], ncomp = 4, mode = "regression", tol = 1e-25)


pls1<-plsr(propvals[,1]~m5data,10,validation="LOO",jackknife=TRUE,method="widekernelpls")
summary(pls1,what="all")

## 偏最小二乘回归
library(lars)
library(pls)

ap = plsr(propvals[,1]~m5data, 10, validation = "CV")
# CV准则下，不同评价指标
validationplot(ap)
# 不同准则
RMSEP(ap)
MSEP(ap)
R2(ap)
