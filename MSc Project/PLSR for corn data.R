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



# NOT RUN {
data(yarn)
nir.mvr <- mvr(density ~ NIR, ncomp = 5, data = yarn[yarn$train,])

## Predicted responses for models with 1, 2, 3 and 4 components
pred.resp <- predict(nir.mvr, ncomp = 1:4, newdata = yarn[!yarn$train,])

## Predicted responses for a single model with components 1, 2, 3, 4
predict(nir.mvr, comps = 1:4, newdata = yarn[!yarn$train,])

## Predicted scores
predict(nir.mvr, comps = 1:3, type = "scores", newdata = yarn[!yarn$train,])
# }