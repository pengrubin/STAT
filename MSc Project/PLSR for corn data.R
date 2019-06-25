library(R.matlab)
library(pls) 
library(lars)
rawdata <- readMat("/Users/hongwei/Downloads/corn.mat")
m5data <- rawdata$m5spec$data
mp5data <- rawdata$mp5spec$data
mp6data <- rawdata$mp6spec$data
propvals <- rawdata$propvals$data

n=60                             #the number of calibration
sample <- sample(1:80)           #set random order; the begin of reset order
calibration <- cbind(propvals[,1][sample[1:n] ],m5data[sample[1:n], ])#first nth rows for calibration
prediction <- cbind(propvals[,1][sample[n+1:80] ],m5data[sample[n+1:80], ])#n+1th to the end sample for  prediction

corn.pls <- plsr(calibration[,1]~calibration[,-1], ncomp = 10,data= calibration, validation="LOO",jackknife=TRUE,method="widekernelpls")
summary(corn.pls,what="all")
pred.resp <- predict(corn.pls, ncomp = 1:10,newdata = m5data[prediction])




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
