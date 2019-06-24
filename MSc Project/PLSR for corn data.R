library("R.matlab")
library(pls) 
rawdata <- readMat("/Users/hongwei/Downloads/corn.mat")
m5data <- rawdata$m5spec$data
mp5data <- rawdata$mp5spec$data
propvals <- rawdata$propvals$data

corn.pls <- plsr(m5data, propvals[,1], ncomp = 4, mode = "regression", tol = 1e-25)



install.packages("pls")

library(pls)

n <- 50

x1 <- rnorm(n); xx1 <- scale(x1)

x2 <- rnorm(n); xx2 <- scale(x2)

y <- x1 + x2 + rnorm(n,0,0.1); yy <- scale(y)

p <- plsr(yy ~ xx1+xx2, ncomp=1)

( w <- loading.weights(p) )#看这个主成分与原始xx1与xx2之间的回归系数

a <- w["xx1",]

b <- w["xx2",]

a^2+b^2

cor(y, a*xx1+b*xx2)#a,b是如何确定的，就是让这个回归系数最大，也就是带有主成分回归模型R方最大

p$coef #这样就获取了yy跟xx1与xx2之间的回归系数的回归系数，如何获取的，请看下面的分析

x <- a*xx1+b*xx2

coef(lm(yy~0+x))

coef(lm(yy~0+x))*a

coef(lm(yy~0+x))*b

#有了上面这个系数，可以顺利写出yy跟xx1与xx2之间的回归回归方程



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
