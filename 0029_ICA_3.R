install.packages("lars")
library(lars)#加载包
setwd("/Users/hongwei/Desktop")
rawdata <- read.table("emissionssw.dat",header = TRUE)
x = as.matrix(rawdata[, 2:4])
y = as.matrix(rawdata[, 1])
la<-lars(x,y,type='lar')
plot(la)
summary(la)
la
model <- lm(nox~noxem+ws-1, data=rawdata)
summary(model)
AIC(model)
