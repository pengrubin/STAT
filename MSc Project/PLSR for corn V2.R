library(R.matlab)
library(pls) 
library(lars)
library(ggplot2)
rawdata <- readMat("/Users/hongwei/Downloads/corn.mat")
m5data <- rawdata$m5spec$data
mp5data <- rawdata$mp5spec$data
mp6data <- rawdata$mp6spec$data
propvals <- rawdata$propvals$data

corn_PLS=function(n){                                #n is the number of calibration
  NV <- 10                                                  #number of variables 
  sample <- sample(1:80)                             #set random order; the begin of reset order
  DF <- data.frame(NIR = I(m5data),                  #input data
                   y=propvals[,1])
  class(DF$NIR) <- "matrix"                          # just to be certain, it was "AsIs"
  #str(DF)                                           #check point
  DF$train <- rep(FALSE, 80)
  DF$train[sample<=n] <- TRUE                        #chose calibration
  corn.pls <- plsr(y ~ NIR, data = DF, ncomp = NV, validation="LOO", jackknife = TRUE, subset = train)
  #summary(corn.pls,what="all")                      #check point
  RMSECV <- RMSEP(corn.pls)$val[1,1,NV]
  #print(RMSECV)                                     #check point
  predict <- predict(corn.pls, ncomp = NV, newdata = DF[!DF$train,])
  RMSEP <- sqrt(sum((predict-DF[!DF$train,]$y)^2)/(80-n))
  #print(RMSEP)                                      #check point
  #plot(R2(corn.pls))                                #check point
  return(cbind(RMSECV,RMSEP))
}
#corn_PLS(n)
n <- as.matrix(rep(20:70,10))                         #the number of calibration
PlsResult <- apply(n,1,corn_PLS)
PlotData <- as.data.frame(cbind(n,t(PlsResult)))
par(mfrow=c(1,2))
boxplot(V2~V1,data=PlotData)
boxplot(V3~V1,data=PlotData)
PlotData

