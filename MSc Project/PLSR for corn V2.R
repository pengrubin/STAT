library(R.matlab)
library(pls) 
library(lars)
rawdata <- readMat("/Users/hongwei/Downloads/corn.mat")
m5data <- rawdata$m5spec$data
mp5data <- rawdata$mp5spec$data
mp6data <- rawdata$mp6spec$data
propvals <- rawdata$propvals$data

n=60                                                 #the number of calibration

corn_PLS=function(n){                                #n is the number of calibration
  sample <- sample(1:80)                             #set random order; the begin of reset order
  DF <- data.frame(NIR = I(m5data),                  #input data
                   y=propvals[,1])
  class(DF$NIR) <- "matrix"                          # just to be certain, it was "AsIs"
  #str(DF)                                           #check point
  DF$train <- rep(FALSE, 80)
  DF$train[sample<=n] <- TRUE                        #chose calibration
  corn.pls <- plsr(y ~ NIR, data = DF, ncomp = 10, validation="LOO", jackknife = TRUE, subset = train)
  #summary(corn.pls,what="all")                      #check point
  RMSECV <- RMSEP(corn.pls)$val[1,1,11]
  #print(RMSECV)                                     #check point
  predict <- predict(corn.pls, ncomp = 10, newdata = DF[!DF$train,])
  RMSEP <- sqrt(sum((predict-DF[!DF$train,]$y)^2)/(80-n))
  #print(RMSEP)                                      #check point
  #plot(R2(corn.pls))                                #check point
  return(cbind(RMSECV,RMSEP))
}
corn_PLS(n)
PlotData <- data.frame(calibration <- rep(20:70,10)
)
test <- as.matrix(rep(20:70,10))
apply(test,1,corn_PLS)
