library(R.matlab)
library(pls) 
library(lars)
library(ggplot2)
library(parallel)                                      # 载入parallel包
clnum<-detectCores()-1                                 # 计算可用线程数，并设置并行使用线程数
cl <- makeCluster(getOption("cl.cores", clnum))        # 初始化并行
rawdata <- readMat("/Users/hongwei/Downloads/corn.mat")
clusterExport(cl, "rawdata")                           # 额外加载变量
system.time({
  corn_PLS=function(n){                                #n is the number of calibration
    library(pls)
    m5data <- rawdata$m5spec$data
    mp5data <- rawdata$mp5spec$data
    mp6data <- rawdata$mp6spec$data
    propvals <- rawdata$propvals$data
    #m5data <- m5data[,c(-75,-77)]
    #propvals <- propvals[,c(-75,-77)]
    
    NV <- 40                                           #number of variables 
    sample <- sample(1:80)                             #set random order; the begin of reset order
    DF <- data.frame(NIR = I(m5data),                  #input data
                     y=propvals[,2])
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
    
    
    m5data <-  apply(rawdata$m5spec$data,2,scale)
    mp5data <-  apply(rawdata$mp5spec$data,2,scale)
    mp6data <-  apply(rawdata$mp6spec$data,2,scale) 
    propvals <- apply(rawdata$propvals$data,2,scale)
    NV <- 40                                           #number of variables 
    DF <- data.frame(NIR = I(m5data),                  #input data
                     y=propvals[,2])
    class(DF$NIR) <- "matrix"                          # just to be certain, it was "AsIs"
    #str(DF)                                           #check point
    DF$train <- rep(FALSE, 80)
    DF$train[sample<=n] <- TRUE                        #chose calibration
    corn.pls <- plsr(y ~ NIR, data = DF, ncomp = NV, validation="LOO", jackknife = TRUE, subset = train)
    #summary(corn.pls,what="all")                      #check point
    RMSECV1 <- RMSEP(corn.pls)$val[1,1,NV]
    #print(RMSECV)                                     #check point
    predict <- predict(corn.pls, ncomp = NV, newdata = DF[!DF$train,])
    RMSEP1 <- sqrt(sum((predict-DF[!DF$train,]$y)^2)/(80-n))
    #print(RMSEP)                                      #check point
    #plot(R2(corn.pls))                                #check point
   
    
    return(cbind(RMSECV,RMSEP,RMSECV1,RMSEP1))                        #return 1x4matrix
  }

  #corn_PLS(n)
  n <- as.matrix(rep(60,50))                           #the number of calibration, rep(a:b,c): from a to b and repeat c. 
  #PlsResult <- apply(n,1,corn_PLS)                     #loop
  PlsResult <- parSapply(cl,n,corn_PLS)                #optimizated loop
  PlotData <- as.data.frame(cbind(n,t(PlsResult)))     #combind the results
  ##par(mfrow=c(1,2))
  ##boxplot(V2~V1,data=PlotData,xlab="Number of Calibration", ylab="RMSECV",main="PLS")
  ##boxplot(V3~V1,data=PlotData,xlab="Number of Calibration", ylab="RMSEP",main="PLS")
  #PlotDataMean <- apply(PlotData,2,mean)
  #PlotDataSd <- apply(PlotData,2,sd)
  ##cat(PlotDataMean[2]-PlotDataSd[2],PlotDataMean[2],PlotDataMean[2]+PlotDataSd[2],"\n")
  #cat(PlotDataMean[3]-PlotDataSd[3],PlotDataMean[3],PlotDataMean[3]+PlotDataSd[3],"\n")
  lm(V2~V4-1,data = PlotData)
  lm(V3~V5-1,data = PlotData)
  propvals <- rawdata$propvals$data
  apply(propvals,2,sd)
  mean(apply(m5data,2,sd))
  
})
stopCluster(cl)
