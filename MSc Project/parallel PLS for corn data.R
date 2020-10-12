library(R.matlab)
library(pls) 
library(lars)
library(ggplot2)
library(parallel)                                      # 载入parallel包
clnum<-detectCores()-1                                 # 计算可用线程数，并设置并行使用线程数
cl <- makeCluster(getOption("cl.cores", clnum))        # 初始化并行
Myriad_flag=F                                          # myriad_flag TRUE means in cluster environment 
if (Myriad_flag==TRUE)                                   # FALSE means in PC environment
{
  rawdata <- readMat("/home/uczlhpe/Scratch/corn.mat")   # Myriad dir
} else {
  rawdata <- readMat("/Users/hongwei/Downloads/corn.mat")# PC dir
}
clusterExport(cl, "rawdata")                             # input the rawdata to parallel
clusterExport(cl, "Myriad_flag")
system.time({
  corn_PLS=function(n){                                #n is the number of calibration
    library(pls)
    m5data <- rawdata$m5spec$data
    mp5data <- rawdata$mp5spec$data
    mp6data <- rawdata$mp6spec$data
    propvals <- rawdata$propvals$data
    #m5data <- m5data[,c(-75,-77)]
    #propvals <- propvals[,c(-75,-77)]
    
    NV <- 10                                           #number of variables 
    sample <- sample(1:80)                             #set random order; the begin of reset order
    DF <- data.frame(NIR = I(mp5data),                  #input data
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
    return(cbind(RMSECV,RMSEP))                        #return 1x2matrix
  }
  #corn_PLS(n)
  n <- as.matrix(rep(40,10))                           #the number of calibration, rep(a:b,c): from a to b and repeat c. 
  #PlsResult <- apply(n,1,corn_PLS)                     #loop
  PlsResult <- parSapply(cl,n,corn_PLS)                #optimizated loop
  PlotData <- as.data.frame(cbind(n,t(PlsResult)))     #combind the results
  par(mfrow=c(1,2))
  boxplot(V2~V1,data=PlotData,xlab="Number of calibration set", ylab="RMSECV",main="PLS of moisture on m5")
  boxplot(V3~V1,data=PlotData,xlab="Number of calibration set", ylab="RMSEP",main="PLS of moisture on m5")
  #PlotDataMean <- apply(PlotData,2,mean)
  #PlotDataSd <- apply(PlotData,2,sd)
  #cat(PlotDataMean[2]-PlotDataSd[2],PlotDataMean[2],PlotDataMean[2]+PlotDataSd[2],"\n")
  #cat(PlotDataMean[3]-PlotDataSd[3],PlotDataMean[3],PlotDataMean[3]+PlotDataSd[3],"\n")
  if (Myriad_flag==TRUE) {
    save(PlotData,file="1~m5_Number_of_Calibration_set_PlotData.RData")                  # save results in myriad
  }
  clnum
})
stopCluster(cl)
