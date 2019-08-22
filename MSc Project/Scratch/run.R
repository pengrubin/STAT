library(R.matlab)
library(pls)
library(lars)
library(ggplot2)
library(parallel)                                        # load the parallel package
clnum<-detectCores()                                   # Calculate and set the number of available cores
cl <- makeCluster(getOption("cl.cores", clnum))          # Initialization parallel
Myriad_flag=TRUE                                        # myriad_flag TRUE means in cluster environment 
if (Myriad_flag==TRUE)                                   # FALSE means in PC environment
{
  rawdata <- readMat("/home/uczlhpe/Scratch/corn.mat")   # Myriad dir
} else {
  rawdata <- readMat("/Users/hongwei/Downloads/corn.mat")# PC dir
}
clusterExport(cl, "rawdata")                             # input the rawdata to parallel
clusterExport(cl, "Myriad_flag")
system.time({
  corn_PLS=function(NV){                                 # n is the number of calibration
    library(pls)
    m5data <- rawdata$m5spec$data
    mp5data <- rawdata$mp5spec$data
    mp6data <- rawdata$mp6spec$data
    propvals <- rawdata$propvals$data
    #m5data <- m5data[,c(-75,-77)]
    #propvals <- propvals[,c(-75,-77)]
    n=40
    NV <- 15                                            # number of variables 
    sample <- sample(1:80)                               # set random order; the begin of reset order
    DF <- data.frame(NIR = I(m5data),                    # input data
                     y=propvals[,2])
    class(DF$NIR) <- "matrix"                            # just to be certain, it was "AsIs"
    #str(DF)                                             # check point
    DF$train <- rep(FALSE, 80)
    DF$train[sample<=n] <- TRUE                          # chose calibration
    corn.pls <- plsr(y ~ NIR, data = DF, ncomp = NV, validation="LOO", jackknife = TRUE, subset = train)
    #summary(corn.pls,what="all")                        # check point
    RMSECV <- RMSEP(corn.pls)$val[1,1,NV]
    #print(RMSECV)                                       # check point
    predict <- predict(corn.pls, ncomp = NV, newdata = DF[!DF$train,])
    RMSEP <- sqrt(sum((predict-DF[!DF$train,]$y)^2)/(80-n))
    #print(RMSEP)                                        # check point
    #plot(R2(corn.pls))                                  # check point
    return(cbind(RMSECV,RMSEP))                          # return 1x2matrix
  }
  #corn_PLS(n)
  #n <- as.matrix(rep(10,100))                           # the number of calibration, rep(a:b,c): from a to b and repeat c. 
  n <- matrix()                                          # n is the LOOP times
  for (i in seq(10,300,by=10)) {                         
    if (is.na(n)) {
      n <- as.matrix(rep(i,i))
    } else {
      n <- rbind(n,as.matrix(rep(i,i)))
    }
  }
  #PlsResult <- apply(n,1,corn_PLS)                      # loop
  PlsResult <- parSapply(cl,n,corn_PLS)                  # optimizated loop
  PlotData <- as.data.frame(cbind(n,t(PlsResult)))       # combind the results
  par(mfrow=c(1,2))
  boxplot(V2~V1,data=PlotData,xlab="Loop times", ylab="RMSECV",main="PLS")
  boxplot(V3~V1,data=PlotData,xlab="Loop times", ylab="RMSEP",main="PLS")
  PlotDataMean <- apply(PlotData,2,mean)
  PlotDataSd <- apply(PlotData,2,sd)
  #cat(PlotDataMean[2]-PlotDataSd[2],PlotDataMean[2],PlotDataMean[2]+PlotDataSd[2],"\n")
  cat(PlotDataMean[3]-PlotDataSd[3],PlotDataMean[3],PlotDataMean[3]+PlotDataSd[3],"\n")
  if (Myriad_flag==TRUE) {
    save(PlotData,file="loop_times_10_300_10PlotData.RData")                  # save results in myriad
  }
clnum
})
stopCluster(cl)
