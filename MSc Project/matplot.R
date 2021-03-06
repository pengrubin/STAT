load("/Users/hongwei/Documents/GitHub/STAT/MSc Project/paper_writing/no.loop_time_10-500.RData")
#load("/Users/hongwei/Documents/GitHub/STAT/MSc Project/Scratch/loop_times_10_200_5PlotData.RData")
load("/Users/hongwei/Documents/GitHub/STAT/MSc Project/Scratch/loop_times_500_10PlotData.RData")

par(mfrow=c(1,2))
boxplot(V2~V1,data=PlotData,xlab="Loop times", ylab="RMSECV",main="Boxplot of RMSECV",cex.lab=1.5,cex.main=1.5)
boxplot(V3~V1,data=PlotData,xlab="Loop times", ylab="RMSEP",main="Boxplot of RMSEP",cex.lab=1.5,cex.main=1.5)
sd(PlotData$V3)

test <- aggregate(x=PlotData[c("V2",'V3')], by = list(PlotData$V1), FUN=sd)

plot(test$Group.1,test$V2,xlab="Loop times", ylab="Standard deviation",main="RMSECV",cex.lab=1.5,cex.main=1.5)
plot(test$Group.1,test$V3,xlab="Loop times", ylab="Standard deviation",main="RMSEP",cex.lab=1.5,cex.main=1.5)




library(ChemometricsWithR)
wavelengths<-seq(1100, 2498,by=2)
rawdata <- readMat("/Users/hongwei/Downloads/corn.mat")
m5data <- rawdata$m5spec$data
mp5data <- rawdata$mp5spec$data
mp6data <- rawdata$mp6spec$data
propvals <- rawdata$propvals$data
matplot(wavelengths,t(m5data),lty=1,pch=NULL,type="l",xlab="wavelengths(nm)",ylab="abs",main="Spectra on instrument m5 ")


rawdata <- readMat("/Users/hongwei/Downloads/corn.mat")
m5data <-  apply(rawdata$m5spec$data,2,scale)
mp5data <-  apply(rawdata$mp5spec$data,2,scale)
mp6data <-  apply(rawdata$mp6spec$data,2,scale) 
propvals <- apply(rawdata$propvals$data,2,scale)
matplot(wavelengths,t(m5data),lty=1,pch=NULL,type="l",xlab="wavelengths(nm)",ylab="abs",main="Scaled spectra on instrument m5 ")


library(prospectr)
wavelengths<-seq(1100, 2458,by=2)
rawdata <- readMat("/Users/hongwei/Downloads/corn.mat")
m5data <-  t(apply(rawdata$m5spec$data,1,function(x) {
  savitzkyGolay(x,1,2,21)                          #SavitzkyGolay filler
}))
mp5data <- t(apply(rawdata$mp5spec$data,1,function(x) {
  savitzkyGolay(x,1,2,21)                          #SavitzkyGolay filler
}))
mp6data <- t(apply(rawdata$mp6spec$data,1,function(x) {
  savitzkyGolay(x,1,2,21)                          #SavitzkyGolay filler
}))
propvals <- rawdata$propvals$data
matplot(wavelengths,t(m5data),lty=1,pch=NULL,type="l",xlab="wavelengths(nm)",ylab="abs",main="SavitzkyGolay filter spectra on m5 ")



