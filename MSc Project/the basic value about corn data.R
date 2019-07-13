#the basic value
#none
rawdata <- readMat("/Users/hongwei/Downloads/corn.mat")
m5data <- rawdata$m5spec$data
mp5data <- rawdata$mp5spec$data
mp6data <- rawdata$mp6spec$data
propvals <- rawdata$propvals$data
plot(apply(m5data,2,mean))
plot(apply(m5data,2,sd))
apply(propvals,2,sd)

#scaled
rawdata <- readMat("/Users/hongwei/Downloads/corn.mat")
m5data <-  apply(rawdata$m5spec$data,2,scale)
mp5data <-  apply(rawdata$mp5spec$data,2,scale)
mp6data <-  apply(rawdata$mp6spec$data,2,scale) 
propvals <- apply(rawdata$propvals$data,2,scale)
plot(apply(m5data,2,mean))
plot(apply(m5data,2,sd))
apply(propvals,2,sd)

#savitzkyGolay
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
plot(apply(m5data,2,mean))
plot(apply(m5data,2,sd))
apply(propvals,2,sd)
