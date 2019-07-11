#the basic value
#none
rawdata <- readMat("/Users/hongwei/Downloads/corn.mat")
m5data <- rawdata$m5spec$data
mp5data <- rawdata$mp5spec$data
mp6data <- rawdata$mp6spec$data
propvals <- rawdata$propvals$data
plot(apply(m5data,2,mean))
plot(apply(m5data,2,sd))
plot(apply(mp5data,2,sd))
plot(apply(mp6data,2,sd))
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
