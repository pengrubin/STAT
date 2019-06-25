library(R.matlab)
library(pls) 
library(lars)
rawdata <- readMat("/Users/hongwei/Downloads/corn.mat")
m5data <- rawdata$m5spec$data
mp5data <- rawdata$mp5spec$data
mp6data <- rawdata$mp6spec$data
propvals <- rawdata$propvals$data

n=60                             #the number of calibration
sample <- sample(1:80)           #set random order; the begin of reset order
calibration <- cbind(propvals[,1][sample[1:n] ],m5data[sample[1:n], ])#first nth rows for calibration
prediction <- cbind(propvals[,1][sample[n+1:80] ],m5data[sample[n+1:80], ])#n+1th to the end sample for  prediction
corn.pls <- plsr(calibration[,1]~calibration[,-1], ncomp = 10, validation="LOO",jackknife=TRUE,method="widekernelpls")
summary(corn.pls,what="all")
pred.resp <- predict(corn.pls, comps = 1:10, newdata = prediction[,-1])
DF <- data.frame(NIR = I(m5data),
                 y=propvals[,1])
class(DF$NIR) <- "matrix" # just to be certain, it was "AsIs"
str(DF)
DF$train <- rep(FALSE, 20)
DF$train[1:60] <- TRUE
corn.pls <- plsr(y ~ NIR, data = DF, ncomp = 10, validation = 
                   "LOO", jackknife = TRUE, subset = train)
res <- predict(corn.pls, ncomp = 10, newdata = DF[!DF$train,])
sum((res-DF[!DF$train,]$y)^2)
