rawdata <- read.table("/Users/hongwei/Documents/GitHub/STAT/STAT0030_ICA2/cars.dat",header=TRUE) #input data question_1_a
summary(rawdata)
plot(rawdata,main="Plot Between all Variables") #
logdata <- cbind(rawdata[,1],log(rawdata[,c(2,3,4)])) #log data
sunflowerplot(rawdata)
