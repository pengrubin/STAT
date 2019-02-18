#SN:17052480
#question_2_a
rawdata <- read.table("osl.dat", #input data
                      header=TRUE) #the first line as the names of the variables 

#question_2_b
par(mfrow=c(2,1))
plot(rawdata$se,rawdata$de
     xlab="Gear Transmission Type" , #add the xlab title
     ylab="Horsepower", #add the ylab title
     main="GP by TR" )#add the main title
hist(rawdata$de)
text(3,40,paste0("the number of grains is ",length(rawdata$de),
                 "\nthe mean of the equivalent doses is ",format(mean(rawdata$de),digits = 4),
                 "\nthe standard deviation of the equivalent doses is ",format(sd(rawdata$de),digits = 4)))

     