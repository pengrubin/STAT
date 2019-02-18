#SN:17052480
#question_1_a
rawdata <- read.table("cars.dat", #input data
                      header=TRUE) #the first line as the names of the variables 

#question_1_b
summary(rawdata)
table(rawdata$tr)
plot(rawdata,main="Plot Between all Variables") #overlook
pairs(rawdata[,2:4], # plot hp, wt, mpg 
      main = "Plot Between all Variables -- 3 species", #add the main title
      pch = c(21,24)[unclass(rawdata$tr)+1], #different tr shows different shape
      bg = c("red", "green3")[unclass(rawdata$tr)+1]) #different tr shows different colour
logdata <- cbind(rawdata[,1],log(rawdata[,c(2,3,4)])) #log the data
names(logdata) <- c("tr","hp","wt","mpg")#rename the names of the variables 
plot(logdata,main="Plot Between log Variables") #overlook
pairs(logdata[,2:4], # plot log(hp), log(wt), log(mpg)
      main = "Plot Between log Variables -- 3 species", #add the main title
      pch = c(21,24)[unclass(logdata$tr)+1], #different tr shows different shape
      bg = c("red", "green3")[unclass(logdata$tr)+1]) #different tr shows different colour
boxplot(mpg~tr, #MPG by TR
        data=rawdata, #set the dataset
        xlab="Gear Transmission Type" , #add the xlab title
        ylab="Miles Per Gallon", #add the ylab title
        main="MPG by Gear Transmission Type", #add the main title
        names=c("Automatic","Manual")) #change xlab value to character
t.test(mpg~tr, data=logdata)# the t-test above MPG is related to TR.

#question_1_c
model<-lm(mpg~tr+hp+wt, data=logdata); # i.e, full without qsec and gears
summary(model)
summary(model$residuals)
par(mfrow=c(2,2)) #put 4 graphes together
plot(model) #plot 4 graphes as following. 
#addition: if you run the last line code directly, there will be an error "figure margins too large".
#there two way to fix it: 1, click "clear all plots" and then run it. 
#                         2, run >>x11() at first if you are run in Windows computer.
#BTW: there is no error if i use it in R Markdown
best<-step(model, direction="both")
summary(best)
summary(best$residuals)
par(mfrow=c(2,2)) #put 4 graphes together
plot(best)#there will be the same error as "plot(model)" and above addition is the solvtion.

