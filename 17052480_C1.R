#Student number: 17052480
#C1
#install.packages("data.table")#data.table test
#library(data.table)#data.table test
#setwd("/Users/hongwei/Desktop")#test path 
wd <- getwd()#in case used after
setwd(wd)
rawdata <- read.table("ceramic.dat",header = TRUE)#Read data in rawdata
data <- data.table(Lab=rawdata$Lab,Batch=rawdata$Batch,Y=rawdata$Y)#take the usefull data from rawdata
#data[,c(mean(Y),sd(Y)),by=c("Lab","Batch")]#data.table test
cat("Here is the table of means, standard deviations and numbers:\n")
out1 <- aggregate(data$Y,by=list(Lab=data$Lab,Batch=data$Batch),FUN = function(Y){c(mean = mean(Y),sd=sd(Y),number = length(Y))})#tables of means, standard deviations and numbers
print(out1)
out2 <- var.test(Y ~ Batch, data = data)# F test for equality of variances between the two groups
out3 <- t.test(Y ~ Batch, data= data)#appropriate t-test for equality of means
out4 <- wilcox.test(Y ~ Batch,data=data)#nonparametric alternative to this t test.
cat("\n\n")
cat(" Test             Statistic  P\n","Equal variances  ",out2$statistic,out2$p.value,"\n Equal Means      ",out3$statistic,out3$p.value,"\n Nonparametric    ",out4$statistic,"",out4$p.value)
pdf(file="17052480_C1.pdf", height=4, width=6)#plot in pdf with 4inch height and 6inch wide
boxplot(Y~ Batch, data=data,main="boxplot Y~ Batch", xlab="Batch", ylab="Y")
dev.off()#end the plot and put pdf out
cat("

\n\n\nAccording to the result of R program, there are three tests and one graph. Firstly, we can see that in F-test the p-value of F-test is p = 0.3703829 which is greater than the significance level 0.05. Therefore, there is no significant difference between the two-batch variances. Second  of all, the p-value of t-test is p = 6.868631e-35 which is less than the significance level 0.05. As a result, there is a significant difference between the two-batch means.
Finally, from the nonparametric alternative test, the p-value is 2.755647e-35<0.05, hence true location shift is not equal to 0.
From this boxplot, the mean of Batch 1  is greater than Batch 2 ‘s. In addition, they have the same variance. Batch 2 has the lower minimum which Y=345.294 and the greater maximum which Y=821.654 than batch1.\n


")
