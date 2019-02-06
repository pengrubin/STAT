#Student number: 17052480
#B5
#setwd("/Users/hongwei/Desktop")#test path 
wd <- getwd()#in case used after
setwd(wd)
rawdata <- read.table("premgoals.dat",header = TRUE)#Read data in
out1 <- table(rawdata$Away,rawdata$Home)#Create a frequency table
cat("The frequency table of the Premiership results is")
print(out1)
pdf(file="17052480_B5.pdf", height=12, width=6)#plot in pdf with 4inch height each and 6inch wide. 3 graphs mean 4*3=12inch height.
par(mfrow=c(3,1))#put 3 graphics in one page
plot(rawdata$Home, rawdata$Away, main="Scatterplot of Home~Away", xlab="Home", ylab="Away")#Create a scatterplot
print("'Plot' cannot show repeated numerical data. Plot function shows the same shape between single points and repeated points. That is why we need to use sunflowerplot. ")#The problem with this graph.
out2 <- sunflowerplot(rawdata$Home, rawdata$Away, main="Sunflowerplot of Home~Away 1", xlab="Home", ylab="Away")#Create a sunflowerplot
symbols(out2$x,out2$y,circles = (out2$number/max(out2$number))/2,fg=ceiling((out2$number+1)/10),inches = FALSE,main="Sunflowerplot of Home~Away 2", xlab="Home", ylab="Away")#Create a plot where the size of each symbol corresponds to the frequency of each result
dev.off()#end the plot and put pdf out
