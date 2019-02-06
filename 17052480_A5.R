#Student number: 17052480
#A5
p <- (1:999)/1000#from 0.001 to 0.999 including 999 points
x <- qgamma(p,shape=1,scale=2)#gamma distribution 
y <- qlnorm(p, meanlog = log(2^(1/2)), sdlog = (log(2))^(1/2))#log norm distribution with computered mean and sd.
#setwd("/Users/hongwei/Desktop")#test path 
wd <- getwd()#in case used after
setwd(wd)
pdf(file="17052480_A5.pdf", height=4, width=6)#plot in pdf with 4inch height and 6inch wide
plot(p,x,type = "l",lty=1, col="red",main="Gamma&Lnorm quantile functions", xlab="P(x)", ylab="x")#the gamma distribution plot
#par(new=TRUE)
lines(p,y,type = "l",lty=2, col="blue")#add log norm distribution on the before graph
dev.off()#end the plot and put pdf out

