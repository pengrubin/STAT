p <- (1:999)/1000
x <- qgamma(p,shape=1,scale=2)
y <- qlnorm(p, meanlog = 2, sdlog = 4)
setwd("/Users/hongwei/Desktop")
pdf(file="plot1.pdf", height=4, width=6)
plot(p,x,type = "l",lty=1, col="red")
#par(new=TRUE)
lines(p,y,type = "l",lty=2, col="blue")
dev.off()
