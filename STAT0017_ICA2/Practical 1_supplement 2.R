## relevant package (and others)
install.packages("fUnitRoots")
library("fUnitRoots")
library(CDVine)
install.packages("fGarch")
library(fGarch)
install.packages("goftest")
library(goftest)
install.packages("KScorrect")
library(KScorrect)
library(stats)

load("/Users/hongwei/Documents/GitHub/STAT/STAT0017_ICA2/data.RData")

opar <- par("mfrow","mar")

plot(data$ftse100~as.Date(data$date,"%d/%m/%y"),type="l",xaxt='n',yaxt='n',
     xlab="",ylab="Price",col="blue",main="FTSE100 (prices)",xaxs="i", 
     yaxs="i",ylim=c(3000,8000),cex.main=0.8,cex.lab=1)
axis(2, at = seq(3000,8000,1000), tick=TRUE,cex.axis=0.9)
axis.Date(1, cex.axis=0.9,at=seq(as.Date("1999/02/25"), as.Date("2019/02/28"), "4 years"))

plot(data$sp500~as.Date(data$date,"%d/%m/%y"),type="l",xaxt='n',yaxt='n',
     xlab="",ylab="Price",col="blue",main="S&P500 (prices)",xaxs="i", 
     yaxs="i",ylim=c(600,3000),cex.main=0.8,cex.lab=1)
axis(2, at = seq(600,3000,500), tick=TRUE,cex.axis=0.9)
axis.Date(1, cex.axis=0.9,at=seq(as.Date("1999/02/25"), as.Date("2019/02/28"), "4 years"))

unitrootTest(data$ftse100)
unitrootTest(data$sp500)

ret1<-diff(log(data$ftse100), lag=1,na=remove)
ret2<-diff(log(data$sp500), lag=1,na=remove)

plot(ret1~as.Date(data$date[2:length(data$date)],"%d/%m/%y"),type="l",yaxt='n',xaxt='n',
     xlab="",ylab="log-returns",main="FTSE100",xaxs="i", 
     yaxs="i", col="blue",ylim=c(-0.15,0.15),cex.main=0.8,cex.lab=0.8)
axis(2, at = seq(-0.15,0.15,0.1), tick=TRUE,cex.axis=0.7)
axis.Date(1, cex.axis=0.7, at=seq(as.Date("1998/01/04"), as.Date("2019/02/28"), "3 years"))

plot(ret2~as.Date(data$date[2:length(data$date)],"%d/%m/%y"),type="l",yaxt='n',xaxt='n',
     xlab="",ylab="log-returns",main="S&P500",xaxs="i", 
     yaxs="i", col="blue",ylim=c(-0.22,0.2),cex.main=0.8,cex.lab=0.8)
axis(2, at = seq(-0.15,0.15,0.1), tick=TRUE,cex.axis=0.7)
axis.Date(1, cex.axis=0.7, at=seq(as.Date("1998/01/04"), as.Date("2019/02/28"), "3 years"))

unitrootTest(ret1)
unitrootTest(ret2)
     
acf(ret1)
acf(ret1^2)

acf(ret2)
acf(ret2^2)


model1=garchFit(formula=~arma(3,0)+garch(1,1),data=ret1,trace=F,cond.dist="sstd")
res1 <- residuals(model1, standardize=TRUE)
acf(res1)
acf(res1^2)
Box.test(res1, lag = 10, type = c("Ljung-Box"), fitdf = 0)
Box.test(res1^2, lag = 10, type = c("Ljung-Box"), fitdf = 0)
shape1<-coef(model1)[9]
skew1<-coef(model1)[8]
u1<-psstd(res1, mean=0, sd=1, nu=shape1, xi=skew1)
hist(u1)

#Kolmogorov-Smirnov test
KStest1<-LcKS(u1, cdf = "punif")
KStest1$p.value
#Anderson-Darling test
ADtest1<-ad.test(u1, null="punif")
ADtest1$p.value


model2=garchFit(formula=~arma(7,0)+garch(1,1),data=ret2,trace=F,cond.dist="sstd")
res2 <- residuals(model2, standardize=TRUE)
acf(res2)
acf(res2^2)
Box.test(res2, lag = 10, type = c("Ljung-Box"), fitdf = 0)
Box.test(res2^2, lag = 10, type = c("Ljung-Box"), fitdf = 0)
shape2<-coef(model2)[13]
skew2<-coef(model2)[12]
u2<-psstd(res2, mean=0, sd=1, nu=shape2, xi=skew2)
#u2<-pnorm(res2, mean=0, sd=1)
#u2<-pt(res2, df=3)
hist(u2)

#Kolmogorov-Smirnov test
KStest2<-LcKS(u2, cdf = "punif")
KStest2$p.value
#Anderson-Darling test
ADtest2<-ad.test(u2, null="punif")
ADtest2$p.value

# Fit copula
u=cbind(u1,u2)
plot(u)
BiCopEst(u[,2],u[,1],family=1,method="mle",se=TRUE) 
BiCopSelect(u[,1],u[,2],familyset=NA)


# Gausian copula rho and tau
f1 <- function (x) 2/pi*asin(x) -0.5
f <- function (x) 2/pi*asin(x)
c=uniroot(f1, c(0, 1), tol = 0.00001)
ro=c$root
f(ro)

