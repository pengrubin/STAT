load("/Users/hongwei/Documents/GitHub/STAT/STAT0017_ICA2/ICA2_data.RData")
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

#load("/Users/hongwei/Documents/GitHub/STAT/STAT0017_ICA2/data.RData")

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

plot(data$sse~as.Date(data$date,"%d/%m/%y"),type="l",xaxt='n',yaxt='n',
     xlab="",ylab="Price",col="blue",main="FTSE100 (prices)",xaxs="i", 
     yaxs="i",ylim=c(1000,6500),cex.main=0.8,cex.lab=1)
axis(2, at = seq(1000,6500,500), tick=TRUE,cex.axis=0.9)
axis.Date(1, cex.axis=0.9,at=seq(as.Date("1999/02/25"), as.Date("2019/02/28"), "4 years"))


unitrootTest(data$ftse100)
unitrootTest(data$sp500)
unitrootTest(data$sse)

ret1<-diff(log(data$ftse100), lag=1,na=remove)
ret2<-diff(log(data$sp500), lag=1,na=remove)
ret3<-diff(log(data$sse), lag=1,na=remove)

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

plot(ret3~as.Date(data$date[2:length(data$date)],"%d/%m/%y"),type="l",yaxt='n',xaxt='n',
     xlab="",ylab="log-returns",main="S&P500",xaxs="i", 
     yaxs="i", col="blue",ylim=c(-0.22,0.2),cex.main=0.8,cex.lab=0.8)
axis(2, at = seq(-0.15,0.15,0.1), tick=TRUE,cex.axis=0.7)
axis.Date(1, cex.axis=0.7, at=seq(as.Date("1998/01/04"), as.Date("2019/02/28"), "3 years"))

unitrootTest(ret1)
unitrootTest(ret2)
unitrootTest(ret3)

acf(ret1)
acf(ret1^2)

acf(ret2)
acf(ret2^2)

acf(ret3)
acf(ret3^2)

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

model3=garchFit(formula=~arma(3,0)+garch(1,1),data=ret3,trace=F,cond.dist="sstd")
res3 <- residuals(model3, standardize=TRUE)
acf(res3)
acf(res3^2)
Box.test(res3, lag = 10, type = c("Ljung-Box"), fitdf = 0)
Box.test(res3^2, lag = 10, type = c("Ljung-Box"), fitdf = 0)
shape3<-coef(model3)[9]
skew3<-coef(model3)[8]
u3<-psstd(res3, mean=0, sd=1, nu=shape3, xi=skew3)
hist(u3)

#Kolmogorov-Smirnov test
KStest3<-LcKS(u3, cdf = "punif")
KStest3$p.value
#Anderson-Darling test
ADtest3<-ad.test(u3, null="punif")
ADtest3$p.value



#u=cbind(ret1,ret2,ret3)
u=cbind(u1,u2,u3)
cor(u[,1:3],method = c("kendall"))

u1u2u3=cbind(u[,1],u[,2],u[,3])
vinemodel=CDVineCopSelect(u1u2u3,type=2,familyset=c(1:10,13,14,23,24))
vinemodel

N=2000
u1u2u3_sim=CDVineSim(N, family=vinemodel$family, vinemodel$par,  vinemodel$par2, type=2)
cor(u1u2u3_sim,method = c("kendall"))
cor(u1u2u3,method = c("kendall"))
vinemodel_sim=CDVineCopSelect(u1u2u3_sim,type=2,familyset=c(1:10,13,14,23,24))
vinemodel_sim

# Vine selection "manually"
model_1 = BiCopSelect(u[,2],u[,1],familyset=c(1:10,13,14,16,23,24,26))
model_1
model_2 = BiCopSelect(u[,2],u[,3],familyset=c(1:10,13,14,16,23,24,26))
model_2
h1 = BiCopHfunc(u[,2],u[,3],model_1$family,model_1$par,model_1$par2)
h2 = BiCopHfunc(u[,1],u[,3],model_2$family,model_2$par,model_2$par2)
model_3 = BiCopSelect(h1$hfunc2,h2$hfunc2,familyset=c(1:10,13,14,16,23,24,26))


###########

### Compute Value-at-Risk using MC method based on copulas
u1=BiCopSim(2000, model_1$family, model_1$par, par2=model_1$par2)
x <- qnorm(u1)
u2=BiCopSim(2000, model_2$family, model_2$par, par2=model_2$par2)
y <- qnorm(u2)
u3=BiCopSim(2000, model_3$family, model_3$par, par2=model_3$par2)
z <- qnorm(u3)


cor(x[,1],x[,2])
cor(y[,1],y[,2])
cor(z[,1],z[,2])


zz=data.frame(x)
rownames(zz)<-NULL
colnames(zz)<-c("x1", "y1")
p1=ggplot(zz, aes(x1,  y1)) + geom_density2d(colour="blue")+
  geom_point(size=0.4)+guides(alpha=FALSE)+
  geom_point(data=subset(zz, !x1>-2 & !y1>-2), colour="red", size=3)+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),text=element_text(size=14))+ 
  geom_vline(xintercept = -2, linetype="dashed", color = "#339933", size=1)+ 
  geom_hline(yintercept = -2, linetype="dashed", color = "#339933", size=1)+  
  scale_x_continuous(limits = c(-4,4), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-4,4), expand = c(0, 0)) +
  annotate("rect", xmin = c(-4), xmax = c(-2),
           ymin = -4, ymax = -2,
           alpha = 0.2, fill = c("green")) 
p1

zz=data.frame(y)
rownames(zz)<-NULL
colnames(zz)<-c("x2", "y2")
p2=ggplot(zz, aes(x2,  y2)) + geom_density2d(colour="blue")+
  geom_point(size=0.4)+guides(alpha=FALSE)+
  geom_point(data=subset(zz, !x2>-2 & !y2>-2), colour="red", size=3)+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),text=element_text(size=14))+ 
  geom_vline(xintercept = -2, linetype="dashed", color = "#339933", size=1)+ 
  geom_hline(yintercept = -2, linetype="dashed", color = "#339933", size=1)+  
  scale_x_continuous(limits = c(-4,4), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-4,4), expand = c(0, 0)) +
  annotate("rect", xmin = c(-4), xmax = c(-2),
           ymin = -4, ymax = -2,
           alpha = 0.2, fill = c("green")) 
p2
zz=data.frame(z)
rownames(zz)<-NULL
colnames(zz)<-c("x3", "y3")
p3=ggplot(zz, aes(x3,  y3)) + geom_density2d(colour="blue")+
  geom_point(size=0.4)+guides(alpha=FALSE)+
  geom_point(data=subset(zz, !x3>-2 & !y3>-2), colour="red", size=3)+
  theme_minimal()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),text=element_text(size=14))+ 
  geom_vline(xintercept = -2, linetype="dashed", color = "#339933", size=1)+ 
  geom_hline(yintercept = -2, linetype="dashed", color = "#339933", size=1)+  
  scale_x_continuous(limits = c(-4,4), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-4,4), expand = c(0, 0)) +
  annotate("rect", xmin = c(-4), xmax = c(-2),
           ymin = -4, ymax = -2,
           alpha = 0.2, fill = c("green")) 
p3



grid.arrange(p1,p2,p3, ncol=2)    

var=matrix(0,3,2)

retport1=log(1+((exp(x[,1])-1)*0.5+(exp(x[,2])-1)*0.5))
var[1,]=quantile(retport1,c(0.01,0.05))

retport2=log(1+((exp(y[,1])-1)*0.5+(exp(y[,2])-1)*0.5))
var[2,]=quantile(retport2,c(0.01,0.05))

retport3=log(1+((exp(z[,1])-1)*0.5+(exp(z[,2])-1)*0.5))
var[3,]=quantile(retport3,c(0.01,0.05))

### VaR for portfolio returns generated using different coplas
var

