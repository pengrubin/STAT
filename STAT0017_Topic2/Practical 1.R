## Install CDVine package (and others)
install.packages("CDVine")
library("CDVine")

install.packages("gridExtra","ggplot2")
library("gridExtra")
library("ggplot2")
opar <- par("mfrow","mar")


# simulate from a bivariate Gaussian copula, rho=0.3
simdata1 = BiCopSim(1000,1,0.3,par2=0)
plot(simdata1, pch=1, cex=0.8,xlab= "u1",col.lab="red",ylab= "u2",main=expression(paste("Gaussian copula, ",rho,"=0.3")), font.main=1)

# simulate from a bivariate Gaussian copula, rho=0.7
simdata2 = BiCopSim(1000,1,0.7,par2=0)
plot(simdata2, pch=1, cex=0.8,xlab= "u1",col.lab="red",ylab= "u2",main=expression(paste("Gaussian copula, ",rho,"=0.7")), font.main=1)

# simulate from a bivariate Gaussian copula, rho=0.9
simdata3 = BiCopSim(1000,1,0.9,par2=0)
plot(simdata3, pch=1, cex=0.8,xlab= "u1",col.lab="red",ylab= "u2",main=expression(paste("Gaussian copula, ",rho,"=0.9")), font.main=1)

# simulate from a bivariate Gaussian copula, rho=0.99999
simdata4 = BiCopSim(1000,1,0.99999,par2=0)
plot(simdata4, pch=1, cex=0.8,xlab= "u1",col.lab="red",ylab= "u2",main=expression(paste("Gaussian copula, ",rho,"=0.99999")), font.main=1)

# simulate from a bivariate Gaussian copula, rho=1, i.e. comonotonicity copula
u = runif(1000)
simdata5=cbind(u,u)
plot(simdata5, pch=1, cex=0.8,xlab= "u",col.lab="red",ylab= "u",main=expression(paste("Gaussian copula, ",rho,"=1, (comonotonicity)")), font.main=1)


par(mfrow=c(2,2))
plot(simdata1, pch=1, cex=0.7,xlab= "u1", col.main="blue", col.lab="red", ylab= "u2",main=expression(paste("Gaussian copula, ",rho,"=0.3")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata2, pch=1, cex=0.7,xlab= "u1", col.main="blue", col.lab="red", ylab= "u2",main=expression(paste("Gaussian copula, ",rho,"=0.7")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata3, pch=1, cex=0.7,xlab= "u1", col.main="blue", col.lab="red", ylab= "u2",main=expression(paste("Gaussian copula, ",rho,"=0.9")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata4, pch=1, cex=0.7,xlab= "u1", col.main="blue", col.lab="red", ylab= "u2",main=expression(paste("Gaussian copula, ",rho,"=0.99999")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
par(opar) 

### See what happens if you specify a value outside the parameter space
BiCopSim(1000,1,2,par2=0)

# simulate from a bivariate Gaussian copula, rho=-0.3
simdata6 = BiCopSim(1000,1,-0.3,par2=0)
plot(simdata6, pch=1, cex=0.7,xlab= "u1", col.main="blue", col.lab="red", ylab= "u2",main=expression(paste("Gaussian copula, ",rho,"=-0.3")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

# simulate from a bivariate Gaussian copula, rho=-0.7
simdata7 = BiCopSim(1000,1,-0.7,par2=0)
plot(simdata7, pch=1, cex=0.7,xlab= "u1", col.main="blue", col.lab="red", ylab= "u2",main=expression(paste("Gaussian copula, ",rho,"=-0.7")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

# simulate from a bivariate Gaussian copula, rho=-0.9
simdata8 = BiCopSim(1000,1,-0.9,par2=0)
plot(simdata8, pch=1, cex=0.7,xlab= "u1", col.main="blue", col.lab="red", ylab= "u2",main=expression(paste("Gaussian copula, ",rho,"=-0.9")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

# simulate from a bivariate Gaussian copula, rho=-0.99999
simdata9 = BiCopSim(1000,1,-0.99999,par2=0)
plot(simdata9, pch=1, cex=0.7,xlab= "u1", col.main="blue", col.lab="red", ylab= "u2",main=expression(paste("Gaussian copula, ",rho,"= -0.99999")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

# simulate from a bivariate Gaussian copula, rho=-1, i.e. countermonotonicity copula
u = runif(1000)
simdata10=cbind(u,1-u)
plot(simdata10, pch=1, cex=0.7,xlab= "u", col.main="blue", col.lab="red", ylab= "1-u",main=expression(paste("Gaussian copula, ",rho,"=-1, (countermonotonicity)")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))


par(mfrow=c(2,2))
plot(simdata6, pch=1, cex=0.7,xlab= "u1", col.main="blue", col.lab="red", ylab= "u2",main=expression(paste("Gaussian copula, ",rho,"=-0.3")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata7, pch=1, cex=0.7,xlab= "u1", col.main="blue", col.lab="red", ylab= "u2",main=expression(paste("Gaussian copula, ",rho,"=-0.7")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata8, pch=1, cex=0.7,xlab= "u1", col.main="blue", col.lab="red", ylab= "u2",main=expression(paste("Gaussian copula, ",rho,"=-0.9")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata9, pch=1, cex=0.7,xlab= "u1", col.main="blue", col.lab="red", ylab= "u2",main=expression(paste("Gaussian copula, ",rho,"= -0.99999")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
par(opar)

# simulate from a bivariate Gaussian copula, rho=0, i.e. independence copula
u1 = runif(1000)
u2 = runif(1000)
simdata11=cbind(u1,u2)
plot(simdata11, pch=1, cex=0.7,xlab= "u", col.main="blue", col.lab="red", ylab= "1-u",main=expression(paste("Gaussian copula, ",rho,"= 0, (independence)")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

par(mfrow=c(1,3),mar=c(8,2,8,2))
plot(simdata5, pch=1, cex=0.7,xlab= "u1", col.main="blue", col.lab="red", ylab= "u2",main=expression(paste("Gaussian copula, ",rho,"=1, (comonotonicity)")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata11, pch=1, cex=0.7,xlab= "u1", col.main="blue", col.lab="red", ylab= "u2",main=expression(paste("Gaussian copula, ",rho,"=0, (independence)")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata10, pch=1, cex=0.7,xlab= "u1", col.main="blue", col.lab="red", ylab= "u2",main=expression(paste("Gaussian copula, ",rho,"=-1, (countermonotonicity)")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))


#### You can also create "nice" graphs using ggplot2

zz=data.frame(simdata1)
rownames(zz)<-NULL
colnames(zz)<-c("u1", "u2")
p1=ggplot(zz, aes(u1,  u2)) + 
  geom_point(size=1.8,pch=1)+guides(alpha=FALSE)+
  theme_minimal()+
  ggtitle(expression(paste("Gaussian copula, ",rho," = 0.3")))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),text=element_text(size=14),
        plot.title = element_text(color="blue", size=14, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="red", size=14, face="bold"),
        axis.title.y = element_text(color="red", size=14, face="bold")
        )+ 
  scale_x_continuous(limits = c(0,1), expand = c(0, 0),breaks=seq(0,1,0.2)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0),breaks=seq(0,1,0.2)) 
p1

zz=data.frame(simdata2)
rownames(zz)<-NULL
colnames(zz)<-c("u1", "u2")
p2=ggplot(zz, aes(u1,  u2)) + 
  geom_point(size=1.8,pch=1)+guides(alpha=FALSE)+
  theme_minimal()+
  ggtitle(expression(paste("Gaussian copula, ",rho," = 0.7")))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),text=element_text(size=14),
        plot.title = element_text(color="blue", size=14, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="red", size=14, face="bold"),
        axis.title.y = element_text(color="red", size=14, face="bold")
  )+ 
  scale_x_continuous(limits = c(0,1), expand = c(0, 0),breaks=seq(0,1,0.2)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0),breaks=seq(0,1,0.2)) 
p2

zz=data.frame(simdata3)
rownames(zz)<-NULL
colnames(zz)<-c("u1", "u2")
p3=ggplot(zz, aes(u1,  u2)) + 
  geom_point(size=1.8,pch=1)+guides(alpha=FALSE)+
  theme_minimal()+
  ggtitle(expression(paste("Gaussian copula, ",rho," = 0.9")))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),text=element_text(size=14),
        plot.title = element_text(color="blue", size=14, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="red", size=14, face="bold"),
        axis.title.y = element_text(color="red", size=14, face="bold")
  )+ 
  scale_x_continuous(limits = c(0,1), expand = c(0, 0),breaks=seq(0,1,0.2)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0),breaks=seq(0,1,0.2)) 
p3

zz=data.frame(simdata4)
rownames(zz)<-NULL
colnames(zz)<-c("u1", "u2")
p4=ggplot(zz, aes(u1,  u2)) + 
  geom_point(size=1.8,pch=1)+guides(alpha=FALSE)+
  theme_minimal()+
  ggtitle(expression(paste("Gaussian copula, ",rho," = 0.99999")))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),text=element_text(size=14),
        plot.title = element_text(color="blue", size=14, face="bold.italic",hjust = 0.5),
        axis.title.x = element_text(color="red", size=14, face="bold"),
        axis.title.y = element_text(color="red", size=14, face="bold")
  )+ 
  scale_x_continuous(limits = c(0,1), expand = c(0, 0),breaks=seq(0,1,0.2)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0),breaks=seq(0,1,0.2)) 
p4

grid.arrange(p1,p2,p3,p4, ncol=2)    





################## simulate from a bivariate t-copula ############### 
#####################################################################
# simulate from a bivariate t copula, rho=0.7, v=3
simdata12 = BiCopSim(1000,2,0.7,par2=3)
plot(simdata12, pch=1, cex=0.8, xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("t copula, ",rho,"= 0.7, ",nu," = 3")), font.main=1)

# simulate from a bivariate t copula, rho=0.7, v=10
simdata13 = BiCopSim(1000,2,0.7,par2=10)
plot(simdata13, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("t copula, ",rho,"= 0.7, ",nu," = 10")), font.main=1)

# simulate from a bivariate t copula, rho=0.7, v=30
simdata14 = BiCopSim(1000,2,0.7,par2=30)
plot(simdata14, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("t copula, ",rho,"= 0.7, ",nu," = 30")), font.main=1)

# simulate from a bivariate t copula, rho=0.7, v=90
simdata15 = BiCopSim(1000,2,0.7,par2=90)
plot(simdata15, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("t copula, ",rho,"= 0.7, ",nu," = 90")), font.main=1)

# simulate from a bivariate t copula, rho=0.99999, v=3
set.seed(12121)
simdata16 = BiCopSim(1000,2,0.99999,par2=2.1)
plot(simdata16, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("t copula, ",rho,"= 0.99999, ",nu," = 2.1")), font.main=1)

# simulate from a bivariate t copula, rho=0.99999, v=90 i.e. comonotonicity copula
set.seed(12121)
simdata17 = BiCopSim(1000,2,0.99999,par2=90)
plot(simdata17, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("t copula, ",rho,"= 0.99999, ",nu," = 90")), font.main=1)


par(mfrow=c(2,2))
plot(simdata12, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("t copula, ",rho,"= 0.7, ",nu," = 3")), font.main=1)
plot(simdata13, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("t copula, ",rho,"= 0.7, ",nu," = 10")), font.main=1)
plot(simdata14, pch=1, cex=0.8,xlab= "u1",col.main="blue", col.lab="red",ylab= "u2",main=expression(paste("t copula, ",rho,"= 0.7, ",nu," = 30")), font.main=1)
plot(simdata15, pch=1, cex=0.8,xlab= "u1",col.main="blue", col.lab="red",ylab= "u2",main=expression(paste("t copula, ",rho,"= 0.7, ",nu," = 90")), font.main=1)
par(opar)

par(mfrow=c(1,2))
plot(simdata16, pch=1, cex=0.8,xlab= "u1", col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("t copula, ",rho,"= 0.99999, ",nu," = 2.1")), font.main=1)
plot(simdata4, pch=1, cex=0.7,xlab= "u1", col.main="blue", col.lab="red", ylab= "u2",main=expression(paste("Gaussian copula, ",rho,"=0.99999")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
par(opar)



################## simulate from a Clayton copula ############### 
#####################################################################
# simulate from a bivariate Clayton copula, theta=0.7
simdata18 = BiCopSim(1000,3,0.7,par2=0)
plot(simdata18, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Clayton copula, ",theta,"= 0.7")), font.main=1)

# simulate from a bivariate Clayton copula, theta=3
simdata19 = BiCopSim(1000,3, 3,par2=0)
plot(simdata19, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Clayton copula, ",theta,"= 3")), font.main=1)

# simulate from a bivariate Clayton copula, theta=7
simdata20 = BiCopSim(1000,3,7,par2=0)
plot(simdata20, pch=1, cex=0.8,xlab= "u1",col.lab="red",ylab= "u2",main=expression(paste("Clayton copula, ",theta,"= 7")), font.main=1)

# simulate from a bivariate Clayton copula, theta=60
simdata21 = BiCopSim(1000,3,60,par2=0)
plot(simdata21, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Clayton copula, ",theta,"= 60")), font.main=1)

# simulate from a bivariate Clayton copula, theta=100
simdata22 = BiCopSim(1000,3,90,par2=0)
plot(simdata22, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Clayton copula, ",theta,"= 0.7")), font.main=1)

# simulate from a bivariate Clayton copula, theta=0.00001
simdata23 = BiCopSim(1000,3,0.00001,par2=0)
plot(simdata23, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Clayton copula, ",theta,"= 0.00001")), font.main=1)


par(mfrow=c(2,2))
plot(simdata18, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Clayton copula, ",theta,"= 0.7")), font.main=1)
plot(simdata19, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Clayton copula, ",theta,"= 3")), font.main=1)
plot(simdata20, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Clayton copula, ",theta,"= 7")), font.main=1)
plot(simdata21, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Clayton copula, ",theta,"= 60")), font.main=1)
par(opar)

par(mfrow=c(1,2))
plot(simdata23, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Clayton copula, ",theta,"= 0.00001")), font.main=1)
plot(simdata11, pch=1, cex=0.7,xlab= "u", col.main="blue", col.lab="red", ylab= "1-u",main=expression(paste("Gaussian copula, ",rho,"= 0, (independence)")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
par(opar)


################## simulate from a Gumbel copula ############### 
#####################################################################
# simulate from a bivariate Gumbel copula, theta=2
simdata24 = BiCopSim(1000,4,2,par2=0)
plot(simdata24, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Gumbel copula, ",theta,"= 2")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

# simulate from a bivariate Gumbel copula, theta=3
simdata25 = BiCopSim(1000,4,3,par2=0)
plot(simdata25, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Gumbel copula, ",theta,"= 3")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

# simulate from a bivariate Gumbel copula, theta=10
simdata26 = BiCopSim(1000,4,10,par2=0)
plot(simdata26, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Gumbel copula, ",theta,"= 10")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

# simulate from a bivariate Gumbel copula, theta=60
simdata27 = BiCopSim(1000,4,60,par2=0)
plot(simdata27, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Gumbel copula, ",theta,"= 60")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

# simulate from a bivariate Gumbel copula, theta=100
simdata28 = BiCopSim(1000,4,100,par2=0)
plot(simdata28, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Gumbel copula, ",theta,"= 100")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

par(mfrow=c(2,2))
plot(simdata24, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Gumbel copula, ",theta,"= 2")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata25, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Gumbel copula, ",theta,"= 3")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata26, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Gumbel copula, ",theta,"= 10")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata27, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Gumbel copula, ",theta,"= 60")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
par(opar)

# simulate from a bivariate Gumbel copula, theta=1
simdata29 = BiCopSim(1000,4,1,par2=0)
plot(simdata29, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Gumbel copula, ",theta,"= 1")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

par(mfrow=c(1,2))
plot(simdata29, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Gumbel copula, ",theta,"= 1, (independence)")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata11, pch=1, cex=0.7,xlab= "u", col.main="blue", col.lab="red", ylab= "1-u",main=expression(paste("Gaussian copula, ",rho,"= 0, (independence)")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
par(opar)



################## simulate from a Frank copula ############### 
#####################################################################
# simulate from a bivariate Frank copula, theta=2
simdata30 = BiCopSim(1000,5,2,par2=0)
plot(simdata30, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= 2")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

# simulate from a bivariate Frank copula, theta=10
simdata31 = BiCopSim(1000,5,10,par2=0)
plot(simdata31, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= 10")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

# simulate from a bivariate Frank copula, theta=20
simdata33 = BiCopSim(1000,5,20,par2=0)
plot(simdata33, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= 20")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

# simulate from a bivariate Frank copula, theta=30
simdata34 = BiCopSim(1000,5,30,par2=0)
plot(simdata34, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= 30")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

# simulate from a bivariate Frank copula, theta=36
simdata35 = BiCopSim(1000,5,36,par2=0)
plot(simdata35, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= 36")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

par(mfrow=c(2,2))
plot(simdata30, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= 2")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata31, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= 10")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata33, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= 20")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata34, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= 30")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
par(opar)

# simulate from a bivariate Frank copula, theta=0.000001
simdata36 = BiCopSim(1000,5,0.000001,par2=0)
plot(simdata36, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= 0.000001")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

par(mfrow=c(1,2))
plot(simdata36, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= 0.000001")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata11, pch=1, cex=0.7,xlab= "u", col.main="blue", col.lab="red", ylab= "1-u",main=expression(paste("Gaussian copula, ",rho,"= 0, (independence)")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
par(opar)


# simulate from a bivariate Frank copula, theta=-2
simdata37 = BiCopSim(1000,5,-2,par2=0)
plot(simdata37, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= -2")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

# simulate from a bivariate Frank copula, theta=-10
simdata38 = BiCopSim(1000,5,-10,par2=0)
plot(simdata38, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= -10")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

# simulate from a bivariate Frank copula, theta=-20
simdata39 = BiCopSim(1000,5,-20,par2=0)
plot(simdata39, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= -20")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

# simulate from a bivariate Frank copula, theta=-30
simdata40 = BiCopSim(1000,5,-30,par2=0)
plot(simdata40, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= -30")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

# simulate from a bivariate Frank copula, theta=-36
simdata41 = BiCopSim(1000,5,-36,par2=0)
plot(simdata41, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= -36")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))

par(mfrow=c(2,2))
plot(simdata37, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= -2")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata38, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= -10")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata39, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= -20")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata40, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= -30")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
par(opar)

par(mfrow=c(1,3),mar=c(8,2,8,2))
plot(simdata35, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= 36")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata36, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= 0.000001")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))
plot(simdata41, pch=1, cex=0.8,xlab= "u1",col.main="blue",col.lab="red",ylab= "u2",main=expression(paste("Frank copula, ",theta,"= -36")), font.main=1,xaxs="i", yaxs="i", xlim=c(0, 1),ylim=c(0, 1))



### Compute Value-at-Risk using MC method based on copulas
set.seed(1)
u1=BiCopSim(2000, 1, 0.71, par2=0)
x <- qnorm(u1)
u2=BiCopSim(2000, 3, 2/(2-1), par2=0)
y <- qnorm(u2)
u3=BiCopSim(2000, 4, 1/(1-0.5), par2=0)
z <- qnorm(u3)
u4=BiCopSim(2000, 2, 0.71, par2=4)
t <- qnorm(u4)

cor(x[,1],x[,2])
cor(y[,1],y[,2])
cor(z[,1],z[,2])
cor(t[,1],t[,2])

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


zz=data.frame(t)
rownames(zz)<-NULL
colnames(zz)<-c("x4", "y4")
p4=ggplot(zz, aes(x4,  y4)) + geom_density2d(colour="blue")+
  geom_point(size=0.4)+guides(alpha=FALSE)+
  theme_minimal()+ 
  geom_point(data=subset(zz, !x4>-2 & !y4>-2), colour="red", size=3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),text=element_text(size=14))+ 
  geom_vline(xintercept = -2, linetype="dashed", color = "#339933", size=1)+ 
  geom_hline(yintercept = -2, linetype="dashed", color = "#339933", size=1)+  
  scale_x_continuous(limits = c(-4,4), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-4,4), expand = c(0, 0)) +
  annotate("rect", xmin = c(-4), xmax = c(-2),
           ymin = -4, ymax = -2,
           alpha = 0.2, fill = c("green"))  
p4

grid.arrange(p1,p2,p3,p4, ncol=2)    

var=matrix(0,4,2)

retport1=log(1+((exp(x[,1])-1)*0.5+(exp(x[,2])-1)*0.5))
var[1,]=quantile(retport,c(0.01,0.05))

retport2=log(1+((exp(y[,1])-1)*0.5+(exp(y[,2])-1)*0.5))
var[2,]=quantile(retport2,c(0.01,0.05))

retport3=log(1+((exp(z[,1])-1)*0.5+(exp(z[,2])-1)*0.5))
var[3,]=quantile(retport3,c(0.01,0.05))

retport4=log(1+((exp(t[,1])-1)*0.5+(exp(t[,2])-1)*0.5))
var[4,]=quantile(retport4,c(0.01,0.05))

### VaR for portfolio returns generated using different coplas
var

## Fit of a copula to standardized residuals
data(worldindices)
# 1=S&P500 
# 2=Nikkei225 
# 3=SSE Composite Index 
# 4=DAX 
# 5=CAC 40 
# 6=FTSE100 
cor(worldindices,method="kendall")
BiCopEst(worldindices[,1],worldindices[,6],family=1,method="mle",se=TRUE) 

### S&P500, SSE Composite (396 obs)
param=BiCopSelect(worldindices[,4], worldindices[,5], familyset=NA, selectioncrit="AIC",indeptest=TRUE,level=0.05)

data=cbind(worldindices[,4], worldindices[,5])
zz=data.frame(data)
rownames(zz)<-NULL
colnames(zz)<-c("u1", "u2")
p=ggplot(zz, aes(u1,  u2)) + geom_density2d(colour="blue")+
  geom_point(size=0.4)+guides(alpha=FALSE)+
  theme_minimal()+ 
  geom_point(data=subset(zz, !u1>-2 & !u2>-2), colour="red", size=3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),text=element_text(size=14))
#  geom_vline(xintercept = 0.1, linetype="dashed", color = "#339933", size=1)+ 
#  geom_hline(yintercept = 0.1, linetype="dashed", color = "#339933", size=1)+  
#  scale_x_continuous(limits = c(0,1), expand = c(0, 0)) +
#  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
#  annotate("rect", xmin = c(0), xmax = c(0.1),
#           ymin = 0, ymax = 0.1,
#           alpha = 0.2, fill = c("green"))  
p

BiCopMetaContour(u1=NULL,u2=NULL,bw=1,size=100,
                 levels=c(0.01,0.05,0.1,0.15,0.2),
                 family=2,par=param$par,par2=param$par2,margins="unif",main="")

BiCopMetaContour(u1=NULL,u2=NULL,bw=1,size=100,
                 levels=c(0.01,0.05,0.1,0.15,0.2),
                 family=2,par=param$par,par2=param$par2,margins="t",margins.par=10,main="")

ret1 <- qnorm(worldindices[,4])
ret2 <- qnorm(worldindices[,5])

data=cbind(ret1,ret2)
zz=data.frame(data)
rownames(zz)<-NULL
colnames(zz)<-c("x", "y")
p=ggplot(zz, aes(x,  y)) + geom_density2d(colour="blue")+
  geom_point(size=0.4)+guides(alpha=FALSE)+
  theme_minimal()+ 
  geom_point(data=subset(zz, !x>-2 & !y>-2), colour="red", size=3)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),text=element_text(size=14))+ 
  geom_vline(xintercept = -2, linetype="dashed", color = "#339933", size=1)+ 
  geom_hline(yintercept = -2, linetype="dashed", color = "#339933", size=1)+  
  scale_x_continuous(limits = c(-4,4), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-4,4), expand = c(0, 0)) +
  annotate("rect", xmin = c(-4), xmax = c(-2),
           ymin = -4, ymax = -2,
           alpha = 0.2, fill = c("green"))  
p
