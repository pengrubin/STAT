library("data.table")
library("HH")
#setwd("C:\\Users\\asus-PS\\Desktop")
setwd("/Users/hongwei/Desktop")
rawdata <- read.csv("fillter.csv")
rawdata <- as.data.table(rawdata)
rawdata[,"per":= rawdata$diff/rawdata$before]
data <- rawdata[,c(1,2,3,7)]
data <- data[c(-1,-11,-21,-31,-41,-51,-61,-71,-81,-91,-101,-111,-121,-131,-141),]
summary(data)
#
##plots before modelling
#
x11()
plot(as.numeric(data$district), data$per, 
     pch = as.vector(data$brand), cex = 1.2, 
     col=  as.numeric(data$brand),
     main = "efficiency by district", 
     xlab = "district", ylab = "efficiency")
interaction.plot(data$district,data$brand,data$per,
                 ylab="efficiency",xlab="district",trace.label="brand")
par(mfrow = c(1,2))
plot(data$brand,data$per, main = "Box Plot for brand", 
     xlab = "brand", ylab = "fillter efficiency",col="grey")
plot(data$district,data$per, main = "Box Plot for district", 
     xlab = "district", ylab = "fillter efficiency",col="grey")

#
#ANOVA with interaction(with outliers not deleted)
#
options(contrasts=c("contr.sum", "contr.sum"))
fit1 <- aov(data$per~data$brand*data$district)
cat("\nExample 6: one-way ANOVA table\n")
cat("================================\n")
print(summary(fit1))
#
# Coefficient estimates (NB by default, R uses corner-point
# parameterisation with mixture A as the reference level)
#
cat("Coefficient estimates:\n")
print(coefficients(fit1))
#
##CHECK ASSUMPTIONS
#
plot(fit1)
resid <- residuals(fit1)
fitted <- fitted.values(fit1)
opar <- par(mfrow = c(2,2))         
hist(resid, main="Histogram of residuals",col="red")
plot(fitted, resid, main = "Residuals v. Fitted Values",pch=19,
     cex=1.2,col="blue")
qqnorm(resid, main = "Normal Q-Q Plot",pch=19, col="blue",cex=1.2,
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles", plot.it = TRUE)
qqline(resid,lwd=1.2)
par(opar)
#
#Tukey multiple comparison test
#
tuk1 <- TukeyHSD(fit1,which = "data$brand")
tuk1
plot(tuk1)
tuk2 <- TukeyHSD(fit1,which = "data$district")
tuk2
plot(tuk2)

