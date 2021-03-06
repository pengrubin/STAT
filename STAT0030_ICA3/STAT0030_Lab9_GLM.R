#Use read.table to read the data into R
storm.data <- read.table("/Users/hongwei/Documents/GitHub/STAT/STAT0030_ICA3/STAT0030_Lab8&9_GLM/nstorms.dat",header = T)
nino.data <- read.table("/Users/hongwei/Documents/GitHub/STAT/STAT0030_ICA3/STAT0030_Lab8&9_GLM/nino3.dat",header = T)

#By adding 1 to the years in nino.data frame before merging, we match the storm data.
nino.data$Year <- nino.data$Year + 1
storm.data <- merge(storm.data,nino.data)

#scatterplot
plot(storm.data$Storms,storm.data$N3.m09)

#storm.model1
storm.model1 <-glm(Storms ~ N3.m09,family=poisson(link="log"),data=storm.data)
summary(storm.model1)
#see the correlation be- tween the estimated coefficients
summary(storm.model1,correlation=TRUE)

#Residuals, fitted values and diagnostics
storm.model1$residuals
resid(storm.model1)

#Response residuals
resid(storm.model1,type="response")
mu <- fitted(storm.model1)
storm.data$Storms - mu
#Pearson residuals
(storm.data$Storms-mu)/sqrt(mu)
resid(storm.model1,type="pearson")
#Deviance residuals
sum(resid(storm.model1)^2)
storm.model1$deviance
#Working residuals
par(mfrow=c(2,2))
plot(storm.model1)
#1
predict(storm.model1)
log(fitted(storm.model1))

#Predicting future values
newnino.data <- data.frame(N3.m09=-0.226)
predict(storm.model1,newdata=newnino.data,se.fit=TRUE)

#2.3 Model comparison Chi
anova(storm.model1,test="Chi")

#storm.model2
storm.model2 <- glm(Storms ~ N3.m09 + N3.m10, family=poisson(link="log"),data=storm.data)
summary(storm.model2)
anova(storm.model1,storm.model2,test="Chi")
anova(storm.model2,test="Chi")

#3 Models for binary data
my.glm <- glm(y/n ~ x1 + x2,family=binomial,weights=n,data=mydataframe)
