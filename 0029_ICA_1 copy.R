#R Code
cat("\n nox:NOx concentration in ambient air [parts per billion].\n")
cat("\n noxem:Sum of NOx emission of cars on this motorway.\n")
cat("\n ws:Wind speed in meters per second.\n")
cat("\n humidity:Absolute humidity in the air in grams per kilogram. \n")
rawdata <- read.table("emissionssw.dat",header = TRUE)
cat("\n We first take a summary of this data and find out that the ranges of these variables are different.\n")
print(summary(rawdata))
cat("\n Draw a matrix plot for all variables in this data to show the relationships between each two variable in plot 1.\n")
pdf(file="0029_ICA_1.pdf", height=4, width=6)#plot in pdf with 4inch height each and 6inch wide.
pairs(rawdata[1:4], main = "Plot 1:emissionssw",pch = 21)

cat("\n First Model: \n")
model1<- lm(nox~noxem+ws+humidity, data=rawdata)
cat("\n Use the given original data to create a linear model of nox.\n")
print(summary(model1))
cat("\n The result shows that the variable humidity is non-significant in model1 since the p-value of it is greater than the significant level(regardless of taking the value 0.05 or 0.1).Hence,it seems this model is not that fitted.\n")

model2 <- lm(nox~log(noxem)+log(ws)+log(humidity), data=rawdata)
cat("\n We then take all variables in log scale to make them in the same range and create model2\n")
print(summary(model2))
cat("\n The variable humidity is still non-significant in model2.\n")
data <- as.data.frame(scale(rawdata))
cat("\n Then we want to have a try on the standardised values and summarize them.\n")
print(summary(data))

pairs(data[1:4], main = "Plot 2:emissionssw",pch = 21)
cat("\n Plot 2 shows the relationships between each two variables from the new dataset. \n")

cat("\n  \n")

model3 <- lm(nox~noxem+ws+humidity, data=data)
cat("\n After summerising the data, we create a new model by using the standardised values.\n")
print(summary(model3))
cat("\n Interesingly, in the model3, we get 2 non-significant variables which are intercept and humidity. Since the p value of the intercept is 1,we can delete this term immediately.\n")
model4 <- lm(nox~noxem+ws+humidity-1, data=data)
print(summary(model4))
cat("\n After analysing the result of model4,it seems the humidity is still non-significant. so it is reasonable to guess the data of the humidity maybe not useful to model nox.\n")

par(mfrow=c(2,2))
hist(data$nox)
hist(data$noxem)
hist(data$ws)
hist(data$humidity)
cat("\n These four plots show the frequency of variables to see the dispersion in plot 3. Obviously,the dispersion of each kind of data are good enough other than the Humidity term. Most of the data are concerntrated in interval(-1,1).\n") 
par(mfrow=c(1,1))
boxplot(data$humidity,outline = F,main="Plot 4:boxplot$humudity")
cat("\n Plot 4 is the boxplot of the data Humidity.The range of the data is big, which beyond our imagination. So it may not be the true reason that the parameter of the Humidity term is always rejected when doing F-test.\n")

cat("\n \n")


print(kappa(data[-1]))
cat("\n Hence we introduce the Kappa function to find out if there exists multicollinearity between each variables. If output is smaller than 100, then it means that these variables are not multicollinearity. Obviously, the value are far less than 100, so we can deduce the multicollinearity does not exist. \n")

print(eigen(cor(data[-1])))
cat("\n Besides,we can also calculate the eigenvalues and vectors to conclude whether the multicollinearity exist. When the terms in the output are near 0, it shows the variables are multicollinearity. From the result shown above, the multicollinearity does not exist for the values in the matrix are far from 0. After talking about the possible reasons which may cause the parameter of Humidity non-significant,we finally conclude that the humidity itself is not that relevant to the dependant variable nox. \n")
model5 <- lm(nox~noxem+ws-1, data=data)
print(summary(model5))
cat("\n So it is reasonable to delete the humidity term in model5. Next we delete intercept and humidity terms to set up a new model where all estimate are accepted.  \n")
print(AIC(model1))
print(AIC(model5))
cat("\n Then by calculating AIC of the model1 and model5,we can conclude that model5 with the smaller AIC tends to be the better fitted model.\n")


cat("\n Second Model:\n")
step.model <- step(model1, direction = "both",trace = FALSE)
print(summary(step.model))
cat("\n By using the stepwise regression,we create a new model where the humidity term is deleted and all the other parameters are accepted. \n")
cat("\n AIC of step model:\n")
print(AIC(step.model))

#3
cat("\n Third Model:\n")
#install.packages("lars")
library(lars)
x = as.matrix(rawdata[, 2:4])
y = as.matrix(rawdata[, 1])
la<-lars(x,y,type='lar',normalize=T)
par(mfrow=c(1,1))
plot(la)

cat("\n By using Lasso Model to draw the plot 5, the end point of the solution path of Humidity is close to 0, which means we can delete the humidity variable term. \n")

print(summary(la))
cat("\n An anova object is returned, with rownames the step number, and with components:
    Df: Estimated degree of freedom
    Rss:The Residual sum of Squares
    Cp: The Cp statistic
 From this summary, we can conclude that there is a dramtical decrease when Df=3, so we build the following model.
\n")

model6 <- lm(nox~noxem+ws-1, data=rawdata)
print(summary(model6))
cat("\n Producing a new model6 with noxem and ws terms,we find that both the 2 parameters are accepted. \n")
cat("\n AIC of model6:\n")
print(AIC(model6))


cat("\n Summary: \n")
cat("\n Comparing the AIC values of the three final models, we conclude that the model5 with the smallest AIC is the fitted model we prefer to choose.  \n")
cat("\n In model5, the coefficient of the noxem is 0.56577,which means the nox and the noxem are positively correlated.\n")
cat("\n And the coefficient of the ws is -0.41347,which means the nox and the ws are negatively correlated. \n")
cat("\n Above all,the absolute value of coefficient of the noxem is greater than that of the ws,which means noxem brings effects compared with the ws.\n")
cat("\n In other words,the Sum of NOx emission of cars on this motorway will improve the level of the NOx (nitrogen oxide) pollution,while the wind may reduce the pollution level. Hence,to reduce the level of the Nox pollution,it is necessary to restrict the upper bound of NOx emission during the producing process.\n")
dev.off()#end the plot and put pdf out