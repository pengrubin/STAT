##################################################################################
##################################################################################

#### EXERCISE 1 ##################################################################

##################################################################################
##################################################################################


##### Ignore these 2 commands if data "USAccDeaths" are already into your R system
setwd("/Users/hongwei/Documents/GitHub/STAT/STAT0010_Workshop")
data <- scan(file="data1.txt")

USAccDeaths <- ts(data, freq=12, start=c(1973,1))

#################################################################################

plot(USAccDeaths,type="l")

#### It is easier for the plots that follow to use "data" rather than "USAccDeaths" 

diff1 <- diff(as.vector(USAccDeaths), lag=12)

diff2 <- diff(diff1, lag=1)

#################################################################################

par(mfrow=c(2,1))

acf(diff2, lag = 26); pacf(diff2, lag = 26)

fit1 <- arima(data, order = c(0,1,1), seasonal = list(order = c(0,1,1), period=12))

fit2 <- arima(data, order = c(0,1,1), seasonal = list(order = c(1,1,0), period=12))

fit1; fit2

acf( fit1$residuals, lag=26 ); pacf( fit1$residuals, lag=26  )

acf( fit2$residuals, lag=26 ); pacf( fit2$residuals, lag=26  )

#################################################################################
##### We can now perform forecasting!

par(mfrow = c(1,1))

fit1.for <- predict(fit1, n.ahead = 12)

plot(data, type="l", xlim=c(0,86), ylim=c(6500, 12500), ylab="USAccDeaths")

lines( fit1.for$pred, lty=5, pch=18, lwd=1, type="b")

lines( fit1.for$pred + 1.96*fit1.for$se, lty=2, pch=20, lwd=1, type="l")

lines( fit1.for$pred - 1.96*fit1.for$se, lty=2, pch=20, lwd=1, type="l")

##################################################################################
##################################################################################

#### EXERCISE 2 ##################################################################

##################################################################################
##################################################################################

#### Read the data

Y3 = as.matrix( read.table(file="series3.dat")) ### ARMA(3,1)

plot(Y3, type="l")

plot(Y3, type="l", xlim=c(0,180), ylim = c(-50,50))

fit3 <- arima(Y3, order = c(3,0,1))

fit3.f <- predict(fit3, n.ahead = 24)

lines( fit3.f$pred, lty=5, pch=18, lwd=1, type="b")

#### Superimpose 90% confidence lines

lines( fit3.f$pred + 1.645*fit3.f$se , lty=2, pch=20, lwd=1, type="l")

lines(fit3.f$pred - 1.645*fit3.f$se, lty=2, pch=20, lwd=1, type="l")

#####################

Y4 = as.matrix( read.table(file="series4.dat")) ### MA(2)

plot(Y4, type="l")

plot(Y4, type="l", xlim=c(0,170), ylim = c(-1,1))

fit4 <- arima(Y4, order = c(0,0,2))

fit4.f <- predict(fit4, n.ahead = 12)

lines( fit4.f$pred, lty=5, pch=18, lwd=1, type="b")

#### Superimpose 90% confidence lines

lines( fit4.f$pred + 1.645*fit4.f$se , lty=2, pch=20, lwd=1, type="l")

lines(fit4.f$pred - 1.645*fit4.f$se, lty=2, pch=20, lwd=1, type="l")

##################################################################################
##################################################################################

#### EXERCISE 3 ##################################################################

##################################################################################
##################################################################################

#### Principles of Model Diagnostics, Forecasting, etc. are similar for many classes of models
#### GARCH Models - install fGarch package in R
#### Download data from yahoo.finance S&P500 - last 5 years of data
#### Try to fit an ARMA type of model; it is simple Random Walk?

SP500.all <- read.csv(file="SP500.csv", header=TRUE, sep=",")

SP500 <- SP500.all[,6]

plot(log(SP500), type="l", xlab="time", ylab="S&P500")

diff.SP <- diff(log(SP500));
plot(diff.SP, type="l")

par(mfrow=c(2,1))
acf(diff.SP); pacf(diff.SP)

#### Now fit a GARCH model.

install.packages("fGarch")

library(fGarch)

GARCH1.1 <- garchFit( formula = ~ garch(1, 1), data = diff.SP, trace = F)
GARCH1.2 <- garchFit( formula = ~ garch(1, 2), data = diff.SP, trace = F)
GARCH2.1 <- garchFit( formula = ~ garch(2, 1), data = diff.SP, trace = F)
GARCH2.2 <- garchFit( formula = ~ garch(2, 2), data = diff.SP, trace = F)

par(mfrow=c(1,1))

#### Plot estimated log-variances

plot(log(GARCH1.1@h.t), type="l", xlab="time", ylab = "log-volatility")

#### Calculate relevant residuals and carry out diagnostics

res <- diff.SP/GARCH1.1@sigma.t

plot(res, type="l")

acf(res); pacf(res)

#### One can do forecasting for the log(variances) 

mu <- 7.022e-04
omega <- 4.204e-06
alpha1 <- 1.974e-01
beta1 <- 7.466e-01

Tau <- length(diff.SP)
s.sq.Tau <- GARCH1.1@h.t[length(GARCH1.1@h.t)]
n <- 12
N <- 1000

s.sq <- matrix(nrow = n, ncol = N) 

s.sq[1,] <- rep( omega + alpha1*(diff.SP[Tau]-mu)^2 + beta1*s.sq.Tau , N )

for (i in 2:n) {
  
  y <- rnorm(N,0,sqrt(s.sq[i-1,]))
  
  s.sq[i,] <- omega + alpha1*y^2 + beta1*s.sq[i-1,]
  
}

#### Plot forecasts and CIs

plot(log(GARCH1.1@h.t), type="l", xlim = c(0,1350), xlab="time", ylab="log-volatility")

lines( (Tau+1):(Tau+n), rowMeans(log(s.sq)), lty=5, pch=18, lwd=1, type="b")

CI <- apply(log(s.sq), 1, quantile, probs = c(0.10, 0.90))

lines( (Tau+1):(Tau+n), CI[1,], lty=2, pch=20, lwd=1, type="l")

lines( (Tau+1):(Tau+n), CI[2,], lty=2, pch=20, lwd=1, type="l")

##################################################################################
##################################################################################



