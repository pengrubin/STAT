#------------------------------- The ismev package ---------------------------#

library(ismev)
data(fremantle) # See page 111 of Coles (2001)
?fremantle # in ismev

plot(fremantle[,1],fremantle[,2],pch=16,xlab="year",ylab="sea level (m)")
plot(fremantle[,3],fremantle[,2],pch=16,xlab="SOI",ylab="sea level (m)")

fit0 <- gev.fit(fremantle[,2])                  # no covariates
gev.diag(fit0)                                  # model diagnostics

ydat <- fremantle[,c(1,3)]                      # year in column 1, SOI in column 2
fit1 <- gev.fit(fremantle[,2],ydat=ydat,mul=1)  # convergence problems !

# Try again ...
scaled.year <- (fremantle[,1]-1897)/(1989-1897) # scale year to [0,1]
ydat <- cbind(scaled.year,fremantle[,3])        # year in column 1, SOI in column 2

fit1 <- gev.fit(fremantle[,2],ydat=ydat,mul=1)  # does reg. coeff (2nd parameter) seem signficant?
pchisq(2*(fit0$nllh-fit1$nllh),1,lower.tail=F)  # p-value from lik ratio test
gev.diag(fit1)                                  # model diagnostics

# Add SOI ...

fit2 <- gev.fit(fremantle[,2],ydat=ydat,mul=1:2)
pchisq(2*(fit1$nllh-fit2$nllh),1,lower.tail=F)  # p-value from lik ratio test
gev.diag(fit2)                                  # model diagnostics

# Covariate effects on scale (sigma)?

# Sometimes it can help to set initial estimates that start the estimation
# from a fitted simpler model
muinit <- fit2$mle[1:3]
siginit <- c(log(fit2$mle[4]), 0)
shinit <- fit2$mle[5]
fit3 <- gev.fit(fremantle[,2], ydat = ydat, mul = 1:2, sigl = 1, siglink = exp,
                muinit = muinit, siginit = siginit, shinit = shinit)
pchisq(2*(fit2$nllh-fit3$nllh),1,lower.tail=F)  # p-value from lik ratio test
gev.diag(fit3)                                  # model diagnostics

fit4 <- gev.fit(fremantle[,2], ydat = ydat, mul = 1:2, sigl = 2, siglink = exp,
                muinit = muinit, siginit = siginit, shinit = shinit)
pchisq(2*(fit2$nllh-fit4$nllh),1,lower.tail=F)  # p-value from lik ratio test
gev.diag(fit4)                                  # model diagnostics

# Typically, it is difficult to estimate covariate effects on the shape 
# parameter (xi): if we don't have lots of data then we tend to get 
# convergence problems.  ... but you could give it a go!

#-------------------------------- The evd package ----------------------------#

library(evd)
# The evd package has an anova() S3 method that performs likelihood ratio tests
# and other helpful methods like: confint, logLik, fitted, vcov, std.errors
# ... but the GEV fitting function evd::fgev() it only allows covariate 
# effects in location

evd_fit0 <- fgev(fremantle[,2])
covar <- data.frame(scaled_year = ydat[, 1], SOI = ydat[, 2])
evd_fit1 <- fgev(fremantle[,2], nsloc = covar[, 1])
evd_fit2 <- fgev(fremantle[,2], nsloc = covar)

anova(evd_fit2, evd_fit1, evd_fit0)

fitted(evd_fit2) # strictly speaking this should be coef() 
std.errors(evd_fit2)
vcov(evd_fit2)
confint(evd_fit2)
plot(evd_fit2)