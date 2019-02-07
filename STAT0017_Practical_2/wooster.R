library(ismev)
data(wooster) # See page 136 of Coles (2001)
?wooster # in ismev

w.year <- seq(1983,1988,len=length(wooster))
neg.temp <- -wooster
plot(w.year,neg.temp,ylab="daily min temp (deg F below zero)",xlab="year")

# Simple covariates to represent seasonality
x.sin <- sin(2*pi*w.year) # sin term with period of 1 year 
x.cos <- cos(2*pi*w.year) # cos term with period of 1 year

# Use quantile regression to set threshold at, say, 90% conditional quantile.

library(quantreg)
rq.thresh <- rq(neg.temp~x.sin+x.cos,tau=0.90) # quantile regression
u <- fitted(rq.thresh)
lines(w.year,u,col="red",lwd=2)

# Fit NHPP model using 90% threshold

ydat <- cbind(x.sin,x.cos)
fit0 <- my.pp.fit(neg.temp,threshold=u)                   # mu constant
pp.diag(fit0)
fit1 <- my.pp.fit(neg.temp,threshold=u,ydat=ydat,mul=1:2) # mu periodic (consistent with threshold)
pp.diag(fit1)

pchisq(2*(fit0$nllh-fit1$nllh),2,lower.tail=F)  # p-value from lik ratio test

# Parameter stability ...
# This will take a minute or so to run

pp.range <- my.pp.fitrange.cov(neg.temp,ydat=ydat,mul=1:2,pmin=0.7,pmax=0.95,nint=11,use.rq=T)

my.pp.fitrange.plot(pp.range,ngraphs=3,par.order=c(1,4,5)) # mu, sigma, xi
my.pp.fitrange.plot(pp.range,ngraphs=2,par.order=c(2,3))   # regression coefficients

# Extend model to have sigma periodic ...
# This will take a while to fit!

init.ests <- c(fit1$mle[1:4],0,0,fit1$mle[5]) # Start 
# mu and sigma periodic (still consistent with threshold)
fit2 <- my.pp.fit(neg.temp,threshold=u,ydat=ydat,mul=1:2,sigl=1:2,init.ests=init.ests) 
pchisq(2*(fit1$nllh-fit2$nllh),2,lower.tail=F)  # p-value from lik ratio test


