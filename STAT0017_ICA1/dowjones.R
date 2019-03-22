library(ismev)
data(dowjones)
?dowjones  # in ismev

DJ <- dowjones[,2]                                  # Dow Jones index
t <- (dowjones[,1]-dowjones[1,1])/(3600*24*365)     # time in years
plot(t+1996,DJ,xlab="years",ylab="Dow Jones index") # TS plot

DJ.ret <- -100*diff(log(DJ)) # negated % daily log-returns (so large values
                             # correspond to big falls in the index)
t <- t[-1]
plot(t+1996,DJ.ret,xlab="years",ylab="% daily losses") # variability increases over time?

#-------------------------- Threshold diagnostics ----------------------------#

library(evd)
par(mfrow=c(1,1))
evd::mrlplot(DJ.ret)
tlim <- c(-1,3)
evd::mrlplot(DJ.ret,tlim)

tlim <- quantile(DJ.ret,probs=c(0.1,0.99))  # try changing the probs argument
# [tcplot in evd is an alternative to gpd.fitrange in ismev.]
tcplot(DJ.ret,tlim)
tcplot(DJ.ret,tlim,nt=100,lwd=3,type="l")
tcplot(DJ.ret,tlim, model = "pp",npp=365.25)

u <- 0.5                # pick a threshold
p.u <- mean(DJ.ret > u) # prob of exceedance

#-------------------------- Model fitting ----------------------------#

fit <- fpot(DJ.ret,threshold=u)  # evd alternatives to ismev's gpd.fit and gpd.diag
fit
par(mfrow=c(2,2))
plot(fit)
?plot.uvevd  # in evd

par(mfrow=c(2,1)) # Profile log-likelihoods 
plot(profile(fit))

fit.100 <- fpot(DJ.ret,threshold=u,mper=100) # Reparameterise in terms of
plot(profile(fit.100))                       # m-year ret level and shape.

#---------------------------- Covariates? ---------------------------#

# We use the GP model, despite the argument that it is better to use the NHPP

fit.ismev <- gpd.fit(DJ.ret,threshold=u)
gpd.diag(fit.ismev)

ydat <- matrix(t,ncol=1)
# sigma.u linear in time
fit.year <- gpd.fit(DJ.ret,threshold=u,ydat=ydat,sigl=1) 
# ln(sigma.u) linear in time
fit.year <- gpd.fit(DJ.ret,threshold=u,ydat=ydat,sigl=1,siglink=exp) 
gpd.diag(fit.year)                                       # constant threshold u

# Likelihood ratio test
pchisq(2*(fit.ismev$nllh-fit.year$nllh),1,lower.tail=F) # p-value

# Select a non-constant threshold (at conditional 100(1-pu)% quantile, assumed to
# be linear in time t) using quantile regression.

library(quantreg)
rq.thresh <- rq(DJ.ret ~ t, tau = 1 - p.u) # quantile regression
u.t <- fitted(rq.thresh)            # threshold
plot(t+1996,DJ.ret,xlab="years",ylab="% daily losses") # variability increases over time?
lines(t+1996,u.t,col="red",lwd=2)

# NHPP 

# The following fits will take several seconds to run

# use u.t even though model is stationary
# (for compatability with fits below)

# Which models are fitted?

fit.pp <- pjn.pp.fit(DJ.ret,threshold=u.t) 
np1 <- length(fit.pp$mle)                 

fit.mu <- pjn.pp.fit(DJ.ret,threshold=u.t,ydat=ydat,mul=1)
np2 <- length(fit.mu$mle)

fit.sig <- pjn.pp.fit(DJ.ret,threshold=u.t,ydat=ydat,sigl=1)
np3 <- length(fit.sig$mle)

fit.musig <- pjn.pp.fit(DJ.ret,threshold=u.t,ydat=ydat,mul=1,sigl=1)
np4 <- length(fit.musig$mle)

nllhs <- c(fit.pp$nllh,fit.mu$nllh,fit.sig$nllh,fit.musig$nllh)
npars <- c(np1,np2,np3,np4)
cbind(nllhs,npars)

pp.diag(fit.mu) # diagnostic plots

