#==============================================================================#
#       STAT0017 : R code for the rainfall example from Coles (2001)           #
#==============================================================================#

library(ismev)

data(rain)
?rain  # in ismev

# Time series plot

my.date <- seq(as.Date("1914/1/1"), by="day", length.out=length(rain))
plot(my.date,rain,bty="l",pch=20,ylab="rainfall total / mm",xlab="year")

#-----------------------------------------------------------------------------#
#                                Fit GEV model                                #
#-----------------------------------------------------------------------------#

year <- 1900 + as.POSIXlt(my.date)$year       ### extract year from data
rain.AM <- as.numeric(tapply(rain,year,max))  ### calculate annual maxima
plot(min(year):max(year),rain.AM,pch=16,xlab="year",
     ylab="annual maximum daily rainfall / mm")

rain.gev <- gev.fit(rain.AM) ### fit GEV model
rain.gev$mle                 ### GEV MLEs
rain.gev$se                  ### standard errors of MLEs

#-----------------------------------------------------------------------------#
#                              Model diagnostics                              #
#-----------------------------------------------------------------------------#

pjn.gev.diag(rain.gev) ### PJN's GEV diagnostics

#-----------------------------------------------------------------------------#
#               Inference about parameters and return levels                  #
#-----------------------------------------------------------------------------#

### Symmetric 100*conf% confidence intervals for mu, sigma and xi ...

pjn.gev.conf(rain.gev)

### Profile log-likelihood for the GEV shape parameter xi ...

pjn.gev.profxi(rain.gev,-0.15,0.4)                    ### PJN's version

### Symmetric 100*conf% confidence intervals for return levels ...

pjn.gev.conf.ret.levels(rain.gev)

### Profile log-likelihood for the 100 year return level ...

pjn.gev.prof(rain.gev,m=100,76,170)                 ### PJN's version

#====================================================================================#
#---------------------- Threshold modelling : bin-BP  -------------------------------#
#====================================================================================#

# Threshold selection 

mrl.plot(rain)                   # MRL plot
library(threshr)
u_vec <- 0:45
rain_stab <- stability(rain, u_vec = u_vec)
plot(rain_stab, prob = FALSE)    # threshold stability plot

u <-  30                         # set threshold 
rain.gp <- gpd.fit(rain,u)       # fit bin-GP model with u=30

gpd.diag(rain.gp)                # model checking

my.gpd.profxi(rain.gp,xlow=-0.05,0.55) # profile-likelihood-based CI for xi

my.gpd.sym.CI(rain.gp,m=100)           # symmetric 95% CI for 100-year return level

my.gpd.prof(rain.gp,m=100,xlow=75,xup=195) # asymmetric CI

#---------------------- Threshold modelling : PP  -------------------------------#

u <-  30; npy <- 365.25

# Try ismev::pp.fit()
rain.pp <- pp.fit(rain,u,npy=365.25)     
# Look at the MLEs and (in particular) the SEs
# .. something has gone wrong
# The problem is that pp.fit() uses poor initial values in the optimisation

# Using better initial value fixes the problem
rain.pp <- pjn.pp.fit(rain,u,npy=365.25)     

# Model diagnostics: plots of residuals that should have a unit exponential
# distribution if the model is correct
pp.diag(rain.pp)

# To show that fits of bin-GP and NHPP are almost identical ...
mu <- rain.pp$mle[1]; sigma <- rain.pp$mle[2]; xi <- rain.pp$mle[3]
sigma.u <-  sigma + xi*(u-mu)
p.u <- (1/npy)*(1+xi*(u-mu)/sigma)^(-1/xi)
rain.pp$mle                 # mu, sigma, xi
c(p.u,sigma.u)
c(rain.gp$rate,rain.gp$mle) # pu, sigma_u, xi

#-----------------------------------------------------------------------------#
#                    Estimating the extremal index theta                      #
#-----------------------------------------------------------------------------#

# 2 different implementations of the Ferro and Segers (2003) intervals 
# estimator of theta.

library(extRemes)
thetahat <- extremalindex(rain,30)        # in extRemes 
ci(thetahat)
library(evd)
theta.est <- exi(rain,30,r=0) # in evd      
theta.est
exiplot(rain, tlim = c(10, 45))

# Calculates N-year return levels #based on GP fit and estimate of theta
# npy = mean number of observations per year
my.gpd.ret.lev <- function(u,sigma,xi,N,npy,theta,pu){ 
 u+sigma*((N*npy*theta*pu)^xi-1)/xi                    
}                                                     

# N-year return level for theta=theta.ests
# You might like to look at different values of theta to see what effect that has
my.gpd.ret.lev(u=30,sigma=rain.gp$mle[1],xi=rain.gp$mle[2],
                N=100,npy=365.25,theta=theta.est,pu=rain.gp$rate)

# Ideally one would want to take uncertainty in theta into account when doing things
# like producing confidence intervals for return levels - but I don't currently have
# code to do that!