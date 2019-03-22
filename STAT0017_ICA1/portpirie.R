#=============================================================================#
#   STAT0017 : R code for the portpirie sea level example from Coles (2001)   #
#=============================================================================#

# Load revdbayes, to access the portpirie data
library(revdbayes)

?portpirie                                     ### description of the data

# if ismev version of data is loaded then convert it to evd/revdbayes format ...
if (!is.null(ncol(portpirie))) {
  portpirie <- portpirie[, "SeaLevel"]
}  

par(mfrow = c(1, 1))
plot(portpirie, bty = "l", pch = 16, ylab = "sea level / m", xlab = "year") 

#=============================================================================#
#         Frequentist inference using maximum likelihood estimation           # 
#=============================================================================#

library(ismev)

#-----------------------------------------------------------------------------#
#                                Fit GEV model                                #
#-----------------------------------------------------------------------------#

portpirie.gev <- gev.fit(portpirie) 

portpirie.gev$mle           ### GEV MLEs
portpirie.gev$se            ### standard errors of MLEs

#-----------------------------------------------------------------------------#
#                              Model diagnostics                              #
#-----------------------------------------------------------------------------#

# PJN's GEV diagnostics (ismev::gev.diag has PP axis labels the wrong way round) 
pjn.gev.diag(portpirie.gev) 

# Do you notice something strange about the behaviour of the lower limit of 
# the confidence in the bottom left plot, for long return periods?
# Why do you think that this occurs?  How could we improve this plot?

#-----------------------------------------------------------------------------#
#               Inference about parameters and return levels                  #
#-----------------------------------------------------------------------------#

### Symmetric 100*conf% confidence intervals for mu, sigma and xi ...

pjn.gev.conf(portpirie.gev, conf = 0.95)

### Profile log-likelihood for the GEV shape parameter xi ...

?gev.profxi

# PJN's version of ismev::gev.profxi (CI limits added to plots)
pjn.gev.profxi(portpirie.gev, -0.3, 0.3, conf = 0.95)          

### Symmetric 100*conf% confidence intervals for the m-year return level ...

pjn.gev.conf.ret.levels(portpirie.gev, m = 100, conf = 0.95)

### Profile log-likelihood for the m-year return level ...

?gev.prof

# PJN's version
pjn.gev.prof(portpirie.gev, m = 100, 4.4, 6.0)

# Maximum likelihood estimation for the GEV distribution is available in 
# several R packages, such as evd, evir, extRemes and fExtremes and texmex.

# Consider the evd package.  Use

help(package = evd)

# to find the evd function that performs MLE for the GEv distribution.
# Check that you can reproduce (approximately) the results obtained using the 
# ismev package.  What can you do with the object returned from this function?

#=============================================================================#
#                      A Bayesian approach, using MCMC                        #
#=============================================================================#

library(evdbayes)

# We'll use these numbers later on ...
portpirie.gev$mle           ### GEV MLEs
portpirie.gev$se            ### standard errors of MLEs

?posterior                  # help file for posterior() function in evdbayes

### First attempt ...

# Set independent normal priors with large variances (uninformative?)
prior.cov.mat <- diag(c(10000, 10000, 100))         
pn <- prior.norm(mean = c(0, 0, 0), cov = prior.cov.mat) 

# Starting well away from the MLEs
init <- c(5,1,0.1)                
# (poor choices of) SDs of the proposal distributions for (mu, ln(sigma), xi)
prop.sd <- c(0.001,10,100)        

# Produce MCMC sample
post <- posterior(3000, init = init, prior = pn, lh = "gev", 
                  data = portpirie, psd = prop.sd)
# Trace plots
par(mfrow = c(3,1))
my.ylab <- c("mu","sigma","xi")
for (i in 1:3) plot(post[, i], ylab = my.ylab[i]) 

# Look at the acceptance rates (first row of output)
attr(post,"ar")               

# Can you see what the problems are ...?

### Second attempt ...

# We already know roughly what the posterior of (mu,sigma,xi)
# should look like from the MLE fit of the GEV.  Marginally,
# the parameters should be approximately normally with respective
# means portpirie.gev$mle and standard deviations portpirie.gev$se

# Therefore, proposal SDs equal to the SEs of (mu, ln(sigma), xi)
# [note the ln in ln(sigma)] might be better than the ones in the
# first attempt

init <- portpirie.gev$mle      
prop.sd <- portpirie.gev$se / c(1, portpirie.gev$mle[2], 1)   
                 
post <- posterior(3000, init = init, prior = pn, lh = "gev", 
                  data = portpirie, psd = prop.sd)
for (i in 1:3) plot(post[,i],ylab=my.ylab[i]) 
attr(post,"ar")               

### Third attempt ...

# evdbayes has a function ar.choice to help set the proposal SDs
# It does this by searching for values that achieve approximately
# target values for the acceptance rates (default 0.4 for all parameters)
?ar.choice
prop.sd.auto <- ar.choice(init = init, prior = pn, lh = "gev", 
                          data = portpirie, psd = prop.sd, 
                          tol = rep(0.02, 3))$psd
post <- posterior(3000, init = init, prior = pn, lh = "gev", 
                  data = portpirie, psd = prop.sd.auto)
for (i in 1:3) plot(post[,i],ylab=my.ylab[i]) 
attr(post,"ar")               

# Posterior dependence among parameters
pairs(post)

# The coda package makes it easy to produce posterior summaries 
# It also provides MCMC convergence diagnostics, to inform the choice of 
# burn-in period, but we don't get into that here.

library(coda)

# Create an object in the correct format using coda::mcmc()
post_for_coda <- mcmc(post)
# burn = means using all the data. In practice burn > 0 would be used
burnin <- 0                     # choose a burn-in period
post_for_coda <- window(post_for_coda, start = burnin + 1)
summary(post_for_coda)
plot(post_for_coda)


# Posterior samples for xi and 10-year and 100-year return levels

par(mfrow = c(3, 1))
hist(post[,3], nclass = 50, prob = TRUE, main = "xi")
u.10 <- mc.quant(post, p = 0.9, lh = "gev")
u.100 <- mc.quant(post, p = 0.99, lh = "gev")
hist(u.10, nclass = 50, prob = TRUE, main = "10 year return level")
hist(u.100, nclass = 50, prob = TRUE, main = "100 year return level")

#=============================================================================#
# Now use revdbayes:                                                          #
#   easier (no MCMC convergence issues)                                       #
#   better (produces a random sample, not a dependent sample)                 #
#   and quicker!                                                              #
#=============================================================================#

# Note: revdbayes can use priors set by evdbayes
gevp  <- rpost(n = 3000, model = "gev", prior = pn, data = portpirie)
class(gevp)
# gevp has class "evpost".  It has it's own summary and plot methods. See
?summary.evpost
?plot.evpost

summary(gevp)
plot(gevp)

# revbayes allows us to use the plotting features in the bayesplot package

library(bayesplot)

plot(gevp, use_bayesplot = TRUE)
plot(gevp, use_bayesplot = TRUE, pars = "xi", prob = 0.95)
plot(gevp, use_bayesplot = TRUE, fun_name = "dens")
plot(gevp, use_bayesplot = TRUE, fun_name = "intervals")
plot(gevp, use_bayesplot = TRUE, pars = "xi", fun_name = "intervals")

#=============================================================================#
#                         Posterior predictive inference                      #
#=============================================================================#

# See the Posterior Predictive Extreme Value Inference vignette of the 
# revdbayes package for details that are relevant to the final two sections

#-----------------------------------------------------------------------------#
#                                Model checking                               #
#-----------------------------------------------------------------------------#

# General idea: 
#  1. simulate datasets from the posterior predictive distribution
#  2. compare the real dataset to these datasets: it shouldn't look unusual
# See the help documentation for more information ...

?pp_check

# We add the argument nrep to ask for simulated datasets

gevp  <- rpost(n = 10000, model = "gev", prior = pn, data = portpirie, nrep = 50)

# Some examples 

library(ggplot2)

pp_check(gevp, type = "overlaid", subtype = "ecdf") + 
  ggtitle("GEV: empirical c.d.f.s")

pp_check(gevp, stat = "max")

#-----------------------------------------------------------------------------#
#                           Extreme value inference                           #
#-----------------------------------------------------------------------------#

# Posterior predictive density for the largest value in 100 years

plot(predict(gevp, type = "d", n_years = 100))

# Posterior predictive intervals for the largest value in 100 years

i_gevp <- predict(gevp, n_years = 100, level = c(50, 95, 99), hpd = TRUE)
plot(i_gevp, which_int = "both")

# Use 

?predict.evpost 

# to find out about the two types of interval on the plot

