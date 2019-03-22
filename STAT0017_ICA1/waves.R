#=============================================================================#
#   STAT0017 : R code for the wave heights data from Northrop et al (2017)    #
#=============================================================================#

# In this script far fewer commands are provided than in portpirie.R.  
# The general idea is the same: we wish to fit and check an extreme value model 
# and inferences; but now our modelling is based on the GP distribution.

# Where general instructions are provided rather than code please make use of 
# the help system to help you to construct the code that you need.
# In some cases code has been provided but with missing arguments (?).

#=============================================================================#
#          Storm peak significant wave heights from the North Sea             #
#=============================================================================#

# Load the threshr package, to access the ns data
library(threshr)

# Read the help file for the ns data
?ns

# Plot the data (how you do it is up to you)

# Set a threshold, u
# For the moment we set a threshold in an arbitrary manner: 
# at the sample 80% quantile
# We return to the issue of threshold selection later

u <- quantile(ns, probs = 0.8)

# The mean number of observations per year (npy)

# We need to know this to make extreme value inferences (return levels etc)
# It tells us (indirecty) how many years are spanned by the data 
# (it's 31 years - note that the data are from Oct-Mar winter seasons)
ns.npy <- length(ns) / 31

#=============================================================================#
#         Frequentist inference using maximum likelihood estimation           # 
#=============================================================================#

library(ismev)

#-----------------------------------------------------------------------------#
#                                Fit GP model                                 #
#-----------------------------------------------------------------------------#

?gpd.fit
data(rain)
# Fit the GP model to excesses of threshold u
# Don't forget npy !
ns.gp <- gpd.fit(rain, 10)

ns.gp$mle
ns.gp$se
ns.gp$rate
# What is the following quantity?
sqrt(ns.gp$rate * (1 - ns.gp$rate) / ns.gp$nexc)

#-----------------------------------------------------------------------------#
#                              Model diagnostics                              #
#-----------------------------------------------------------------------------#

# PJN's GP diagnostics (ismev::gpd.diag has PP axis labels the wrong way round) 
pjn.gpd.diag(ns.gp)                

#-----------------------------------------------------------------------------#
#               Inference about parameters and return levels                  #
#-----------------------------------------------------------------------------#

### Symmetric 100*conf% confidence intervals for p_u, sigma_u and xi ...

pjn.gpd.conf(ns.gp, conf = 0.95)

### Profile log-likelihood for the GP shape parameter xi ...
?gpd.profxi

# PJN's version of ismev::gpd.profxi (CI limits added to plots)
pjn.gpd.profxi(ns.gp, xlow = 0, xup = 0.2, conf = 0.95)          

### Symmetric 100*conf% confidence intervals for the m-year return level ...

pjn.gpd.conf.ret.levels(ns.gp, m = 100, npy = ns.npy, conf = 0.95)

### Profile log-likelihood for the m-year return level ...

pjn.gpd.prof(ns.gp, m = 100, xlow = 50, xup = 70, npy = ns.npy, conf = 0.95) 

#=============================================================================#
#                   A Bayesian approach, using revdbayes                      #
#=============================================================================#

library(revdbayes)

# Read the help file for set_prior to see which types of prior are available
?set_prior

# We make inferences for both the probability p_u that a value exceeds the
# threshold u (binomial model) and for the GP model parameters (sigma_u, xi), 
# based on threshold excesses of u.
# We set separate prior distributions for p_u and for (sigma_u, xi), that is,
# we take p_u and (sigma_u, xi) to be independent a priori.
# The particular priors that we choose are `uninformative': perhaps better
# described as `weakly informative'.

fp <- set_prior(prior = "flat", model = "gp", min_xi = -1)
bp <- set_bin_prior(prior = "jeffreys")
bgpg <- rpost(n = 10000, model = "bingp", prior = fp, thresh = u, data = ns,
              bin_prior = bp, npy = ns.npy)
plot(bgpg, pu_only = TRUE)
plot(bgpg, add_pu = FALSE)
plot(bgpg, add_pu = TRUE)

# You could experiment to see how much difference using a different prior makes

#=============================================================================#
#                         Posterior predictive inference                      #
#=============================================================================#

# Adapt the code in portpirie.R to perform 
#   1. posterior predictive model checking, using the pp_check() function, 
#   2. posterior predictive extreme value inferences for the 100-year maximum,
#      using the predict.evpost() function.

#-----------------------------------------------------------------------------#
#                                Model checking                               #
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
#                           Extreme value inference                           #
#-----------------------------------------------------------------------------#


#=============================================================================#
#                           Threshold selection                               #
#=============================================================================#

# We consider the two simple graphical techniques outlined in the lecture 
# slides: a threshold stability plot and a mean residual life plot

#-----------------------------------------------------------------------------#
#                         Threshold stability plot                            #
#-----------------------------------------------------------------------------#

# Set a vector of thresholds to examine
u_vec_ns <- quantile(ns, probs = seq(0, 0.95, by = 0.05))

# ismev package
gpd.fitrange(ns, umin = u_vec_ns[1], u_vec_ns[length(u_vec_ns)], nint = 20)

# threshr package
?stability
ns_stab <- stability(ns, u_vec = u_vec_ns)
?plot.stability
plot(ns_stab, top_scale = "opposite")
plot(ns_stab, top_scale = "excesses")

# What does adding the argument prof = TRUE do?
# Why does the fitting take longer?
ns_stab <- stability(ns, u_vec = u_vec_ns, prof = TRUE)
plot(ns_stab, top_scale = "opposite")

# What are the differences between ismev and threshr in relation to these plots?

#-----------------------------------------------------------------------------#
#                         Mean residual life plot                             #
#-----------------------------------------------------------------------------#

# ismev package
mrl.plot(ns)

# Which threshold would you pick?

# If you repeat the analyses above with your new choice of threshold then
# how much difference does it make?

#=============================================================================#
#        Storm peak significant wave heights from the Gulf of Mexico          #
#=============================================================================#

?gom

# Repeat the analyses for data from the Gulf of Mexico.
# Note: these data span a period of 105 years.

# How do these extreme value inferences from the North Sea and Gulf of Mexico
# storm peak significant wave heights differ?



