#==============================================================================#
# STAT0017 : a random assortment of R functions, partly to supplement/replace  #
#            functions in the package ismev                                        #
#==============================================================================#

# These functions are functional but rather a mess!  They are inelegant and not
# annotated.  I suggest that you don't waste your time looking at the contents.
# Paul Northrop, 24/1/2018

#-----------------------------------------------------------------------------#
#                           GEV model diagnostics                             #
#-----------------------------------------------------------------------------#

pjn.gev.diag <- function (z, xlab="return period"){
    n <- length(z$data)
    x <- (1:n)/(n + 1)
    if (z$trans) {
        oldpar <- par(mfrow = c(1, 2))
        plot(x, exp(-exp(-sort(z$data))), ylab = "empirical", 
            xlab = "model")
        abline(0, 1, col = 4)
        title("residual probability plot")
        plot(-log(-log(x)), sort(z$data), ylab = "empirical", 
            xlab = "model")
        abline(0, 1, col = 4)
        title("residual quantile plot (Gumbel scale)")
    }
    else {
        oldpar <- par(mfrow = c(2, 2))
        pjn.gev.pp(z$mle, z$data)
        pjn.gev.qq(z$mle, z$data)
        pjn.gev.rl(z$mle, z$cov, z$data, xlab)
        pjn.gev.his(z$mle, z$data)
    }
    par(oldpar)
    invisible()
}

pjn.gev.pp <- function (a, dat){
    plot((1:length(dat))/length(dat), gevf(a, sort(dat)), ylab = "empirical", 
        xlab = "model", main = "probability plot")
    abline(0, 1, col = 4)
}

pjn.gev.qq <- function (a, dat){
    plot(gevq(a, 1 - (1:length(dat)/(length(dat) + 1))), sort(dat), 
        ylab = "empirical", xlab = "model", main = "quantile plot")
    abline(0, 1, col = 4)
}

pjn.gev.rl <- function (a, mat, dat, xlab){
    eps <- 1e-06
    a1 <- a;a2 <- a;a3 <- a
    a1[1] <- a[1] + eps;a2[2] <- a[2] + eps;a3[3] <- a[3] + eps
    f <- c(seq(0.01, 0.09, by = 0.01), 0.1, 0.2, 0.3, 0.4, 0.5, 
        0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.995, 0.999)
    q <- gevq(a, 1 - f)
    d1<-(gevq(a1,1-f)-q)/eps;d2<-(gevq(a2,1-f)-q)/eps;d3<-(gevq(a3,1-f)-q)/eps
    d <- cbind(d1, d2, d3)
    v <- apply(d, 1, q.form, m = mat)
    plot(-1/log(f), q, log = "x", type = "n", xlim = c(0.1, 1000), 
        ylim = c(min(dat, q), max(dat, q)), xlab = xlab, 
        ylab = "return Level",axes=F)
    axis(1,at=c(0.1,1,10,100,1000),labels=c(0.1,1,10,100,1000))
    axis(2)
    box(bty="l")
    title("return level plot")
    lines(-1/log(f), q)
    lines(-1/log(f), q + 1.96 * sqrt(v), col = 4)
    lines(-1/log(f), q - 1.96 * sqrt(v), col = 4)
    points(-1/log((1:length(dat))/(length(dat) + 1)), sort(dat))
}

pjn.gev.his <- function (a, dat){
    h <- hist(dat, plot = FALSE)
    if (a[3] < 0) {
        x <- seq(min(h$breaks), min(max(h$breaks), (a[1] - a[2]/a[3] - 
            0.001)), length = 100)
    }
    else {
        x <- seq(max(min(h$breaks), (a[1] - a[2]/a[3] + 0.001)), 
            max(h$breaks), length = 100)
    }
    y <- gev.dens(a, x)
    hist(dat, prob=T, ylim = c(0, max(y)), xlab = "z", ylab = "f(z)", 
        main = "density plot",col=8)
    points(dat, rep(0, length(dat)))
    lines(x, y)
}

#-----------------------------------------------------------------------------#
#     GEV profile likelihood-based confidence interval for return levels      #
#-----------------------------------------------------------------------------#

pjn.gev.prof <- function (z,m=100,xlow,xup,conf=0.95,nint=100,npy=1){
#
# m is the "m" in "m year return level".
# npy is the number of observations per year, e.g. if the data are 6-monthly
# maxima then npy=2.
#
    if (m <= 1) stop("`m' must be greater than one")
    cat("If routine fails, try changing plotting interval", fill = TRUE)
    p <- 1/(m*npy)	### m multiplied by npy (PJN)
    v1 <- numeric(nint); v2 <- numeric(nint)
    a <- z$mle
    xp.mle <- a[1]+(a[2]/a[3])*((-log(1-p))^(-a[3])- 1)  ### estimate of xp 

    gev.plik <- function(a) {
        if (abs(a[2]) < 10^(-6)) {
            mu <- xp + a[1] * log(-log(1 - p))
            y <- (z$data - mu)/a[1]
            if (is.infinite(mu) || a[1] <= 0) 
                l <- 10^6
            else l <- length(y) * log(a[1]) + sum(exp(-y)) + 
                sum(y)
        }
        else {
            mu <- xp - a[1]/a[2] * ((-log(1 - p))^(-a[2]) - 1)
            y <- (z$data - mu)/a[1]
            y <- 1 + a[2] * y
            if (is.infinite(mu) || a[1] <= 0 || any(y <= 0)) 
                l <- 10^6
            else l <- length(y) * log(a[1]) + sum(y^(-1/a[2])) + 
                sum(log(y)) * (1/a[2] + 1)
        }
        l
    }

### Upper tail ...

    x2 <- seq(xp.mle, xup, length = nint)
    sol <- c(z$mle[2], z$mle[3])
    for (i in 1:nint) {
        xp <- x2[i]
        opt <- optim(sol, gev.plik)
        sol <- opt$par
        v2[i] <- opt$value
    }

### Lower tail ...

    x1 <- seq(xp.mle, xlow, length = nint)
    sol <- c(z$mle[2], z$mle[3])
    for (i in 1:nint) {
        xp <- x1[i]
        opt <- optim(sol, gev.plik)
        sol <- opt$par
        v1[i] <- opt$value
    }

    x <- c(rev(x1),x2); v <- c(rev(v1),v2)
    plot(x, -v, type = "l", xlab = paste(round(m,0), "year return level"), ylab = " profile log-likelihood")
    ma <- -z$nllh
    abline(h = ma, col = 4)
    abline(h = ma - 0.5 * qchisq(conf, 1), col = 4)
 
 u <- par("usr")								### extract plotting coords
 yaxis <- -v; xaxis <- x;conf.line <- ma - 0.5 * qchisq(conf, 1)
 temp <- diff(yaxis-conf.line>0)			### to find where curve crosses CI line

 loc <- which(temp==-1)					### upper limit of CI
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 up.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=up.lim,lty=2);text(up.lim,u[3]-0.02*(u[4]-u[3]),round(up.lim,2),xpd=T,cex=0.75)

 loc <- which(temp==1)
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 low.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=low.lim,lty=2);text(low.lim,u[3]-0.02*(u[4]-u[3]),round(low.lim,2),xpd=T,cex=0.75)

 abline(v=xp.mle,lty=2);text(xp.mle,u[3]-0.02*(u[4]-u[3]),round(xp.mle,2),xpd=T,cex=0.75)

#    invisible()
     list(low.lim=low.lim,xp.mle=xp.mle,up.lim=up.lim)
}

#-----------------------------------------------------------------------------#
#   GEV profile likelihood-based confidence interval for shape parameter xi   #
#-----------------------------------------------------------------------------#

pjn.gev.profxi <- function (z, xlow, xup, conf = 0.95, nint = 100){
    cat("If routine fails, try changing plotting interval", fill = TRUE)
    v1 <- numeric(nint); v2 <- numeric(nint)
    x <- seq(xup, xlow, length = nint)
    xi.mle <- z$mle[3]
    gev.plikxi <- function(a) {
        if (abs(xi) < 10^(-6)) {
            y <- (z$data - a[1])/a[2]
            if (a[2] <= 0) 
                l <- 10^6
            else l <- length(y) * log(a[2]) + sum(exp(-y)) + 
                sum(y)
        }
        else {
            y <- (z$data - a[1])/a[2]
            y <- 1 + xi * y
            if (a[2] <= 0 || any(y <= 0)) 
                l <- 10^6
            else l <- length(y) * log(a[2]) + sum(y^(-1/xi)) + 
                sum(log(y)) * (1/xi + 1)
        }
        l
    }

#    for (i in 1:nint) {
#        xi <- x[i]
#        opt <- optim(sol, gev.plikxi)
#        sol <- opt$par
#        v[i] <- opt$value
#    }

### Upper tail ...

    x2 <- seq(xi.mle, xup, length = nint)
    sol <- c(z$mle[1], z$mle[2])
    for (i in 1:nint) {
        xi <- x2[i]
        opt <- optim(sol, gev.plikxi)
        sol <- opt$par
        v2[i] <- opt$value
    }

### Lower tail ...

    x1 <- seq(xi.mle, xlow, length = nint)
    sol <- c(z$mle[1], z$mle[2])
    for (i in 1:nint) {
        xi <- x1[i]
        opt <- optim(sol, gev.plikxi)
        sol <- opt$par
        v1[i] <- opt$value
    }

    x <- c(rev(x1),x2); v <- c(rev(v1),v2)
 
    plot(x, -v, type = "l", xlab = expression("shape parameter "*xi), ylab = "profile log-likelihood")
    ma <- -z$nllh
    abline(h = ma, col = 4)
    abline(h = ma - 0.5 * qchisq(conf, 1), col = 4)

 u <- par("usr")								### extract plotting coords
 yaxis <- -v; xaxis <- x;conf.line <- ma - 0.5 * qchisq(conf, 1)
 temp <- diff(yaxis-conf.line>0)			### to find where curve crosses CI line

 loc <- which(temp==-1)					### upper limit of CI
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 up.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=up.lim,lty=2);text(up.lim,u[3]-0.02*(u[4]-u[3]),round(up.lim,2),xpd=T,cex=0.75)

 loc <- which(temp==1)
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 low.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=low.lim,lty=2);text(low.lim,u[3]-0.02*(u[4]-u[3]),round(low.lim,2),xpd=T,cex=0.75)

 abline(v=xi.mle,lty=2);text(xi.mle,u[3]-0.02*(u[4]-u[3]),round(xi.mle,2),xpd=T,cex=0.75)

 list(low.lim=low.lim,xi.mle=xi.mle,up.lim=up.lim)
}

#-----------------------------------------------------------------------------#
#            Symmetric confidence intervals for GEV parameters                #
#-----------------------------------------------------------------------------#

pjn.gev.conf <- function(z,conf=0.95){
 mles <- z$mle
 ses <- z$se
 low.lim <- mles - qnorm(1-(1-conf)/2)*ses ### lower limits for mu,sigma,xi
 up.lim <- mles + qnorm(1-(1-conf)/2)*ses ### upper limits for mu,sigma,xi
 list(low.lim=low.lim,up.lim=up.lim)
}

#-----------------------------------------------------------------------------#
#              Symmetric confidence intervals for return levels               #
#-----------------------------------------------------------------------------#

pjn.gev.conf.ret.levels <- function(z,m=100,conf=0.95,npy=1){
#
# m is the "m" in "m year return level".
# npy is the number of observations per year, e.g. if the data are 6-monthly
# maxima then npy=2.
#
 p <- 1/(m*npy)	### m multiplied by npy (PJN)
 mu <- z$mle[1]
 sigma <- z$mle[2]
 xi <- z$mle[3]
 zp <- gevq(z$mle,p)
 yp <- -log(1-p)
 V <- z$cov
 dz1 <- 1
 dz2 <- -(1-yp^(-xi))/xi
 dz3 <- sigma*(1-yp^(-xi))/xi^2-sigma*yp^(-xi)*log(yp)/xi
 dz <- c(dz1,dz2,dz3)
 se.zp <- sqrt(t(dz)%*%V%*%dz)
 low.lim <- zp - qnorm(1-(1-conf)/2)*se.zp ### lower limits for mu,sigma,xi
 up.lim <- zp + qnorm(1-(1-conf)/2)*se.zp ### lower limits for mu,sigma,xi
 list(low.lim=low.lim,zp.mle=zp,up.lim=up.lim)
}

#-----------------------------------------------------------------------#
#                 Functions for GEV diagnostics                         #
#-----------------------------------------------------------------------#

my.gev.diag <- function (z, xlab="Return period"){
    n <- length(z$data)
    x <- (1:n)/(n + 1)
    if (z$trans) {
        oldpar <- par(mfrow = c(1, 2))
        plot(x, exp(-exp(-sort(z$data))), ylab = "Empirical", 
            xlab = "Model")
        abline(0, 1, col = 4)
        title("Residual Probability Plot")
        plot(-log(-log(x)), sort(z$data), ylab = "Empirical", 
            xlab = "Model")
        abline(0, 1, col = 4)
        title("Residual Quantile Plot (Gumbel Scale)")
    }    else {
        oldpar <- par(mfrow = c(2, 2))
        my.gev.pp(z$mle, z$data)
        my.gev.qq(z$mle, z$data)
        my.gev.rl(z$mle, z$cov, z$data, xlab)
        my.gev.his(z$mle, z$data)
    }
    par(oldpar)
    invisible()
}

my.gev.pp <- function (a, dat){
    plot((1:length(dat))/length(dat), gevf(a, sort(dat)), ylab = "Empirical", 
        xlab = "Model", main = "Probability Plot")
    abline(0, 1, col = 4)
}

my.gev.qq <- function (a, dat){
    plot(gevq(a, 1 - (1:length(dat)/(length(dat) + 1))), sort(dat), 
        ylab = "Empirical", xlab = "Model", main = "Quantile Plot")
    abline(0, 1, col = 4)
}

my.gev.rl <- function (a, mat, dat, xlab){
    eps <- 1e-06
    a1 <- a;a2 <- a;a3 <- a
    a1[1] <- a[1] + eps;a2[2] <- a[2] + eps;a3[3] <- a[3] + eps
    f <- c(seq(0.01, 0.09, by = 0.01), 0.1, 0.2, 0.3, 0.4, 0.5, 
        0.6, 0.7, 0.8, 0.9, 0.95, 0.99, 0.995, 0.999)
    q <- gevq(a, 1 - f)
    d1<-(gevq(a1,1-f)-q)/eps;d2<-(gevq(a2,1-f)-q)/eps;d3<-(gevq(a3,1-f)-q)/eps
    d <- cbind(d1, d2, d3)
    v <- apply(d, 1, q.form, m = mat)
    plot(-1/log(f), q, log = "x", type = "n", xlim = c(0.1, 1000), 
        ylim = c(min(dat, q), max(dat, q)), xlab = xlab, 
        ylab = "Return Level",axes=F)
    axis(1,at=c(0.1,1,10,100,1000),labels=c(0.1,1,10,100,1000))
    axis(2)
    box(bty="l")
    title("Return Level Plot")
    lines(-1/log(f), q)
    lines(-1/log(f), q + 1.96 * sqrt(v), col = 4)
    lines(-1/log(f), q - 1.96 * sqrt(v), col = 4)
    points(-1/log((1:length(dat))/(length(dat) + 1)), sort(dat))
}

my.gev.his <- function (a, dat){
    h <- hist(dat, plot = FALSE)
    if (a[3] < 0) {
        x <- seq(min(h$breaks), min(max(h$breaks), (a[1] - a[2]/a[3] - 
            0.001)), length = 100)
    }
    else {
        x <- seq(max(min(h$breaks), (a[1] - a[2]/a[3] + 0.001)), 
            max(h$breaks), length = 100)
    }
    y <- gev.dens(a, x)
    hist(dat, prob=T, ylim = c(0, max(y)), xlab = "z", ylab = "f(z)", 
        main = "Density Plot",col=8)
    points(dat, rep(0, length(dat)))
    lines(x, y)
}


#-----------------------------------------------------------------------#
#      Functions to fit GEV distribution to interval-censored data      #
#-----------------------------------------------------------------------#

gev.cens.fit <- function (xdat.lower, xdat.upper, init=NULL, ydat=NULL, mul=NULL, 
    sigl = NULL, shl = NULL, mulink = identity, siglink = identity, shlink = identity, 
    show = TRUE, method = "Nelder-Mead", maxit = 10000,...){
    z <- list()
    npmu <- length(mul) + 1
    npsc <- length(sigl) + 1
    npsh <- length(shl) + 1
    z$trans <- FALSE
#
    xdat <- (xdat.lower+xdat.upper)/2		### Use averages of intervals to 
    xdat.new <- xdat[is.finite(xdat)]		### calculate initial estimates
								### (but remove Infs)
    in2 <- sqrt(6 * var(xdat.new))/pi
    in1 <- mean(xdat.new) - 0.57722 * in2
    if (is.null(mul)) {
        mumat <- as.matrix(rep(1, length(xdat.lower)))
        muinit <- in1
    }
    else {
        z$trans <- TRUE
        mumat <- cbind(rep(1, length(xdat.lower)), ydat[, mul])
        muinit <- c(in1, rep(0, length(mul)))
    }
    if (is.null(sigl)) {
        sigmat <- as.matrix(rep(1, length(xdat.lower)))
        siginit <- in2
    }
    else {
        z$trans <- TRUE
        sigmat <- cbind(rep(1, length(xdat.lower)), ydat[, sigl])
        siginit <- c(in2, rep(0, length(sigl)))
    }
    if (is.null(shl)) {
        shmat <- as.matrix(rep(1, length(xdat.lower)))
        shinit <- 0.1
    }
    else {
        z$trans <- TRUE
        shmat <- cbind(rep(1, length(xdat.lower)), ydat[, shl])
        shinit <- c(0.1, rep(0, length(shl)))
    }
    z$model <- list(mul, sigl, shl)
    z$link <- deparse(substitute(c(mulink, siglink, shlink)))
    if (is.null(init)) init <- c(muinit, siginit, shinit)
    gev.cens.loglik <- function(a) {
        mu <- mulink(mumat %*% (a[1:npmu]))
        sc <- siglink(sigmat %*% (a[seq(npmu + 1, length = npsc)]))
        xi <- shlink(shmat %*% (a[seq(npmu + npsc + 1, length = npsh)]))
        if (any(sc <= 0)) return(10^6)
	  pars <- cbind(mu,sc,xi)
	  log.lik <- sum(log(gev.cdf(xdat.upper,pars)-gev.cdf(xdat.lower,pars)))
	  -log.lik
    }
    x <- optim(init, gev.cens.loglik, hessian = TRUE, method = method, 
        control = list(maxit = maxit, ...))
    z$conv <- x$convergence
#-----------------------------------------------------------------------------------#
### Replace interval censored data with point values.                               #
### For each interval inpute using the order statistics                             # 
### e.g. 1 value in interval, replace with median (on the interval)                 #
###      2 values in interval, replace with 33.333%ile and 66.666%ile etc.          #
#-----------------------------------------------------------------------------------#
#
	
### Calculate matrix of fitted parameter values.

    mu <- mulink(mumat %*% (x$par[1:npmu]))
    sc <- siglink(sigmat %*% (x$par[seq(npmu + 1, length = npsc)]))
    xi <- shlink(shmat %*% (x$par[seq(npmu + npsc + 1, length = npsh)]))
    pars <- cbind(mu,sc,xi)

### Replace data intervals by expected order stats under fitted model

    if (!z$trans) xdat <- gev.cens.impute(xdat.lower,xdat.upper,pars)
    if (z$trans) xdat <- gev.cens.impute.cov(xdat.lower,xdat.upper,pars,ydat)
#
    z$nllh <- x$value
    z$data <- xdat
    if (z$trans) {
        z$data <- -log(as.vector((1 + (xi * (xdat - mu))/sc)^(-1/xi)))
    }
    z$mle <- x$par
    z$cov <- solve(x$hessian)
    z$se <- sqrt(diag(z$cov))
    z$vals <- cbind(mu, sc, xi)
    if (show) {
        if (z$trans) 
            print(z[c(2, 3, 4)])
        else print(z[4])
        if (!z$conv) 
            print(z[c(5, 7, 9)])
    }
    z$x.low <- xdat.lower
    z$x.up <- xdat.upper
    invisible(z)
}

gev.cdf <- function(y,a){					### cdf of GEV(a[1],a[2],a[3])
	mm <- a[,1]; ss<-a[,2]; xx <- a[,3]			### matrix input
	ifelse(xx!=0,gev.pjn(y,a),gum.pjn(y,a))
}
gev.df <- function(a,z){					### GEV c.d.f., vector input
    if (a[3]!=0) exp(-(1+(a[3]*(z-a[1]))/a[2])^(-1/a[3]))
    else gumbel.df(z,a[1],a[2])
}

gumbel.df <- function(x,a,b) exp(-exp(-(x-a)/b))		### Gumbel c.d.f., vector input

gev.quan <- function(a,p){					### GEV quantiles
    mm <- a[,1]; ss<-a[,2]; xx <- a[,3]			### xi < 0 and xi > 0.
    ifelse(xx!=0,mm+(ss*((-log(p))^(-xx)-1))/xx,gum.quan(p,mm,ss))
}

gum.quan <- function(x,a,b) a-b*log(-log(x))	 	### Gumbel quantiles

gev.pjn <- function(y,a){					### distinguishes between 
	mm <- a[,1]; ss<-a[,2]; xx <- a[,3]			### xi < 0 and xi > 0.
	ifelse(xx>0,gev.xig0(y,a),gev.xil0(y,a))
}
gum.pjn <- function(y,a){					### Gumbel case
	mm <- a[,1]; ss<-a[,2]; xx <- a[,3]
	exp(-exp(-(y-mm)/ss))
}
gev.xig0 <- function(y,a){					### GEV xi > 0 
	mm <- a[,1]; ss<-a[,2]; xx <- a[,3]
	temp <- 1+xx*(y-mm)/ss
	ifelse(temp>0,exp(-(1+(xx*(y-mm))/ss)^(-1/xx)),0)
}
gev.xil0 <- function(y,a){					### GEV xi < 0
	mm <- a[,1]; ss<-a[,2]; xx <- a[,3]
	temp <- 1+xx*(y-mm)/ss
	ifelse(temp>0,exp(-(1+(xx*(y-mm))/ss)^(-1/xx)),1)
}

gev.cens.impute <- function(x.low,x.up,a){
#
# Imputes values for interval centred data based on fitted GEV distribution.
# x.low : lower bound on value
# x.up  : upper bound on value
# a     : GEV parameters mu, sigma and xi
#
	temp <- as.data.frame(table(x.low,x.up))			### tabulate interval 
	temp <- temp[which(temp[,3]>0),]				### censored data
	num <- sequence(temp[,3])					
	den <- rep(temp[,3],times=temp[,3])+1			### (1:n)/(n+1)
	p.vec <- num/den							### in each interval
      temp <- rank(x.low,ties.method="first")
#-----------------------------------------------------------#
#      temp <- order(x.low)
#      x.low <- x.low[temp]	# different way to do this.
#      x.up <- x.up[temp]
#	a <- a[temp,]
#-----------------------------------------------------------#
      num <- num[temp]
      den <- den[temp]
      p.vec <- num/den
	temp <- gev.cdf(x.low,a)+p.vec*(gev.cdf(x.up,a)-gev.cdf(x.low,a))
	gev.quan(a,temp)	
}


#-----------------------------------------------------------------------#
#   Functions to produce confidence intervals for xi and return levels  #
#-----------------------------------------------------------------------#

my.gev.profxi <- function (z, xlow, xup, conf = 0.95, nint = 100){
    cat("If routine fails, try changing plotting interval", fill = TRUE)
    v1 <- numeric(nint); v2 <- numeric(nint)

    gev.plikxi <- function(a) {
        if (abs(xi) < 10^(-6)) {
            y <- (z$data - a[1])/a[2]
            if (a[2] <= 0) 
                l <- 10^6
            else l <- length(y) * log(a[2]) + sum(exp(-y)) + 
                sum(y)
        }
        else {
            y <- (z$data - a[1])/a[2]
            y <- 1 + xi * y
            if (a[2] <= 0 || any(y <= 0)) 
                l <- 10^6
            else l <- length(y) * log(a[2]) + sum(y^(-1/xi)) + 
                sum(log(y)) * (1/xi + 1)
        }
        l
    }

### Upper tail ...

    x2 <- seq(z$mle[3], xup, length = nint)
    sol <- c(z$mle[1], z$mle[2])
    for (i in 1:nint) {
        xi <- x2[i]
        opt <- optim(sol, gev.plikxi)
        sol <- opt$par
        v2[i] <- opt$value
    }

### Lower tail ...

    x1 <- seq(z$mle[3], xlow, length = nint)
    sol <- c(z$mle[1], z$mle[2])
    for (i in 1:nint) {
        xi <- x1[i]
        opt <- optim(sol, gev.plikxi)
        sol <- opt$par
        v1[i] <- opt$value
    }

    x <- c(rev(x1),x2); v <- c(rev(v1),v2)
    plot(x, -v, type = "l", xlab = "Shape Parameter", ylab = "Profile Log-likelihood")
    ma <- -z$nllh
    abline(h = ma, col = 4)
    abline(h = ma - 0.5 * qchisq(conf, 1), col = 4)

 u <- par("usr")								### extract plotting coords
 yaxis <- -v; xaxis <- x;conf.line <- ma - 0.5 * qchisq(conf, 1)
 temp <- diff(yaxis-conf.line>0)			### to find where curve crosses CI line

 loc <- which(temp==-1)					### upper limit of CI
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 up.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=up.lim,lty=2);text(up.lim,u[3]-0.02*(u[4]-u[3]),round(up.lim,2),xpd=T,cex=0.75)

 loc <- which(temp==1)
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 low.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=low.lim,lty=2);text(low.lim,u[3]-0.02*(u[4]-u[3]),round(low.lim,2),xpd=T,cex=0.75)

 xi <- z$mle[3]
 abline(v=xi,lty=2);text(xi,u[3]-0.02*(u[4]-u[3]),round(xi,2),xpd=T,cex=0.75)

    invisible()
}

gev.cens.profxi <- function (z, xlow, xup, conf = 0.95, nint = 100){
    cat("If routine fails, try changing plotting interval", fill = TRUE)
    v1 <- numeric(nint); v2 <- numeric(nint)
    gev.cens.ploglikxi <- function(a) {
        mu <- a[1]
        sc <- a[2]
        if (sc <= 0) return(10^6)
	  pars <- cbind(mu,sc,xi)
	  log.lik <- sum(log(gev.df(pars,z$x.up)-gev.df(pars,z$x.low)))
	  -log.lik
    }

### Upper tail ...

    x2 <- seq(z$mle[3], xup, length = nint)
    sol <- c(z$mle[1], z$mle[2])
    for (i in 1:nint) {
        xi <- x2[i]
        opt <- optim(sol, gev.cens.ploglikxi)
        sol <- opt$par
        v2[i] <- opt$value
    }

### Lower tail ...

    x1 <- seq(z$mle[3], xlow, length = nint)
    sol <- c(z$mle[1], z$mle[2])
    for (i in 1:nint) {
        xi <- x1[i]
        opt <- optim(sol, gev.cens.ploglikxi)
        sol <- opt$par
        v1[i] <- opt$value
    }

    x <- c(rev(x1),x2); v <- c(rev(v1),v2)
    plot(x, -v, type = "l", xlab = "Shape Parameter", ylab = "Profile Log-likelihood")
    ma <- -z$nllh
    abline(h = ma, col = 4)
    abline(h = ma - 0.5 * qchisq(conf, 1), col = 4)

 u <- par("usr")								### extract plotting coords
 yaxis <- -v; xaxis <- x;conf.line <- ma - 0.5 * qchisq(conf, 1)
 temp <- diff(yaxis-conf.line>0)			### to find where curve crosses CI line

 loc <- which(temp==-1)					### upper limit of CI
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 up.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=up.lim,lty=2);text(up.lim,u[3]-0.02*(u[4]-u[3]),round(up.lim,2),xpd=T,cex=0.75)

 loc <- which(temp==1)
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 low.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=low.lim,lty=2);text(low.lim,u[3]-0.02*(u[4]-u[3]),round(low.lim,2),xpd=T,cex=0.75)

 xi <- z$mle[3]
 abline(v=xi,lty=2);text(xi,u[3]-0.02*(u[4]-u[3]),round(xi,2),xpd=T,cex=0.75)

    invisible()
}

gev.cens.prof <- function (z, m=100, xlow, xup, conf = 0.95, nint = 100){
    if (m <= 1) stop("`m' must be greater than one")
    cat("If routine fails, try changing plotting interval", fill = TRUE)
    p <- 1/m
    v1 <- numeric(nint); v2 <- numeric(nint)
    a <- z$mle
    xp.mle <- a[1]+(a[2]/a[3])*((-log(1-p))^(-a[3])- 1)  ### estimate of xp 

    gev.cens.ploglik <- function(a) {
        sc <- a[1]
        xi <- a[2]
	  mu <- xp - (a[1]/a[2]) * ((-log(1 - p))^(-a[2]) - 1)
        if (is.infinite(mu) || sc <= 0) return(10^6)
	  pars <- cbind(mu,sc,xi)
	  log.lik <- sum(log(gev.df(pars,z$x.up)-gev.df(pars,z$x.low)))
	  -log.lik
    }

### Upper tail ...

    x2 <- seq(xp.mle, xup, length = nint)
    sol <- c(z$mle[2], z$mle[3])
    for (i in 1:nint) {
        xp <- x2[i]
        opt <- optim(sol, gev.cens.ploglik)
        sol <- opt$par
        v2[i] <- opt$value
    }

### Lower tail ...

    x1 <- seq(xp.mle, xlow, length = nint)
    sol <- c(z$mle[2], z$mle[3])
    for (i in 1:nint) {
        xp <- x1[i]
        opt <- optim(sol, gev.cens.ploglik)
        sol <- opt$par
        v1[i] <- opt$value
    }

    x <- c(rev(x1),x2); v <- c(rev(v1),v2)
    plot(x, -v, type = "l", xlab = "Return Level", ylab = " Profile Log-likelihood")
    ma <- -z$nllh
    abline(h = ma, col = 4)
    abline(h = ma - 0.5 * qchisq(conf, 1), col = 4)
 
 u <- par("usr")								### extract plotting coords
 yaxis <- -v; xaxis <- x;conf.line <- ma - 0.5 * qchisq(conf, 1)
 temp <- diff(yaxis-conf.line>0)			### to find where curve crosses CI line

 loc <- which(temp==-1)					### upper limit of CI
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 up.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=up.lim,lty=2);text(up.lim,u[3]-0.02*(u[4]-u[3]),round(up.lim,2),xpd=T,cex=0.75)

 loc <- which(temp==1)
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 low.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=low.lim,lty=2);text(low.lim,u[3]-0.02*(u[4]-u[3]),round(low.lim,2),xpd=T,cex=0.75)

 abline(v=xp.mle,lty=2);text(xp.mle,u[3]-0.02*(u[4]-u[3]),round(xp.mle,2),xpd=T,cex=0.75)

#    invisible()
     list(low.lim=low.lim,xp.mle=xp.mle,up.lim=up.lim)
}

my.gev.prof <- function (z,m=100,xlow,xup,conf=0.95,nint=100,npy=1){
#
# npy is the number of observations per year.
#
    if (m <= 1) stop("`m' must be greater than one")
    cat("If routine fails, try changing plotting interval", fill = TRUE)
    p <- 1/(m*npy)	### m multiplied by npy (PJN)
    v1 <- numeric(nint); v2 <- numeric(nint)
    a <- z$mle
    xp.mle <- a[1]+(a[2]/a[3])*((-log(1-p))^(-a[3])- 1)  ### estimate of xp 

    gev.plik <- function(a) {
        if (abs(a[2]) < 10^(-6)) {
            mu <- xp + a[1] * log(-log(1 - p))
            y <- (z$data - mu)/a[1]
            if (is.infinite(mu) || a[1] <= 0) 
                l <- 10^6
            else l <- length(y) * log(a[1]) + sum(exp(-y)) + 
                sum(y)
        }
        else {
            mu <- xp - a[1]/a[2] * ((-log(1 - p))^(-a[2]) - 1)
            y <- (z$data - mu)/a[1]
            y <- 1 + a[2] * y
            if (is.infinite(mu) || a[1] <= 0 || any(y <= 0)) 
                l <- 10^6
            else l <- length(y) * log(a[1]) + sum(y^(-1/a[2])) + 
                sum(log(y)) * (1/a[2] + 1)
        }
        l
    }

### Upper tail ...

    x2 <- seq(xp.mle, xup, length = nint)
    sol <- c(z$mle[2], z$mle[3])
    for (i in 1:nint) {
        xp <- x2[i]
        opt <- optim(sol, gev.plik)
        sol <- opt$par
        v2[i] <- opt$value
    }

### Lower tail ...

    x1 <- seq(xp.mle, xlow, length = nint)
    sol <- c(z$mle[2], z$mle[3])
    for (i in 1:nint) {
        xp <- x1[i]
        opt <- optim(sol, gev.plik)
        sol <- opt$par
        v1[i] <- opt$value
    }

    x <- c(rev(x1),x2); v <- c(rev(v1),v2)
    plot(x, -v, type = "l", xlab = paste(round(m,0), "year return level"), ylab = " Profile Log-likelihood")
    ma <- -z$nllh
    abline(h = ma, col = 4)
    abline(h = ma - 0.5 * qchisq(conf, 1), col = 4)
 
 u <- par("usr")								### extract plotting coords
 yaxis <- -v; xaxis <- x;conf.line <- ma - 0.5 * qchisq(conf, 1)
 temp <- diff(yaxis-conf.line>0)			### to find where curve crosses CI line

 loc <- which(temp==-1)					### upper limit of CI
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 up.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=up.lim,lty=2);text(up.lim,u[3]-0.02*(u[4]-u[3]),round(up.lim,2),xpd=T,cex=0.75)

 loc <- which(temp==1)
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 low.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=low.lim,lty=2);text(low.lim,u[3]-0.02*(u[4]-u[3]),round(low.lim,2),xpd=T,cex=0.75)

 abline(v=xp.mle,lty=2);text(xp.mle,u[3]-0.02*(u[4]-u[3]),round(xp.mle,2),xpd=T,cex=0.75)

#    invisible()
     list(low.lim=low.lim,xp.mle=xp.mle,up.lim=up.lim)
}

gev.rl.CI <- function (z, m=100,npy=1,conf=0.95){ 
#
# npy is the number of observations per year.
#
    a <- z$mle
    mat <- z$cov
    dat <- z$data
    f <- 1-1/(m*npy)  ### m multiplied by npy (PJN)
    eps <- 1e-06; a1 <- a; a2 <- a; a3 <- a
    a1[1] <- a[1] + eps; a2[2] <- a[2] + eps; a3[3] <- a[3] + eps
    q <- gevq(a, 1 - f); d1 <- (gevq(a1, 1 - f) - q)/eps
    d2 <- (gevq(a2, 1 - f) - q)/eps; d3 <- (gevq(a3, 1 - f) - q)/eps
    d <- cbind(d1, d2, d3); v <- apply(d, 1, q.form, m = mat)
    rl.mle <- q
    alpha <- 1-conf
    rl.lower <- q - qnorm(1-alpha/2) * sqrt(v)
    rl.upper <- q + qnorm(1-alpha/2) * sqrt(v)
    print(c(rl.mle,rl.lower,rl.upper))
    list(mle=rl.mle,lower=rl.lower,upper=rl.upper)
}

gum.rl.CI <- function (z, m=100,npy=1){ 
#
# npy is the number of observations per year.
#
    a <- z$mle ; a <- c(a,0)
    mat <- z$cov
    dat <- z$data
    f <- 1-1/(m*npy)  ### m multiplied by npy (PJN)
    eps <- 1e-06; a1 <- a; a2 <- a
    a1[1] <- a[1] + eps; a2[2] <- a[2] + eps
    q <- gevq(a, 1 - f); d1 <- (gevq(a1, 1 - f) - q)/eps
    d2 <- (gevq(a2, 1 - f) - q)/eps
    d <- cbind(d1, d2); v <- apply(d, 1, q.form, m = mat)
    rl.mle <- q
    rl.lower <- q - 1.96 * sqrt(v)
    rl.upper <- q + 1.96 * sqrt(v)
    print(c(rl.mle,rl.lower,rl.upper))
    list(mle=rl.mle,lower=rl.lower,upper=rl.upper)
}


#-----------------------------------------------------------------------------#
#            Symmetric confidence intervals for GP parameters                #
#-----------------------------------------------------------------------------#

pjn.gpd.conf <- function(z, conf = 0.95){
 mles <- z$mle
 ses <- z$se
 # normal quantile
 zval <- qnorm(1-(1-conf)/2)
 # GP parameters: sigma_u, xi
 low.lim <- mles - zval*ses ### lower limits for sigma_u,xi
 up.lim <- mles + zval*ses ### upper limits for sigma_u,xi
 # Exceedance probability : p_u
 pu_se <- sqrt(z$rate * (1 - z$rate) / z$nexc) 
 low_p <- z$rate - zval * pu_se
 up_p <- z$rate + zval * pu_se
 lows <- c(low_p, low.lim)
 names(lows) <- c("pu", "sigmau", "xi")
 ups <- c(up_p, up.lim)
 names(ups) <- c("pu", "sigmau", "xi")
 list(low.lim = lows, up.lim = ups)
}

my.gpd.prof <- function (z, m, xlow, xup, npy = NULL, conf = 0.95, nint = 100){

    cat("If routine fails, try changing plotting interval", fill = TRUE)
    xdat <- z$data
    u <- z$threshold
    la <- z$rate
    v <- numeric(nint)
    x <- seq(xlow, xup, length = nint)
    if (is.null(npy)) {
      npy <- z$npy
    }
    m <- m * npy
    sol <- z$mle[2]
    sigma <- z$mle[1]
    xi <- z$mle[2]
    pu <- z$rate
    xm.mle <- u+sigma*((m*pu)^xi-1)/xi
    gpd.plik <- function(a) {
        if (m != Inf) 
            sc <- (a * (xp - u))/((m * la)^a - 1)
        else sc <- (u - xp)/a
        if (abs(a) < 10^(-4)) 
            l <- length(xdat) * log(sc) + sum(xdat - u)/sc
        else {
            y <- (xdat - u)/sc
            y <- 1 + a * y
            if (any(y <= 0) || sc <= 0) 
                l <- 10^6
            else l <- length(xdat) * log(sc) + sum(log(y)) * 
                (1/a + 1)
        }
        l
    }

    for (i in 1:nint) {
        xp <- x[i]
        opt <- optim(sol, gpd.plik, method = "BFGS")
        sol <- opt$par
        v[i] <- opt$value
    }

par(mfrow=c(1,1),mar=c(4.5,4,2,1)) 

    plot(x, -v, type = "l", xlab = "Return Level", ylab = "Profile Log-likelihood")
    ma <- -z$nllh
    abline(h = ma)
    abline(h = ma - 0.5 * qchisq(conf, 1))

 u <- par("usr")								### extract plotting coords
 yaxis <- -v; xaxis <- x;conf.line <- ma - 0.5 * qchisq(conf, 1)
 temp <- diff(yaxis-conf.line>0)			### to find where curve crosses CI line

 loc <- which(temp==-1)					### upper limit of CI
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 up.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=up.lim,lty=2);text(up.lim,u[3]-0.02*(u[4]-u[3]),round(up.lim,2),xpd=T,cex=0.75)

 loc <- which(temp==1)
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 low.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=low.lim,lty=2);text(low.lim,u[3]-0.02*(u[4]-u[3]),round(low.lim,2),xpd=T,cex=0.75)

 abline(v=xm.mle,lty=2);text(xm.mle,u[3]-0.02*(u[4]-u[3]),round(xm.mle,2),xpd=T,cex=0.75)

    invisible()
     list(low.lim=low.lim,xm.mle=xm.mle,up.lim=up.lim)
}

pjn.gpd.prof <- my.gpd.prof

my.gpd.profxi <- function(z, xlow, xup, conf = 0.95, nint = 100){
    cat("If routine fails, try changing plotting interval", fill = TRUE)
    xdat <- z$data
    u <- z$threshold
    v <- numeric(nint)
    x <- seq(xup, xlow, length = nint)
    sol <- z$mle[1]
    gpd.plikxi <- function(a) {
        if (abs(xi) < 10^(-4)) 
            l <- length(xdat) * log(a) + sum(xdat - u)/a
        else {
            y <- (xdat - u)/a
            y <- 1 + xi * y
            if (any(y <= 0) || a <= 0) 
                l <- 10^6
            else l <- length(xdat) * log(a) + sum(log(y)) * (1/xi + 
                1)
        }
        l
    }
    for (i in 1:nint) {
        xi <- x[i]
        opt <- optim(sol, gpd.plikxi, method = "BFGS")
        sol <- opt$par
        v[i] <- opt$value
    }

par(mfrow=c(1,1),mar=c(4.5,4,2,1)) 

    plot(x, -v, type = "l", xlab = "Shape Parameter", ylab = "Profile Log-likelihood")
    ma <- -z$nllh
    abline(h = ma, lty = 1)
    abline(h = ma - 0.5 * qchisq(conf, 1), lty = 1)

 u <- par("usr")								### extract plotting coords
 yaxis <- -v; xaxis <- x;conf.line <- ma - 0.5 * qchisq(conf, 1)
 temp <- diff(yaxis-conf.line>0)			### to find where curve crosses CI line

 loc <- which(temp==-1)					### upper limit of CI
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 low.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=low.lim,lty=2);text(low.lim,u[3]-0.02*(u[4]-u[3]),round(low.lim,2),xpd=T,cex=0.75)

 loc <- which(temp==1)
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 up.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=up.lim,lty=2);text(up.lim,u[3]-0.02*(u[4]-u[3]),round(up.lim,2),xpd=T,cex=0.75)

 xi <- z$mle[2]
 abline(v=xi,lty=2);text(xi,u[3]-0.02*(u[4]-u[3]),round(xi,2),xpd=T,cex=0.75)

    invisible()
     list(low.lim=low.lim,xi=xi,up.lim=up.lim)
}

pjn.gpd.profxi <- my.gpd.profxi

my.gpd.sym.CI <- function(z, m = 100, npy = NULL, conf = 0.95){
#
# m    : return period (in years)
# npy  : number of observations per year
# conf : desired level of confidence interval

 n <- z$n                 # number of observations
 pu <- z$rate             # number of exceedances / n
 sigma <- z$mle[1]        # MLE of sigma_u
 xi <- z$mle[2]           # MLE of xi
 u <- z$threshold         # threshold
 if (is.null(npy)) {
   npy <- z$npy
 }
 m <- m*npy               # return period (in number of observations)

 xm.mle <- u+sigma*((m*pu)^xi-1)/xi # MLE of m year return level

 V <- matrix(0,ncol=3,nrow=3)                 # V matrix (page 82 of Coles (2001))
 V[1,1] <- pu*(1-pu)/n
 V[2,2] <- z$cov[1,1];  V[3,3] <- z$cov[2,2]
 V[2,3] <- z$cov[1,2];  V[3,2] <- z$cov[2,1]

 dpu <- sigma*m^xi*pu^(xi-1)
 dmu <- ((m*pu)^xi-1)/xi
 dsig <- -sigma*((m*pu)^xi-1)/xi^2 + sigma*(m*pu)^xi*log(m*pu)/xi
 delta <- matrix(c(dpu,dmu,dsig),ncol=1,nrow=3)

 ret.se <- sqrt(t(delta)%*%V%*%delta)
 zval <- qnorm((1-conf)/2,lower.tail=F)
 low.lim <- xm.mle - zval*ret.se
 up.lim <- xm.mle + zval*ret.se

 list(low.lim=low.lim,xm.mle=xm.mle,up.lim=up.lim)
}

pjn.gpd.conf.ret.levels <- my.gpd.sym.CI

my.gpd.fitrange.old <- function (data, umin, umax, nint = 10, show = FALSE, add.u=NULL,my.probs=NULL,...){
    m <- s <- up <- ul <- matrix(0, nrow = nint, ncol = 2)
    u <- seq(umin, umax, length = nint)
#
    if (is.null(my.probs)) my.probs <- c(1:9/10,0.95,0.99)
    my.q <- quantile(data,probs=my.probs)
#
    for (i in 1:nint) {
        z <- gpd.fit(data, u[i], show = show, ...)
        m[i, ] <- z$mle
        m[i, 1] <- m[i, 1] - m[i, 2] * u[i]
        d <- matrix(c(1, -u[i]), ncol = 1)
        v <- t(d) %*% z$cov %*% d
        s[i, ] <- z$se
        s[i, 1] <- sqrt(v)
        up[i, ] <- m[i, ] + 1.96 * s[i, ]
        ul[i, ] <- m[i, ] - 1.96 * s[i, ]
    }
    names <- c("Modified Scale", "Shape")
    oldpar <- par(mfrow = c(2, 1))
    for (i in 1:2) {
        um <- max(up[, i])
        ud <- min(ul[, i])
        plot(u, m[, i], ylim = c(ud, um), xlab = "Threshold", 
            ylab = names[i], type = "b")
        for (j in 1:nint) lines(c(u[j], u[j]), c(ul[j, i], up[j, i]))
        axis(3,at=my.q,label=my.probs)
        if (!is.null(add.u)){
          abline(v=add.u,lty=2)
          axis(1,at=add.u); axis(3,at=add.u,label=round(mean(data<=add.u),2))
        }
    }
    par(oldpar)
    invisible(list(thresholds = u, mle = m, se = s, ci.low = ul, 
        ci.up = up))
}

my.gpd.fitrange <- function(data, u.vec, nint, do.xi=T, my.xlab=NULL, add.nexc=F, 
                            u.ps=NULL,prof=F,mult=c(1,2),...){
    zz <- list()
    m <- s <- up <- ul <- matrix(0, nrow = nint, ncol = 2)
    u <- u.vec
    u.x <- u.vec
    if (!is.null(u.ps)) u.x <- 100*u.ps
    nexc <- unlist(lapply(u,function(x)sum(data>x))) # numbers of excesses of each threshold
    for (i in 1:nint) {
        z <- gpd.fit(data, u[i], show = FALSE)
        m[i, ] <- z$mle
        m[i, 1] <- m[i, 1] - m[i, 2] * u[i]
        d <- matrix(c(1, -u[i]), ncol = 1)
        v <- t(d) %*% z$cov %*% d
        s[i, ] <- z$se
        s[i, 1] <- sqrt(v)
        up[i, ] <- m[i, ] + 1.96 * s[i, ]
        ul[i, ] <- m[i, ] - 1.96 * s[i, ]
        if (prof){
           xlow <- m[i,2]-1.96*mult[1]*s[i,2]
           xup <- m[i,2]+1.96*mult[2]*s[i,2]
           temp <- my.gpd.profxi(z,xlow=xlow,xup=xup)
           ul[i,2] <- temp[1]
           up[i,2] <- temp[2]
         }
    }
    names <- c(expression(paste("MLE of ",sigma[u]-xi*u)),expression(paste("MLE of ",xi)))
    which.plot <- ifelse(do.xi,2,1)
    if (is.null(my.xlab)) my.xlab <- "threshold"
    for (i in which.plot) {
        um <- max(up[, i])
        ud <- min(ul[, i])
        plot(u.x, m[, i], ylim = c(ud, um), xlab = my.xlab, 
            ylab = names[i], type = "b", pch=16,...)
        for (j in 1:nint) lines(c(u.x[j], u.x[j]), c(ul[j, i], up[j, 
            i]))
        if (add.nexc) axis(3,at=u,labels=nexc,cex.axis=0.7)
    }
    zz$u <- u; zz$nexc <- nexc; zz$m <- m
    invisible(zz)
}


#============================================================================================#
#                                    my.pp.fit()                                             #
#                                                                                            #
#  Fits point process extreme value regression models.                                       #
#  Uses quantile regression to set thresholds.                                               #
#  Model parameterized in terms of mu, sigma and xi, the location, scale and shape           #
#    parameters of the GEV distribution of annual maxima, conditional on covariate values    #
#============================================================================================#

# Inputs ...
#
# xdat          : response data
# threshold     : optional input threshold
# npy           : average number of observations per year
# ydat          : matrix of covariate data (each column contains one set of covariate data)
# p.exc         : probability of exceedance (used to set threshold using quantile regression)
# mul           : indicates which columns of ydat are included in the model for mu (location)
# sigl          : similarly for sigma (scale)
# shl           : similarly for xi (shape)
# sig.propto.mu : if TRUE fit a model in which sigma is proportional to mu
# mulink        : link function relating mu to the covariates (mu = mulink(linear predictor))
# siglink       : similarly for sigma (scale)
# shlink        : similarly for xi (shape)
# init.ests     : (optional) initial estimates [vector of form (muinit,sigmainit,xiinit)]
#                 [takes precedence over muinit,siginit and xiinit.]
# muinit        : (optional) initial estimates for mu parameters
# siginit       : (optional) initial estimates for sigma parameters
# shinit        : (optional) initial estimates for xi parameters
# cluster       : cluster indicator, e.g. indicator of separate storm periods
# site          : site (spatial location) indicator
# num.hess      : if TRUE numerical derivatives are used (works for all models)
# zero.pos      : indicator parameters set to zero in null hypothesis of a likelihood ratio test
# show          : if T (true) estimated parameters etc. are printed to the screen
# method        : method used by optim() to minimize the negated log-likelihood 
#                 [default: "Nelder-Mead" (generally reliable for at least an initial fit.
#                 "BFGS" can be useful once one is close to the solution.]
# maxit         : maximum number of iterations
#                 if FALSE use algebraic derivatives (only works for models in which mu and/or sigma 
#                 are linear or log-linear in the covariates and xi is linear in the covariates).

# use.init.ests : use initial estimates from function pp.init.ests() --- experimental!?

my.pp.fit <- function (xdat, threshold=NULL, npy = 365, ydat = NULL, p.exc = NULL, 
    mul = NULL, sigl = NULL, shl = NULL, sig.propto.mu = F,
    mulink = identity, siglink = identity, shlink = identity, 
    init.ests = NULL, muinit = NULL, siginit = NULL, shinit = NULL,  ### arguments and default values ###
    cluster = NULL, site = NULL, num.hess = FALSE, zero.pos = NULL,
    show = TRUE, method = "Nelder-Mead", maxit = 10000, 
    use.init.ests = F, ...){                                         

# Set up of some basic information 

    mulink.name <- deparse(substitute(mulink))    # names of link functions for mu, sigma and xi
    siglink.name <- deparse(substitute(siglink))
    shlink.name <- deparse(substitute(shlink))

    z <- list()                                             # create a list called z in which to store results
    npmu <- length(mul) + 1                                 # number of parameters for mu
    npsc <- length(sigl) + 1;  if (sig.propto.mu) npsc <- 1 # number of parameters for sigma
    npsh <- length(shl) + 1                                 # number of parameters for xi
    tot.par <- npmu+npsc+npsh                               # total number of parameters
    n <- length(xdat)                                       # length of response vector xdat
    z$trans <- FALSE                                        # z$trans is FALSE if there are no covariates 
                                                            # (likely to be changed to TRUE later)

    if (is.function(threshold)) stop("`threshold' cannot be a function") # stop with error message if threshold is a function

# Set threshold.  If p.exc is not NULL (i.e. probability p.exc of exceedance has been supplied) and
#                 threshold is NULL (i.e. no threshold has been supplied by the user) 
#                 use quantile regression to set the threshold (i.e. call function thresh.set).

    thresh.coeffs <- NULL                                   # thresh.coeffs will be overwritten below if thresh.set() is used.
    if (!is.null(p.exc) & is.null(threshold)) {				            
      temp <- thresh.set(y=xdat,x=ydat,p.exc=p.exc,mul=mul,sigl=sigl,mulink.name=mulink.name,
                              siglink.name=siglink.name,sig.propto.mu=sig.propto.mu)
      threshold <- temp$u                                   # threshold vector
      thresh.coeffs <- temp$coeff                           # coefficients that define the threshold
      if (use.init.ests){                                   # experimental : use function pp.ini.ests to come up with
       temp <- pp.init.ests(xdat,ydat[,mul],p.exc=p.exc,rq.coeffs=thresh.coeffs,npy=npy) # initial estimates automatically.
       init.ests <-c(temp[1],thresh.coeffs[-1],temp[2:3])
      } # ... end of 2nd if statement
    } # ... end of 1st if statement

    u <- rep(threshold, length.out = n)				# (if necessary) make threshold the same length as the response xdat
    xind <- (1:n)[xdat > u]                                 # T/F indicator of threshold exceedance
    xdatu <- xdat[xind] 						# Data above threshold
    nexc <- length(xind)                                    # number of threshold exceedances
    if (length(unique(u)) > 1) z$trans <- TRUE              # if u is non-constant set z$trans to TRUE

    in2 <- sqrt(6 * var(xdat))/pi
    in1 <- mean(xdat) - in2 * (0.57722 - log(npy))          # initial estimates of (intercepts) mu_0 and sigma_0 relevant to AM

# Set up design matrices to relate mu, sigma and xi to the covariate values ...

    if (is.null(mul)) {                                            # if no covariates in mu ...
        mumat2 <- as.matrix(rep(1, length(xdat)))                  # n by 1 matrix of ones
        if (is.null(muinit)) muinit <- in1                         # initial estimate of mu
    }
    else {                                                         # if covariates in mu ...
        z$trans <- TRUE                                     
        mumat2 <- cbind(rep(1, length(xdat)), ydat[, mul])         # n by npmu matrix. Col 1: ones, other cols: covariate data
        if (is.null(muinit)) muinit <- c(in1, rep(0, length(mul))) # initial esimates: (mu_0,0,...,0)
    }
    if (is.null(sigl)) {                                           # ... and similarly for sigma ...
        sigmat2 <- as.matrix(rep(1, length(xdat)))
        if (is.null(siginit)) siginit <- in2 
        if (sig.propto.mu) siginit <- in2/in1
    }
    else {
        z$trans <- TRUE
        sigmat2 <- cbind(rep(1, length(xdat)), ydat[, sigl])
        if (is.null(siginit)) siginit <- c(in2, rep(0, length(sigl)))
    }
    if (is.null(shl)) {                                            # ... and similarly for xi ...
        shmat2 <- as.matrix(rep(1, length(xdat)))
        if (is.null(shinit)) shinit <- 0.1
    }
    else {
        z$trans <- TRUE
        shmat2 <- cbind(rep(1, length(xdat)), ydat[, shl])
        if (is.null(shinit)) shinit <- c(0.1, rep(0, length(shl)))
    }
    mumat <- as.matrix(mumat2[xind,])                              # design matrices for exceedances only
    sigmat <- as.matrix(sigmat2[xind,])
    shmat <- as.matrix(shmat2[xind,])

    init <- c(muinit, siginit, shinit)                             # collect initial estimates into one vector
    if (!is.null(init.ests)) init <- init.ests                     # overwrite these if init.ests is given
    z$model <- list(mul, sigl, shl)                                # save form of model in z
    z$link <- deparse(substitute(c(mulink, siglink, shlink)))      # save link functions in z
    z$threshold <- ifelse(z$trans,u,unique(u))                     # save threshold in z #ifelse added 8/3/2012
    z$npy <- npy                                                   # save npy in z
    z$nexc <- length(xdatu)                                        # save number of exceedances in z
    z$data <- xdatu                                                # save data above threshold in z

#--------------------- pp.lik returns the negated log-likelihood of the point process model -------------------#

    pp.lik <- function(a) {                                                # a contains the parameter values of the model
        mu2 <- mulink(mumat2 %*% (a[1:npmu]))                              # fitted value of mu for each data point
        sc2 <- siglink(sigmat2 %*% (a[seq(npmu + 1, length = npsc)]))      # fitted value of sigma for each data point
        if (sig.propto.mu) sc2 <- a[npmu+1]*mu2                            # if sigma=c mu ...
        xi2 <- shlink(shmat2 %*% (a[seq(npmu + npsc + 1, length = npsh)])) # fitted value of xi for each data point
        mu <- mulink(mumat %*% (a[1:npmu]))                                # fitted value of mu for threshold exceedances only
        sc <- siglink(sigmat %*% (a[seq(npmu + 1, length = npsc)]))        # fitted value of sigma for threshold exceedances only
        if (sig.propto.mu) sc <- a[npmu+1]*mu                              # if sigma=c mu ...
        xi <- shlink(shmat %*% (a[seq(npmu + npsc + 1, length = npsh)]))   # fitted value of xi for threshold exceedances only
        if (any(sc <= 0)) return(10^6)                                     # return something big if any sigmas are non-positive

        if (min(1 + ((xi2 * (u - mu2))/sc2)) < 0) {                        # return something big if any of the data points
            l <- 10^6                                                      # violate this constaint on the parameters
        }                                                                  
        else {                                                             
            y <- (xdatu - mu)/sc                                           # calculate 1+xi(xdatu-mu)/sigma
            y <- 1 + xi * y                                                # (for threshold exceedances only)
            if (min(y) <= 0)                                               # return something big if any of the
                l <- 10^6                                                  # new y values are non-positive
            else l <- sum(log(sc)) + sum(log(y) * (1/xi + 1)) +            # if all OK calculate the negated log-likelihood
                n/npy * mean((1 + (xi2 * (u - mu2))/sc2)^(-1/xi2))
        }
        l                                                                  # return the negated log-likelihood
    } # ... end of function pp.lik

#--- pp.grad returns the first derivatives (the `scores') of the (negated) log-likelihood of the point process model -----#

    pp.grad <- function(a,grad=TRUE) {        # grad controls the form of the output (see end of function)
        mu2 <- mulink(mumat2 %*% (a[1:npmu]))
        sc2 <- siglink(sigmat2 %*% (a[seq(npmu + 1, length = npsc)]))
        if (sig.propto.mu) sc2 <- a[npmu+1]*mu2
        xi2 <- shlink(shmat2 %*% (a[seq(npmu + npsc + 1, length = npsh)]))
        mu <- mulink(mumat %*% (a[1:npmu]))
        sc <- siglink(sigmat %*% (a[seq(npmu + 1, length = npsc)]))
        if (sig.propto.mu) sc <- a[npmu+1]*mu
        xi <- shlink(shmat %*% (a[seq(npmu + npsc + 1, length = npsh)]))
        if (any(sc <= 0)) return(Inf)
        yy <- (xdatu - mu)/sc
        y <- 1 + xi * yy     

        yy2 <- (xdat - mu2)/sc2			### all observations
        y2 <- 1 + xi2 * yy2
        yy2[-xind] <- 1                         ### set to 1 for non-exceedances
        y2[-xind] <- 1                          ### won't be used, but avoids 
                                                ### problems with missings
        yyu <- (u-mu2)/sc2   
        yu <- 1 + xi2 * yyu 
        if (any(yu < 0)) return(Inf)
	  m <- 1/npy

	  II <- numeric(length(u)); II[xind] <- 1	### indicator of exceedance

	  dmu <- II*(xi2+1)/(y2*sc2)-m*yu^(-1-1/xi2)/sc2
	  dmu <- matrix(dmu,nrow=nrow(mumat2),ncol=ncol(mumat2),byrow=F)
        dmu <- mumat2*dmu
        if (mulink.name=="exp"){                                     # i.e. if ln(mu) is linear in covariates
         dmu <- dmu*matrix(mu2,nrow=nrow(dmu),ncol=ncol(dmu),byrow=F)
        }
	  dsc <- -II/sc2+II*(xi2+1)*(yy2/y2)/sc2-m*yu^(-1-1/xi2)*yyu/sc2
	  dsc <- matrix(dsc,nrow=nrow(sigmat2),ncol=ncol(sigmat2),byrow=F)
        dsc <- sigmat2*dsc
        if (sig.propto.mu) dsc <- mu2*dsc
        if (siglink.name=="exp"){                                    # i.e. if ln(sigma) is linear in covariates
         dsc <- dsc*matrix(sc2,nrow=nrow(dsc),ncol=ncol(dsc),byrow=F)
        }
	  dxi <- II*log(y2)/(xi2^2)-II*(1+1/xi2)*yy2/y2-m*yu^(-1/xi2)*(log(yu)/(xi2^2)-yyu/(yu*xi2))
	  dxi <- matrix(dxi,nrow=nrow(shmat2),ncol=ncol(shmat2),byrow=F)
        dxi <- shmat2*dxi
        temp <- cbind(dmu,dsc,dxi)            # temp contains the derivatives of the (non-negated) log-likelihood
        if (!grad) return(temp)               # n by tot.par matrix of contributions to score
        if (grad) return(-apply(temp,2,sum))  # vector (of length tot.par) of (negated) scores 
                                              # [ apply(x,2,sum) sums over the columns of matrix x ]
    } #... end of function pp.grad()                                                                  

#--------- pp.hess returns the first derivative of the negated log-likelihood of the point process model -------------------#

    pp.hess <- function(a) {                  # Hessian of negated log-likelihood
        mu2 <- mulink(mumat2 %*% (a[1:npmu]))
        sc2 <- siglink(sigmat2 %*% (a[seq(npmu + 1, length = npsc)]))
        if (sig.propto.mu) sc2 <- a[npmu+1]*mu2
        xi2 <- shlink(shmat2 %*% (a[seq(npmu + npsc + 1, length = npsh)]))
        mu <- mulink(mumat %*% (a[1:npmu]))
        sc <- siglink(sigmat %*% (a[seq(npmu + 1, length = npsc)]))
        if (sig.propto.mu) sc <- a[npmu+1]*mu
        xi <- shlink(shmat %*% (a[seq(npmu + npsc + 1, length = npsh)]))
        if (any(sc <= 0)) return(Inf)
        yy <- (xdatu - mu)/sc
        y <- 1 + xi * yy     

        yy2 <- (xdat - mu2)/sc2			### all observations
        y2 <- 1 + xi2 * yy2
        yy2[-xind] <- 1                         ### set to 1 for non-exceedances
        y2[-xind] <- 1                          ### won't be used, but avoids 
                                                ### problems with missings

        yyu <- (u-mu2)/sc2   
        yu <- 1 + xi2 * yyu 
        if (any(yu < 0)) return(Inf)
	  m <- 1/npy

	  II <- numeric(length(u)); II[xind] <- 1	### indictor of exceedance

        d2g.dmu2 <- (1+xi2)*yu^(-2-1/xi2)/sc2^2
        d2g.dmu.dsc <- -yu^(-1-1/xi2)/sc2^2+(1+xi2)*yyu*yu^(-2-1/xi2)/sc2^2
        d2g.dsc2 <- -2*yyu*yu^(-1-1/xi2)/sc2^2+yyu^2*(1+xi2)*yu^(-2-1/xi2)/sc2^2
        d2g.dmu.dxi <- yu^(-1-1/xi2)*(log(yu)/xi2^2-(1+1/xi2)*yyu/yu)/sc2
        d2g.dsc.dxi <- (yyu*yu^(-1-1/xi2)/sc2)*(log(yu)/xi2^2-(1+1/xi2)*yyu/yu)

        g.xi <- yu^(-1/xi2)
        k.xi <- log(yu)/xi2^2-yyu/yu/xi2
        dg.xi <- g.xi*k.xi
        dk.xi <- -2*log(yu)/xi2^3+2*yyu/yu/xi2^2+yyu^2/yu^2/xi2
        d2g.dxi2 <- g.xi*dk.xi+dg.xi*k.xi

        d2h.dmu2 <- -xi2*(1+xi2)/y2^2/sc2^2
        d2h.dmu.dsc <- (1+xi2)/y2^2/sc2^2
        d2h.dsc2 <- -1/sc2^2+(1+xi2)*yy2/y2/sc2^2+(1+xi2)*yy2/y2^2/sc2^2
        d2h.dmu.dxi <- -1/y2/sc2+(1+xi2)*yy2/y2^2/sc2
        d2h.dsc.dxi <- -yy2*(1-yy2)/y2^2/sc2
        d2h.dxi2 <- 2*log(y2)/xi2^3-2*yy2/y2/xi2^2-(1+1/xi2)*yy2^2/y2^2

        d2f.dmu2 <- m*d2g.dmu2+II*d2h.dmu2
        d2f.dmu.dsc <- m*d2g.dmu.dsc+II*d2h.dmu.dsc
        d2f.dmu.dxi <- m*d2g.dmu.dxi+II*d2h.dmu.dxi
        d2f.dsc2 <- m*d2g.dsc2+II*d2h.dsc2
        d2f.dsc.dxi <- m*d2g.dsc.dxi+II*d2h.dsc.dxi
        d2f.dxi2 <- m*d2g.dxi2+II*d2h.dxi2

        if (sig.propto.mu){
	    df.dsc <- -II/sc2+II*(xi2+1)*(yy2/y2)/sc2-m*yu^(-1-1/xi2)*yyu/sc2
          d2f.dsc2 <- mu2^2*d2f.dsc2
          d2f.dmu.dsc <- df.dsc + mu2*d2f.dmu.dsc
          d2f.dsc.dxi <- mu2*d2f.dsc.dxi
        }

        if (max(npmu,npsc,npsh)==1){                        # If no covariates ...
          return(matrix(c(sum(d2f.dmu2),sum(d2f.dmu.dsc),sum(d2f.dmu.dxi),
                        sum(d2f.dmu.dsc),sum(d2f.dsc2),sum(d2f.dsc.dxi),
                        sum(d2f.dmu.dxi),sum(d2f.dsc.dxi),sum(d2f.dxi2)),ncol=3,nrow=3))
        }

        if (mulink.name=="identity" & siglink.name=="identity"){ # if mu and sigma are linear in covariates
         dfs <- array(c(
          rep( c(rep(d2f.dmu2,npmu),rep(d2f.dmu.dsc,npsc),rep(d2f.dmu.dxi,npsh)) , npmu),
          rep( c(rep(d2f.dmu.dsc,npmu),rep(d2f.dsc2,npsc),rep(d2f.dsc.dxi,npsh)) , npsc),
          rep( c(rep(d2f.dmu.dxi,npmu),rep(d2f.dsc.dxi,npsc),rep(d2f.dxi2,npsh)) , npsh)
                ),dim=c(n,tot.par,tot.par))
         dfs <- aperm(dfs,c(3,2,1))
         temp2 <- array(rep(c(mumat2,sigmat2,shmat2),tot.par),dim=c(n,tot.par,tot.par))
         temp2 <- aperm(temp2,c(3,2,1))
         temp3 <- aperm(temp2,c(2,1,3))
         return(apply(dfs*(temp2*temp3),c(1,2),sum))
        }

        if (mulink.name=="exp" & siglink.name=="identity"){      # if log(mu) and sigma are linear in covariates
 	   df.dmu <- II*(xi2+1)/(y2*sc2)-m*yu^(-1-1/xi2)/sc2
         dfs <- array(c(
          rep( c(rep(mu2*df.dmu+mu^2*d2f.dmu2,npmu),rep(mu2*d2f.dmu.dsc,npsc),rep(mu2*d2f.dmu.dxi,npsh)) , npmu),
          rep( c(rep(mu2*d2f.dmu.dsc,npmu),rep(d2f.dsc2,npsc),rep(d2f.dsc.dxi,npsh)) , npsc),
          rep( c(rep(mu2*d2f.dmu.dxi,npmu),rep(d2f.dsc.dxi,npsc),rep(d2f.dxi2,npsh)) , npsh)
                ),dim=c(n,tot.par,tot.par))
         dfs <- aperm(dfs,c(3,2,1))
         temp2 <- array(rep(c(mumat2,sigmat2,shmat2),tot.par),dim=c(n,tot.par,tot.par))
         temp2 <- aperm(temp2,c(3,2,1))
         temp3 <- aperm(temp2,c(2,1,3))
         return(apply(dfs*(temp2*temp3),c(1,2),sum))
        }

        if (mulink.name=="identity" & siglink.name=="exp"){      # if mu and log(sigma) are linear in covariates
         df.dsc <- -II/sc2+II*(xi2+1)*(yy2/y2)/sc2-m*yu^(-1-1/xi2)*yyu/sc2
         dfs <- array(c(
          rep( c(rep(d2f.dmu2,npmu),rep(sc2*d2f.dmu.dsc,npsc),rep(d2f.dmu.dxi,npsh)) , npmu),
          rep( c(rep(sc2*d2f.dmu.dsc,npmu),rep(sc2*df.dsc+sc2^2*d2f.dsc2,npsc),rep(sc2*d2f.dsc.dxi,npsh)) , npsc),
          rep( c(rep(d2f.dmu.dxi,npmu),rep(sc2*d2f.dsc.dxi,npsc),rep(d2f.dxi2,npsh)) , npsh)
                ),dim=c(n,tot.par,tot.par))
         dfs <- aperm(dfs,c(3,2,1))
         temp2 <- array(rep(c(mumat2,sigmat2,shmat2),tot.par),dim=c(n,tot.par,tot.par))
         temp2 <- aperm(temp2,c(3,2,1))
         temp3 <- aperm(temp2,c(2,1,3))
         return(apply(dfs*(temp2*temp3),c(1,2),sum))
        }

        if (mulink.name=="exp" & siglink.name=="exp"){           # if log(mu) and log(sigma) are linear in covariates
 	   df.dmu <- II*(xi2+1)/(y2*sc2)-m*yu^(-1-1/xi2)/sc2
         df.dsc <- -II/sc2+II*(xi2+1)*(yy2/y2)/sc2-m*yu^(-1-1/xi2)*yyu/sc2
         dfs <- array(c(
          rep( c(rep(mu2*df.dmu+mu^2*d2f.dmu2,npmu),rep(mu2*sc2*d2f.dmu.dsc,npsc),rep(mu2*d2f.dmu.dxi,npsh)) , npmu),
          rep( c(rep(mu2*sc*d2f.dmu.dsc,npmu),rep(sc2*df.dsc+sc2^2*d2f.dsc2,npsc),rep(sc2*d2f.dsc.dxi,npsh)) , npsc),
          rep( c(rep(mu2*d2f.dmu.dxi,npmu),rep(sc2*d2f.dsc.dxi,npsc),rep(d2f.dxi2,npsh)) , npsh)
                ),dim=c(n,tot.par,tot.par))
         dfs <- aperm(dfs,c(3,2,1))
         temp2 <- array(rep(c(mumat2,sigmat2,shmat2),tot.par),dim=c(n,tot.par,tot.par))
         temp2 <- aperm(temp2,c(3,2,1))
         temp3 <- aperm(temp2,c(2,1,3))
         return(apply(dfs*(temp2*temp3),c(1,2),sum))
        }
    } # ... and of function pp.hess()

#------------------------------------- Minimization of the negated log-likelihood ------------------------------------------#

# Now we minimize the negated log-likelihood (calculated by pp.lik) with respect to the parameters of the model.  

# There are two options for calculating derivatives and Hessians:
# 
# 1. num.hess=T : using finite differences within the optimizer function optim().
#
# 2. num.hess=F : using function pp.grad() and pp.hess().  Only available for certain models (see conditions below)

    mulink.condition <- mulink.name=="identity" | mulink.name=="exp"     # conditions on link functions for pp.grad()
    siglink.condition <- siglink.name=="identity" | siglink.name=="exp"  # and pp.hess() to work.
    shlink.condition <- shlink.name=="identity"

    fscale <- abs(pp.lik(init))                                          # value by which to scale the objective function.

# We use optim() to minimize the negated log-lilelihood ...
#
# init      : vector of initial estimates
# pp.lik    : function to be minimized
# gr        : function returning the gradient of pp.lik
# method    : Nelder-Mead or BFGS
# control   : additional control parameters for optim

    if (mulink.condition & siglink.condition & mulink.condition & !num.hess){ # only try algebraic derivatives for certain models
      x <- optim(init, pp.lik, gr=pp.grad, hessian = FALSE, method = method, control = list(maxit = maxit,fnscale = fscale,...))
# hessian=FALSE : don't bother estimating Hessian once the solution is found (because we will calculate this using pp.hess())
      x$hessian <- pp.hess(x$par)      # algebraic Hessian
    }
    else{
      x <- optim(init, pp.lik, hessian = TRUE, method = method, control = list(maxit = maxit,fnscale = fscale,...))
# hessian=TRUE : Hessian will be returned in x$hessian
    }

#------------------------------------- Saving results of model fitting ------------------------------------------#

    mu2 <- mulink(mumat2 %*% (x$par[1:npmu]))                              # fitted values of mu, sigma, xi
    sc2 <- siglink(sigmat2 %*% (x$par[seq(npmu + 1, length = npsc)]))      # at each data point.
    xi2 <- shlink(shmat2 %*% (x$par[seq(npmu + npsc + 1, length = npsh)]))

    z$conv <- x$convergence                     # measure of quality of convergence
    z$nllh <- x$value                           # minimized value of negated log-likelihood
    z$vals <- cbind(mu2, sc2, xi2, u)           # n by 4 matrix with columns mu2,sc2,xi2,u

# The following line of code calculates, for each data point, the fitted values of
# (a) the probability of exceedance of the threshold u
# (b) the scale (sigma_u) and shape of the generalized Pareto (GP) distribution of the
#     amounts by which responses will exceed u
# apply(x,1,ppp,npy) applies function ppp() to each row of x and passes the value npy
# The function ppp() calculates (a) and (b) above.

    z$gpd <- apply(z$vals, 1, pjn.ppp, npy)         # z$gpd is a 3 by n matrix.  
                                                # row 1: values of prob of exceedance
                                                # row 2: values of scale parameter of GP
                                                # row 3: values of shape parameter of GP

    if (z$trans) {  # if covariates are involved, replace previous z$data by residuals
        z$data <- as.vector((1 + (xi2[xind] * (xdatu - u[xind]))/z$gpd[2,xind])^(-1/xi2[xind]))
    }                              
    z$mle <- x$par                              # vector of MLEs
    NA.matrix <- matrix(NA,tot.par,tot.par)     # matrix of NAs (missing value code)
    if (z$mle[tot.par]>-1) z$cov <- solve(x$hessian) # if estimated shape parameter > -1 only.
    else z$cov <- NA.matrix                     # estimated variance-covariance matrix of MLEs
                                                # [Don't even try to invert Hessian if xihat <= -1.]
    z$se <- sqrt(diag(z$cov))                   # calculate standard errors

    if (show) {                                 # if show=T print some results to screen
        if (z$trans) 
            print(z[c(2, 3)])
        if (length(z[[4]]) == 1) 
            print(z[4])
        print(z[c(5, 6, 8)])
        if (!z$conv) 
            print(z[c(9, 12, 14)])
    } # ... end of `show' if statement

    if (z$mle[tot.par]>-1) z$corr <- cov2cor(z$cov) # estimated correlation matrix of MLEs
    else z$corr <- NA.matrix   

    z$thresh.coeffs <- thresh.coeffs                # save the threshold coefficients

# Save various other things in list z

    z$n <- n                                    
    z$xdatu <- xdatu                            
    z$mumat <- mumat; z$mumat2 <- mumat2; z$sigmat <- sigmat; z$sigmat2 <- sigmat2
    z$shmat <- shmat; z$shmat2 <- shmat2
    z$npmu <- npmu; z$npsc <- npsc; z$npsh <- npsh
    z$mulink <- mulink; z$siglink <- siglink; z$shlink <- shlink
    if (!is.null(cluster)) z$clus.exc <- cluster[xind]   ### cluster number of threshold exceedances
    if (!is.null(site)) z$site.exc <- site[xind]         ### site number of threshold exceedances
    z$ydat <- ydat[xind,]                                ### response values of threshold exceedances

    class(z) <- "pp.fit"
    invisible(z)

    if (is.null(cluster)) return(invisible(z)) # If cluster is NULL then standard errors don't need adjusting.  
                                               # Return results in list z as they are.

#------------------------------------- Adjustment of standard errors for cluster dependence -------------------------------#

    scores <- pp.grad(z$mle,grad=FALSE)                           # n by tot.par matrix of score values for each data point
    scores <- as.matrix(aggregate(scores,list(cluster),sum)[,-1]) # ... summed within different clusters

    invH_theta <- z$cov      		        # estimate of H_I^{-1} 
    H_IND <- x$hessian   			  # estimate of Hessian H_I of independence log-likelihood
    new.scores <- scores%*%invH_theta       # transform scores using H_I^{-1} (rejoinder of Northrop and Jonathan (2011))
    Var_theta <- t(new.scores)%*%new.scores # adjusted variance-covariates matrix of the parameter estimates
    H_ADJ <- solve(Var_theta)               # estimate of Hessian H_A of adjusted log-likelihood

    z$HI <- H_IND					  # store parameters of adjustment for theta, H_IND
    z$HA <- H_ADJ                           # ... and H_ADJ
    z$cov.adj <- Var_theta			  # store adjusted V-C matrix for theta
    z$se.adj <- sqrt(diag(z$cov.adj))	  # store adjusted s.e.s for theta
    z$corr.adj <- cov2cor(z$cov.adj)        # store adjusted correlation matrix

if (show) print(z$se.adj)

#--- pp.lik.adj() returns the negated vertically adjusted log-likelihood of the point process model at parameter value a ---#

    pp.lik.adj <- function(a){		  
        theta <- a                          # vector theta contains values of the parameters of the model
        thetahat <- z$mle                   # thetahat contains the MLEs of the parameters
        H_ADJ <- z$HA                       # extract H_ADJ
        H_IND <- z$HI                       # ... and H_IND
        snum <- t(theta-thetahat)%*%H_ADJ%*%(theta-thetahat) # numerator and denominator of log-likelihood adjustment 
        sden <- t(theta-thetahat)%*%H_IND%*%(theta-thetahat) # factor on page 4 of Northrop and Jonathan (2011)
        S <- ifelse(sum((theta-thetahat)^2)!=0,snum/sden,0)  # if theta=thetahat set S=0 (no adjustment)
	  l_INDtheta <- -pp.lik(theta)                         # independence log-likelihood at theta
 	  l_INDthetahat <- -z$nllh                             # independence log-likelihood at MLE thetahat
	  l_ADJ <- l_INDthetahat+S*(l_INDtheta-l_INDthetahat)  # vertically adjusted log-likelihood
	  -l_ADJ                                               # negated vertically adjusted log-likelihood
    }

#------- pp.lik.adj.zero.pos() returns the negated vertically adjusted log-likelihood of the point process model ---------#
#------------------ evaluated with elements positions zero.pos in parameter vector a set to zero -------------------------#

    pp.lik.adj.zero.pos <- function(a,zero.pos){ 
	  n.par <- length(a)+length(zero.pos)                  # number of parameters
	  theta <- numeric(n.par)                              # set theta to zeros initially
	  theta[(1:n.par)[-zero.pos]] <- a                     # set non-zero elements to those in a
        thetahat <- z$mle                                    # thetahat contains the MLEs of the parameters
        H_ADJ <- z$HA                                        # extract H_ADJ
        H_IND <- z$HI                                        # ... and H_IND
        snum <- t(theta-thetahat)%*%H_ADJ%*%(theta-thetahat) # numerator and denominator of log-likelihood adjustment 
        sden <- t(theta-thetahat)%*%H_IND%*%(theta-thetahat) # factor on page 4 of Northrop and Jonathan (2011)
        S <- ifelse(sum((theta-thetahat)^2)!=0,snum/sden,0)  # if theta=thetahat set S=0 (no adjustment)
	  l_INDtheta <- -pp.lik(theta)                         # independence log-likelihood at theta
 	  l_INDthetahat <- -z$nllh                             # independence log-likelihood at MLE thetahat
	  l_ADJ <- l_INDthetahat+S*(l_INDtheta-l_INDthetahat)  # vertically adjusted log-likelihood
	  -l_ADJ                                               # negated vertically adjusted log-likelihood
    }

#-----------------------------------------------------------------------------------#
#  log-likelihood ratio test comparing full model with model with parameters in     #
#  positions given in zero.pos set to zero.                                         #
#-----------------------------------------------------------------------------------#

    if (!is.null(zero.pos)){	                               # only do test is argument zero.pos is given to my.pp.fit()
      init <- z$mle[-zero.pos]                               # initial estimates based on MLEs of full model
      x <- optim(init, pp.lik.adj.zero.pos, zero.pos=zero.pos, hessian = FALSE, 
         method = method, control = list(maxit = maxit,...)) # minimize l_ADJ subject to a[zero.pos]=0 (reduced model).
      z$nllh.reduced <- x$value                              # minimized value of l_ADJ under reduced model
      z$alrts <- -2*(z$nllh-z$nllh.reduced)                  # adjusted likelihood ratio test statistic
      z$df <- length(zero.pos)                               # degrees of freedom = number of parameters set to zero
      z$p.value <- 1-pchisq(z$alrts,df=z$df)                 # p-value = P(Chi^2_df > alrts)
      print(c(z$alrts,z$df,z$p.value))                       # print (test stat, df, p-value) to screen
    }

    invisible(z)                                             # return list z
}

pjn.pp.fit <- my.pp.fit

#-----------------------------------------------------------------------------------------------------------#

#============================================================================================#
#                                    thresh.set()                                            #
#                                                                                            #
#  Sets a threshold based on the form of the point process extreme value model being fitted. #
#  At the moment the code does not permit covariates in the shape parameter xi.              #
#  Covariates are allowed in the location (mu) and/or scale (sigma).                         #
#  There are various separate cases.                                                         #
#  The simplest case is where mu and/or sigma are linear in the covariates when linear       #
#    quantile regression is used.  This works quickly and without problems.                  #
#  The code also supports log(mu) and/or log(sigma) linear in the covariates.  Non-linear    #
#    quantile regression is used here.  This is much less straightforward.  It can be slow   #
#    and the results can depend on the initial estimates.                                    #
#  The case where sigma is proportional to mu (sig.propto.mu=T) is also possible.            #
#============================================================================================#

# Inputs ...
#
# y     : vector of response data
# x     : matrix of covariate data
# p.exc : probability of exceedance
# mul           : indicates which columns of x are included in the model for mu (location)
# sigl          : similarly for sigma (scale)
# mulink        : link function relating mu to the covariates (mu = mulink(linear predictor))
# mulink.name   : name of mulink in character format
# siglink       : similarly for sigma (scale)
# siglink.name  : name of siglink in character format
# sig.propto.mu : if TRUE fit a model in which sigma is proportional to mu
# small.value   : if any y values are below small.value we will avoid calculating log(y)
# nlrq.fn       : for case log(mu) linear in covariates and sigma = c mu use function nlrq() if
#                 nlrq.fn=T and use ad hoc minimization in nlrq.fn=F.

thresh.set <- function(y,x=NULL,p.exc=0.1,mul=NULL,sigl=NULL,mulink=identity,mulink.name=NULL,
   siglink=identity,siglink.name=NULL,sig.propto.mu=F,small.value=1e-6,nlrq.fn=F){

# Preliminaries ...

 n <- length(y)                                           # sample size
 tau <- 1-p.exc                                           # non-exceedance probability
 u.m <- as.numeric(quantile(y,probs=tau,na.rm=T,type=8))  # marginal sample tau-quantile
 if (sig.propto.mu) sigl <- mul                           # if sigma = c mu, mu and sigma covs are the same

#----- function my.dev() to calculate the deviance function of a quantile regression fit -----#
#----- Useful for calculating the deviance function when I don't use the standard ------------#
#----- rq() or nlrq() functions from the library quantreg.------------------------------------#

 my.dev <- function(x,tau) sum(x * (tau - (x < 0)))       # x contains residuals (y-fitted)

# Now treat each of the cases separately ...

#------------------------------------ no covariates ------------------------------------------#

 if (is.null(x) | (is.null(mul) & is.null(sigl))){           # if no covariates use quantile()
  dev <- my.dev(y-u.m,tau)                                   # u.m already calculated above
  return(list(u=rep(u.m, length.out = n),coeff=u.m,dev=dev)) # return threshold, coefficient and deviance as a list
 }

#---------------------------------------------------------------------------------------------#

# Set up covariate information

 if (is.null(mulink.name))  mulink.name <- deparse(substitute(mulink))   # name of (inverse) mu link
 if (is.null(siglink.name)) siglink.name <- deparse(substitute(siglink)) # name of (inverse) sigma link
 covs <- union(mul,sigl)                                                 # union of mu and sigma covariates

#--------------------------- mu and/or sigma linear in covariates ----------------------------#
#-------- (also includes the case where mu is linear in covariates and sigma = c mu) ---------#
 
 cond.1 <- mulink.name=="identity" & siglink.name=="identity" # mu and sigma linear in covariates
 cond.2 <- mulink.name=="identity" & is.null(sigl)            # mu linear in covariates and sigma constant
 cond.3 <- is.null(mul) & siglink.name=="identity"            # mu constant and sigma linear in covariates
 cond.4 <- mulink.name=="identity" & sig.propto.mu            # mu linear in covariates and sigma propnal to mu
 if (cond.1 | cond.2 | cond.3 | cond.4){
  rq.ft <- rq(y~x[,covs],tau=tau)                             # linear quantile regression
  u <- fitted(rq.ft)                                          # threshold
  dev <- rq.ft$rho                                            # deviance
  coeff <- as.numeric(coef(rq.ft))                            # coefficients
  return(list(u=u,coeff=coeff,dev=dev))                       # return threshold, coefficient and deviance as a list
 }

#---------------- log(mu) or log(sigma) linear in covariates (but not both) -----------------#

 cond.1 <- (mulink.name=="exp" & siglink.name=="identity" & !sig.propto.mu)
 cond.2 <- (mulink.name=="identity" & siglink.name=="exp" & !sig.propto.mu)
 if (cond.1 | cond.2){

 if (mulink.name=="exp") xdash <- as.matrix(x[,mul])   # cond.1
 if (siglink.name=="exp") xdash <- as.matrix(x[,sigl]) # cond.2

   ob.fn <- function(b,finished=F){
#   ydash <- y-(exp(b[1]+xdash%*%b[-1])-exp(b[1])) # old
   p2 <- b[1]
   p3 <- b[2]
   if (p3<=0) return(Inf)
   ydash <- y-p2^2*(exp(p3*xdash/p2)-1)/p3         # new
   if (cond.1){
    if (is.null(sigl)) rq.ft <- rq(ydash~1,tau=tau)
    if (!is.null(sigl)) rq.ft <- rq(ydash~x[,sigl],tau=tau)
   }
   if (cond.2){
    if (is.null(mul)) rq.ft <- rq(ydash~1,tau=tau)
    if (!is.null(mul)) rq.ft <- rq(ydash~x[,mul],tau=tau)
   }

   if (finished){
    u <- y-ydash+fitted(rq.ft)
    dev <- rq.ft$rho
    coeff <- as.numeric(c(coef(rq.ft),b)) # old
    p2 <- b[1]
    p3 <- b[2]
    beta <- p3/p2
    alpha <- log(p2^2/p3)
    coeff <- c(as.numeric(coef(rq.ft)),alpha,beta)# new
    return(list(u=u,coeff=coeff,dev=dev))
   }
print(rq.ft$rho)
   rq.ft$rho
  }
#  init <- rep(0,ncol(x)+1) # old
  init <- rep(1,ncol(xdash)+1) # new
#   x2 <- x^2
#   init <- coef(rq(y~x+x2,tau=tau))
#print(init)
#   init <- init[-1]
  fscale <- abs(ob.fn(init))
  z <- optim(init,ob.fn,hessian=FALSE,control=list(fnscale = fscale))
  bhat <- z$par
  return(ob.fn(bhat,finished=T))                # threshold
 }

#--------------------- both log(mu) or log(sigma) linear in covariates ----------------------#

 if (mulink.name=="exp" & siglink.name=="exp" & !sig.propto.mu){

  if (!is.null(mul)) x.mu <- as.matrix(x[,mul])
  if (!is.null(sigl)) x.sigma <- as.matrix(x[,sigl])
  ob.fn <- function(b,finished=F){
   ydash <- y-(exp(b[1]+x%*%b[-1])-exp(b[1]))
   start.vals <- list(a=u.m,c=0,d=1)
   if (!is.null(sigl)) rq.ft <- nlrq(ydash~a+exp(c+x.sigma%*%d)-exp(c),tau=tau,start=start.vals)
   if (finished) return(y-ydash+rq.ft$fitted.values)
   rq.ft$rho
  }
  init <- c(log(u.m),rep(0,ncol(x.mu)+1))
  z <- optim(init,ob.fn)
  bhat <- z$par
  u <- ob.fn(bhat,finished=T)                # threshold
 }

#---------------------- log(mu) linear in covariates and sigma = c mu ------------------------#

 if (mulink.name=="exp" & sig.propto.mu){
  x <- x[,mul]
  if (!nlrq.fn){                                        # using rq() after linearisation
   if (min(y)==0) y[y==0] <- small.value
   rq.ft <- rq(log(y)~x,tau=tau)
   u <- exp(fitted(rq.ft))
   dev <- my.dev(y-u,tau)
   coeff <- coef(rq.ft)
  }
  if (nlrq.fn){                                         # using nlrq()
   start.vals <- list(a=log(u.m),b=rep(0,ncol(x)))
   rq.ft <- nlrq(rain~exp(a+x%*%b),tau=tau,start=start.vals)
   u <- fitted(rq.ft)
   dev <- deviance(rq.ft)
   coeff <- coef(rq.ft)
  }
 }

#---------------------------------------------------------------------------------------------#

 list(u=u,coeff=coeff,dev=dev)
}

#============================================================================================#
#                                      ppp()                                                 #
#                                                                                            #
#  Inputs : a   - vector of length 4 containing the values of mu,sigma,xi and u              #
#         : npy - average number of observations per year                                    #
#============================================================================================#

pjn.ppp <- function (a, npy){ 
    u <- a[4]                                                    # threshold
    la <- 1 - exp(-(1 + (a[3] * (u - a[1]))/a[2])^(-1/a[3])/npy) # probability of exceedance
    sc <- a[2] + a[3] * (u - a[1])                               # sigma_u
    xi <- a[3]                                                   # xi
    c(la, sc, xi)
}

#-----------------------------------------------------------------------------------------------------------#

#============================================================================================#
#                                    pp.init.ests()                                          #
#                                                                                            #
#  Experimental function to calculate good initial estimates.                                #
#  Works pretty well for simple models in which mu is linear in covariates.                  #
#  Otherwise, or vene then, it is probably better to base initial estimates on the fit of a  #
#  nested simpler model.                                                                     #
#============================================================================================#

pp.init.ests <- function(x,y,p.exc,threshold,rq.coeffs,npy=365){
# x         : response data
# y         : covariate matrix
# p.exc     : probability of exceedance
# threshold : threshold
# rq.coeffs : quantile regression parameters
# npy       : number of observations per year
 y <- cbind(rep(1,nrow(y)),y)        # add column of ones to y
 t <- y%*%rq.coeffs                  # threshold 
 u <- rq.coeffs[1]                   # constant threshold for gev.dat
 stat.dat <- x-t+u                   # create approximately stationary data
 temp <- gpd.fit(stat.dat,u,show=F)  # fit GP distn to exceedances over u
 sig.u <- temp$mle[1]                # estimate of sigma_u
 xi <- temp$mle[2]                   # estimate of xi
 sig <- sig.u/((p.exc*npy)^(-xi))    # convert sigma_u to sigma
 mu <- u-(sig.u-sig)/xi              # estimate of mu
 c(mu,sig,xi)                        # return (mu,sigma,xi)
}

#-----------------------------------------------------------------------------------------------------------#

my.pp.fitrange <- function(data,umin=NULL,umax=NULL,pmin=NULL,pmax=NULL,npy=365,nint=10,show=FALSE,
                  super=NULL,cluster=NULL){
    zz <- list()
    conv <- NULL
    m <- s <- up <- ul <- matrix(0, nrow = nint, ncol = 3)
    if (!is.null(pmin)){
      p <- seq(from=pmin,to=pmax,length=nint)    
      qs <- as.numeric(quantile(data,probs=p,na.rm=T,type=8))
      x.scale <- "p"
    }
    if (!is.null(umin)){
      qs <- seq(from=umin,to=umax,length=nint)
      p <- ecdf(data)(qs)
      x.scale <- "u"
    }

    for (i in 1:nint) {
print(c(i,nint))
      if (x.scale=="p"){
	  if (is.null(cluster)) z <- my.pp.fit(data, p.exc=1-p[i], npy=npy, show = show)
        if (!is.null(cluster)) z <- my.pp.fit(data, p.exc=1-p[i], npy=npy, show = show,cluster=cluster)
      }
      if (x.scale=="u"){
	  if (is.null(cluster)) z <- my.pp.fit(data, threshold=qs[i], npy=npy, show = show)
        if (!is.null(cluster)) z <- my.pp.fit(data, threshold=qs[i], npy=npy, show = show,cluster=cluster)
      }
        m[i, ] <- z$mle
        conv[i] <- z$conv
        if (is.null(cluster)) s[i, ] <- z$se
	  if (!is.null(cluster)) s[i, ] <- z$se.adj
        up[i, ] <- z$mle + 1.96 * s[i,]
        ul[i, ] <- z$mle - 1.96 * s[i,]
    }
    names <- c("location", "scale", "shape")
    oldpar <- par(mfrow = c(3,1),oma=c(0,0,0,0),mar=c(4,4,2,2)+0.1)
    for (i in 1:3) {
      um <- max(up[, i],super[i],na.rm=T)
      ud <- min(ul[, i],super[i],na.rm=T)
      if(x.scale=="u") my.xlab <- "threshold"
      if(x.scale=="p") my.xlab <- "probability of non-exceedance"
	if (i==1 | i==2) my.xlab <- ""
      if (x.scale=="p"){
        plot(p, m[, i], ylim = c(ud, um), xlab = my.xlab, ylab = names[i], type = "b")
        axis(3,at=p,labels=round(qs,1))
      }
      if (x.scale=="u"){
        plot(qs, m[, i], ylim = c(ud, um), xlab = my.xlab, ylab = names[i], type = "b")
        axis(3,at=qs,labels=round(1-p,2))
      }
	if (!is.null(super)) abline(h=super[i],lty=2)
      if (x.scale=="u") for (j in 1:nint) lines(c(qs[j], qs[j]), c(ul[j, i], up[j, i]))
      if (x.scale=="p") for (j in 1:nint) lines(c(p[j], p[j]), c(ul[j, i], up[j, i]))
    }
    par(oldpar)
    zz$mles <- m
    zz$ses <- s
    zz$up <- up
    zz$ul <- ul
    zz$qs <- qs
    zz$p.exc <- 1-p
    zz$names <- names
    zz$conv <- conv
    invisible(zz)
}

#-----------------------------------------------------------------------------------------------------------#

my.pp.fitrange.cov <- function(data,pmin=0.5,pmax=0.95,npy=365,nint=10,
                      show=FALSE,ydat=NULL,mul=NULL,sigl=NULL,cluster=NULL,use.rq=F,unadj=F,init.ests=NULL){
    zz <- list()
    names <- c("location",paste("mu",1:length(mul),sep=" "),"scale","shape")
#    names <- c("location",paste("mu",1:length(mul),sep=" "),"scale",paste("sigma",1:length(sigl),sep=" "),"shape")
    n.pars <- 3+length(mul)+length(sigl)
    m <- s <- up <- ul <- matrix(NA, nrow = nint, ncol = n.pars)
    rq.ests <- matrix(NA, nrow = nint, ncol = length(mul)+1)
    conv <- NULL
    p <- seq(pmin,pmax,length=nint)    ### probability of *non-exceedance*
    qs <- quantile(data,probs=p,type=8)
    for (i in 1:nint) {

# Need to use thresh.set() here!
    if (use.rq){
      rq.fit <- rq(data~ydat[,mul],tau=p[i])
#	if (length(mul)==2) rq.fit <- rq(data~ydat[,1]+ydat[,2],tau=p[i])
#	if (length(mul)==5) rq.fit <- rq(data~ydat[,1]+ydat[,2]+ydat[,3]+ydat[,4]+ydat[,5],tau=p[i])
	a <- rq.fit$coeff[1]			### intercept from rq fit.
	b <- rq.fit$coeff[-1]			### slopes from rq fit.
	rq.ests[i,] <- rq.fit$coeff
#print(c(a,b))
	threshold <- a+ydat[,1:length(mul)]%*%matrix(b,ncol=1,nrow=length(mul))
    }

      if (is.null(cluster) & is.null(threshold)) z <- my.pp.fit(data,p.exc=1-p[i],npy=npy,show=show,ydat=ydat,mul=mul,init.ests=init.ests)
      if (!is.null(cluster) & is.null(threshold)) z <- my.pp.fit(data,p.exc=1-p[i],npy=npy,show=show,ydat=ydat,mul=mul,cluster=cluster,init.ests=init.ests)
      if (is.null(cluster) & !is.null(threshold)) z <- my.pp.fit(data,npy=npy,show=show,ydat=ydat,mul=mul,threshold=threshold,init.ests=init.ests)
      if (!is.null(cluster) & !is.null(threshold)) z <- my.pp.fit(data,npy=npy,show=show,ydat=ydat,mul=mul,sigl=sigl,cluster=cluster,threshold=threshold,reltol=1e-30,method="BFGS",init.ests=init.ests)
	init.ests <- z$mle
        conv[i] <- z$conv
print(c(i,nint,z$conv,p[i]))
        m[i, ] <- z$mle
#print(z$mle)
#print(z$se)
#print(z$se.adj)
        if (is.null(cluster)) s[i, ] <- z$se
	  if (!is.null(cluster)) s[i, ] <- z$se.adj

if (unadj) s[i,] <- z$se

        up[i, ] <- z$mle + 1.96 * s[i,]
        ul[i, ] <- z$mle - 1.96 * s[i,]
#rq.fit <- rq(data ~ ydat[,1]+ydat[,2], tau=p[i])
#rq.ests[i,2] <- rq.fit$coeff[2]
#rq.ests[i,3] <- rq.fit$coeff[3]
    }
    zz$mles <- m
    zz$ses <- s
    zz$up <- up
    zz$ul <- ul
    zz$qs <- qs
    zz$p.exc <- 1-p
    zz$names <- names
    zz$conv <- conv
    zz$rq.ests <- rq.ests
    zz$npy <- npy
    if (is.null(sigl)){
     xihat <- m[,ncol(m)]
     sigmahat <- m[,(ncol(m)-1)]
     chat <- ((npy*zz$p.exc)^(-xihat)-1)/xihat
     u0 <- rq.ests[,1]
     zz$uhats <- cbind(u0-chat*sigmahat,rq.ests[,-1],NA,NA)
    }
    invisible(zz)
}

#-----------------------------------------------------------------------------------------------------------#

my.pp.fitrange.plot <- function(x,ylabels=NULL,ngraphs=3,par.order=1:3,super=NULL,which.ps=1:length(x$mles),my.xlab="",mar.vec=c(4,4,1,2)+0.1,n.xlab=3,yats=NULL,ncolumns=1,yscale=1,rq.super=F,...){
p <- 1-x$p.exc
n.par <- ncol(x$mles)
    nint <- nrow(x$mles)
    if (is.null(ylabels)) ylabels <- x$names
    for (j in 1:length(ngraphs)){		### loop over graphs
	par(mfrow = c(ngraphs[j]/ncolumns,ncolumns))
    par(oma=c(0,0,0,0),mar=mar.vec) # bottom, left, top, right
	up <- cumsum(ngraphs)
	ul <- cumsum(ngraphs)-ngraphs+1
	for (k in ul[j]:up[j]){			### loop over parameters within a graph
	  i <- par.order[k]	    	  	  
	  my.ylab <- ylabels[i]
        um <- max(x$up[, i])*yscale
        ud <- min(x$ul[, i])*yscale
	  if (!is.null(super)){
          um <- max(x$up[, i],x$super[i])*yscale
          ud <- min(x$ul[, i],x$super[i])*yscale
	  }
if (k==n.xlab) my.xlab="probability p of non-exceedance"
        if (!rq.super) plot(p,x$mles[,i],ylim=c(ud,um),xlab=my.xlab,ylab=my.ylab,type="b",axes=F,...)
        if (rq.super) matplot(p,cbind(x$mles[,i],x$uhats[,i]),ylim=c(ud,um),xlab=my.xlab,ylab=my.ylab,type="b",axes=F,pch=c(16,16),...)
        axis(1,at=p[which.ps],labels=round(p[which.ps],2))
        if (i!=1 & i!=(n.par-1)) abline(h=0,lty=2)
        temp <- pretty(min(x$ul[,i]):max(x$up[,i]),3)
       if (!is.null(yats)){
        if (!is.na(yats[1,k])) axis(2,at=yats[,k],labels=yats[,k],xpd=T)
        if (is.na(yats[1,k])) axis(2,at=NULL)
       }
       else axis(2,xpd=T)
        box(bty="l")
        for (jj in 1:nint) lines(c(p[jj],p[jj]),c(x$ul[jj,i],x$up[jj,i]))
	  if (!is.null(super)) abline(h=super[i],lty=2)
	}
    if (j < length(ngraphs)) x11()
    }
}

#-----------------------------------------------------------------------------------------------------------#

my.pp.profxi <- function (z, xlow, xup, conf = 0.95, nint = 100, ...){
    cat("If routine fails, try changing plotting interval", fill = TRUE)

    pp.lik <- function(a){
        mu2 <- z$mulink(z$mumat2 %*% (a[1:z$npmu]))
        sc2 <- z$siglink(z$sigmat2 %*% (a[seq(z$npmu + 1, length = z$npsc)]))
        mu <- z$mulink(z$mumat %*% (a[1:z$npmu]))
        sc <- z$siglink(z$sigmat %*% (a[seq(z$npmu + 1, length = z$npsc)]))
        n <- z$n
        npy <- z$npy
        xi2 <- xi
        u <- z$threshold
 
        if (any(sc <= 0)) return(10^20)
        if (min(1 + ((xi2 * (u - mu2))/sc2)) < 0) {
            l <- 10^20
        }
        else {
            y <- (z$xdatu - mu)/sc
            y <- 1 + xi * y
            if (min(y) <= 0){ 
                l <- 10^20
            }  
            else l <- sum(log(sc)) + sum(log(y) * (1/xi + 1)) + 
                n/npy * mean((1 + (xi2 * (u - mu2))/sc2)^(-1/xi2))
        }
        l
    }

    pp.lik.adj.zero.pos <- function(a,zero.pos,null.values=NULL){ 
	  n.par <- length(a)+length(zero.pos)
	  theta <- numeric(n.par)
	  theta[(1:n.par)[-zero.pos]] <- a
        if (!is.null(null.values)) theta[zero.pos] <- null.values
        thetahat <- z$mle
        H_ADJ <- z$HA
        H_IND <- z$HI
        snum <- t(theta-thetahat)%*%H_ADJ%*%(theta-thetahat)
        sden <- t(theta-thetahat)%*%H_IND%*%(theta-thetahat)
        S <- ifelse(sum((theta-thetahat)^2)!=0,snum/sden,0)
	  l_INDtheta <- -pp.lik(theta)
 	  l_INDthetahat <- -z$nllh
	  l_ADJ <- l_INDthetahat+S*(l_INDtheta-l_INDthetahat)
	  -l_ADJ
    }
    v1 <- numeric(nint); v2 <- numeric(nint)

### Upper tail ...

    x2 <- seq(z$mle[length(z$mle)], xup, length = nint)
    sol <- z$mle[-length(z$mle)]
    for (i in 1:nint) {
        xi <- x2[i]
        opt <- optim(sol, pp.lik.adj.zero.pos, zero.pos=length(z$mle), null.values=xi,method="BFGS",control=list(fnscale=z$nllh,...))
#        opt <- optim(opt$par, pp.lik.adj.zero.pos, zero.pos=length(z$mle), null.values=xi,method="BFGS",control=list(fnscale=z$nllh,reltol=1e-20,...))
        sol <- opt$par
        v2[i] <- opt$value
print(c(xi,opt$value))
    }

### Lower tail ...

    x1 <- seq(z$mle[length(z$mle)], xlow, length = nint)
    sol <- z$mle[-length(z$mle)]
    for (i in 1:nint) {
        xi <- x1[i]
        opt <- optim(sol, pp.lik.adj.zero.pos, zero.pos=length(z$mle), null.values=xi,control=list(fnscale=z$nllh,...))
        opt <- optim(opt$par, pp.lik.adj.zero.pos, zero.pos=length(z$mle), null.values=xi,method="BFGS",control=list(fnscale=z$nllh,reltol=1e-20,...))
        sol <- opt$par
        v1[i] <- opt$value
print(c(xi,opt$value))
    }

    x <- c(rev(x1),x2); v <- c(rev(v1),v2)
    plot(x, -v, type = "l", xlab = "Shape Parameter", ylab = "Profile Log-likelihood")
    ma <- -z$nllh
    abline(h = ma, col = 4)
    abline(h = ma - 0.5 * qchisq(conf, 1), col = 4)

 u <- par("usr")								### extract plotting coords
 yaxis <- -v; xaxis <- x;conf.line <- ma - 0.5 * qchisq(conf, 1)
 temp <- diff(yaxis-conf.line>0)			### to find where curve crosses CI line

 loc <- which(temp==-1)					### upper limit of CI
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 up.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=up.lim,lty=2);text(up.lim,u[3]-0.02*(u[4]-u[3]),round(up.lim,2),xpd=T,cex=0.75)

 loc <- which(temp==1)
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 low.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=low.lim,lty=2);text(low.lim,u[3]-0.02*(u[4]-u[3]),round(low.lim,2),xpd=T,cex=0.75)

 xi <- z$mle[length(z$mle)]
 abline(v=xi,lty=2);text(xi,u[3]-0.02*(u[4]-u[3]),round(xi,2),xpd=T,cex=0.75)

 c(low.lim,xi,up.lim)
}

#-----------------------------------------------------------------------------------------------------------#

my.pp.prof <- function (z, m, xlow, xup, conf = 0.95, nint = 100, nsite=1, no.CI=F, ...){

    if (m <= 1) stop("`m' must be greater than one")
#    cat("If routine fails, try changing plotting interval", fill = TRUE)
 
    mu.covs <- unique(as.matrix(z$mumat)[z$site.exc==nsite,])   ### mu covariate values for site nsite
    mu.covs <- matrix(mu.covs,ncol=1,nrow=length(mu.covs))
    sig.covs <- unique(as.matrix(z$sigmat)[z$site.exc==nsite,]) ### sigma covariate values for site nsite
    sig.covs <- matrix(sig.covs,ncol=1,nrow=length(sig.covs))

    pp.lik <- function(a) {
        mu2 <- z$mulink(z$mumat2 %*% (a[1:z$npmu]))
        sc2 <- z$siglink(z$sigmat2 %*% (a[seq(z$npmu + 1, length = z$npsc)]))
        mu <- z$mulink(z$mumat %*% (a[1:z$npmu]))
        sc <- z$siglink(z$sigmat %*% (a[seq(z$npmu + 1, length = z$npsc)]))
        xi2 <- z$shlink(z$shmat2 %*% (a[seq(z$npmu + z$npsc + 1, length = z$npsh)]))
        xi <- z$shlink(z$shmat %*% (a[seq(z$npmu + z$npsc + 1, length = z$npsh)]))

        n <- z$n
        npy <- z$npy
        u <- z$threshold
 
        if (any(sc <= 0)) return(10^6)
        if (min(1 + ((xi2 * (u - mu2))/sc2)) < 0) {
            l <- 10^6
        }
        else {
            y <- (z$xdatu - mu)/sc
            y <- 1 + xi * y
            if (min(y) <= 0) 
                l <- 10^6
            else l <- sum(log(sc)) + sum(log(y) * (1/xi + 1)) + 
                n/npy * mean((1 + (xi2 * (u - mu2))/sc2)^(-1/xi2))
        }
        l
    }

    pp.lik.adj.zero.pos <- function(a,zero.pos){ 

	  n.par <- length(a)+length(zero.pos)
	  theta <- numeric(n.par)

        mu.lin.pred.minus.mu0 <- t(mu.covs[-1]) %*% a[1:(z$npmu-1)]
        sig.lin.pred <- z$siglink(t(sig.covs) %*% a[seq(z$npmu, length = z$npsc)])
        xxi <- a[length(a)]  ### assumes constant xi
        fxip <- (1-yp^(-xxi))/xxi
        mu0 <- xp-mu.lin.pred.minus.mu0+sig.lin.pred*fxip
        theta[zero.pos] <- mu0
	  theta[(1:n.par)[-zero.pos]] <- a

        thetahat <- z$mle
        H_ADJ <- z$HA
        H_IND <- z$HI
        snum <- t(theta-thetahat)%*%H_ADJ%*%(theta-thetahat)
        sden <- t(theta-thetahat)%*%H_IND%*%(theta-thetahat)
        S <- ifelse(sum((theta-thetahat)^2)!=0,snum/sden,0)
	  l_INDtheta <- -pp.lik(theta)
 	  l_INDthetahat <- -z$nllh
	  l_ADJ <- l_INDthetahat+S*(l_INDtheta-l_INDthetahat)
	  -l_ADJ
    }

    p <- 1/m
    v1 <- numeric(nint); v2 <- numeric(nint)
    
### Calculate the MLE of xp (at site in question).

### Assume identity link for mu ...

    mu.lin.pred <- t(mu.covs) %*% z$mle[1:z$npmu]
#    mu.lin.pred.minus.mu0 <- t(mu.covs[-1]) %*% z$mle[2:z$npmu]
    sig.lin.pred <- z$siglink(t(sig.covs) %*% z$mle[seq(z$npmu + 1, length = z$npsc)])
    xi <- z$mle[length(z$mle)]
    yp <- -log(1-p)
    fxip <- (1-yp^(-xi))/xi
    xp.mle <- mu.lin.pred-sig.lin.pred*fxip

    if (no.CI) return(c(NA,xp.mle,NA))

### Also assume identity link for sigma ...

    V <- z$cov.adj
    dxp1 <- mu.covs
    dxp2 <- -sig.covs*fxip
    dfxip.dxi <- (xi*yp^(-xi)*log(yp)-1+yp^(-xi))/xi^2
    dxp3 <- -sig.lin.pred*dfxip.dxi
    dxp <- c(dxp1,dxp2,dxp3)
    se.xp <- sqrt(t(dxp)%*%V%*%dxp)
    xp.low <- xp.mle-1.96*se.xp
    xp.up <- xp.mle+1.96*se.xp

cat(paste("Site number = ", nsite, " MLE = ", round(xp.mle,1), " symmetric CI = ", round(xp.low,1), round(xp.up,1)),fill=TRUE)

#    if (xp.up > xup){
#      xup <- 1.1*xp.up
#      print(paste("upper limit changed to",xup))
#    }
#    if (xp.low < xlow){
#      xlow <- xp.low/1.1
#      print(paste("lower limit changed to",xlow))
#    }

### Upper tail ...

    x2 <- seq(xp.mle, xup, length = nint)

    sol <- z$mle[-1]
    for (i in 1:length(x2)) {
        xp <- x2[i]
#        mu0 <- xp-mu.lin.pred.minus.mu0+sig.lin.pred*fxip
        opt <- optim(sol, pp.lik.adj.zero.pos, zero.pos=1,method="BFGS",control=list(fnscale=z$nllh),...)
#        opt <- optim(opt$par, pp.lik.adj.zero.pos, zero.pos=1,method="BFGS",control=list(fnscale=z$nllh,...))
        sol <- opt$par
        v2[i] <- opt$value
print(c(xp,opt$value))
    }

### Lower tail ...

    x1 <- seq(xp.mle, xlow, length = nint)

    sol <- z$mle[-1]
    for (i in 1:length(x1)){
        xp <- x1[i]
#        mu0 <- xp-mu.lin.pred.minus.mu0+sig.lin.pred*fxip
        opt <- optim(sol, pp.lik.adj.zero.pos, zero.pos=1,method="BFGS",control=list(fnscale=z$nllh),...)
#        opt <- optim(opt$par, pp.lik.adj.zero.pos, zero.pos=1,method="BFGS",control=list(fnscale=z$nllh,...))
        sol <- opt$par
        v1[i] <- opt$value
print(c(xp,opt$value))
    }

    x <- c(rev(x1),x2); v <- c(rev(v1),v2)
    plot(x, -v, type = "l", xlab = paste(m," year return level"), ylab = "Profile Log-likelihood",main=paste("site",nsite))
    ma <- -z$nllh
    abline(h = ma, col = 4)
    abline(h = ma - 0.5 * qchisq(conf, 1), col = 4)

 u <- par("usr")								### extract plotting coords
 yaxis <- -v; xaxis <- x;conf.line <- ma - 0.5 * qchisq(conf, 1)
 temp <- diff(yaxis-conf.line>0)			### to find where curve crosses CI line

 loc <- which(temp==-1)					### upper limit of CI
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 up.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=up.lim,lty=2);text(up.lim,u[3]-0.02*(u[4]-u[3]),round(up.lim,2),xpd=T,cex=0.75)

 loc <- which(temp==1)
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 low.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=low.lim,lty=2);text(low.lim,u[3]-0.02*(u[4]-u[3]),round(low.lim,2),xpd=T,cex=0.75)

 xi <- z$mle[3]
 abline(v=xp.mle,lty=2);text(xp.mle,u[3]-0.02*(u[4]-u[3]),round(xp.mle,2),xpd=T,cex=0.75)

 c(low.lim,xp.mle,up.lim)
}

#-----------------------------------------------------------------------------------------------------------#

my.pp.xp.conf <- function (z, m, conf = 0.95, nsite=1, ...){

    if (m <= 1) stop("`m' must be greater than one")
 
    mu.covs <- unique(as.matrix(z$mumat)[z$site.exc==nsite,])   ### mu covariate values for site nsite
    mu.covs <- matrix(mu.covs,ncol=1,nrow=length(mu.covs))
    sig.covs <- unique(as.matrix(z$sigmat)[z$site.exc==nsite,]) ### sigma covariate values for site nsite
    sig.covs <- matrix(sig.covs,ncol=1,nrow=length(sig.covs))

    p <- 1/m

### Calculate the MLE of xp (at site in question).

### Assume identity link for mu ...

    mu.lin.pred <- t(mu.covs) %*% z$mle[1:z$npmu]
    sig.lin.pred <- z$siglink(t(sig.covs) %*% z$mle[seq(z$npmu + 1, length = z$npsc)])
    xi <- z$mle[length(z$mle)]
    yp <- -log(1-p)
    fxip <- (1-yp^(-xi))/xi
    xp.mle <- mu.lin.pred-sig.lin.pred*fxip

### Also assume identity link for sigma ...

    V <- z$cov.adj
    dxp1 <- mu.covs
    dxp2 <- -sig.covs*fxip
    dfxip.dxi <- (xi*yp^(-xi)*log(yp)-1+yp^(-xi))/xi^2
    dxp3 <- -sig.lin.pred*dfxip.dxi
    dxp <- c(dxp1,dxp2,dxp3)
    se.xp <- sqrt(t(dxp)%*%V%*%dxp)
    xp.low <- xp.mle-1.96*se.xp
    xp.up <- xp.mle+1.96*se.xp

cat(paste("Site number = ", nsite, " MLE = ", round(xp.mle,1), " symmetric CI = ", round(xp.low,1), round(xp.up,1)),fill=TRUE)

 c(xp.low,xp.mle,xp.up)
}


#-----------------------------------------------------------------------------------------------------------#

my.pp.xp.conf.general <- function (z,m,conf=0.95,mu.covs=NULL,sig.covs=NULL){

    if (m <= 1) stop("`m' must be greater than one")
 
    if (is.null(mu.covs)) mu.covs <- c(1,rep(0,z$npmu-1))
    if (is.null(sig.covs)) sig.covs <- c(1,rep(0,z$npsc-1))

    mu.covs <- matrix(mu.covs,ncol=1,nrow=length(mu.covs))
    sig.covs <- matrix(sig.covs,ncol=1,nrow=length(sig.covs))

    p <- 1/m

### Calculate the MLE of xp (at site in question).

### Assume identity link for mu ...

    mu.lin.pred <- t(mu.covs) %*% z$mle[1:z$npmu]
    sig.lin.pred <- z$siglink(t(sig.covs) %*% z$mle[seq(z$npmu + 1, length = z$npsc)])
    xi <- z$mle[length(z$mle)]
    yp <- -log(1-p)
    fxip <- (1-yp^(-xi))/xi
    xp.mle <- mu.lin.pred-sig.lin.pred*fxip

### Also assume identity link for sigma ...

    V <- z$cov.adj
    dxp1 <- mu.covs
    dxp2 <- -sig.covs*fxip
    dfxip.dxi <- (xi*yp^(-xi)*log(yp)-1+yp^(-xi))/xi^2
    dxp3 <- -sig.lin.pred*dfxip.dxi
    dxp <- c(dxp1,dxp2,dxp3)
    se.xp <- sqrt(t(dxp)%*%V%*%dxp)
    xp.low <- xp.mle-1.96*se.xp
    xp.up <- xp.mle+1.96*se.xp

cat("mu covariates = ", t(mu.covs),fill=TRUE)

cat(paste(" MLE = ", round(xp.mle,2), " symmetric CI = ", round(xp.low,2), round(xp.up,2)),fill=TRUE)

 c(xp.low,xp.mle,xp.up)
}

#-----------------------------------------------------------------------------------------------------------#

my.pp.prof.general <- function (z,m,xlow,xup,conf=0.95,nint=10,no.CI=F,mu.covs=NULL,sig.covs=NULL,...){

    if (m <= 1) stop("`m' must be greater than one")

    if (is.null(mu.covs)) mu.covs <- c(1,rep(0,z$npmu-1))
    if (is.null(sig.covs)) sig.covs <- c(1,rep(0,z$npsc-1))

    mu.covs <- matrix(mu.covs,ncol=1,nrow=length(mu.covs))
    sig.covs <- matrix(sig.covs,ncol=1,nrow=length(sig.covs))
 
    pp.lik <- function(a) {
        mu2 <- z$mulink(z$mumat2 %*% (a[1:z$npmu]))
        sc2 <- z$siglink(z$sigmat2 %*% (a[seq(z$npmu + 1, length = z$npsc)]))
        mu <- z$mulink(z$mumat %*% (a[1:z$npmu]))
        sc <- z$siglink(z$sigmat %*% (a[seq(z$npmu + 1, length = z$npsc)]))
        xi2 <- z$shlink(z$shmat2 %*% (a[seq(z$npmu + z$npsc + 1, length = z$npsh)]))
        xi <- z$shlink(z$shmat %*% (a[seq(z$npmu + z$npsc + 1, length = z$npsh)]))

        n <- z$n
        npy <- z$npy
        u <- z$threshold
 
        if (any(sc <= 0)) return(10^6)
        if (min(1 + ((xi2 * (u - mu2))/sc2)) < 0) {
            l <- 10^6
        }
        else {
            y <- (z$xdatu - mu)/sc
            y <- 1 + xi * y
            if (min(y) <= 0) 
                l <- 10^6
            else l <- sum(log(sc)) + sum(log(y) * (1/xi + 1)) + 
                n/npy * mean((1 + (xi2 * (u - mu2))/sc2)^(-1/xi2))
        }
        l
    }

    pp.lik.adj.zero.pos <- function(a,zero.pos){ 

	  n.par <- length(a)+length(zero.pos)
	  theta <- numeric(n.par)

        mu.lin.pred.minus.mu0 <- t(mu.covs[-1]) %*% a[1:(z$npmu-1)]
        sig.lin.pred <- z$siglink(t(sig.covs) %*% a[seq(z$npmu, length = z$npsc)])
        xxi <- a[length(a)]  ### assumes constant xi
        fxip <- (1-yp^(-xxi))/xxi
        mu0 <- xp-mu.lin.pred.minus.mu0+sig.lin.pred*fxip
        theta[zero.pos] <- mu0
	  theta[(1:n.par)[-zero.pos]] <- a

        thetahat <- z$mle
        H_ADJ <- z$HA
        H_IND <- z$HI
        snum <- t(theta-thetahat)%*%H_ADJ%*%(theta-thetahat)
        sden <- t(theta-thetahat)%*%H_IND%*%(theta-thetahat)
        S <- ifelse(sum((theta-thetahat)^2)!=0,snum/sden,0)
	  l_INDtheta <- -pp.lik(theta)
 	  l_INDthetahat <- -z$nllh
	  l_ADJ <- l_INDthetahat+S*(l_INDtheta-l_INDthetahat)
	  -l_ADJ
    }

    p <- 1/m
    v1 <- numeric(nint); v2 <- numeric(nint)
    
### Calculate the MLE of xp (at site in question).

### Assume identity link for mu ...

    mu.lin.pred <- t(mu.covs) %*% z$mle[1:z$npmu]
#    mu.lin.pred.minus.mu0 <- t(mu.covs[-1]) %*% z$mle[2:z$npmu]
    sig.lin.pred <- z$siglink(t(sig.covs) %*% z$mle[seq(z$npmu + 1, length = z$npsc)])
    xi <- z$mle[length(z$mle)]
    yp <- -log(1-p)
    fxip <- (1-yp^(-xi))/xi
    xp.mle <- mu.lin.pred-sig.lin.pred*fxip

    if (no.CI) return(c(NA,xp.mle,NA))

### Also assume identity link for sigma ...

    V <- z$cov.adj
    dxp1 <- mu.covs
    dxp2 <- -sig.covs*fxip
    dfxip.dxi <- (xi*yp^(-xi)*log(yp)-1+yp^(-xi))/xi^2
    dxp3 <- -sig.lin.pred*dfxip.dxi
    dxp <- c(dxp1,dxp2,dxp3)
    se.xp <- sqrt(t(dxp)%*%V%*%dxp)
    xp.low <- xp.mle-1.96*se.xp
    xp.up <- xp.mle+1.96*se.xp

cat("mu covariates = ", t(mu.covs),fill=TRUE)
cat("sigma covariates = ", t(sig.covs),fill=TRUE)
cat(paste(" MLE = ", round(xp.mle,2), " symmetric CI = ", round(xp.low,2), round(xp.up,2)),fill=TRUE)

#    if (xp.up > xup){
#      xup <- 1.1*xp.up
#      print(paste("upper limit changed to",xup))
#    }
#    if (xp.low < xlow){
#      xlow <- xp.low/1.1
#      print(paste("lower limit changed to",xlow))
#    }

### Upper tail ...

    x2 <- seq(xp.mle, xup, length = nint)

    sol <- z$mle[-1]
    for (i in 1:length(x2)) {
        xp <- x2[i]
#        mu0 <- xp-mu.lin.pred.minus.mu0+sig.lin.pred*fxip
        opt <- optim(sol, pp.lik.adj.zero.pos, zero.pos=1,method="BFGS",control=list(fnscale=z$nllh),...)
#        opt <- optim(opt$par, pp.lik.adj.zero.pos, zero.pos=1,method="BFGS",control=list(fnscale=z$nllh,...))
        sol <- opt$par
        v2[i] <- opt$value
print(c(xp,opt$value))
    }

### Lower tail ...

    x1 <- seq(xp.mle, xlow, length = nint)

    sol <- z$mle[-1]
    for (i in 1:length(x1)){
        xp <- x1[i]
#        mu0 <- xp-mu.lin.pred.minus.mu0+sig.lin.pred*fxip
        opt <- optim(sol, pp.lik.adj.zero.pos, zero.pos=1,method="BFGS",control=list(fnscale=z$nllh),...)
#        opt <- optim(opt$par, pp.lik.adj.zero.pos, zero.pos=1,method="BFGS",control=list(fnscale=z$nllh,...))
        sol <- opt$par
        v1[i] <- opt$value
print(c(xp,opt$value))
    }

    x <- c(rev(x1),x2); v <- c(rev(v1),v2)
    plot(x, -v, type = "l", xlab = paste(m," year return level"), ylab = "Profile Log-likelihood")
    ma <- -z$nllh
    abline(h = ma, col = 4)
    abline(h = ma - 0.5 * qchisq(conf, 1), col = 4)

 u <- par("usr")								### extract plotting coords
 yaxis <- -v; xaxis <- x;conf.line <- ma - 0.5 * qchisq(conf, 1)
 temp <- diff(yaxis-conf.line>0)			### to find where curve crosses CI line

 loc <- which(temp==-1)					### upper limit of CI
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 up.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=up.lim,lty=2);text(up.lim,u[3]-0.02*(u[4]-u[3]),round(up.lim,2),xpd=T,cex=0.75)

 loc <- which(temp==1)
 x1 <- xaxis[loc]; x2 <- xaxis[loc+1]; y1 <- yaxis[loc]; y2 <- yaxis[loc+1]
 low.lim <- x1+(conf.line-y1)*(x2-x1)/(y2-y1)
 abline(v=low.lim,lty=2);text(low.lim,u[3]-0.02*(u[4]-u[3]),round(low.lim,2),xpd=T,cex=0.75)

 xi <- z$mle[3]
 abline(v=xp.mle,lty=2);text(xp.mle,u[3]-0.02*(u[4]-u[3]),round(xp.mle,2),xpd=T,cex=0.75)

 c(low.lim,xp.mle,up.lim)
}

#-----------------------------------------------------------------------------#
#                           GP model diagnostics                              #
#-----------------------------------------------------------------------------#

pjn.gpd.diag <- function (z){
    n <- length(z$data)
    x <- (1:n)/(n + 1)
    if (z$trans) {
        oldpar <- par(mfrow = c(1, 2))
        plot(x, 1 - exp(-sort(z$data)), xlab = "Empirical", ylab = "Model")
        abline(0, 1, col = 4)
        title("Residual Probability Plot")
        plot(-log(1 - x), sort(z$data), ylab = "Empirical", xlab = "Model")
        abline(0, 1, col = 4)
        title("Residual Quantile Plot (Exptl. Scale)")
    }
    else {
        oldpar <- par(mfrow = c(2, 2))
par(mar=c(4.5,4,2,1)) 
        pjn.gpd.pp(z$mle, z$threshold, z$data)
        pjn.gpd.qq(z$mle, z$threshold, z$data)
        pjn.gpd.rl(z$mle, z$threshold, z$rate, z$n, z$npy, z$cov, 
            z$data, z$xdata)
        pjn.gpd.his(z$mle, z$threshold, z$data)
    }
    par(oldpar)
    invisible()
}

pjn.gpd.pp <- function (a, u, dat){
    plot((1:length(dat))/length(dat), pjn.gpdf(a, u, sort(dat)), 
        ylab = "Empirical", xlab = "Model", main = "Probability Plot")
    abline(0, 1, col = 4)
}

pjn.gpd.qq <- function (a, u, dat){
    plot(gpdq(a, u, 1 - (1:length(dat)/(length(dat) + 1))), sort(dat), 
        ylab = "Empirical", xlab = "Model", main = "Quantile Plot")
    abline(0, 1, col = 4)
}

pjn.gpd.rl <- function (a, u, la, n, npy, mat, dat, xdat){
    a <- c(la, a)
    eps <- 1e-06
    a1 <- a
    a2 <- a
    a3 <- a
    a1[1] <- a[1] + eps
    a2[2] <- a[2] + eps
    a3[3] <- a[3] + eps
    jj <- seq(-1, 3.75 + log10(npy), by = 0.1)
    m <- c(1/la, 10^jj)
    q <- gpdq2(a[2:3], u, la, m)
    d <- t(gpd.rl.gradient(a = a, m = m))
    mat <- matrix(c((la * (1 - la))/n, 0, 0, 0, mat[1, 1], mat[1, 
        2], 0, mat[2, 1], mat[2, 2]), ncol = 3)
    v <- apply(d, 1, q.form, m = mat)
    plot(m/npy, q, log = "x", type = "n", xlim = c(0.1, max(m)/npy), 
        ylim = c(u, max(xdat, q[q > u - 1] + 1.96 * sqrt(v)[q > 
            u - 1])), xlab = "Return period (years)", ylab = "Return level", 
        main = "Return Level Plot")
    lines(m[q > u - 1]/npy, q[q > u - 1])
    lines(m[q > u - 1]/npy, q[q > u - 1] + 1.96 * sqrt(v)[q > 
        u - 1], col = 4)
    lines(m[q > u - 1]/npy, q[q > u - 1] - 1.96 * sqrt(v)[q > 
        u - 1], col = 4)
    nl <- n - length(dat) + 1
    sdat <- sort(xdat)
    points((1/(1 - (1:n)/(n + 1))/npy)[sdat > u], sdat[sdat > 
        u])
}

pjn.gpd.his <- function (a, u, dat){
    h <- hist(dat, plot = FALSE)
    x <- seq(u, max(h$breaks), length = 100)
    y <- gpd.dens(a, u, x)
    my.breaks <- seq(min(dat),max(dat),len=nclass.Sturges(dat))
    hist(dat, freq = FALSE, ylim = c(0, max(max(h$density), max(y))), 
        xlab = "x", ylab = "f(x)", main = "Density Plot", breaks=my.breaks)
    lines(x, y, col = 4)
}

pjn.gpdf <- function (a, u, z){
    1 - (1 + (a[2] * (z - u))/a[1])^(-1/a[2])
}
