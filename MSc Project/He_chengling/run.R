# ______multivariate normal  EFF(GW)/EFF(RW) VS. dimensions_______
library(coda)
library(mvtnorm)
set.seed(8)

# function
## RW
MN_RW_MwGibbs <- function(num,start,scale){
  x.store <- matrix(NA,nrow=dim,ncol=num)
  x.store[,1] <- start
  x.old <- start
  # judge whether dimension=1
  if (dim == 1){
    for(i in 2:num){
      z <- scale*rnorm(1)
      x1d <- x.old[1] + z
      log_accept1d <- min(0,dnorm(x1d, mu, cov,log=T)-
                            dnorm(x.old, mu, cov,log=T))
      if (log(runif(1)) < log_accept1d){
        x.store[,i] <- x1d
        x.old <- x1d
      }else {
        x.store[,i] <- x.old
      }
    }
  }else{
    for (i in 2:num){
      # update the first variable
      z <- scale*rnorm(1)
      x.new1 <- x.old[1] + z
      mu.new1 <- mu[1] + cov[1,-1]%*%solve(cov[-1,-1])%*%
        (x.store[-1,i-1] - mu[-1])
      cov.new1 <- cov[1,1]-cov[1,-1]%*%solve(cov[-1,-1])%*%cov[-1,1]
      log_accept1 <- min(0,dnorm(x.new1, mu.new1, cov.new1,log=T)-
                           dnorm(x.old[1], mu.new1, cov.new1,log=T))
      if (log(runif(1)) < log_accept1){
        x.store[1,i] <- x.new1
        x.old[1] <- x.new1
      }else {
        x.store[1,i] <- x.old[1]
      }
      # judge whether dimension=2
      if (dim==2){
        z <- scale*rnorm(1)
        x.newd <- x.old[dim] + z
        mu.newd <- mu[dim] + cov[dim,-dim]%*%solve(cov[-dim,-dim])%*%
          (x.store[-dim,i] - mu[-dim])
        cov.newd <- cov[dim,dim]-cov[dim,-dim]%*%solve(cov[-dim,-dim])%*%cov[-dim,dim]
        log_acceptd <- min(0,dnorm(x.newd, mu.newd, cov.newd,log=T)-
                             dnorm(x.old[dim], mu.newd, cov.newd,log=T))
        if (log(runif(1)) < log_acceptd){
          x.store[dim,i] <- x.newd
          x.old[dim] <- x.newd
        }else{
          x.store[dim,i] <- x.old[dim]
        }
      }else{
        # update the remaining (dim-2) variables
        for (j in 2:(dim-1)){
          z <- scale*rnorm(1)
          x.new <- x.old[j] + z
          mu.new <- mu[j] + cov[j,-j]%*%solve(cov[-j,-j])%*%
            (c(x.store[1:(j-1),i],x.store[(j+1):dim,i-1]) - mu[-j])
          cov.new <- cov[j,j]-cov[j,-j]%*%solve(cov[-j,-j])%*%cov[-j,j]
          log_accept <- min(0,dnorm(x.new, mu.new, cov.new,log=T)-
                              dnorm(x.old[j], mu.new, cov.new,log=T))
          if (log(runif(1)) < log_accept){
            x.store[j,i] <- x.new
            x.old[j] <- x.new
          }else{
            x.store[j,i] <- x.old[j]
          }
        }
        # update the last variable
        z <- scale*rnorm(1)
        x.newd <- x.old[dim] + z
        mu.newd <- mu[dim] + cov[dim,-dim]%*%solve(cov[-dim,-dim])%*%
          (x.store[-dim,i] - mu[-dim])
        cov.newd <- cov[dim,dim]-cov[dim,-dim]%*%solve(cov[-dim,-dim])%*%cov[-dim,dim]
        log_acceptd <- min(0,dnorm(x.newd, mu.newd, cov.newd,log=T)-
                             dnorm(x.old[dim], mu.newd, cov.newd,log=T))
        if (log(runif(1)) < log_acceptd){
          x.store[dim,i] <- x.newd
          x.old[dim] <- x.newd
        }else{
          x.store[dim,i] <- x.old[dim]
        }
      }
    }
  }
  return(x.store)
}
##  GW 
MN_GW_MwGibbs <- function(num,start,scale){
  x.store <- matrix(NA,nrow=dim,ncol=num)
  x.store[,1] <- start
  x.old <- start
  p <- sample(c(-1,1),dim,replace=T)
  # judge whether dimension=1
  if (dim == 1){
    for(i in 2:num){
      z <- scale*p*abs(rnorm(1))
      x1d <- x.old[1] + z
      log_accept1d <- min(0,dnorm(x1d, mu, cov,log=T)-
                            dnorm(x.old, mu, cov,log=T))
      if (log(runif(1)) < log_accept1d){
        x.store[,i] <- x1d
        x.old <- x1d
      }else {
        x.store[,i] <- x.old
        p <- -p
      }
    }
  }else{
    for (i in 2:num){
      # update the first variable
      z <- scale*p[1]*abs(rnorm(1))
      x.new1 <- x.old[1] + z
      mu.new1 <- mu[1] + cov[1,-1]%*%solve(cov[-1,-1])%*%
        (x.store[-1,i-1] - mu[-1])
      cov.new1 <- cov[1,1]-cov[1,-1]%*%solve(cov[-1,-1])%*%cov[-1,1]
      log_accept1 <- min(0,dnorm(x.new1, mu.new1, cov.new1,log=T)-
                           dnorm(x.old[1], mu.new1, cov.new1,log=T))
      if (log(runif(1)) < log_accept1){
        x.store[1,i] <- x.new1
        x.old[1] <- x.new1
      }else {
        x.store[1,i] <- x.old[1]
        p[1] <- -p[1]
      }
      # judge whether dimension=2
      if (dim==2){
        z <- scale*p[dim]*abs(rnorm(1))
        x.newd <- x.old[dim] + z
        mu.newd <- mu[dim] + cov[dim,-dim]%*%solve(cov[-dim,-dim])%*%
          (x.store[-dim,i] - mu[-dim])
        cov.newd <- cov[dim,dim]-cov[dim,-dim]%*%solve(cov[-dim,-dim])%*%cov[-dim,dim]
        log_acceptd <- min(0,dnorm(x.newd, mu.newd, cov.newd,log=T)-
                             dnorm(x.old[dim], mu.newd, cov.newd,log=T))
        if (log(runif(1)) < log_acceptd){
          x.store[dim,i] <- x.newd
          x.old[dim] <- x.newd
        }else{
          x.store[dim,i] <- x.old[dim]
          p[dim] <- -p[dim]
        }
      }else{
        # update the remaining (dim-2) variables
        for (j in 2:(dim-1)){
          z <- scale*p[j]*abs(rnorm(1))
          x.new <- x.old[j] + z
          mu.new <- mu[j] + cov[j,-j]%*%solve(cov[-j,-j])%*%
            (c(x.store[1:(j-1),i],x.store[(j+1):dim,i-1]) - mu[-j])
          cov.new <- cov[j,j]-cov[j,-j]%*%solve(cov[-j,-j])%*%cov[-j,j]
          log_accept <- min(0,dnorm(x.new, mu.new, cov.new,log=T)-
                              dnorm(x.old[j], mu.new, cov.new,log=T))
          if (log(runif(1)) < log_accept){
            x.store[j,i] <- x.new
            x.old[j] <- x.new
          }else{
            x.store[j,i] <- x.old[j]
            p[j] <- -p[j]
          }
        }
        # update the last variable
        z <- scale*p[dim]*abs(rnorm(1))
        x.newd <- x.old[dim] + z
        mu.newd <- mu[dim] + cov[dim,-dim]%*%solve(cov[-dim,-dim])%*%
          (x.store[-dim,i] - mu[-dim])
        cov.newd <- cov[dim,dim]-cov[dim,-dim]%*%solve(cov[-dim,-dim])%*%cov[-dim,dim]
        log_acceptd <- min(0,dnorm(x.newd, mu.newd, cov.newd,log=T)-
                             dnorm(x.old[dim], mu.newd, cov.newd,log=T))
        if (log(runif(1)) < log_acceptd){
          x.store[dim,i] <- x.newd
          x.old[dim] <- x.newd
        }else{
          x.store[dim,i] <- x.old[dim]
          p[dim] <- -p[dim]
        }
      }
    }
  }
  return(x.store)
}

# simulation
num <- 100000
scale.rw <- c(2.40, 0.45, 0.35, 0.30, 0.30, 0.28, 0.27, 0.27, 0.26, 0.26, 0.26, 0.25,
              0.25, 0.25, 0.25)
scale.gw <- c(2.00, 0.35, 0.28, 0.25, 0.23, 0.23, 0.22, 0.22, 0.21, 0.21, 0.21, 0.21,
               0.21, 0.21, 0.20)
# correlated with rho=0.9
n <- 15 # dimension
eff_rw_1 <- rep(NA,n)
eff_gw_1 <- rep(NA,n)
# create a blank plot
pdf(file='ESS ratio VS. dimensions.pdf')
plot(0,0,pch='',xlim=c(1,n),ylim=c(0,3),main="ESS(GW)/ESS(RW) VS. Dimensions",
     xlab="dimensions", ylab='ratio')
# simulate for 5 times
for (i in 1:5){
  for (dim in 1:n){
    mu <- rep(0,dim)
    cov <- matrix(c(rep(c(1,rep(0.9,dim)),dim-1),1),nrow=dim)# rho=0.9
    mn.rw <- MN_RW_MwGibbs(num,start=mu,scale=scale.rw[dim])
    mn.gw <- MN_GW_MwGibbs(num,start=mu,scale=scale.gw[dim])
    mc.mnrw <- mcmc(t(mn.rw))#transform
    mc.mngw <- mcmc(t(mn.gw))
    eff_rw_1[dim] <- min(effectiveSize(mc.mnrw))
    eff_gw_1[dim] <- min(effectiveSize(mc.mngw))
  }
  ratio <- eff_gw_1/eff_rw_1
  lines(1:n, ratio, lty=i, col=i)
}  
dev.off()


