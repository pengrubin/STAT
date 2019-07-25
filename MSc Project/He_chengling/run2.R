# ______multivariate normal  EFF(DBPS)/EFF(GW) VS. dimensions_______
library(coda)
library(mvtnorm)
set.seed(8)

## function
# dbps
mn_dbps <- function(num,start,scale){
  x.store <- matrix(NA,nrow=dim,ncol=num)
  x.store[,1] <- start
  x.old <- start
  # sample u which follows unifrom distribution on the unit d-sphere
  v <- rnorm(dim)
  u <- v/sqrt(sum(v^2))
  # set the count of acceptance for the 1st stage 
  count <- 0
  if( dim==1 ){
    for(i in 2:num){
      z <- abs(scale*rnorm(dim)) # generate a vector of delta 
      # first proposal
      x_1p <- x.old + z*u
      u.new1 <- -u
      accep1 <- min(1,dnorm(x_1p,mean=mu,sd=sqrt(cov))/
                      dnorm(x.old,mean=mu,sd=sqrt(cov)))
      if (log(runif(1)) < log(accep1)){ # first judgement
        x.store[,i] <- x_1p
        x.old <- x_1p
        u <- -u.new1 
        count <- count+1 # acceptance rate for the 1st stage
      }else{
        # second proposal
        nabla <- as.vector(-solve(cov)%*%(x_1p-mu))
        nabla_hat <- nabla/sqrt(sum(nabla^2))
        u.new2 <- 2*sum(u*nabla_hat)*nabla_hat - u # new u
        x_2p <- x_1p - z*u.new2 # new x
        alpha2 <- min(1,dnorm(x_1p,mean=mu,sd=sqrt(cov))/
                        dnorm(x_2p,mean=mu,sd=sqrt(cov)))
        pi.ratio <- dnorm(x_2p,mean=mu,sd=sqrt(cov))/
          dnorm(x.old,mean=mu,sd=sqrt(cov))
        accep2 <- min(1, (1-alpha2)/(1-accep1)*pi.ratio)
        if (log(runif(1)) < log(accep2)){
          x.store[,i] <- x_2p
          x.old <- x_2p
          u <- -u.new2
        }else{
          x.store[,i] <- x.old
          u <- -u
        }
      }
    }
  }else{
    for (i in 2:num){
      z <- abs(scale*rnorm(dim)) # generate a vector of delta 
      # first proposal
      x_1p <- x.old + z*u
      u.new1 <- -u
      accep1 <- min(1,dmvnorm(x_1p,mean=mu,sigma=cov)/
                      dmvnorm(x.old,mean=mu,sigma=cov))
      if (log(runif(1)) < log(accep1)){ # first judgement
        x.store[,i] <- x_1p
        x.old <- x_1p
        u <- -u.new1 
        count <- count+1 # acceptance rate for the 1st stage
      }else{
        # second proposal
        nabla <- as.vector(-solve(cov)%*%(x_1p-mu))
        nabla_hat <- nabla/sqrt(sum(nabla^2))
        u.new2 <- 2*sum(u*nabla_hat)*nabla_hat - u # new u
        x_2p <- x_1p - z*u.new2 # new x
        alpha2 <- min(1,dmvnorm(x_1p,mean=mu,sigma=cov)/
                        dmvnorm(x_2p,mean=mu,sigma=cov))
        pi.ratio <- dmvnorm(x_2p,mean=mu,sigma=cov)/
          dmvnorm(x.old,mean=mu,sigma=cov)
        accep2 <- min(1, (1-alpha2)/(1-accep1)*pi.ratio)
        if (log(runif(1)) < log(accep2)){
          x.store[,i] <- x_2p
          x.old <- x_2p
          u <- -u.new2
        }else{
          x.store[,i] <- x.old
          u <- -u
        }
      }
    }
  }
  return(list(sample=x.store, accept=count))
}

#  GW 
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
scale.dbps <- c(2.00, 0.83, 0.65, 0.57, 0.55, 0.54, 0.50, 0.49, 0.49, 0.48, 0.47, 0.47,
                0.46, 0.46, 0.46)
scale.gw <- c(2.00, 0.35, 0.28, 0.25, 0.23, 0.23, 0.22, 0.22, 0.21, 0.21, 0.21, 0.21,
              0.21, 0.21, 0.20)

# correlated with rho=0.9
n <- 15
eff_dbps_2 <- rep(NA,n)
eff_gw_2 <- rep(NA,n)

# create a blank plot
pdf('ESS(DBPS) ratio VS. dimensions.pdf')
plot(0,0,pch='',xlim=c(1,n),ylim=c(0,10),main="ESS(DBPS)/ESS(GW) VS. Dimensions",
     xlab="dimensions", ylab='ratio')

# simulate for 5 times
for (i in 1:5){
  for (dim in 1:n){
    mu <- rep(0,dim)
    cov <- matrix(c(rep(c(1,rep(0.9,dim)),dim-1),1),nrow=dim)# rho=0.9
    mn.dbps <- mn_dbps(num,start=mu,scale=scale.dbps[dim])
    mn.gw <- MN_GW_MwGibbs(num,start=mu,scale=scale.gw[dim])
    mc.mndbps <- mcmc(t(mn.dbps$sample))#transform
    mc.mngw <- mcmc(t(mn.gw))
    eff_dbps_2[dim] <- min(effectiveSize(mc.mndbps))
    eff_gw_2[dim] <- min(effectiveSize(mc.mngw))
  }
  ratio <- eff_dbps_2/eff_gw_2
  lines(1:n, ratio, lty=i, col=i)
}  
dev.off()


