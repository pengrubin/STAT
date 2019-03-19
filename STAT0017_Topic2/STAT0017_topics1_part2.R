# uniform data
data(worldindices) 
u=worldindices
rm(worldindices)



cor(u[,1:3],method = c("kendall"))

u2u3u1=cbind(u[,2],u[,3],u[,1])
vinemodel=CDVineCopSelect(u2u3u1,type=2,familyset=c(1:10,13,14,23,24))
vinemodel

N=2000
u2u3u1_sim=CDVineSim(N, family=vinemodel$family, vinemodel$par,  vinemodel$par2, type=2)
cor(u2u3u1_sim,method = c("kendall"))
cor(u2u3u1,method = c("kendall"))
vinemodel_sim=CDVineCopSelect(u2u3u1_sim,type=2,familyset=c(1:10,13,14,23,24))
vinemodel_sim

# Vine selection "manually"
model_1 = BiCopSelect(u[,2],u[,3],familyset=c(1:10,13,14,16,23,24,26))
model_1
model_2 = BiCopSelect(u[,1],u[,3],familyset=c(1:10,13,14,16,23,24,26))
model_2
h1 = BiCopHfunc(u[,2],u[,3],model_1$family,model_1$par,model_1$par2)
h2 = BiCopHfunc(u[,1],u[,3],model_2$family,model_2$par)
model_3 = BiCopSelect(h1$hfunc2,h2$hfunc2,familyset=c(1:10,13,14,16,23,24,26))


###########



N=10000
u_sim=CDVineSim(N, family=model$family, model$par,  model$par2, type=2)
cor(u_sim,method = c("kendall"))
model_sim=CDVineCopSelect(u_sim,type=2,familyset=c(1:10,13,14,23,24))



# Inverse CDF Skewed T distribution
param=coef(model)
z1 <- qnorm(u[,1], mean = 0, sd = 1)
sigmat <- model@h.t[length(model@h.t)]

rest<-res[length(res)]

sigma_tplus1 <- param[5] + param[6]*sigmat*rest^2 + param1[6]*sigmat
epsilon <- sqrt(sigma_tplus1)*z1

x2_t <- matrix(0, nrow = N, ncol = 1)
x2_lag <- ret[length(ret)]

x_t <- param[1] + param[2]*x2_lag + epsilon  

plot(x1_sim, type='line')
plot(x2_sim, type='line')
plot(x3_sim, type='line')



# estimate parameters
opts <- list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel"  = 1.0e-7, "maxeval"   = 1000 )
lik=nloptr(x0=0.7, eval_f=copula_lik, lb=-0.999, ub=0.999, opts=opts)

BiCopEst(u[,1], u[,2],family = 1) 

factorial(6)/2






# build Normal log-likelihood
normal_lik <- function(par)
{         
  par1<-par(1)
  par2<-par(2)
  
  n=length(xdata)
  vec=matrix(data=0,nrow=n,ncol=1)
  
  vec=dnorm(xdata, mean = par1, sd = par2)  
  
  sumlik=-sum(log(vec[1:n]));
  
  # Output
  cat("Normal log-likelihood ->",sprintf("%4.4f",- sumlik),"\n")
  
  return(sumlik)
}


xdata=rnorm(1000,2, 2)
# Log-likelihood for univariate model: Normal distribution
# estimate parameters
opts <- list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel"  = 1.0e-7, "maxeval"   = 1000 )
param0=as.vector(c(0,3))
nloptr(x0=param0, eval_f=normal_lik, lb=c(-0.999, 2.0001), ub=c(0.999, Inf), opts=opts)