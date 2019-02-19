#SN:17052480
#question_2_a
rawdata <- read.table("osl.dat", #input data
                      header=TRUE) #the first line as the names of the variables 

#question_2_b
par(mfrow=c(2,1))
plot(rawdata$de,rawdata$se,
     ylab="Standard Errors" , #add the ylab title
     xlab="Equivalent Dose", #add the xlab title
     main="Standard Errors against Equivalent Dose" )#add the main title
hist(rawdata$de,
     xlab="Equivalent Dose" , #add the xlab title
     main="Equivalent Dose by Frequency" )#add the main title
text(3,40,paste0("the number of grains is ",length(rawdata$de),
                 "\nthe mean of the equivalent doses is ",format(mean(rawdata$de),digits = 4),
                 "\nthe standard deviation of the equivalent doses is ",format(sd(rawdata$de),digits = 4)))
     
#question_2_c
negll <- function(params,dat) {
  l <- -0.5*sum(log(params[2]^2+dat[,2]^2)+
                  (dat[,1]-params[1])^2/(params[2]^2+dat[,2]^2)) #the log-likelihood omitting the constant term
  return(-l) #the negative log-likelihood
}

#question_2_d
dat <- rawdata[,c(2,3)]
params1 <- c(mean(rawdata[,2]),sd(rawdata[,2])) #params1 use the sd(rawdata[,2])
cat("the the negative log-likelihood value by params1 is ",negll(params1,dat))
params2 <- c(mean(rawdata[,2]),mean(rawdata[,3])) #params2 use the mean(rawdata[,3])
cat("the the negative log-likelihood value by params2 is ",negll(params2,dat))

#question_2_e
est <- nlm(negll,c(1,5),dat=rawdata[,c(2,3)],hessian=T) #nlm estimation
utils::str(nlm(negll,c(1,5),dat=rawdata[,c(2,3)]),hessian=T) #control the output
cat("the estimates of μ and σ are ",est$estimate[1]," and ",est$estimate[2])

#question_2_f
se<-diag(solve(est$hessian)) #the parameters standard errors for these estimates
cat("the μ and σ standard errors for these estimates are ",se[1]," and ",se[2])

