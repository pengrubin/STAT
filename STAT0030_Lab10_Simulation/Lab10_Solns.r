##################################################
# R code for STAT0030 Lab 10 on Statistical simulation #
##################################################
#
#
# Define congruential generator:
test.cg <- function(n=1,y=9) {
    out <- rep(NA,n)
    for(i in 1:n) {
        y <- (1229*y+1) %% 2048
    out[i] <- y/2048
    }
    out
}
#
##################################################
# Exercises: sampling from various distributions #
##################################################
#
# Using the test generator test.cg() above and compare 
# results with R results. 
# 
# Number of samples in comparison (choose an even S):
S <- 1000
U1 <- test.cg(S)
U2 <- runif(n=S,0,1)
#
# Function for printing comparison to the screen:
compare <- function(X1,X2, Mean=NA, Var=NA,digits=5){
  cat("\n______________________________________")
  cat("\nTest with",length(X1),"values for both methods:\n")
  mns <- c(mean(X1),mean(X2),Mean)
  sds <- c(sd(X1),sd(X2),sqrt(Var))
  rnames <- c("Method 1","Method 2","Theoretical")
  test.table <- data.frame(Mean=mns,SD=sds, row.names=rnames)
  print(round(test.table,digits=digits))
  cat("______________________________________\n\n")
}
#
# 1. Uniform(-30,10)
X1 <- 40*U1-30
X2 <- 40*U2-30
cat("\nDistribution U(-30,10):")
compare(X1,X2,Mean=-10,Var=40^2/12)
#
# 2. Bernouilli(0.6):
X1 <- as.numeric(U1 <= 0.6)
X2 <- as.numeric(U2<= 0.6)
cat("\nDistribution Bernouilli(0.6):")
compare(X1,X2,Mean=0.6,Var=0.6*0.4)
#
# 3. Exponential(0.5):
# Use eps to deal with values from U(0,1) very close to zero: 
eps <- 10e-10
lambda <- 1/2
X1 <- -1/lambda*log(U1+eps); R1<-X1
X2 <- -1/lambda*log(U2); R2<-X2
cat("\nDistribution Exponential(0.5):")
compare(X1,X2,Mean=1/lambda,Var=1/lambda^2)
# 
# 3a. Normal(2,5) using central limit:
Z1 <- rep(NA,S)
Z2 <- rep(NA,S)
for(i in 1:S){
  # Mind the seed y when using test.cg:
  U1.12 <- test.cg(12,y=12*i);  Z1[i]<-sum(U1.12)-6
  U2.12 <- runif(n=12,0,1); Z2[i]<-sum(U2.12)-6
}
X1 <- 2+sqrt(5)*Z1
X2 <- 2+sqrt(5)*Z2
cat("\nDistribution Normal(2,5) using central limit:")
compare(X1,X2,Mean=2,Var=5)
#
# 3b. Normal(2,5) using Box-Mueller:
Z1 <- sqrt(R1)*cos(2*pi*U1)
Z2 <- sqrt(R2)*cos(2*pi*U2)
# Transform:
X1 <- 2+sqrt(5)*Z1
X2 <- 2+sqrt(5)*Z2
cat("\nDistribution Normal(2,5) using Box-Mueller:")
compare(X1,X2,Mean=2,Var=5)
#
# 4. From F(x)=1/(1+e^(-x)) (logistic distribution):
# Check: integration of f(x) over R is 1
# F^(-1)(y)=-log(1-1/y)
# Transform:
X1 <- log(U1/(1-U1))
X2 <- log(U2/(1-U2))
cat("\nDistribution F(x)=1/(1+e^(-x)) using inversion method:")
compare(X1,X2,Mean=0,Var=pi^2/3)
#
# 5. Beta(2,3.5) using rejection method:
a <- 2
b <- 3.5
# Parameters a,b > 1, so there is a maximum of f(x):
# For method 1:
U <- U1[1:(S/2)]
Y <- U1[(S/2)+1:S]
threshold <- Y^(a-1)*(1-Y)^(b-1)*( (a+b-2)^(a+b-2) )/( (a-1)^(a-1)*(b-1)^(b-1) )
X1 <- ifelse(U<threshold,Y,NA)
# For method 2:
U <- U2[1:(S/2)]
Y <- U2[(S/2)+1:S]
threshold <- Y^(a-1)*(1-Y)^(b-1)*( (a+b-2)^(a+b-2) )/( (a-1)^(a-1)*(b-1)^(b-1) )
X2 <- ifelse(U<threshold,Y,NA)
# Select only sampled values:
size <- min(sum(!is.na(X1)),sum(!is.na(X2)))
X1 <- X1[!is.na(X1)]; X1<-X1[1:size]
X2 <- X2[!is.na(X2)]; X2<-X2[1:size]
cat("\nDistribution Beta(2,3.5) using rejection method:")
compare(X1,X2,Mean=a/(a+b),Var=a*b/((a+b)^2*(a+b+1)))
