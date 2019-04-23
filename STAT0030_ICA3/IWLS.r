#
# This is an R function to illustrate the use of Iterative Weighted
# Least Squares for estimating the log of a Poisson mean. The arguments
# are y (a vector of counts, assumed IID Poisson) and startval, an
# initial guess at the log mean. The `Steps' refer to Section 3 of the
# Lab 8 lecture notes.
#
IWLS <- function(y,startval) {
  n <- length(y)                  #          For dimensioning
  X <- as.matrix(rep(1,n))        # Step 1:  assemble the matrix X
  betahat <- startval             # Step 2:  initial value
  U <- 10                         #          Define a value for the 
                                  #          score, U (this is just 
                                  #          so that the test for
                                  #          convergence on the next
                                  #          line doesn't fail on the
                                  #          first attempt because we
                                  #          haven't defined U yet)
  iter <- 0                       #          Initialise iteration count
  while(abs(U) > 1e-6) {
    eta <- as.vector(X%*%betahat) # Step 3:  calculate linear
    mu <- exp(eta)                #          predictors, means
    V <- mu                       #          and variances
    W <- (mu^2)/V                 # Step 4:  diagonal elements of W
    z <- eta + ( (y-mu)/mu )      # Step 5:  adjusted dependent variate
    XW <- t(W*X)                  # Step 6:  calculation of X'W
                                  #          (uses elementwise
                                  #          multiplication, exploiting
                                  #          the fact that W will be 
                                  #          recycled to match the number
                                  #          of elements in X)
    XWX <- solve(XW%*%X)          # Step 7:  calculation of [X'WX]^-1,
    XWz <- XW%*%z                 #          X'Wz and U
    U <- XW%*%(z-eta)
    cat(paste("Iteration",iter,   #          Output current values to
              " Estimate",        #          screen (rounded to a
              round(betahat,6),   #          sensible number of decimal
              " Score",           #          places)
              round(U,8),"\n"))   #
    betahat <- XWX%*%XWz          # Step 8:  update betahat, and go back
    iter <- iter + 1              #          if necessary
  }
  phi <- 1                        # Step 9:  not strictly necessary here
  beta.se <- sqrt(diag(XWX))      # Step 10: calculate standard errors
  mle.table <-
    data.frame(Estimate=betahat,  # Step 11: assemble results into a 
               S.E.=beta.se,      #          data frame, and return
               T=betahat/beta.se) #
  mle.table               
}

#Use read.table to read the data into R
storm.data <- read.table("/Users/hongwei/Documents/GitHub/STAT/STAT0030_ICA3/nstorms.dat",header = T)

#Produce a plot showing the number of storms in each year (i.e. a line graph with Year on the x-axis and Storms on the y-axis)
plot(storm.data$Year,storm.data$Storms,type= l)
hist(storm.data$V1)
