IWLS <- function(y,X=1,startval=1) {
  #######Check_y#######
  if(is.null(y)) 
    stop("Data Y cannot be NULL")   #Step 0:  check Y is Null
  if(!(y==sapply(y, round)&&y>0)) #        int and positive or not
    stop("Data Y should be integer and positive.")
  #######check_X#######
  n <- length(y)                  #          For dimensioning
  if(X==1) {
    X <- as.matrix(rep(1,n))      #          assemble the matrix X
  } else {
  if(dim(X)[1]!=length(y))        #          check X has the same dim as Y
    stop("Data X should be the same raw as Y.")
  X <- cbind(as.matrix(rep(1,n)),X)# Step 1:  cbind the Constant term
  }
  #######IWLS#######
  betahat <- startval             # Step 2:  initial value
  U <- 10                         #          Define U 
  iter <- 0                       #          Initialise iteration count
  while(abs(U) > 1e-6) {
    eta <- as.vector(X%*%betahat) # Step 3:  calculate linear
    mu <- exp(eta)+1              #          predictors, means
    V <- mu^2-mu                  #          and variances
    W <- ((mu-1)^2)/V             # Step 4:  diagonal elements of W
    z <- eta + ( (y-mu)/(mu-1))   # Step 5:  adjusted dependent variate
    XW <- t(W*X)                  # Step 6:  calculation of X'W
    XWX <- solve(XW%*%X)          # Step 7:  calculation of [X'WX]^-1,
    XWz <- XW%*%z                 #          X'Wz and U
    U <- XW%*%(z-eta)             #          U is the score vector
    D <-                          #          the residual sum of squares 
      2*n*mu[1]-2*sum(y)+
      2*y%*%log(y/mu)
    cat(paste("Iteration",iter,   #          Output current values to
              " Estimate",        #          screen (rounded to a
              round(betahat,6),   #          sensible number of decimal
              " Score",           #          places)
              round(U,8),
              " Deviance",
              D,"\n"))   
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

X <- storm.data[,c(3,4)]
X[is.na(X)] <- 0
X <- as.matrix(X)
IWLS(storm.data$Storms,X,c(1,2,3))

