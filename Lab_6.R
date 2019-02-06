for (i in 1:5) print(i)
print(1:5)
sum1 <- 0
sum2 <- 0
for (i in 1:5) {
  sum1 <- sum1+i
  sum2 <- sum2+i^2
  cat("i=",i,"sum1=",sum1,"sum of square",sum2,"\n")
}
for (theta in c(0, pi, 2*pi)) print(sin(theta))
for (let in letters[c(20,15,13)]) cat(let); cat("\n")

n <- 0
nfac <- 1
while (nfac < 10000) {
  n <- n+1
  nfac <- nfac*n
  }
nfac

x <- 2.1
y <- 2.5
while (x < y) print(x)

log.base.a <- function(x,a=10) log(x)/log(a)
log.base.a(3.5,5)
log(3.5,base=5)

x <- 2
inc <- function(x) x+1
inc(4)
x

is.factorial <- function(n)
{
  i <- 0
  ifac <- 1
  while (ifac < n) {
    i <- i+1
    ifac <- ifac*i
  }
  ifac==n
}
is.factorial(6)

a <- is.factorial(12)

sillysum <- function(x) {
  z <- 0
  for (i in 1:x) z <- z+i
  z
}
system.time(sillysum(1e6))
system.time(sum(as.numeric(1:1e6)))

log.base.a <- function(x,a=10) {
  if (x <= 0) stop("x must be strictly positive")
  log(x)/log(a)
  }
log.base.a(-2)

general.sqrt <- function(x) {
  #
  # This function returns the square root of
  # any real numeric, whether positive or negative.
  #
  # As usual, you can (and should!) include comments in your functions.
  #
  if(x>=0) return(sqrt(x))
  #
  # No need for "else", because if x is positive then we never get to here
  #
  complex(real=0,imaginary=sqrt(-x))
}
general.sqrt(-1)

convert.temp <- function(fahrenheit,...)
{
  celsius <- (fahrenheit-32)*5/9
  plot(fahrenheit,celsius,...)
  celsius
}
convert.temp(seq(0,100,10),main="Celsius against Fahrenheit")

boxplot

getS3method("boxplot","default")

mymatrix <- matrix(rnorm(80), ncol = 10)
apply(mymatrix, 2, mean)

trapezium <- function(v,a,b) {
  n <- length(v)-1
  h <- ????? ## as defined above
    intv <- ????? ## intv is the calculated integral. You may
    ## need a couple of lines for this, but
    ## DO NOT USE A LOOP!!!
    # and return intv
    ????
}
