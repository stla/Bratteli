
phi <- (1+sqrt(5))/2
theta <- 1/(1+phi) # = 1/phi²
# 
fibo <- function(n) as.integer(gmp::fibnum(abs(n-1)))
# Pr(Vn=1)
pn <- function(n) fibo(n) * (-1)^(n%%2+1) * (fibo(n+1) - fibo(n-1) * theta)
pinf <- 1 - phi/sqrt(5) # = theta/(1+theta)
pn(-30); pinf

n <- -5
fibo(n)/(phi*fibo(n-1)+fibo(n))
pn(n)


# transition n -> n+1
Pn <- function(n) matrix(
  c(fibo(n)/fibo(n-1), fibo(n+1)/fibo(n-1), 1L, 0L), 
  byrow=TRUE, nrow=2)
n <- -3
t(c(1-pn(n), pn(n)))%*%Pn(n); c(1-pn(n+1), pn(n+1))
# 
Pinf <- matrix(
  c(1/phi, 1/phi^2, 1L, 0L), 
  byrow=TRUE, nrow=2)
Pn(-30); Pinf

# 
psi <- -1/phi
n <- -5
(phi^(abs(n)+1)-psi^(abs(n)+1))/sqrt(5)
fibo(n)

contfrac::CF(c(0,3,1), finite=TRUE)

library(contfrac)
convergents(c(0,2,1,1,1,1,1,1,1))


