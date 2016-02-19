library(gmpadds)
cf <- c(0,rep(1,1000))
cf <- c(0, rbinom(1000, 3, 0.5)+1L)
theta <- cf2bigq(cf)

library(gmp) # tu ferais mieux de le mettre en Depends
theta <- as.bigq(0)
fact <- as.bigz(1)
for(i in 1:10){
  fact <- fact*as.bigz(i)
  theta <- theta + as.bigz(1)/(2L^fact)
}

N <- 100
theta_times_2powk <- as.bigq(integer(N))
for(k in 1:N){
  theta_times_2powk[k] <- frac(as.bigq(2)^(k-1L) * theta)
}
theta_times_2powk <- as.vector(theta_times_2powk)

# nb de passages en I 
set.seed(666)
X <- as.bigq(numeric(N))
X[1] <- as.bigq(runif(1))
epsilon <- as.bigq(rbinom(N, 1, .5)) #as.bigq(rep(1L,N)) #
for(i in 2:N){
  X[i] <- frac(X[i-1] + epsilon[i]*theta_times_2powk[k])
}
length(which(X<1/2))/N
length(which(X<3/4+0.1 & X>3/4))/N
length(which(X<0.2))/N
length(which(X<0.1))/N

length(which(X>1/4 & X<1/2))/N
length(which(X>1/4 & X<3/4))/N

curve(ecdf(as.numeric(X))(x))
abline(0,1,col="red")


### je dis que {k in [0, 2^(n-1)] | k est un K_i} vérifie H&P}
n <- 15
A <- integer(2^n)
K <- integer(n)
K[1] <- rbinom(1,1,0.5)
A[K[1]+1L] <- 1L
for(i in 2:n){
  K[i] <- K[i-1] + rbinom(1,1,0.5)*2L^(i-1L)
  A[K[i]+1L] <- A[K[i]+1L] + 1L
}
sum(A)

An <- A/n

# 
theta <- 2-sqrt(2)
itersRot <- numeric(2^n)
itersRot[1] <- runif(1)
for(i in 2:(2^n)){
  itersRot[i] <- (itersRot[i-1] + theta) %% 1
}
word <- itersRot < 1/2
table(rep(word, A)) # trop petit pour voir 

# Bernoulli 
N <- 10000
X <- character(N)
X[1] <-  sample(c("a","b"), 1)
epsilon <- rbinom(N, 1, .5)
for(i in 2:N){
  X[i] <- ifelse(epsilon[i]==0, X[i-1], sample(c("a","b"), 1))
}
table(X)


# v.a. indépedantes : ça marche - weakly mixing devrait se comporter pareil
N <- 10000
X <- numeric(N)
X[1] <-  runif(1)
epsilon <- rbinom(N, 1, .5)
for(i in 2:N){
  X[i] <- ifelse(epsilon[i]==0, X[i-1], runif(1))
}
length(which(X<1/2))/N
length(which(X<0.2))/N
length(which(X<0.1))/N


# Torus 
library(gmpadds)
cf1 <- c(0,rep(1,1000))
theta1 <- cf2bigq(cf1)
cf2 <- c(0,rep(2,1000))
theta2 <- cf2bigq(cf2)
library(gmp) # tu ferais mieux de le mettre en Depends
N <- 500
theta1_times_2powk <- theta2_times_2powk <- as.bigq(integer(N))
for(k in 1:N){
  theta1_times_2powk[k] <- frac(as.bigq(2)^(k-1L) * theta1)
  theta2_times_2powk[k] <- frac(as.bigq(2)^(k-1L) * theta2)
}
theta1_times_2powk <- as.vector(theta1_times_2powk)
theta2_times_2powk <- as.vector(theta2_times_2powk)

# nb de passages en I 
X1 <- X2 <- as.bigq(numeric(N))
X1[1] <- as.bigq(runif(1)); X2[1] <- as.bigq(runif(1))
epsilon <- as.bigq(rbinom(N, 1, .5))
for(i in 2:N){
  X1[i] <- frac(X1[i-1] + epsilon[i]*theta1_times_2powk[k])
  X2[i] <- frac(X2[i-1] + epsilon[i]*theta2_times_2powk[k])
}
length(which(X1<1/2 & X2<1/2))/N
length(which(X1<0.5 & X2<0.8))/N

# 2 copies indépendantes
cf <- c(0,rbinom(1000,2,.5)+1)
theta <- cf2bigq(cf)
library(gmp) # tu ferais mieux de le mettre en Depends
N <- 1000
theta_times_2powk <- as.bigq(integer(N))
for(k in 1:N){
  theta_times_2powk[k] <- frac(as.bigq(2)^(k-1L) * theta)
}
theta_times_2powk <- as.vector(theta_times_2powk)

# nb de passages en I 
X1 <- X2 <- as.bigq(numeric(N))
X1[1] <- as.bigq(runif(1)); X2[1] <- as.bigq(runif(1))
epsilon1 <- as.bigq(rbinom(N, 1, .5))
epsilon2 <- as.bigq(rbinom(N, 1, .5))
for(i in 2:N){
  X1[i] <- frac(X1[i-1] + epsilon1[i]*theta_times_2powk[k])
  X2[i] <- frac(X2[i-1] + epsilon2[i]*theta_times_2powk[k])
}
length(which(X1<1/2 & X2<1/2))/N
length(which(X1<0.5 & X2<0.8))/N
length(which(X1<1/2 & X2>1/2))/N
length(which(X1>0.5 & X2<0.4))/N

# distance S1 ?
# dists <- 
length(which(abs(X1-X2)<0.5))/N
length(which(abs(X1-X2)<0.81))/N
