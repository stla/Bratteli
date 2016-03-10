source('~/Work/Filtrations/Bratteli/Chacon/ChaconTransfo.R')

dn <- function(n) (3L^(n+1)-1L)/2L
f <- Vectorize(function(n,k){ # attention car dans R j'indexe à partir de n=1
  if(k==0L || k==4L) return(0L)
  if(k==1L) return(dn(n-1L))
  if(k==2L) return(2L*dn(n-1L))
  if(k==3L) return(2L*dn(n-1L)+1L)
})

Kn <- function(eps, n){
  sum(f(1:n,eps[1:n]))
}

## processus Vn
nsims <- 10000
N <- 5
Vnsims <- matrix(integer(1), nrow=nsims, ncol=N)
for(i in 1:nsims){
  eps <- reps(N)
  for(n in 1:N){
    Vnsims[i,n] <- ifelse(eps[n] %in% c(2,4), 1L, 0L)
  }
}
colMeans(Vnsims)
1/3^(1:N)

## choix de T 
Rot <- function(x, n=1)  (x + n*(2-sqrt(2))) %% 1

## process (Z_n, epsilon_n)
nsims <- 10000
N <- 5
Zsims <- matrix(numeric(1), nrow=nsims, ncol=N)
Epssims <- Vsims <- matrix(integer(1), nrow=nsims, ncol=N)
Zsims[,1] <- runif(nsims)
for(i in 1:nsims){
  eps <- reps(N)[1:N] -> Epssims[i,]
  for(n in 1:N){
    Vsims[i,n] <- ifelse(eps[n] %in% c(2,4), 2L, 1L)
  }
  for(j in 2:N){
    Zsims[i, j] <- Rot(Zsims[i,1], Kn(eps,j))
  }
}

curve(ecdf(Zsims[,N])(x)); abline(0,1,col="red")
# check independence 
n <- 4
u <- 0.25 
# Pr(Zn <u, Vn=1)
I <- which(Zsims[,n]<u & Vsims[,n]==1L)
length(I)/nsims
u * (1-1/3^n)

