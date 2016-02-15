source('~/Work/Filtrations/Bratteli/Chacon/ChaconTransfo_Numeric.R')
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
Kn(c(0L,1L,2L,1L,3L), 3)

# 8/27 = c(1,1)
Kn(c(1L,1L),2)
# 16/27 = c(3,3)
Kn(c(3L,3L),2)
# 22/27 = c(2,3)
Kn(c(2L,3L),2)
# 8/9 = c(4,2)
Kn(c(4L,2L),2)
## -> ça colle 

# loi de Kn 
nsims <- 5000
sims <- NULL
for(i in 1:nsims){
  eps <- reps(2L)
  if(eps[1]!=4L) sims <- c(sims,f(1,eps[1]))
}
barplot(table(sims))

# loi de Kn 
nsims <- 10000
sims <- integer(nsims)
for(i in 1:nsims){
  eps <- reps(2L)
  while(eps[2]==4L || eps[2]==2L){
    eps <- reps(2L)
  }
  sims[i] <- Kn(eps,2)
}
barplot(table(sims))


nsims <- 5000
sims <- integer(nsims)
for(i in 1:nsims){
  eps <- reps(2L)
  sims[i] <- Kn(eps,2)
}
barplot(table(sims))

