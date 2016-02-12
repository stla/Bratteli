
setwd("~/Work/Filtrations/Bratteli/Odometer")

# EN FAIT CA N'A RIEN A VOIR AVEC BACKWARD

library(magrittr)

# {x1=1}
# <=> [1/2, 1[
# {x1=0, x2=0, x3=1}
# <=> [1/8, 1/4[
# {x1=0, x2=0, x3=0, x4=0, x5=1}
# <=> [1/32, 1/16[
# 
plot(0,0, type="n", xlim=c(0,1), ylim=c(0,2))
for(i in 0:4){
  segments(1/2^(2*i+1), 1, 1/2^(2*i), 1)
}

inA.dyadic <- function(d){
  gtools::odd(which.max(d))
}
inA.dyadic(c(0,0,1,0))
inA.dyadic(c(0,0,0,1))
inA.dyadic(c(1,0,0,0))
inA.u <- function(u){
  sapply(u, function(u) inA.dyadic(myutils::num2dyadic(u)))
}

# 
d <- c(1, 0, 0, 1, 0, 1, 1, 0)
u <- myutils::dyadic2num(d)
traj <- odometer.u(u, niters=20, image="forward")
sapply(c(u, traj), inA.u) %>% as.integer %>% {.+1} %>% {LETTERS[.]} %>% paste0(collapse="-")
# => 2 consécutifs : BA ou BB
# => 4 consécutifs : BABB ou BABA
d <- c(0, 0, 0, 1, 0, 1, 1, 0)
u <- myutils::dyadic2num(d)
traj <- odometer.u(u, niters=20, image="forward")
sapply(c(u, traj), inA.u) %>% as.integer %>% {.+1} %>% {LETTERS[.]} %>% paste0(collapse="-")
# => 2 consécutifs : AB ou BB
# => 4 consécutifs : BBAB ou ABAB
d <- c(0, 1, 0, 1, 0, 1, 1, 0)
u <- myutils::dyadic2num(d)
traj <- odometer.u(u, niters=20, image="forward")
sapply(c(u, traj), inA.u) %>% as.integer %>% {.+1} %>% {LETTERS[.]} %>% paste0(collapse="-")
# => 4 consécutifs : ABBB ou ABAB



### return times and induced transfo 
returntime <- function(u, inP){
  iters <- odometer.u(u, niters=10)
  which.max(inP(iters))
}
u <- NULL
for(i in 0:5){
  u <- c(u, seq(1/2^(2*i+1), 1/2^(2*i)-1*.Machine$double.eps, length.out = 2^(9-i)))
}
plot(u, numeric(length(u)))  
n <- sapply(u, function(u) returntime(u, inA.u))
plot(u,n, pch=".")
length(which(n==2))/length(u)

# induced transfo
inducedP <- function(u, inP){
  n <- returntime(u, inP)
  return(odometer.u(u, niters=n)[n])
}
y <-  sapply(u, function(u) inducedP(u, inA.u))
plot(u,y)








# 
u <- seq(1/2+3/16, 0.999, length.out = 100)
O2u <- sapply(u, function(u) odometer.u(u, niters=2, image="backward")[2]) 
# 
rdyadic <- function(nsims){
  out <- matrix(0L, ncol=52, nrow=nsims)
  for(i in 1:nsims){
    out[i,] <- rbinom(52, 1, .5)
  }
  return(out)
}

sims <- rdyadic(1000)

# ( traj <- as.integer(apply(sims, 1, inA.dyadic)) )

# trajectoire d'un point - attention, @AnthonyQuas prend O^{-1}



