# CONCLUSION: N'A PAS L'AIR ISOMORPHE

source('~/Work/Filtrations/Bratteli/Pascal/Pascal_00.R')
library(magrittr)

returntime <- function(u, inP){
  u <- pascal_num(u)
  k <- 1
  while(!inP(u)){
    u <- pascal_num(u)
    k <- k+1
  }
  return(k)
}

### 1er cuting-stacking ###
inP <- function(u){
  (u < 1/4) || (u>=1/2)
}

u <- c(seq(0.001, 0.25, length.out = 70), seq(0.5, 0.99, length.out = 140)) # seq(1/2^10, 1-1/2^10, by=1/2^10)
Ru <- numeric(length(u))
for(i in 1:length(u)){
  Ru[i] <- u[i] %>% returntime(inP)
}
plot(u, Ru)
abline(v=1/2, col="red"); abline(v=3/4, col="red")

inducedP <- function(u){
  if(u<1/4 || u>3/4) return(pascal_num(u)) else
    return(pascal_num(pascal_num(u)))
}
IPu <- sapply(u, inducedP)
plot(u, IPu)
abline(v=1/4, col="red"); abline(v=1/2, col="red")
abline(h=1/4, col="red"); abline(h=1/2, col="red")

### 2ème cutting-stacking ###
inP <- function(u){
  (u < 1/8) || (u >= 1/2 && u < 5/8) || (u >= 3/4)
}

u <- seq(1/2^10, 1-1/2^10, by=1/2^10)
i <- sapply(seq_along(u), function(i) inP(u[i]))
u <- u[which(i)]
Ru <- numeric(length(u))
for(i in 1:length(u)){
  Ru[i] <- u[i] %>% returntime(inP)
}
plot(u, Ru)
abline(v=1/2, col="red"); abline(v=5/8, col="red")
abline(v=3/4, col="red"); abline(v=7/8, col="red")
