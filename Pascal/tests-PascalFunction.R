setwd("~/Work/Filtrations/Bratteli/Pascal")

library(dyadic)
library(magrittr)

d <- c(0, 0, 1, 1, 0, 1, 0, 0, 0)
i <- which.max(d)
m1 <- i-1L
j <- which.min(d[-(1:i)])
k1 <- j-1L
# check 
all(d[1:(m1+k1+2L)] == c(rep(0L, m1), rep(1L, k1), 1L, 0L))
  
mk <- function(d){
  d <- as.integer(d)
  i <- which.max(d)
  m1 <- i-1L
#   if(i==length(d)) d <- c(d, 0L) # cas d = 000001
#   if(i == 1L) d <- c(d, 0L) # cas d = 1111
#   if(all(d[-(1:i)]==1)) d <- c(d, 0L) # cas d=0001111
#   # finalement suffit de rajouter un 0 !
  d <- c(d, 0L)
  j <- which.min(d[-(1:i)])
  k1 <- j-1L
  # check
  if(!all(d[1:(m1+k1+2L)] == c(rep(0L, m1), rep(1L, k1), 1L, 0L))) stop("Incorrect result")
  return(c(m1,k1))
}


pascal <- function(d){
  d <- as.dyadic(d)
  m1k1 <- mk(d)
  begin <- c(rep(1L, m1k1[2]), rep(0L, m1k1[1]+1L), 1L)
  if(length(d)==sum(m1k1)+1) d <- begin else d[1:(sum(m1k1)+2)] <- begin
  return(d)
}

d <- c(0, 0, 1, 1, 0, 1, 0, 0, 0)
pascal(d)

u <- seq(1/2^10, 1-1/2^10, by=1/2^10)
Pu <- numeric(length(u))
for(i in 1:length(u)){
  Pu[i] <- u[i] %>% num2dyadic %>% pascal %>% dyadic2num
}
plot(u, Pu)



pascal_dyadic <- function(d){
  d <- as.dyadic(d)
  m1k1 <- mk(d)
  begin <- c(rep(1L, m1k1[2]), rep(0L, m1k1[1]+1L), 1L)
  if(length(d)==sum(m1k1)+1) d <- begin else d[1:(sum(m1k1)+2)] <- begin
  return(d)
}
pascal_num <- function(u){
  return(dyadic2num(pascal_dyadic(num2dyadic(u))))
}

u <- runif(100)
Pu <- numeric(length(u))
for(i in 1:length(u)){
  Pu[i] <- pascal_num(u[i])
}
plot(u, Pu)
