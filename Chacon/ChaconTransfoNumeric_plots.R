source('~/Work/Filtrations/Bratteli/Chacon/ChaconTransfo_Numeric.R')

library(magrittr)

u <- seq(0, 0.995, by=.005) 
Cu <- numeric(length(u))
for(i in seq_along(u)){
  Cu[i] <- Chacon_num(u[i])
}
plot(u,Cu, pch=19, cex=0.5, asp=1)

u <- seq(0.05, 0.95, length.out = 50) 
Cu <- numeric(length(u))
for(i in seq_along(u)){
  Cu[i] <- Chacon_num_inverse(u[i])
}
plot(u,Cu, pch=19, cex=0.5, asp=1)

( u <- runif(1) ) 
Chacon_num_inverse(Chacon_num(u))
Chacon_num(Chacon_num_inverse(u))

set.seed(666)
u <- runif(1)
Cu <- u
for(i in 1:50){
  u <- Chacon_num(u)
  Cu <- c(Cu,u)
}
plot(Cu, type="l")


### return time 

#
returntime <- function(u, inP){
  u <- Chacon_num(u)
  k <- 1
  while(!inP(u)){
    u <- Chacon_num(u)
    k <- k+1
  }
  return(k)
}

inP <- function(u){
  (u < 2/3) 
}

u <- seq(0, 0.98, length.out = 250)
i <- sapply(seq_along(u), function(i) inP(u[i]))
u <- u[which(i)]
Ru <- numeric(length(u))
for(i in 1:length(u)){
  Ru[i] <- u[i] %>% returntime(inP)
}
plot(u, Ru, pch=19, cex=.5)

# induced - triadic odometer 
Iu <- numeric(length(u))
for(i in 1:length(u)){
  r <- Ru[i]
  Iu[i] <- ifelse(r==1, Chacon_num(u[i]), Chacon_num(Chacon_num(u[i])))
}
plot(u, Iu, pch=19, cex=.5)
abline(v=2/3*2/3, col=2)
