
library(dyadic)
library(magrittr)

returntime <- function(u, inP){
  u <- dyadic2num(odometer(num2dyadic(u)))
  k <- 1
  while(!inP(u)){
    u <- dyadic2num(odometer(num2dyadic(u)))
    k <- k+1
  }
  return(k)
}

### possibilié 1 
inP <- function(u){
  !((u < 1/2) && (u>=1/4))
}
u <- seq(0.001, 0.99, length.out = 140) # seq(1/2^10, 1-1/2^10, by=1/2^10)
i <- sapply(u, inP)
u <- u[which(i)]
Ru <- numeric(length(u))
for(i in 1:length(u)){
  Ru[i] <- u[i] %>% returntime(inP)
}
plot(u, Ru)
abline(v=1/2, col="red"); abline(v=3/4, col="red")

### possibilié 2 : marche aussi - C'EST CELLE-CI
inP <- function(u){
  u < 3/4
}
u <- seq(0.001, 0.99, length.out = 140) # seq(1/2^10, 1-1/2^10, by=1/2^10)
i <- sapply(u, inP)
u <- u[which(i)]
Ru <- numeric(length(u))
for(i in 1:length(u)){
  Ru[i] <- u[i] %>% returntime(inP)
}
plot(u, Ru)
abline(v=1/2, col="red"); abline(v=3/4, col="red")

# induite : n'a pas l'air d'être l'odo - p-ê le (3, 3, 3 ..)-odomètre ? 
#   (le grand machin est 2/3 du tout)
# ou le 3 - 4 - 4
inducedP <- function(u){
  if(u<1/4 || u>1/2) return(dyadic2num(odometer(num2dyadic(u)))) else
    return(dyadic2num(odometer(odometer(num2dyadic(u)))))
}
IPu <- sapply(u, inducedP)
plot(u, IPu)
abline(v=1/2, col="red"); abline(v=1/4, col="red")
abline(v=0.25+0.5*1/4, col="green")
abline(v=0.25+0.5*1/4 + 0.5*1/8, col="blue")

#### 2ème cut-stack en prenant la poss 2
inP <- function(u){
  (u < 1/8) || ((u>1/4 && u<1/2)) || (u>1/2 && u<5/8)
}
u <- seq(0.001, 0.99, length.out = 140) # seq(1/2^10, 1-1/2^10, by=1/2^10)
i <- sapply(u, inP)
u <- u[which(i)]
Ru <- numeric(length(u))
for(i in 1:length(u)){
  Ru[i] <- u[i] %>% returntime(inP)
}
plot(u, Ru)
abline(v=1/2, col="red"); abline(v=3/4, col="red")
