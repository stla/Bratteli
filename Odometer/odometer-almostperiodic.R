
library(dyadic)

set.seed(666)
u <- runif(1)
niters <- 100
Ou <- odometer(u, niters=niters)
plot(Ou, type="l")

epsilon <- 0.05
i0 <- 2
abline(h=Ou[i0]+c(-1,1)*epsilon, lty="dashed", col="red")
for(i in i0:niters){
  if(abs(Ou[i]-Ou[i0])<epsilon) points(i, Ou[i], pch=19, col="red")
}

plot(c(u,Ou), type="l")
w <- cut(c(u,Ou), seq(0,1,len=6), right=FALSE)
points(c(u,Ou), col=w, pch=19)


f <- function(u) LETTERS[as.integer(u<2/3)+1]
paste(f(Ou[1:8]),collapse="-")


