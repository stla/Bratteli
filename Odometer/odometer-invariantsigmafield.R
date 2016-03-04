
setwd("~/Work/Filtrations/Bratteli/Odometer")

library(magrittr)
library(dyadic)

T4 <- Vectorize(function(u){
  odometer(u, niters=4)[4]
})

u <- seq(0, .999, len=100)
plot(u, T4(u))
abline(h=c(.5,.75))
abline(v=c(.5,.75))


T8 <- Vectorize(function(u){
  odometer(u, niters=8)[8]
})
u <- seq(0, .999, len=100)
plot(u, T8(u))
abline(h=c(.5,.75))
abline(v=c(.5,.75))
