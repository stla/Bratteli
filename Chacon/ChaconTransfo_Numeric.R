
library(gmp)
Chacon_num <- function(x){
  n <- 1L
  while(x > 1L - 1L/as.bigz(3)^(n+1)) n <- n+1
  while(x >= as.bigq(2,3)- 2L/as.bigq(3)^(n+1L) && x < as.bigq(2/3)) n <- n+1L
  i <- 1L; A <- as.bigq(c(0, 2, 6, 4), 9)
  while(i < n){
    A <- c(A, A + 2L/as.bigq(3)^(i+2L), 1L - 1L/as.bigz(3)^(i+1L), A + 4L/as.bigq(3)^(i+2L))
    i <- i+1L
  }
  dists <- x-as.numeric(A)
  j <- which(dists >= 0 & dists < 2L/as.bigq(3)^(n+1L))
  # à cette issue, x est dans le j-ième plateau de la n-ième tour (et pas dans le top)
  # les extrémités gauches de la n-ième tour sont données dans A
  return(as.numeric(A[j+1])+dists[j]) # c'est l'image de x par Chacon 
}

u <- seq(0, 0.99, by=.01) 
Cu <- numeric(length(u))
for(i in seq_along(u)){
  Cu[i] <- Chacon_num(u[i])
}

plot(u,Cu, pch=19, cex=0.5)
