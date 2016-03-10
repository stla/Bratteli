
library(gmp)
Chacon_num <- function(x){ # plante pour x=2/3 et x=4/9 si je mets pas as.numeric
  n <- 1L
  while(x >= as.numeric(1L - 1L/as.bigz(3)^(n+1))) n <- n+1L # si je mets >, pas d'image pour 8/9
  while(x >= as.numeric(as.bigq(2,3)- 2L/as.bigq(3)^(n+1L)) && x < as.bigq(2/3)) n <- n+1L
  i <- 1L; A <- as.bigq(c(0, 2, 6, 4), 9)
  while(i < n){
    A <- c(A, A + 2L/as.bigq(3)^(i+2L), 1L - 1L/as.bigz(3)^(i+1L), A + 4L/as.bigq(3)^(i+2L))
    i <- i+1L
  }
  dists <- x-as.numeric(A)
  j <- which(dists >= 0 & dists < as.numeric(2L/as.bigq(3)^(n+1L)))
  # à cette issue, x est dans le j-ième plateau de la n-ième tour (et pas dans le top)
  # les extrémités gauches de la n-ième tour sont données dans A
  return(as.numeric(A[j+1L])+dists[j]) # c'est l'image de x par Chacon 
}

Chacon_num_inverse <- function(x){
  n <- 1L
  while(x > 1L - 1L/as.bigz(3)^(n+1)) n <- n+1
  while(x < 2L/as.bigq(3)^(n+1L)) n <- n+1L
  i <- 1L; A <- as.bigq(c(0, 2, 6, 4), 9)
  while(i < n){
    A <- c(A, A + 2L/as.bigq(3)^(i+2L), 1L - 1L/as.bigz(3)^(i+1L), A + 4L/as.bigq(3)^(i+2L))
    i <- i+1L
  }
  dists <- x-as.numeric(A)
  j <- which(dists >= 0 & dists < as.numeric(2L/as.bigq(3)^(n+1L)))
  # à cette issue, x est dans le j-ième plateau de la n-ième tour (et pas dans le top)
  # les extrémités gauches de la n-ième tour sont données dans A
  return(as.numeric(A[j-1L])+dists[j]) # c'est l'image de x par Chacon 
}

