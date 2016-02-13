
source('~/Work/Filtrations/Bratteli/Chacon/ChaconTransfo.R')


checkeps(c(0L,1L))
checkeps(c(2L,1L))
checkeps(c(4L,2L))
checkeps(c(1L,2L))
checkeps(c(3L,4L)) 
checkeps(c(4L, 4L, 3L))
checkeps(c(4L, 4L, 2L))


T(c(0L, 0L))
T(c(3L, 1L))
T(c(4L, 2L))
T(c(2L, 3L))
T(c(3L, 3L)) 
T(c(3L, 3L, 1L))
T(c(0L, 0L, 0L))
T(c(4L, 4L, 2L))
T(c(4L, 4L, 3L))
T(c(3L, 3L, 3L, 1L)) 
T(c(3L, 3L, 3L, 3L, 0L))  


eps <- rep(0L, 4)
niters <- 30
iters <- matrix(integer(length(eps)*niters), nrow=niters)
iters[1, ] <- eps
for(i in 2:niters){
  iters[i, ] <- T(iters[i-1L, ])
}



eps <- 0
while(eps[1]!=4L) eps <- reps()
eps
T(eps)

( eps <- reps() )
T(eps)

# check first coord ergodic
niters <- 100
iters <- integer(niters)
eps <- reps()
iters[1] <- eps[1]
Eps <- list(eps)
for(i in 2:niters){
  eps <- T(eps)
  iters[i] <- eps[1]
  Eps[[i+1]] <- eps
}

table(iters)/niters
