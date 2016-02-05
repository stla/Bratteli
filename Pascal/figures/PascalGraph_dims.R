setwd("~/Work/Filtrations/Bratteli/Pascal/figures")

library(myutils)

Pascal_Mn <- function(n){
  M <- matrix(0, nrow=n+1, ncol=n+2)
  for(i in 1:(n+1)){
    M[i, ][c(i, i+1)] <- 1
  }
  return(M)
}

N <- 4
Pascal_Dims <- Bdims(Pascal_Mn, N)

fun_Mn <- function(n){
  M <- Pascal_Mn(n)
  colnames(M) <- Pascal_Dims[[n+1]]
  return(M)
}

BgraphTikZ("PascalGraph_dims_R.tex", fun_Mn, N)


