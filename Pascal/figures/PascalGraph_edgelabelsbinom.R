setwd("~/Work/Filtrations/Bratteli/Pascal/figures")

library(myutils)

Pascal_Mn <- function(n){
  M <- matrix(0, nrow=n+1, ncol=n+2)
  for(i in 1:(n+1)){
    M[i, ][c(i, i+1)] <- 1
  }
  return(M)
}

N <- 5
Pascal_Dims <- Bdims(Pascal_Mn, N)
fun_Mn <- function(n){
  M <- Pascal_Mn(n)
  colnames(M) <- Pascal_Dims[[n+1]]
  return(M)
}

fedgelabels <- Vectorize(function(n, from, to){
  if(from==1) return(0)
  if(to==from+1) return(0)
  return(choose(n, from-2))
})
BgraphTikZ("PascalGraph_edgelabelsbinom_R.tex", fun_Mn, N=N, fedgelabels = fedgelabels)


