setwd("~/Work/Filtrations/Bratteli/Pascal/figures")
library(myutils)

Pascal_Mn <- function(n){
  M <- matrix(0, nrow=n+1, ncol=n+2)
  for(i in 1:(n+1)){
    M[i, ][c(i, i+1)] <- 1
  }
  return(M)
}

N <- 3
Mn <- Bwalk(Pascal_Mn, N, 3)

fedgelabels <- Vectorize(function(n, from, to) as.character(gmp::as.bigq(nchar(colnames(Mn[[n+1]])[to]), nchar(rownames(Mn[[n+1]])[from]))))

BgraphTikZ("PascalWalk_R.tex", function(n) Mn[[n+1]], N, 
           fedgelabels = fedgelabels, 
           ROOTLABEL=rownames(Mn[[1]]),
           mirror=TRUE)

