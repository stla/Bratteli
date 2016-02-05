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
Mn <- Bwalk(Pascal_Mn, N, 3)
Mn_copy <- Mn

library(gmp) # ajouter usepackage{units}
fedgelabels <- Vectorize(function(n, from, to){
  frac <- as.bigq(nchar(colnames(Mn_copy[[n+1]])[to]), nchar(rownames(Mn_copy[[n+1]])[from]))
  deno <- as.character(denominator(frac))
  nume <- as.character(numerator(frac))
  if(deno=="1") return("1") else return(sprintf("\\nicefrac{%s}{%s}", nume, deno))
}
)

for(i in 1:N){
  M <- Mn[[i]]
  for(j in 1:ncol(M)){
    k <- which(letters==stringr::str_sub(colnames(M)[j], 1, 1)) - 1L
    colnames(M)[j] <- ifelse(k==0, "x", ifelse(k==1, "Tx", sprintf("T^{%s}x", k)))
  }
  Mn[[i]] <- M
}

# fedgelabels <- Vectorize(function(n, from, to){
#   colnames(Mn[[n+1]])[to]
# })

BgraphTikZ("PascalZn_R.tex", function(n) Mn[[n+1]], N, 
           fedgelabels = fedgelabels, 
           ROOTLABEL=sprintf("Z_%s=x", N),
           mirror=TRUE)

