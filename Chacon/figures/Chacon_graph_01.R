setwd("~/Work/Filtrations/Bratteli/Chacon")

Chacon <- function(n){
  if(n==0) return(t(c(1L,1L)))
  return(t(matrix(c(3L, 1L, 0L, 1L), byrow=TRUE, nrow=2)))
}

N <- 4

#### Graphe avec les labels ####

fedgelabels <- Vectorize(function(n, from, to, mindex){
  if(n==0) return(".")
  if(from==2) return(ifelse(to==1, "2", "0")) 
  if(mindex %in% c(1,2)) return(mindex-1) else return(3)
})
myutils::BgraphTikZ("Chacon_Labels_R.tex", Chacon, N=N, 
                    bending=2,
                    fedgelabels=fedgelabels)


#### Graphe avec les probas et les dimensions ####

Dims <- myutils::Bdims(Chacon, N+1)
library(gmp)
ckernels <- myutils::Bkernels(Chacon, N+1) 
fedgelabels <- Vectorize(function(n, from, to, mindex){
  if(n==0) return("XX")
  p <- as.bigq(ckernels[[n+1]][to, from])
  if(from==1 && to==1) p <- p/3
  ifelse(denominator(p)==1, "1", sprintf("\\nicefrac{%s}{%s}", numerator(p), denominator(p)))
})

myutils::BgraphTikZ("Chacon_DimsProbs_R.tex", Chacon, N=N,
                    bending=2, 
                    fvertexlabels=function(n) Dims[[n+1]],
                    fedgelabels=fedgelabels,
                    packages="nicefrac")


