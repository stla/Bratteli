setwd("~/Work/Filtrations/Bratteli/Chacon2")

Chacon2S <- function(n){
  if(n==0) M <- t(as.integer(c(1,1,1,1))) else
    M <- matrix(as.integer(c(rep(c(1,0,1,0), 3),0,1,0,1)), byrow=TRUE, nrow=4)
  return(M)
}

Chacon2M <- function(n){
  if(n==0) M <- t(c(1L,1L))
    else M <- t(matrix(c(2L, 1L, 0L, 1L), byrow=TRUE, nrow=2))
  colnames(M) <- c("a","b")
  return(M)
}

myutils::BgraphTikZ("Chacon2S_R.tex", Chacon2S, N=3)

myutils::BgraphTikZ("Chacon2S_nolabs_R.tex", Chacon2S, N=3, 
                    fedgelabels=NA)

myutils::BgraphTikZ("Chacon2S_orderlabs_R.tex", Chacon2S, N=3, fedgelabels="order")

myutils::BgraphTikZ("Chacon2M_R.tex", Chacon2M, N=3, 
                    bending=2, fedgelabels="order")

library(gmp)
myutils::Bmetrics(Chacon2S, 3)
(ckernels <- myutils::Bkernels(Chacon2S, 3))

library(gmp)
(ckernels <- myutils::Bkernels(Chacon2S, 3, class="bigq"))
ckernels_numer <- lapply(ckernels, function(x) as.character(numerator(x)))
ckernels_denom <- lapply(ckernels, function(x) as.character(denominator(x)))
fedgelabels <- function(level, from, to, m){
  if(ckernels_denom[[level+1]][to,from]=="1") return("1")
  sprintf("\\nicefrac{%s}{%s}", ckernels_numer[[level+1]][to,from], ckernels_denom[[level+1]][to,from])
}

myutils::BgraphTikZ("Chacon2S_probs_R.tex", Chacon2S, N=3, 
                    fedgelabels=Vectorize(fedgelabels), 
                    packages="nicefrac")


Wn <- myutils::Bwalk_powers(Chacon2S, N=3, v=1, labels="words")
myutils::BgraphTikZ("Chacon2Walk_N3v1.tex", function(n) Wn$Mn[[n+1]], 
                    N=3, 
                    mirror=TRUE)


