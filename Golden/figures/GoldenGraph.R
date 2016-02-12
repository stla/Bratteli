setwd("~/Work/Filtrations/Bratteli/Golden/figures")

library(myutils)

Golden_Mn <- function(n){
  if(n==0) return(t(c(1,1)))
  M <- matrix(1, nrow=2, ncol=2)
  if(n%%2==0) M[2,1] <- 0 else M[1,2] <- 0
  if(n%%2==1) colnames(M) <- c(0,1) else colnames(M) <- c(1,0) 
  return(unname(M))
}

fedgelabels <- Vectorize(function(n, from, to){
  if(n==0) return("")
  if(n%%2==0){
    if(from==1) return(0)
    if(from==2) return(1)
  }
  if(n%%2==1){
    if(from==1) return(1)
    if(from==2) return(0)
  }
})

fvertexlabels <- function(n){
  if(n%%2==1) c("{\\boldsymbol 0}", "{\\boldsymbol 1}") else c("{\\boldsymbol 1}", "{\\boldsymbol 0}")
}

BgraphTikZ("GoldenGraph_R.tex", Golden_Mn, N=5, 
           fedgelabels = fedgelabels,
           fvertexlabels = fvertexlabels)

