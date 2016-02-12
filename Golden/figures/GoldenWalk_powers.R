setwd("~/Work/Filtrations/Bratteli/Golden/figures")

library(myutils)

Golden_Mn <- function(n){
  if(n==0) return(t(c(1,1)))
  M <- matrix(1, nrow=2, ncol=2)
  if(n%%2==0) M[2,1] <- 0 else M[1,2] <- 0
  if(n%%2==1) colnames(M) <- c(0,1) else colnames(M) <- c(1,0) 
  return(unname(M))
}

N <- 4; v <- 2
Walk <- Bwalk_powers(Golden_Mn, N, v)
Mn <- Walk$Mn

BgraphTikZ("GoldenWalk_powers_N4_v2_R.tex", function(n) Mn[[n+1]], N,
           mirror=TRUE, ROOTLABEL=rownames(Mn[[1]])) #, fedgelabels = fedgelabels)



# fedgelabels <- Vectorize(function(n, from, to){
#   if(n==0) return("")
#   if(n%%2==1){
#     if(from==1) return(0)
#     if(from==2) return(1)
#   }
#   if(n%%2==0){
#     if(from==1) return(1)
#     if(from==2) return(0)
#   }
# })
