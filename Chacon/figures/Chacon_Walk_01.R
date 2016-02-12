setwd("~/Work/Filtrations/Bratteli/Chacon")

ChaconS <- function(n){
  if(n==0) M <- t(as.integer(c(1,1,1,1,1))) else
    M <- matrix(as.integer(c(rep(c(1,1,0,1,0), 4),0,0,1,0,1)), byrow=TRUE, nrow=5)
  return(M)
}

N <- 2
W <- myutils::Bwalk_powers(ChaconS, N, 1, labels="words")
myutils::BgraphTikZ("ChaconWalk_words_R.tex", 
                    function(n) W$Mn[[n+1]], N=N,
                    mirror=TRUE,
                    ROOTLABEL=paste0("W_{-2}=", rownames(W$Mn[[1]])))

N <- 2
W <- myutils::Bwalk_powers(ChaconS, N, 1, labels="powers")
fvertexlabels <- function(n){
  labs <- colnames(W$Mn[[n]])
  for(i in seq_along(labs)){
    k <- labs[i]
    labs[i] <- ifelse(k==0, "x", ifelse(k==1, "Tx", sprintf("T^{%s}x", k)))
  }
  return(labs)
}
myutils::BgraphTikZ("ChaconWalk_powers_N2_R.tex", 
                    function(n) W$Mn[[n+1]], N=N,
                    fvertexlabels=fvertexlabels,
                    ROOTLABEL="Z_{-2}=x",
                    mirror=TRUE)

N <- 3
W <- myutils::Bwalk_powers(ChaconS, N, 1, labels="powers")
myutils::BgraphTikZ("ChaconWalk_powers_N3_R.tex", 
                    function(n) W$Mn[[n+1]], N=N,
                    mirror=TRUE)
