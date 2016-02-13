setwd("~/Work/Filtrations/Bratteli/Chacon/figures")

# En fait les labels que j'utilise correspondent aux labels usuels sur le graphe de la walk

ChaconS <- function(n){
  if(n==0) M <- t(as.integer(c(1,1,1,1,1))) else
    M <- matrix(as.integer(c(rep(c(1,1,0,1,0), 4),0,0,1,0,1)), byrow=TRUE, nrow=5)
  return(M)
}


myutils::BgraphTikZ("Chacon_CorrespondanceLabels_R.tex", 
                    ChaconS, N=3,
                    fedgelabels=NA, 
                    fvertexlabels = function(n) 0:4
                    )
N <- 2
W <- myutils::Bwalk_powers(ChaconS, N, 1, labels="words")
myutils::BgraphTikZ("Chacon_CorrespondanceLabels_R.tex", 
                    function(n) W$Mn[[n+1]], N=N,
                    mirror=TRUE,
                    ROOTLABEL=paste0("W_{-2}=", rownames(W$Mn[[1]])))
