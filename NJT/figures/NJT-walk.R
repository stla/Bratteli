setwd("~/Work/Filtrations/Bratteli/NJT/figures")

NJT <- function (n) 
{
  M <- matrix(0, nrow = n + 1, ncol = n + 2)
  for (i in 1:(n + 1)) {
    M[i, ][c(i, n + 2)] <- 1
  }
  return(M)
}


N <- 4
v <- 6

W <- myutils::Bwalk_powers(NJT, N, v, labels="words")
Mn <- W$Mn

myutils::BgraphTikZ("NJT-walk_R.tex", function(n) Mn[[n+1]], N, 
                    scale=c(1,80), 
                    mirror=TRUE,
                    ROOTLABEL=rownames(Mn[[1]]))




