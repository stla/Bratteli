setwd("~/Work/Filtrations/Bratteli/NJT/figures")

NJT <- function (n) 
{
  M <- matrix(0, nrow = n + 1, ncol = n + 2)
  for (i in 1:(n + 1)) {
    M[i, ][c(i, n + 2)] <- 1
  }
  return(M)
}

N <- 3
myutils::BgraphTikZ("NJT-graph_R.tex", NJT, N, scale=c(1,60))



