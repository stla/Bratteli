setwd("~/Work/Filtrations/Bratteli/Odometer/figures")

Mn <- function(n){
  if(n==0) return(t(c(1,1)))
  return(matrix(1, nrow=2,ncol=2))
}

library(myutils)
splittedword <- "baabaaaa"
Wn <- Bwalk_powers(Mn, 3, 2, labels="words", splitted_word = splittedword)

BgraphTikZ("OdometerGraphWalk_R.tex", function(n) Wn$Mn[[n+1]], 3, ROOTLABEL = splittedword, 
           mirror=TRUE)
