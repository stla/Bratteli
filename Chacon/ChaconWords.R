source('~/Work/Filtrations/Bratteli/Chacon/ChaconTransfo_Numeric.R')

Word <- function(u, size){
  out <- character(size)
  out[1] <- ifelse(u < 2/3, "a", "b")
  for(i in 2:size){
    u <- Chacon_num(u)
    out[i] <- ifelse(u < 2/3, "a", "b")
  }
  return(out)
}

## sur 0 - 2/9 : aaba
u <- runif(1, 0, 2/9)
Word(u, 4)

## sur 2/9 - 4/9
# sur le premier tiers : abaa
u <- runif(1, 2/9, 2/9+2/27)
Word(u, 4)
# sur le 2èm tiers tiers : abab
u <- runif(1, 2/9+2/27, 2/9+4/27)
Word(u, 4)
# sur le 3ème tiers : l'un ou l'autre 
u <- runif(1, 2/9+4/27, 4/9)
Word(u, 4)

# mots de 5 lettres 
words <- NULL
nsims <- 1000
for(i in 1:nsims){
  u <- runif(1)
  words <- c(words, paste0(Word(u,5), collapse=""))
}
table(words)
