source('~/Work/Filtrations/Bratteli/Chacon/ChaconTransfo.R')

# but : process Zn pour T=Chacon

Zn2Wn <- function(z, n){ # z sous forme chemin
  dn <- (3^(n)-1)/2
  Wn <- character(dn)
  Wn[1] <- ifelse(z[1] %in% c(2L,4L), "b", "a")
  for(i in 2:dn){
    z <- T(z)
    Wn[i] <- ifelse(z[1] %in% c(2L,4L), "b", "a")
  }
  return(Wn)
}

z <- reps()
Zn2Wn(z, 2)

# Zn2Xn 
Zn2Xn <- function(z,n){
  B <- FALSE
  for(i in 1:(n-1)){
    B <- c(B, B, TRUE, B)
  }
  Wn <- Zn2Wn(z,n)
  i <- which(!B)
  return(Wn[i])
}

z <- reps()
Zn2Xn(z, 2)

# simulations Wn 
n <- 4
nsims <- 1500
Wn <- character(nsims)
for(i in 1:nsims){
  z <- reps()
  Wn[i] <- paste0(Zn2Wn(z, n), collapse="")
}
length(unique(Wn)) # 7 pour n=2, 25 pour n=3, 79 pour n=4 : on trouve 3^n-2

probs <- table(Wn)/nsims
-sum(probs*log(probs))

n <- 4
nsims <- 1500
Xn <- character(nsims)
for(i in 1:nsims){
  z <- reps()
  Xn[i] <- paste0(Zn2Xn(z, n), collapse="")
}
length(unique(Xn)) # 6 pour n=2, 23 pour n=3, 77 pour n=4


