source('~/Work/Filtrations/Bratteli/Chacon/ChaconTransfo.R')

# but : process Zn pour T=Chacon

Zn2Wn <- function(z, n){ # z sous forme chemin
  dn <- (3^(n+2)-1)/2
  Wn <- character(dn)
  Wn[1] <- ifelse(z[1] %in% c(2L,4L), "b", "a")
  for(i in 2:dn){
    z <- T(z)
    Wn[i] <- ifelse(z[1] %in% c(2L,4L), "b", "a")
  }
  return(Wn)
}

z <- reps()
Zn2Wn(z, 1)

# Zn2Xn 
Zn2Xn <- function(z,n){
  B <- FALSE
  for(i in 1:(n+1)){
    B <- c(B, B, TRUE, B)
  }
  Wn <- Zn2Wn(z,n)
  i <- which(!B)
  return(Wn[i])
}

z <- reps()
Zn2Xn(z, 1)

# simulation (V_n, eps_n)
