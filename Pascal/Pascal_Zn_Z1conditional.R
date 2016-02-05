Pascal_Mn <- function(n){
  M <- matrix(0, nrow=n+1, ncol=n+2)
  for(i in 1:(n+1)){
    M[i, ][c(i, i+1)] <- 1
  }
  return(M)
}

library(gmp) 

N <- 5; v <- 3
Qn <- Bwalk(Pascal_Mn, N, v)
Pn <- lapply(Qn, function(P){
  Pcopy <- as.bigq(matrix(0, nrow=nrow(P), ncol=ncol(P)))
  for(i in 1:nrow(P)){
    j <- which(P[i,]!=0)
    Pcopy[i,j] <- as.bigq(nchar(colnames(P)[j]), nchar(rownames(P)[i]))
  }
  return(Pcopy)
})



for(i in 1:N){ # remplacer les noms des Qn par les puissances de T
  M <- Qn[[i]]
  for(j in 1:ncol(M)){
    k <- which(letters==stringr::str_sub(colnames(M)[j], 1, 1)) - 1L
    colnames(M)[j] <- k #ifelse(k==0, "x", ifelse(k==1, "Tx", sprintf("T^{%s}x", k)))
  }
  Qn[[i]] <- M
}

Zm_given_ZN <- vector("list", N)
Zm_given_ZN[[1]] <- Pn[[1]]
for(i in 2:N){
  Zm_given_ZN[[i]] <- Zm_given_ZN[[i-1]] %*% Pn[[i]]  
}
Zm_given_ZN <- lapply(Zm_given_ZN, as.character)
for(i in 1:N){
  rownames(Zm_given_ZN[[i]]) <- rownames(Qn[[1]])
  colnames(Zm_given_ZN[[i]]) <- colnames(Qn[[i]])
}

# ajout des zéros 
kmax <- nchar(rownames(Qn[[1]])) - 1L
Zm_given_ZN_zeros <- lapply(1:N, function(i) setNames(rep("0", kmax+1L), 0:kmax))
for(i in 1:N){
  Zm_given_ZN_zeros[[i]][colnames(Zm_given_ZN[[i]])] <- Zm_given_ZN[[i]]
}

# je retire les quinzième (N=5) car c'est la longueur 
