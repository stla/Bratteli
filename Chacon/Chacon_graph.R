setwd("~/Work/Filtrations/Bratteli/Chacon")

Chacon <- function(n){
  if(n==0) M <- t(as.integer(c(1,1,1,1,1))) else
    M <- matrix(as.integer(c(rep(c(1,1,0,1,0), 4),0,0,1,0,1)), byrow=TRUE, nrow=5)
  return(M)
}

myutils::BgraphTikZ("Chacon_R.tex", Chacon, N=3)

myutils::Bdims(Chacon, 4)

library(gmp)
ckernels <- myutils::Bkernels(Chacon, 15) 
ckernels[1:4]

library(kantorovich)
RHO <- lapply(ckernels, function(kernel) matrix("", nrow=nrow(kernel), ncol=nrow(kernel)))  
RHO[[1]] <- (diag(nrow(ckernels[[1]])) + 1) %% 2
n <- length(ckernels)-1
for(k in 1:n){
  diag(RHO[[k+1]]) <- "0"
  K <- nrow(RHO[[k+1]])
  kernel <- ckernels[[k+1]]
  for(i in 1:(K-1)){
    for(j in (i+1):K){
      RHO[[k+1]][i,j] <- RHO[[k+1]][j,i] <- 
        as.character(kantorovich(as.bigq(kernel[i,]), as.bigq(kernel[j,]), dist = RHO[[k]]))
    }
  }
}
RHO

# 12/13 
# 9/10 = 108/120 - 108=9*12 - 120=108+12 
# 108/121 - 108=9*12 - 121=108+13
# 81/91 = 972/1092 - 972=9*108 - 1092=972+120
# 972/1093 - 972=9*108 - 1093=972+121
# 729/820 = 8748/9840 - 8748=9*972 - 9840=8748+1092
# 8749/9841
# (9*8748)/(9*8748+9840)
# etc... on retrouve les dénominateurs dans les dims
# ça tend vers 8/9 

# On voit que les points 1-2-4 sont égaux (les trois tours) et les points 3-5 sont égaux (le spacer coupé en deux)
# donc ce serait plus clair avec deux vertex seulement (arêtes multiples)
# La longueur du spacer est 1/3^n 
# Ça va être standard car asymptotiquement on n'est pas dans le 1/3^n


#### ARETES MULTIPLES ####

Chacon <- function(n){
  if(n==0) return(t(c(1L,1L)))
  return(t(matrix(c(3L, 1L, 0L, 1L), byrow=TRUE, nrow=2)))
}

myutils::BgraphTikZ("Chacon_Multiples_R.tex", Chacon, N=3)

myutils::Bdims(Chacon, 4)

library(gmp)
ckernels <- myutils::Bkernels(Chacon, 15) 
ckernels[1:4]

library(kantorovich)
RHO <- lapply(ckernels, function(kernel) matrix("", nrow=nrow(kernel), ncol=nrow(kernel)))  
RHO[[1]] <- (diag(nrow(ckernels[[1]])) + 1) %% 2
n <- length(ckernels)-1
for(k in 1:1){
  diag(RHO[[k+1]]) <- "0"
  K <- nrow(RHO[[k+1]])
  kernel <- ckernels[[k+1]]
  for(i in 1:(K-1)){
    for(j in (i+1):K){
      RHO[[k+1]][i,j] <- RHO[[k+1]][j,i] <- 
        as.character(kantorovich(as.bigq(kernel[i,]), as.bigq(kernel[j,]), dist = RHO[[k]]))
    }
  }
}
RHO

# on trouve pas pareil - et ça tend vers 2/3 = 8/9 / 4/3
# est-ce normal ? 
# est-ce la dispersion qui ne change pas ? 

# n=2 dispersion - cas multiple rho=3/4
#       6/9   3/9 
# [1,] "0"   "3/4"
# [2,] "3/4" "0" 
2*6/9*3/9*3/4

# pour l'autre - cas simple rho = 1
#      2/9  2/9  2/9  2/9  1/9
# [1,] "0"  "0"  "1"  "0"  "1" 
# [2,] "0"  "0"  "1"  "0"  "1" 
# [3,] "1"  "1"  "0"  "1"  "0" 
# [4,] "0"  "0"  "1"  "0"  "1" 
# [5,] "1"  "1"  "0"  "1"  "0" 
2/9*2/9*3 + 2/9*2/9*3 + 2/9*1/9*3 + 1/9*2/9*3

# pas pareil car pas la même distance de départ ! 

# ça se plonge dans du Nabla-adique ? non c'est proche du 3-adique ! 