
# renormalization [0,1] -> [a,b]
affine0 <- function(a, b){
  forward <- function(x) a + (b-a)*x
  backward <- function(y) (y-a)/(b-a)
  return(list(f=forward, b=backward))
}
# affine map [a,b] -> [c,d]
affine <- function(a,b,c,d){
  f0 <- function(x) affine0(a,b)$b(x)
  f1 <- function(x) affine0(c,d)$f(x)
  return(function(x) f1(f0(x)))
}

f <- affine(0,2/9,2/9,4/9)
curve(f(x), from=0, to=2/9)

plot(0,0, type="n", xlim=c(0,1), ylim=c(0,1))
f <- affine(0, 2/9, 2/9, 4/9)
curve(f(x), from=0, to=2/9, add=TRUE)
f <- affine(2/9, 4/9, 2/3, 8/9)
curve(f(x), from=2/9, to=4/9, add=TRUE)
f <- affine(2/3, 8/9, 4/9, 2/3)
curve(f(x), from=2/3, to=8/9, add=TRUE)

#### Extrémités gauches des bases ####
library(gmp)

N <- 3 
# B (base de droite)
B <- 1L - 1L/as.bigz(3)^(2:(N+1))
# A (bases de gauche)
A <- vector("list", N)
A[[1]] <- as.bigq(c(0, 2, 6, 4), 9)
for(i in 2:N){
  A[[i]] <- c(A[[i-1]], A[[i-1]] + 2L/as.bigq(3)^(i+1L), B[i-1L], A[[i-1]] + 4L/as.bigq(3)^(i+1L))
}
# Top levels
TopA <- TopB <- vector("list", N)
for(i in 1:N){
  TopA[[i]] <- c(as.bigq(2,3)- 2L/as.bigq(3)^(i+1L), as.bigq(2,3))
  TopB[[i]] <- c(B[i], as.bigq(1))
}

# fonction belongs to interval
# tant que dans Top, augmenter N
# ensuite regarder quelle différence entre x et les extrémités gauche est plus petit que la longueur

# 
x <- runif(1)
n <- 1
while(x > B[n]) n <- n+1
while(x >= TopA[[n]][1] && x < TopA[[n]][2]) n <- n+1 
dists <- x-as.numeric(A[[n]])
which(dists >= 0 & dists < 2L/as.bigq(3)^(n+1L))

# sans les B, TopA, etc 
n <- 1L
while(x > 1L - 1L/as.bigz(3)^(2:(n+1))) n <- n+1
while(x >= as.bigq(2,3)- 2L/as.bigq(3)^(n+1L) && x < as.bigq(2/3)) n <- n+1
i <- 1L; A <- as.bigq(c(0, 2, 6, 4), 9)
while(i < n){
  A <- c(A, A + 2L/as.bigq(3)^(n+1L), 1L - 1L/as.bigz(3)^(n+1), A + 4L/as.bigq(3)^(n+1L))
  i <- i+1L
}
dists <- x-as.numeric(A)
j <- which(dists >= 0 & dists < 2L/as.bigq(3)^(n+1L))
# à cette issue, x est dans le j-ième plateau de la n-ième tour (et pas dans le top)
# les extrémités gauches de la n-ième tour sont données dans A
as.numeric(A[j+1])+dists[j] # c'est l'image de x par Chacon 

Chacon_num <- function(x){
  n <- 1L
  while(x > 1L - 1L/as.bigz(3)^(2:(n+1))) n <- n+1
  while(x >= as.bigq(2,3)- 2L/as.bigq(3)^(n+1L) && x < as.bigq(2/3)) n <- n+1
  i <- 1L; A <- as.bigq(c(0, 2, 6, 4), 9)
  while(i < n){
    A <- c(A, A + 2L/as.bigq(3)^(n+1L), 1L - 1L/as.bigz(3)^(n+1), A + 4L/as.bigq(3)^(n+1L))
    i <- i+1L
  }
  dists <- x-as.numeric(A)
  j <- which(dists >= 0 & dists < 2L/as.bigq(3)^(n+1L))
  # à cette issue, x est dans le j-ième plateau de la n-ième tour (et pas dans le top)
  # les extrémités gauches de la n-ième tour sont données dans A
  return(as.numeric(A[j+1])+dists[j]) # c'est l'image de x par Chacon 
}

Chacon_num(0.5)

