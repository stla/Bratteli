Word <- function(theta, u, size){
  out <- character(size)
  out[1] <- ifelse(u < theta, "a", "b")
  for(i in 2:size){
    u <- (u+theta) %% 1
    out[i] <- ifelse(u < theta, "a", "b")
  }
  return(out)
}

theta <- sqrt(2)-1 

l <- 32

sturmianwords <- function(theta, l , outmatrix=TRUE){
  words_matrix <- matrix(character(0), nrow=l+1, ncol=l)
  words <- NULL
  i <- 0L
  while(i < l+1L){
    u <- runif(1)
    word_row <- Word(theta, u, l)
    word <- paste(word_row, collapse="")
    if(! word %in% words){
      words <- c(words, word)
      words_matrix[i+1L, ] <- word_row
      i <- i+1L
    }
  }
  if(outmatrix) return(words_matrix) else return(words)
}

sturmianwords(theta, 3, FALSE)

#### tests si des mots w=w1w2 de taille 2^n sont tels que w2w1 est dans L_{2^n}(theta) ####
reversedword <- function(w){
  return(c(tail(w, length(w)/2), c(head(w, length(w)/2))))
}
reversedword(c("a","b","c","d"))

n <- 4
words_matrix <- sturmianwords(theta, 2^n)
words <- apply(words_matrix, 1, function(w) paste0(w, collapse=""))
reverse_is_in <- function(w) reversedword(w) != w && paste0(reversedword(w), collapse="") %in% words
any(apply(words_matrix, 1, reverse_is_in))

# TRUE pour n=7 ! 
howmany <- which(apply(words_matrix, 1, reverse_is_in)) 
length(howmany) # 56! !
i <- howmany[1]
words_matrix[i, 1:5]
words_matrix[i, 64 + 1:5]


#### matrices de Bratteli pour Golden x Sturmien ####
fibo <- function(n) as.integer(gmp::fibnum(n+2))
Mn <- list()
n <- 0 
M0 <- t(c(1L, 1L))
words_matrix0 <- t(t(c("a","b")))
words0 <- apply(words_matrix0, 1, function(w) paste0(w, collapse=""))
colnames(M0) <- words0
Mn[[1]] <- M0
for(n in 1:5){
  words_matrix1 <- sturmianwords(theta, fibo(n))
  words1 <- apply(words_matrix1, 1, function(w) paste0(w, collapse=""))
  rownames(words_matrix1) <- words1
  M1 <- matrix(0L, nrow=ncol(M0), ncol=length(words0)+length(words1))
  rownames(M1) <- colnames(M0)
  colnames(M1) <- c(words0, words1)
  ## connections ...
  M1[words0, words0] <- diag(length(words0))
  for(w in rownames(M1)){
    for(word1 in words1){
      if(nchar(w)==fibo(n-2L)){ 
        M1[w, word1] <- ifelse(paste0(tail(words_matrix1[word1,], nchar(w)),collapse="")==w, 1L, 0L) 
      }else{ # si je fais else le cas n =1 n'est pas correct
        M1[w, word1] <- ifelse(paste0(head(words_matrix1[word1,], nchar(w)),collapse="")==w, 1L, 0L)
      }
    }
  }
  Mn[[n+1]] <- M1 
  M0 <- M1
  words0 <- words1
}
#correction M_1
Mn[[2]][, c("ab", "ba")] <- 1L
Mn[[2]][, c("bb")] <- c(0L, 2L)

#### ckernels #### (normalement je retrouve les probas de golden)
library(gmp)
Mn_fun <- function(n) Mn[[n+1]]
myutils::Bkernels(Mn_fun, 5)

myutils::Bmetrics(Mn_fun, 4) # marche jusque 4
