library(dyadic)

#' returns m1(x), k1(x) (Vershik's notations)
mk <- function(d){
  d <- as.integer(d)
  i <- which.max(d)
  m1 <- i-1L
  #   if(i==length(d)) d <- c(d, 0L) # cas d = 000001
  #   if(i == 1L) d <- c(d, 0L) # cas d = 1111
  #   if(all(d[-(1:i)]==1)) d <- c(d, 0L) # cas d=0001111
  #   # finalement suffit de rajouter un 0 !
  d <- c(d, 0L)
  j <- which.min(d[-(1:i)])
  k1 <- j-1L
  # check
  if(!all(d[1:(m1+k1+2L)] == c(rep(0L, m1), rep(1L, k1), 1L, 0L))) stop("Incorrect result")
  return(c(m1,k1))
}

pascal_dyadic <- function(d){
  d <- as.dyadic(d)
  m1k1 <- mk(d)
  begin <- c(rep(1L, m1k1[2]), rep(0L, m1k1[1]+1L), 1L)
  if(length(d)==sum(m1k1)+1) d <- begin else d[1:(sum(m1k1)+2)] <- begin
  return(d)
}

pascal_num <- Vectorize(function(u){
  return(dyadic2num(pascal_dyadic(num2dyadic(u))))
})
