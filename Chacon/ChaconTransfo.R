

checkeps <- function(eps){
  if(!any(c(2L, 4L) %in% eps)) return(TRUE)
    else{
      if(4L %in% eps){
        i <- which.min(eps==4L)
        if(eps[i] != 2L) return(FALSE)
        if(i>1L && !all(eps[1:(i-1)]==4L)) return(FALSE)
      }
      i <- which(eps==2L)
      if(length(i)>1L) return(FALSE)
      if(length(i)==1 && i>1L) return(all(eps[1:(i-1)]==4L))
    }
  return(TRUE)
}


T <- function(eps){ # prendre eps assez long 
  if(!checkeps(eps)) stop("Incorrect sequence")
  if(eps[1] %in% c(0L,1L,2L)) return(c(eps[1]+1L, tail(eps,-1)))
  if(eps[1]==3L){
    i <- which.min(eps==3L)
    if(i==1L){ # case (3,3,3)
      eps <- c(eps, 0L)
      i <- which.min(eps==3L)
    }
    if(eps[i]==0L) return(c(rep(0L, i-1L), 1L, tail(eps,-i)))
    if(eps[i]==1L) return(c(rep(4L, i-1L), 2L, tail(eps,-i)))
  }
  if(eps[1]==4L){
    i <- which.min(eps==4L)
#     if(i==1L){ # case (4, 4, 4) : pas besoin car incorrect sequence
#       eps <- c(eps, 2L)
#       i <- which.min(eps==3L)
#     }
    return(c(rep(0L, i-1L), 3, tail(eps,-i)))
  }
}

reps <- function(){ # Normalement le eps=0 de droite faudrait mettre 4
  out <- integer(20L)
  eps <- out[1] <- sample(0:4, 1, prob=c(2,2,2,2,1)/9)
  for(i in 1:length(out)){
    if(eps != 4L){
      eps <- sample(c(0L,1L,3L), 1, prob = c(1,1,1)/3)
    }else{
      eps <- sample(c(2L,4L), 1, prob = c(2,1)/3)
    }
    out[i] <- eps
  }
  if(all(out==4L)) out <- c(out,2L)
  if(all(out==3L)) out <- c(out, 0L)
  return(out)
}
