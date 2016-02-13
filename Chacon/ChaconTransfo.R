
# action sur (eps0, eps1)
# valeurs possibles {0,1,2,3} x {0, 1, 2, 3} sauf si eps1=2 : uniquement (0,2)
T0 <- function(eps0, eps1){
  if(eps1 == 2L) return(c(0L, 3L))
  if(eps0 != 3L) return(c(eps0+1L, eps1))
  if(eps1 != 3L) return(c(0L, eps1+1L))
  if(eps0 == 3L) return(Inf)
}

T0(0L, 0L)
T0(3L, 1L)
T0(0L, 2L)
T0(2L, 3L)
T0(3L, 3L)

checkeps <- function(eps){
  if(! 2L %in% eps) return(TRUE)
    else{
      i <- which(eps==2L)
      if(length(i)>1L) return(FALSE)
      if(i>1L) return(all(eps[1:(i-1)]==0L)) else return(TRUE)
    }
}

checkeps(c(0L,1L))
checkeps(c(2L,1L))
checkeps(c(0L,2L))
checkeps(c(1L,2L))

T <- function(eps){ # prendre eps assez long 
  if(!checkeps(eps)) stop("Incorrect sequence")
  i <- which(eps==2L)
  if(length(i)!=0L && i!=1L) return(c(rep(0L, i-1), 3L))
  t <- T0(eps[1], eps[2])
  if(t[1] != Inf) return(c(t, tail(eps,-2)))
  if(length(eps)==2L){
    return(c(0L, T(c(tail(eps,-1),0L))))
  }else{
    return(c(0L, T(c(tail(eps,-1)))))
  }
  #  else c(0L, T(c(tail(eps,-1),0L)))
}

# non, on prend eps0=1 si (0, 0, 2) (c'est l'arc de la root au niveau 1)
T(c(0L, 0L))
T(c(3L, 1L))
T(c(0L, 2L))
T(c(2L, 3L))
T(c(3L, 3L))
T(c(0L, 0L, 0L))
T(c(0L, 0L, 2L))

reps <- function(){
  eps <- integer(20L)
  v <- v0 <- (runif(1)>2/3) + 1L
  for(i in 1:length(eps)){
    if(v ==2L){
      v <- sample(c(1L, 2L), 1, prob = c(2/3,1/3))
      eps[i] <- ifelse(v==1L, 2L, 0L)
    }else{
      eps[i] <- sample(c(0L,1L,3L), 1, prob = c(1,1,1)/3)
    }
  }
  return(c(ifelse(v0==1L, -Inf, Inf), eps))
}

reps()

Chacon <- function(eps){ # prendre eps assez long 
  if(eps[1]==Inf){
    i <- which(eps==2L)
    return(c(xxxx))
  }
  
  xxx sinon chopper le 1er zéro et appliquer T 
  
  t <- T0(eps[1], eps[2])
  if(t[1] != Inf) return(c(t, tail(eps,-2)))
  if(length(eps)==2L){
    return(c(0L, T(c(tail(eps,-1),0L))))
  }else{
    return(c(0L, T(c(tail(eps,-1)))))
  }
  #  else c(0L, T(c(tail(eps,-1),0L)))
}