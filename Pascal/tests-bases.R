
### retourne les left-points des bases des colonnes ###

a0 <- c(0, 1) # à multiplier par 1/2
a1 <- c(0,  a0[2]*2, 4-1)
a2 <- c(0, a1[2]*2, a1[3]*2, 8-1)
a3 <- c(0, a2[2]*2, a2[3]*2, a2[4]*2, 16-1)
# etc.. 

# une fois que j'ai les left-points, j'applique pascal pour avoir les points au dessus
# j'en n'ai pas besoin pour déterminer le palier d'un point:
  # suffit d'itérer jusqu'à tomber dans une base

### retourne les lefts-points des top-levels ###
a0 <- c(0,1)
a1 <- c(a0, 4-1)
a2 <- c(a1, 8-1)
a3 <- c(a2, 16-1)
  # => c'est simplement 2^(1:(n+1))-1
