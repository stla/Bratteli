source('~/Work/Filtrations/Bratteli/Pascal/Pascal_00.R')
library(magrittr)

u <- seq(1/2^10, 1-1/2^10, by=1/2^10)
Pu <- pascal_num(u)
par(mar=c(4,4,0,0))
plot(u, Pu, pty="s", pch=19, xlab=NA, ylab=NA, xlim=c(0,1))
abline(v=1/2, col="red", lty="dashed")