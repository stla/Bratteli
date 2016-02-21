setwd("~/Work/Filtrations/Bratteli/OrderedBratteli/figures")

M0 <- t(c(1L,1L,1L))
M1 <- rbind(
        c(1L, 0L, 0L),
        c(1L, 1L, 1L),
        c(0L, 0L, 1L)
)
M2 <- rbind(
        c(1L, 1L),
        c(1L, 1L),
        c(0L, 1L)
)
Mn <- list(M0, M1, M2)

N <- 3

myutils::BgraphTikZ("OB1_idlabels_R.tex", function(n) Mn[[n+1]], N=N,
                    fedgelabels="default")

myutils::BgraphTikZ("OB1_orderlabels_R.tex", function(n) Mn[[n+1]], N=N,
                    fedgelabels="order")


Mn[[4]] <- t(t(c(1L,1L)))
Wn <- myutils::Bwalk_powers(function(n) Mn[[n+1]], N=3,v=1)

myutils::BgraphTikZ("OB1_walk_R.tex", function(n) Wn$Mn[[n+1]], N=3,
                    mirror=TRUE)



