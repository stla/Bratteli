n <- 6
B <- 0
for(i in 1:n){
  B <- c(B, B, 1, B)
}
length(B)
sum(B)

length(B)/sum(B)
