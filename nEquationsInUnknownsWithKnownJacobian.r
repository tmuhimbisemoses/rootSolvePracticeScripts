library(rootSolve)
A <- matrix(nrow=500,ncol=500,runif(500*500))
B <- runif(500)
print(system.time(x1<-solve(A,B)))
jfun <- function(x)A
fun <- function(x)A%*%x-B
print(system.time(
  X <- multiroot(start=1:500,f=fun,jactype="fullusr",jacfunc=jfun))
)
sum((X1-X$y)^2)