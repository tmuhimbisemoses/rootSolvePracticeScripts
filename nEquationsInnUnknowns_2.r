library(rootSolve)
f2 <- function(x){
  X <- matrix(nr=5,x)
  X%*%X%*%X-matrix(nrow=5,data=1:25,byrow=TRUE)
}
print(system.time(
  x <- multiroot(f2,start=1:25)$root
))
(X <- matrix(nrow=5,x))
X%*%X%*%X