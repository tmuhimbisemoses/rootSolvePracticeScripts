library(rootSolve)
fun <- function(x)cos(2*x)^3
curve(fun(x),0,8)
abline(h=0,lty=3)
uni <- uniroot(fun,c(0,8))$root
points(uni,0,pch=16,cex=2)
