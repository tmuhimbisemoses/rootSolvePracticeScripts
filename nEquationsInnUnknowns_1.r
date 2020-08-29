library(rootSolve)
model <- function(x){
  F1 <- x[1]+x[2]+x[3]^2-12
  F2 <- x[1]^2-x[2]+x[3]-2
  F3 <- 2*x[1]-x[2]^2+x[3]-1
  c(F1=F1,F2=F2,F3=F3)
}

#first solution
(ss <- multiroot(f=model,start=c(1,1,1)))

root
f.root
iter
estim.precis

#second solution; use different start values
(ss2 <- multiroot(model,c(0,0,0)))$root

#the function value of the root
model(ss2$root)
