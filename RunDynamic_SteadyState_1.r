library(rootSolve)
model <- function(t, y, pars) {
 with (as.list(c(y, pars)),{
    
 oxicmin = r*OM*(O2/(O2+ks))
 anoxicmin = r*OM*(1-O2/(O2+ks))* SO4/(SO4+ks2)
    
 dOM = Flux - oxicmin - anoxicmin
 dO2 = -oxicmin -2*rox*HS*(O2/(O2+ks)) + D*(BO2-O2)
 dSO4 = -0.5*anoxicmin +rox*HS*(O2/(O2+ks)) + D*(BSO4-SO4)
 dHS = 0.5*anoxicmin -rox*HS*(O2/(O2+ks)) + D*(BHS-HS)
    
 list(c(dOM, dO2, dSO4, dHS), SumS = SO4+HS)
})
}