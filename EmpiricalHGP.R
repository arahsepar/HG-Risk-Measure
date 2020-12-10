#This function is dealing for a higher degree, i.e. p>2, by numerically
#finding the optimum value, using nleqslv as a nonlinear solver. 
#In general, the approach  is the same as we discuss in the thesis.

EmpiricalHGP<-function(A,alpha,p){
  library("nleqslv")
  source("EM_Nonlinp.R")
  
level<-1-alpha
Xsort<-sort(-A)
n<-length(Xsort)

fun<- function(t){
  EM_Nonlinp(A,level,t,p)
  }
  
y<-nleqslv(min(A), fun, global="dbldog",method="Broyden",control=list(maxit = 1e+8,ftol=5e-50,xtol=5e-50))
s<--y$x
df<-data.frame(Xsort+s,rep(0,n))
df$max <- do.call(pmax, c(df, na.rm = TRUE))
HGp_Value<-((sum((df$max)^p)/n)/(1-level))^(1/p)-s

return(list(S=s,Value=HGp_Value))
}