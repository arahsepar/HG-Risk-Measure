
#This is the nonlinear eq. that we will find the unique solution of this eq.
Nonlinp<-function(level,s,p,Dist_type,Name){

return(funcEp(s,p-1,level,Dist_type,Name)^p-(1-level)*(funcEp(s,p,level,Dist_type,Name))^(p-1))
    }
