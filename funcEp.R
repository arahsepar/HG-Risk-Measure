
#INPUTS:
  # s: candidate minimizer
  # p: \var(x)=x^p
  # level: Risk tolerance
  # Name: name of distribution 

# Method Of integral can be  method =c("Kronrod","Richardson","Clenshaw","Simpson","Romberg")))
#This function is using "infinite domains Gauss integration is applied" 

funcEp<-function(s,p,level,Dist_type,Name){
  integral<- pracma::integral
  
  upper<-q.l(Dist_type)(1)
  lower<-q.l(Dist_type)(0)                  #quantile 1
  fkt <- function(x){(x-s)^p*d(Dist_type)(x)}
  if (Name=="Uniform") {
    if (level<1/4){
    return(integral(fkt, Min(Dist_type), upper, reltol = 1e-14))
    } else {
    return(integral(fkt, s, upper, reltol = 1e-14))
    }
    
    } else if (Name=="Exp") { 
    if (level>1/2){
    return(integral(fkt,s,upper, reltol = 1e-14))
    } else {
      return(integral(fkt, 0, upper, reltol = 1e-14))
    }
    
  } else if (Name=="No") {
    
  return(integral(fkt, s, upper, reltol = 1e-14))
  }
  
  }
              