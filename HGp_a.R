  #INPUTS
    #Dist_type: Distribution 
    #level:     Risk Level
    #p:         Power Young Function
    #esp :      Error for bisection and Integral
  #OUTPUTS
    #S_opt: Minimizer \hat{(s^*)}
    #HG_p:  \pi_{(p,\alpha)}
    #fval:  the value nonoline eq. at \hat{(s^*)}

HGp_a<-function(Dist_type,Name,level,p,eps){
  #level is the  right tail
s_star<-Opt_s(Dist_type,Name,1-level,p,eps)
if(s_star==Inf || s_star==-Inf){
    S_opt<-s_star
    print("Value of s* is either Inf or -Inf")
    HGp_Value<- S_opt
    fval<-"NAN"
  } else { 
  HGp_Value<-(funcEp(s_star,p,level,Dist_type,Name)/(1-level))^(1/p)+s_star
  S_opt<-s_star
  fval<-Nonlinp(level,S_opt,p,Dist_type,Name)
  }
  
  return(list(HG_p=HGp_Value,S_opt=S_opt, fval=fval))
}










