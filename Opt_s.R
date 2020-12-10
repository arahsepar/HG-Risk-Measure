#The following function searches for s in the support of the distribution 
  
#Part1: Calculate \hat{s} using by following steps
#i)     By using E[X|\pi] setting defualt number_Partition=13 we find s_p
#ii)    Set interval for the bisection metohd as [s_p-1%,s_p+1%]
#iii)   Apply Biscestion Method 

Opt_s<-function(Dist_type,Name,level,p,eps){
bisect<- pracma::bisect
  
  t<-level
  m<-13
  #level here is left tail
  s_p<--Bounds(Dist_type,Num_partition=m,level=t)
  if(s_p==-Inf|| s_p==Inf){
    s_start<-s_p
    print("Value of s* is either Inf or -Inf")
    } else { 

  #then we need level at right 
  i=1
  while(Nonlinp(1-t,s_p-.01*i,p,Dist_type,Name)*Nonlinp(1-t,s_p+.01*i,p,Dist_type,Name)>0){
    i<-i+1
  }
    u<-s_p+.01*i
    l<-s_p-.01*i
  num<-floor(log2((u-l)/eps) )+1
  alpha<-1-t
     oldw <- getOption("warn")
  options(warn = -1)
  f<-function(s){Nonlinp(1-t,s,p,Dist_type,Name)}
  
  ##Important Not: 
            #One can either use Bisec_Nonp form this package, 
            #or use bisect from pracma package
  #num<-floor(log2((2*ub)/eps) )+1
  #s_start<-Bisec_Nonp(l=l,u=u,num=num,eps=eps,level=alpha)$Point
  s_start<-bisect(f, l, u, maxiter= 800, tol = eps)$root
  options(warn = oldw)
    }
return(s_start)
}


