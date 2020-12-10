#The Exact solution for Uniform when p=2
HG_Uniform<-function(a,b,alpha){
  a<-a
  b<-b
if(alpha<1/4){
S_Actual=(b+a)/2-(b-a)*sqrt((1-alpha)/(3*alpha))/2 
HG_Actual=(b+a)/2+(b-a)*sqrt(alpha/(3*(1-alpha)))/2
} else{  
S_Actual=b-4*(b-a)*(1-alpha)/3
HG_Actual=b-4*(b-a)*(1-alpha)/9
}
  return(list(S_Actual=S_Actual,HG_Actual=HG_Actual))
}
