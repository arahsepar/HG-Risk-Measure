#The Exact solution for Exponetial when p=2

HG_Exponetial<-function(lambda,alpha){
a<-alpha 
lambda<-lambda
p=2
if(a>1/2){
S_Actual=log(gamma(p+1)/((1-a)*(p^p)))/lambda
HG_Actual=log(gamma(p+1)/((1-a)*(p^p)))/lambda+p/lambda
} else{
#for a>1/2
S_Actual=1/lambda-sqrt((1-a)/a)
HG_Actual=1/lambda+sqrt(a/(1-a))/lambda
}
return(list(S_Actual=S_Actual,HG_Actual=HG_Actual))
}

