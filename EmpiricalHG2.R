#This is the emperical method to find s^*, can works individually
#when we have loss function X form real data.
#INPUTS:
     # xsort: Data, partition 
     # level: Risk tolerance level \alpha
#OUTPUTS:
     # \pi_{(2,\alpha)} 
     # \hat{s}^*
     # location of this minimizer
EmpiricalHG2<-function(xsort,level){
  xsort<-sort(xsort)
  ap<-level
  n<-length(xsort);
  k<-ceiling(ap*n);
  if (k==1){
    s<-xsort[1]
    if(s<=xsort[k+1]){
      s<-s
      Value<-1/(sqrt(ap)*sqrt(n))*sqrt(sum((s*rep(1,k)-xsort[1:k])^2))-s
    } 
  } else{
  
  location=0;
  while (location==0) {
    mu<-mean(xsort[1:k]);
    sigma<-sqrt(mean((mu*rep(1,k)-xsort[1:k])^2));
    s<-mu+sigma*sqrt(ap*n/(k-ap*n));
    if (s<=xsort[k+1] | k==n){
      location=-1 
      #stop
    } else{
      k<-k+1}
  }
  s<-mu+sigma*sqrt(ap*n/(k-ap*n))
  Value<-1/(sqrt(ap)*sqrt(n))*sqrt(sum((s*rep(1,k)-xsort[1:k])^2))-s
  }
  return((list(Value=Value, S=s,location=k)))
}

