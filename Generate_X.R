#This funcrion generate a partition of X, we use this partition to find an approximation of 
#s_p such that the root that we are looking for is in this interval.

Generate_X<-function(Dist_type,Num_partition){
  integral<- pracma::integral
  xsort<-c();
  for (i in c(1:2^Num_partition))
  {
    
    # xsort1[i]<--(1+log(2^Num_partition)+(2^Num_partition-i)*log(2^Num_partition-i)
    #                         -(2^Num_partition-i+1)*log(2^Num_partition-i+1));
  
    f <- function(x){x*d(Dist_type)(x)}
    xsort[i]<-2^Num_partition*integral(f,                          
                                q.l(Dist_type)((i-1)/2^Num_partition),
                                q.l(Dist_type)((i)/2^Num_partition),reltol = 1e-14)
                            
  }
  return(xsort)
}
