# This Function gives us a very good bound for the bisection method. for the Number_partition
#large enough we will have a very good approximation for s^*.
#INPUTS 
   #Num_partition: the number of generating X_i
   #Dist_type
Bounds<-function(Dist_type,Num_partition,level){
  
    xsort<-Generate_X(Dist_type,Num_partition)
    s_p<-EmpiricalHG2(-xsort,level=level)$S
    return(s_s=s_p)
  }
  