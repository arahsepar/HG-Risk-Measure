#This code is for dist hat we do not have Actual solutions
#Inputs:
#b as level of risks
#Dist_type and Name 
#p:2, this code is working for any p when HG_p is needed
#esp: Tolerance level
#Outputs:
#Realized and Actual Errors
Error_TableELSE<-function(Dist_type,Name,p,b,eps){
  
  HGN<-c()
  SN<-c()
  ErrorA<-c()
    for (i in 1:length(b)){
      a<-b[i]    
      G<-HGp_a(Dist_type,Name,a,p,eps)
      HGN[i]<-G$HG_p
      SN[i]<-G$S_opt
      ErrorA[i]<-(1+1/(1-a))*eps+abs(G$fval)/(1-a)
  }
  return(list(HGN=HGN,SN=SN,ErrorA=ErrorA))
}