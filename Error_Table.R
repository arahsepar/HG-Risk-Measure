#This code is for EXP and Unif that we have Actual solutions
#Inputs:
        #b as level of risks
        #Dist_type and Name 
        #p:2, this code is working for any p when HG_p is needed
        #esp: Tolerance level
#Outputs:
#Realized and Actual Errors
Error_Table<-function(Dist_type,Name,p,b,eps){
  
HGN<-c()
SN<-c()
ErrorR<-c()

HGA<-c()
SA<-c()
ErrorA<-c()

ErrorS<-c()
if(Name=="Exp"){
  lambda <-as.integer( readline(prompt="Enter Lambda: "))
for (i in 1:length(b)){
  a<-b[i]    
  G<-HGp_a(Dist_type,Name,a,p,eps)
  HGN[i]<-G$HG_p
  HGA[i]<-HG_Exponetial(lambda,a)$HG_Actual
  SN[i]<-G$S_opt
  SA[i]<-HG_Exponetial(lambda,a)$S_Actual
  #Erorres
  ErrorS[i]<-abs(G$S_opt-HG_Exponetial(lambda,a)$S_Actual)
  ErrorA[i]<-(1+1/(1-a))*eps+abs(G$fval)/(1-a)
  ErrorR[i]<-abs(HG_Exponetial(lambda,a)$HG_Actual-G$HG_p)
}
  } else{
  a1 <-as.integer( readline(prompt="Enter a: "))
  b1 <-as.integer( readline(prompt="Enter b: "))
  for (i in 1:length(b)){
    a<-b[i]    
    G<-HGp_a(Dist_type,Name,a,p,eps)
    HGN[i]<-G$HG_p
    HGA[i]<-HG_Uniform(a1,b1,a)$HG_Actual
    #
    ErrorA[i]<-(1+1/(1-a))*eps+abs(G$fval)/(1-a)
    ErrorR[i]<-abs(HG_Uniform(a1,b1,a)$HG_Actual-G$HG_p)
    SN[i]<-G$S_opt
    SA[i]<-HG_Uniform(a1,b1,a)$S_Actual
    ErrorS[i]<-abs(G$S_opt-HG_Uniform(a1,b1,a)$S_Actual)
}
  }
return(list(HGN=HGN,HGA=HGA,SN=SN,SA=SA,ErrorA=ErrorA,ErrorR=ErrorR,ErrorS=ErrorS))
}