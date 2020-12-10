#This is the main function that Calculate pi_{(p,\alpha)} values 
#when \varphi(x)=X^p p>0

rm(list=ls()) 
library(distrEx)
library(RobExtremes) #we need RobExtrim for some Dist like Pareto,Gubmle ..

source("Generate_X.R")
source("Bisec_Nonp.R")
source("funcEp.R")
source("Nonlinp.R")
source("HGp_a.R")
source("Opt_s.R")
source("Bounds.R")
source("EmpiricalHG2.R")
###
source("HG_Uniform.R")
source("HG_Exponetial.R")
source("Error_Table.R")
source("Error_TableELSE.R")

#INPUTS
#1           Dist_type:
#Here are the examples of the distributions that we worked on in this Thesis, 
#For the list of other Dist. Check the distrEX  and RobExtremes library
      #Exponetial Dis     Exp(lambda)
      #Uniform Dis        Unif(a,b)
      #Normal Dis         Norm(mu,sigma)
      #T Dis              Td(nu)
#2        Name 

#Very Important to change this to dist name, 
      #Exponetial Dis     Exp
      #Uniform Dis        Uniform
      #Normal Dis         No
      #T Dis              NO

#3        a  the level of risk
#4        p  \varphi(x)=x^p 
Dist_type<-Norm(0,1) 
Name<-"No"
a<-0.95 
p<-2
eps<-10^-13#(.Machine$double.eps)^0.8

#Part 1 HG_p Value
options(digits=14)
HG_Value<-HGp_a(Dist_type=Dist_type,Name=Name,level=a,p=p,eps=eps)
print(HG_Value)

#Part 2 calculate the absolute error of pi and \hat{pi} when p=2
HGp_ErrorA<-(1+1/(1-level))*eps+HG_Value$fval/(1-level) #Theoretical Error
#For Unif and Exp distributions, we can find realized Error as well.
Dist_type<-Unif(0,1)
Name<-"Uniform"
HG_Value<-HGp_a(Dist_type=Dist_type,Name=Name,level=a,p=p,eps=eps)
HGp_ErrorA<-(1+1/(1-level))*eps+HG_Value$fval/(1-level)         #Theoretical Error
HGp_ErrorR<-abs(HG_Uniform(0,1,a)-HG_Value$HG)                  #Realized Error
print(HGp_ErrorA)
print(HGp_ErrorR)
############
Dist_type<-Exp(1)
Name<-"Exp"
HG_Value<-HGp_a(Dist_type=Dist_type,Name=Name,level=a,p=p,eps=eps)
HGp_ErrorA<-(1+1/(1-level))*eps+HG_Value$fval/(1-level)          #Theoretical Error
HGp_ErrorR<-abs(HG_Exponetial(1,a)-HG_Value$HG)                  #Realized Error
print(HGp_ErrorA)
print(HGp_ErrorR)

