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


eps<-10^-13 #(.Machine$double.eps)^0.8
p<-2
b1<-c(.5,.9,.95,.975,.99,.995,.999) #Sequence of alpha 
#part 3: Error Tables

###EXP Dist###
lambda<-1
Dist_type<-Exp(lambda)
Name<-"Exp"
#NOTE NOTE:run each line separately to creat tables
Table1<-Error_Table(Dist_type=Dist_type,Name=Name,p=p,b=b1,eps=eps)
#NOTE NOTE:run each line separately to creat tables
Table2<-Error_Table(Dist_type=Dist_type,Name=Name,p=p,b=1-b1,eps=eps)
df.Tabel1<-data.frame(b1,Table1)
df.Tabel2<-data.frame(1-b1,Table2)
library(xtable)
xtable(df.Tabel1,digits=-8) 
xtable(df.Tabel2,digits=-8)

###Uniform Dist
Dist_type<-Unif(0,1)
Name<-"Uniform"
#NOTE NOTE:run each line separately to creat tables
Table1<-Error_Table(Dist_type=Dist_type,Name=Name,p=p,b=b1,eps=eps)
#NOTE NOTE:run each line separately to creat tables
Table2<-Error_Table(Dist_type=Dist_type,Name=Name,p=p,b=1-b1,eps=eps)
df.Tabel1<-data.frame(b1,Table1)
df.Tabel2<-data.frame(1-b1,Table2)
library(xtable)
xtable(df.Tabel1,digits=-8) 
xtable(df.Tabel2,digits=-8)
##Normal Dist
Dist_type<-Norm(0,1)
Name<-"No"
#NOTE NOTE:run each line separately to creat tables
Table1<-Error_TableELSE(Dist_type=Dist_type,Name=Name,p=p,b=b1,eps=eps)
#NOTE NOTE:run each line separately to creat tables
Table2<-Error_TableELSE(Dist_type=Dist_type,Name=Name,p=p,b=1-b1,eps=eps)
df.Tabel1<-data.frame(b1,Table1)
df.Tabel2<-data.frame(1-b1,Table2)
library(xtable)
xtable(df.Tabel1,digits=-8) 
xtable(df.Tabel2,digits=-8)

#Part4 Figuurs data: All data has been saved 
b<-seq(0.01, 0.99, length.out=23)
p<-2
eps<-10^-13 
#####EXP Dist
lambda<-1
Dist_type<-Exp(lambda)
Name<-"Exp"
Table1<-Error_Table(Dist_type=Dist_type,Name=Name,p=p,b=b,eps=eps)
df.Tabel1<-data.frame(b,Table1)
saveRDS(df.Tabel1, file="p2-Exp1.rds")
#####Unif Dist
Dist_type<-Unif(0,1)
Name<-"Uniform"
Table2<-Error_Table(Dist_type=Dist_type,Name=Name,p=p,b=b,eps=eps)
df.Tabel2<-data.frame(b,Table2)
saveRDS(df.Tabel2, file="p2-Unif01.rds")
#####Normal Dist
Dist_type<-Norm(0,1)
Name<-"No"
Table3<-Error_TableELSE(Dist_type=Dist_type,Name=Name,p=p,b=b,eps=eps)
df.Tabel3<-data.frame(b,Table3)
saveRDS(df.Tabel3, file="p2-Norm01.rds")

