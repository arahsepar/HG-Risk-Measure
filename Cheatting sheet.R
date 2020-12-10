
1-Create your own discrete distribution using DiscreteDistribution
Example:
D <- DiscreteDistribution(supp = r(Norm(0,1))(100), prob = rep(1/100,100))

2-Create your own  absolutely continuos distribution  using AbscontDistribution
Example: Distribution with density proportional to exp(|x|^3)
AC<- AbscontDistribution(d = function(x) exp(-abs(x)^3), withStand = TRUE)

3-we can mix the distribution
Example: 
UnivarMixingDistribution(Norm(), Pois(lambda=1), Norm(), withSimplify = FALSE)

Note:
For the pareto we have to use another package "RobExtremes"

###Inputs:
        # alpha as the level of risk
        # p for x^p
        #Distribution with its parameter as it is in distrEx package 
        #Distribution Model weather is Continuous or Discontinuous 
        #Upper Bound to evaluate the expected value
        #we need a  boundery to fird stating point to find the solution 
        #to equation. 

####So far Dis_type that I checked as the following
#HG2<-function(Dis_type=list(Norm(),Exp(),Unif(),Lnorm(), DiscreteDistribution(for random normal)),a=alpha){

###########################################

The Exact solution for Exponetial is:
alpha<-b#.95
lambda<-1
pi_value<-log(gamma(p+1)/((1-alpha)*(p^p)))/lambda+p/lambda


Pareto : let Y in exponetial then with rate alpha, then  x_m*exp(Y) is pareto with min X_m and index alpha 
x_m<-1
alpha<-1
Dist_type<-x_m*exp(Exp(alpha))
############plots



#######name of file saved###########
#saveRDS(TestdataSample, file="TXS502p1234a95.rds");
#saveRDS(TestdataSample, file="TXS502p1234a975.rds");
#saveRDS(TestdataSample, file="TXS502p1234a99.rds");
#saveRDS(TestdataSample, file="TXS502p1234a995.rds")
#saveRDS(TestdataSample, file="TXS251p1234a95.rds");
#saveRDS(TestdataSample, file="TXS251p1234a975.rds");
#saveRDS(TestdataSample, file="TXS251p1234a99.rds");
#saveRDS(TestdataSample, file="TXS251p1234a995.rds")
#saveRDS(TestdataSample, file="TXS126p1234a95.rds");
#saveRDS(TestdataSample, file="TXS63p1234a95.rds")
#######name of fig saved###########
#ggsave("TXS502p1234a95.eps",dpi=8000, dev='eps',width = 7.5, height = 3.5, unit="in", plot = fig1)
#ggsave("TXS502p1234a975.eps",dpi=8000, dev='eps',width = 7.5, height = 3.5, unit="in", plot = fig1)
#ggsave("TXS502p1234a99.eps",dpi=8000, dev='eps',width = 7.5, height = 3.5, unit="in", plot = fig1)
#ggsave("TXS502p1234a995.eps",dpi=8000, dev='eps',width = 7.5, height = 3.5, unit="in", plot = fig1)
#ggsave("TXS251p1234a95.eps",dpi=8000, dev='eps',width = 7.5, height = 3.5, unit="in", plot = fig1)
#ggsave("TXS251p1234a975.eps",dpi=8000, dev='eps',width = 7.5, height = 3.5, unit="in", plot = fig1)
#ggsave("TXS251p1234a99.eps",dpi=8000, dev='eps',width = 7.5, height = 3.5, unit="in", plot = fig1)
#ggsave("TXS251p1234a995.eps",dpi=8000, dev='eps',width = 7.5, height = 3.5, unit="in", plot = fig1)
#ggsave("TXS126p1234a95.eps",dpi=8000, dev='eps',width = 7.5, height = 3.5, unit="in", plot = fig1)
#ggsave("TXS63p1234a95.eps",dpi=8000, dev='eps',width = 7.5, height = 3.5, unit="in", plot = fig1)



