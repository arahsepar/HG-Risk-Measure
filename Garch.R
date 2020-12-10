library("quantmod")
library(zoo)
library("fGarch")
library(xts)
library(zoo)
getSymbols("NFLX",from="2019-04-24",to="2020-04-24")
getSymbols("AMZN",from="2019-04-24",to="2020-04-24")
getSymbols("BA",from="2019-04-24",to="2020-04-24")
getSymbols("ZM",from="2019-04-24",to="2020-04-24")
getSymbols("FB",from="2019-04-24",to="2020-04-24")
getSymbols("ShOP",from="2019-04-24",to="2020-04-24")
getSymbols("AC",from="2019-04-24",to="2020-04-24")
plot(AC$AC.Close)
plot(SHOP$SHOP.Close)
plot(BA$BA.Close)
plot(AMZN$AMZN.Close)
plot(NFLX$NFLX.Close)
plot(ZM$ZM.Close)
AMZNlogreturns<-diff(log(AMZN$AMZN.Close))[-1]
NFLXlogreturns<-diff(log(NFLX$NFLX.Close))[-1]
ZMlogreturns<-diff(log(MSFT$MSFT.Close))[-1]
FBlogreturns<-diff(log(FB$FB.Close))[-1]
BAlogreturns<-diff(log(BA$BA.Close))[-1]
SHOPlogreturns<-diff(log(SHOP$SHOP.Close))[-1]
AClogreturns<-diff(log(AC$AC.Close))[-1]

AC_shares<-25;ZM_shares<-100;AMZN_shares<-10;NFLX_shares<-40;BA_shares<-25; ShOP_shares<-10
portfolio_value<-
  AC_shares*AC$AC.Close+AMZN_shares*AMZN$AMZN.Close+BA_shares*BA$BA.Close+ShOP_shares*SHOP$SHOP.Close
#+NFLX_shares*NFLX$NFLX.Close+ZM*ZM$ZM.Close;
dailly_loss<--diff(portfolio_value)[-1];

mydata<-data.frame(dailly_loss)
library(tseries)
library(tidyverse)
library(ggthemes)
library(forecast)
library(tseries)
library(gridExtra)
library(rugarch)
adf.test(mydata[,1]) #Small P-value (<0.01) suggests there is sufficient evidence to reject the null hypothesis, therefore time series are considered stationary.
model.arima = auto.arima(mydata[,1], max.order = c(3 , 0 ,3) , stationary = TRUE
                         , trace = T , ic = 'aicc')

model.arima
model.arima$residuals %>% ggtsdisplay(plot.type = 'hist' , lag.max = 14)
ar.res = model.arima$residuals
Box.test(model.arima$residuals , lag = 14 , fitdf = 2 , type = 'Ljung-Box')



garch.loss <- garchFit(~arma(0,1)+garch(1,1), data=dailly_loss,
                       cond.dist='std');

s<-garch.loss@fit$coef[6];
#can s be not in \N, 
s<-floor(s)
vargarch<-dailly_loss-garch.loss@residuals+garch.loss@sigma.t*qt(0.95,s)/sqrt(s/(s-2));
#vargarchHG1<-dailly_loss-garch.loss@residuals+
 # garch.loss@sigma.t*((qt(0.95,s)^2+s)/(s-1))/sqrt(s/(s-2))*(dt(qt(0.95,s),s)/(1-.95));
vargarchHG1<-dailly_loss-garch.loss@residuals+garch.loss@sigma.t*HGp_a(Dist_type=Td(s),Model=Model,level=.95,p=1,eps=eps)$HG
#ES(qt,x=0.05,dist.type = "qf", df = s)
vargarchHG2<-dailly_loss-garch.loss@residuals+garch.loss@sigma.t*HGp_a(Dist_type=Td(s),Model=Model,level=.95,p=2,eps=eps)$HG
varhs <- rollapply(dailly_loss, 200, quantile, probs=0.95, align="right")
HGhs<-rollapply(dailly_loss, 200, p , probs=0.95, align="right")
HG_Value<-HGp_a(Dist_type=Dist_type,Model=Model,level=1-a,p=p,eps=eps)

##########################
alpha.tolerance <- 0.95
ESH<-function(alpha.tolerance,Data){
VaR.hist <- quantile(Data, probs = alpha.tolerance, 
                                              names = FALSE)
ES.hist <- mean(Data[Data > VaR.hist])
return(ES.hist)}
Partition(-mydata[,1][1:200],.05)$Value

#Rolling 
Window<-200
a<-.95
HG1<-vector()
HG2<-vector()

for(i in 1:52){
  j<-i-1+Window
  HG1[i]<-ESH(a,mydata[,1][i:j])
  HG2[i]<-Partition(-mydata[,1][i:j],1-a)$Value
}

plot(dailly_loss)
lines(vargarch,col=2)
lines(varhs,col=6)

plot(dailly_loss,ylim=c(-10000,10000))
lines(vargarchHG1,col=2,ylim=c(-10000,10000))
lines(vargarchHG2,col=3,ylim=c(-10000,10000))
lines(varhs,col=6)