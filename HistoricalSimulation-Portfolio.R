
#INPUTS:
# Time Series: Here is an example that how we obtain the data
                #sp500 <- new.env();
                #getSymbols("^GSPC", env = sp500,  src = "yahoo",
                #from = as.Date("1990-01-04"), to = as.Date("2020-12-01"));
                #GSPC <- sp500$GSPC;
                #GSPCReturn<-diff(log(GSPC$GSPC.Close))[-1];
                #Testdata<--diff(log(GSPC$GSPC.Close))[-1]*100
#M1: Rolling Window size, e.g 502 for two years 
#alpha
#p

# We use the rolling historical method here.

library("quantmod")
library("zoo")
library("ggplot2")
source("EmpiricalHG2.R")
source("EmpiricalHGP.R")

#Nasdaq<-new.env();getSymbols("^IXIC", env = Nasdaq,  src = "yahoo",from = as.Date("1990-01-04"), to = as.Date("2020-12-01"));IXIC <- Nasdaq$IXIC
#TO<- new.env();getSymbols("^GSPTSE", env = TO,  src = "yahoo", from = as.Date("1990-01-04"), to = as.Date("2020-12-01"));GSPTSE <- TO$GSPTSE
#Nikie<- new.env();getSymbols("^N225", env = Nikie,  src = "yahoo",from = as.Date("1990-01-04"), to = as.Date("2020-12-01"));N225 <- Nikie$N225

#readRDS(file="NasdaqRawData.rds")
#Testdata<--diff(log(IXIC$IXIC.Close))[-1]*100
readRDS(file="NikieRawData.rds")
Testdata<--diff(log(N225$N225.Close))[-1]*100
#readRDS(file="TSXRawData.rds")
#Testdata<--diff(log(GSPTSE$GSPTSE.Close))[-1]*100

###########################################################


M1<-502; 
alpha <-1-0.95;
p3<-3;
p4<-4;
require("cvar")
Testdata<-na.approx(Testdata)
xsortT<--data.matrix(Testdata, rownames.force = NA)
m<-length(xsortT)
hg1<- rollapply(xsortT, M1, function(xsortT) ES(xsortT,p=alpha, method="historical"), align="right")
hg2<- rollapply(xsortT, M1, function(xsortT) EmpiricalHG2(xsortT,alpha)$Value, align="right")
hg3<- rollapply(xsortT, M1, function(xsortT) EmpiricalHGP(xsortT,alpha,p3)$Value, align="right")
hg4<- rollapply(xsortT, M1, function(xsortT) EmpiricalHGP(xsortT,alpha,p4)$Value, align="right")
TestdataSample<-cbind(Testdata[63:m],hg1,hg2,hg3,hg4)
#++++++++++++++++++Can used from save data++++++++++++++++++#
#TestdataSample<-readRDS("NIK502p1234a995.rds")
library(extrafont)
loadfonts(device = "win")
windowsFonts(Times = windowsFont("TT Times New Roman"))
fig1<-  ggplot() + 
  geom_line(aes(index(TestdataSample), TestdataSample$N225.Close), color="grey")+
  geom_line(aes(index(TestdataSample), TestdataSample$N225.Close.1, color="p=1")) + 
  geom_line(aes(index(TestdataSample), TestdataSample$N225.Close.2, color="p=2")) + 
  geom_line(aes(index(TestdataSample), TestdataSample$N225.Close.3, color="p=3")) + 
  geom_line(aes(index(TestdataSample), TestdataSample$N225.Close.4, color="p=4")) + 
  theme_set(theme_bw(base_size=8, base_family='Times New Roman'))+
  theme_minimal()+scale_colour_hue(l = 40)+
  labs(x="year", y=NULL,color = NULL) + theme(legend.position="bottom")
  plot(fig1)
