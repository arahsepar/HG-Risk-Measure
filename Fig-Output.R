#The following Files have been saved: 
        #saveRDS(df, file="p2-Exp1.rds")
        #saveRDS(df, file="p2-Unif01.rds"),
        #saveRDS(df, file="p2-Norm01.rds")
df2 <- readRDS("p2-Exp1.rds")
#df2 <- readRDS("p2-Unif01.rds")
#df2 <- readRDS("p2-Norm01.rds")

library(ggplot2)
theme_set(theme_bw())

  gg1<-ggplot(df2, aes(x=b, y=SN, fill=SN>=0)) +
  geom_bar(stat = "identity", width = 0.01) +
  labs(y = " value s*", x = expression(alpha)) +
  coord_flip() + theme_minimal()+scale_fill_hue(l = 50)+
  guides(fill = FALSE)+ ylim(-6.5, 6.5)+xlim(0,1)
  plot(gg1)  
  #ggsave("p2-EXP1-s.eps",width = 4, height = 6, plot = gg1)
  #ggsave("p2-Unif01-s.eps",width = 4, height = 6, plot = gg1)
  #ggsave("p2-Norm01-s.eps",width = 4, height = 6, plot = gg1)
 
  #Make sure you change this for each dist. 
      #a0<-1/2 for Exp 2
      #a0<-1/4 for Unif 2/3
      #a0<-1-1/pi, sqrt(pi/2)
  a0<-sqrt(pi/2)
  gg2<-ggplot(df2, aes(x=b, y=HGN, fill=HGN>=2))+
  geom_bar(stat = "identity", width = 0.01) +
  labs(y = "HG value", x = expression(alpha)) +
  coord_flip() + theme_minimal()+scale_fill_hue(l = 50)+
  guides(fill = FALSE)+ ylim(-6.5, 6.5)+xlim(0,1)
  plot(gg2) 
  ggsave("p2-EXP1-v.eps",width = 4, height = 6, plot = gg2)
  #ggsave("p2-Unif01-v.eps",width = 4, height = 6, plot = gg2)
  #ggsave("p2-Norm01-v.eps",width = 4, height = 6, plot = gg2)
  
  
  #
  dfa <-readRDS("p17Norm01-v.rds")
  dfb<-readRDS("p17Norm01-s.rds")

  #here we remove alpha=.01, since we facing +Inf  for p=7,8
  dfa<-dfa[-c(1),]
  dfb<-dfb[-c(1),]
  
  View()
  require(ggplot2)
  #require(dplyr)
  gg3<-ggplot() +     # basic graphical object
    geom_point(aes(dfa$b,dfa$H0 , color="p=1"))  +  
    geom_point(aes(dfa$b,dfa$H1, color="p=2")) +  
    geom_point(aes(dfa$b,dfa$H2, color="p=3"))  +  
    geom_point(aes(dfa$b, dfa$H3 , color="p=4")) +  
    geom_point(aes(dfa$b,dfa$H4, color="p=5")) +  
    geom_point(aes(dfa$b,dfa$H5, color="p=6"))  +  
    geom_point(aes(dfa$b, dfa$H6 , color="p=7")) +  
   # geom_hline(yintercept = sqrt(pi/2),linetype="dotted", 
  #             color = "grey", size=1.5)+
    geom_smooth()+ coord_flip() +
    theme_minimal()+scale_colour_hue(l = 40)+
    labs(x=expression(alpha), y="HG value ") + 
    theme(legend.position="bottom")
  plot(gg3)
 # ggsave("p17-Norm01-v.eps",width = 4, height = 6, plot = gg3)
  
  
  gg4<-ggplot() +     # basic graphical object
    geom_point(aes(dfb$b,dfb$S0 , color="p=1"))  +  
    geom_point(aes(dfb$b,dfb$S1, color="p=2")) +  
    geom_point(aes(dfb$b,dfb$S2, color="p=3"))  +  
    geom_point(aes(dfb$b, dfb$S3 , color="p=4")) +  
    geom_point(aes(dfb$b,dfb$S4, color="p=5")) +  
    geom_point(aes(dfb$b,dfb$S5, color="p=6"))  +  
    geom_point(aes(dfb$b, dfb$S6 , color="p=7")) + 
    geom_hline(yintercept = 0, color = "grey", size=1)+
    geom_smooth()+ coord_flip() +
    theme_minimal()+scale_colour_hue(l = 40)+
    labs(x=expression(alpha), y="value s*") + 
    theme(legend.position="bottom")
  plot(gg4)
 # ggsave("p17-Norm01-s.eps",width = 4, height = 6, plot = gg4)
  
  
  
  
  
  