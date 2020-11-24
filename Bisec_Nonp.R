#This function use the bisection method to calculate the unit root to the equation 
#E[(X-s^)+]^{p-1}]^p-(1-alpha)*(E[(X-s)^+]p)^{p-1}.
  #Inputs:
    #Function that want to find the roots
    #An interval [l,u] 
    #Number of iteration and the tol level 
    #Level of Risk
  #Out Put: 
    #Root to the eqaution
    # NUmber of Itr
Bisec_Nonp<- function (l, u, num, eps,level)
{
  a<-l
  b<-u
  source("Nonlinp.R")
  # if (!is.na(eps)) 
  #  warning("Deprecated: Argument 'eps' not used anymore.")
  # if (Nonlinp(level,a,p) * Nonlinp(level,b,p) > 0) 
  #  stop("f(a) and f(b) must have different signs.")
  
  h = abs(u - l)/num
  i = 0;
  j = 0;
  a1 = b1 = 0;
  while (i <= num) {
    a1 = l + i * h
    b1 = a1 + h
    if (Nonlinp(level,a1,p) == 0) {
      Point<-a1
      Value<-Nonlinp(level,a1,p)
      #print(a1)
      #print(Nonlinp(a,a1,p))
      break
    }
    else if (Nonlinp(level,b1,p) == 0) {
      Point<-b1
      Value<-Nonlinp(level,b1,p)
   #   print(b1)
   #   print(Nonlinp(a,b1,p))
      break
    }
    else if (Nonlinp(level,a1,p) * Nonlinp(level,b1,p) < 0) {
      repeat {
        if (abs(b1 - a1) < eps)
          break
        x <- (a1 + b1)/2
        if (Nonlinp(level,a1,p) * Nonlinp(level,x,p) < 0)
          b1 <- x
        else a1 <- x
      }
    #  print(j + 1)
      j = j + 1;
      p<-a1
      w<-b1
      Point = (a1 + b1)/2
      Value = Nonlinp(level,(a1 + b1)/2,p)
     # print(point)
      #print(Nonlinp(a,(a1 + b1)/2,p))
    }
    i = i + 1;
  }
  if (j != 0){
    print("Finding root is successful. ")}
 if (j==0) {print("Finding root is fail, change in the interval or increase the itr")
 }
  return(list(Value=Value,Point=Point,itr=j))
}
