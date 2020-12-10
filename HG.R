HG<-function(sstart,Dist_type,Model,level,p, control=list(), quiet=FALSE, ... )
  {
    ctrl <- list(maxit = 2500, tol =.25*.Machine$double.eps, trace = FALSE)
    namc <- names(control)
    if (!all(namc %in% names(ctrl))) 
      stop("unknown names in control: ", namc[!(namc %in% names(ctrl))])
    ctrl[namc] <- control
    maxit <- ctrl$maxit
    tol <- ctrl$tol
    trace <- ctrl$trace
  upper<-upper<-q.l(Dist_type)(1) #quantile 1
  eqn<-function(s){y<-numeric(1)
  #y<-Nonlin(a,s)}
  y<-Nonlinp(level,s,p)}
  
  oldw <- getOption("warn")
  options(warn = -1)
  #Note the option #y1<-BBoptim(par=sstart,fn=eqn, method=c(2,3,1), control=list(maxit=3500,ftol =.Machine$double.eps, gtol=.Machine$double.eps,eps= .Machine$double.eps,trace = TRUE, triter = 1))
  #y1<-nleqslv(sstart, eqn, global="dbldog",method="Broyden",control=list(maxit = 2500,ftol=tol,xtol=.25*.Machine$double.eps))
  
  y1<-BBsolve(par=sstart,fn=eqn, method=c(2,3,1), quiet=FALSE, control=list(trace = TRUE, triter = 1, NM=TRUE),...)
  if(y1$message=="Maximum limit for iterations exceeded"){
    stop("\n Try another starting value or Increase the maxit\n")
  }
  
  options(warn = oldw)
  #sstar<-y1$x
  sstar<-y1$par
  
  #HG_Value<-funcE1(s)/(1-a)+s
  HG_Valuep<-(funcEp(sstar,p)/(1-level))^(1/p)+sstar
  # return(list(Value=HG_Value, min=s))
  return(list(Value=HG_Valuep, min=sstar,detail=y1))
}
