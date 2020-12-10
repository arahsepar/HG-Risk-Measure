#This is the nonlinear eq. that we will help to find the unique solution of this eq. when p>2
EM_Nonlinp<-function(A,level,t,p){
  Xsort<-sort(-A)
  n<-length(Xsort)
  p<-p
  df<-data.frame(Xsort-t,rep(0,n))
  df$max <- do.call(pmax, c(df, na.rm = TRUE))
  sum((df$max)^(p-1))^p-(1-level)*n*((sum((df$max)^p))^(p-1))
}
