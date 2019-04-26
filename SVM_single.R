###Fit SVM for a single value of lambda

library(Matrix)
library(Rmosek)

fitsvm = function(x,y,lambda=1){
  if(length(y)!=length(x[,1])){stop("check data input")}
  n = length(y); p=ncol(x)
  r = list()
  r$sense="min"
  r$c=c(rep(1,n),0,rep(lambda,p),rep(lambda,p))
  A1=Diagonal(n)
  A2=y
  A3=y*x; A4=-y*x
  r$A=cbind(A1,A2,A3,A4)
  blc=rep(1,n); buc=rep(Inf,n); r$bc=rbind(blc,buc)
  blx=c(rep(0,n),-Inf,rep(0,2*p));bux=c(rep(Inf,n),Inf,rep(Inf,2*p));r$bx=rbind(blx,bux)
  out=mosek(r, opts=list(verbose=1))
  b0=out$sol$itr$xx[n+1]
  coef=out$sol$itr$xx[((n+2):(n+p+1))]-out$sol$itr$xx[((n+p+2):(n+2*p+1))]
  coef = round(coef,8)
  return(list(b0 = b0, coef = coef))}


#example
# n=200;p=100
# x=matrix(rnorm(n*p),n,p)
# x[1:(n/2),(1:2)]=x[1:(n/2),(1:2)]+2
# y=c(rep(-1,(n/2)),rep(1,(n/2)))
# plot(x[,1:2],col=3-y)
# 
# 
# fit=fitsvm(x,y,1)
# 
# plot(x[,1:2],col=3-y)
# abline(a=-(fit$b0/fit$coef[2]), b=-(fit$coef[1]/fit$coef[2]))
# 

##Check acc





