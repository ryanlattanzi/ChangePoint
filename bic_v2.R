###bic


source("SVM_single.R")
bic=function(x,y,la.grid, type="aic"){
  times=Sys.time()
  n=nrow(x); p=ncol(x);  tloss=c();
  if(type=="aic"){const=2}; if(type=="bic"){const=log(n)}
  for(i in 1:(length(la.grid))){
    model=fitsvm(x,y,lambda = la.grid[i])
    S=which(model$coef!=0);subcoef=model$coef[S]
    predS=model$b0+as.matrix(x[,S])%*%subcoef
    hinge_predS=ifelse(((1-y*predS)>0),(1-y*predS),0)
    loss=(sum(hinge_predS));    nz=length(S)
    pen=nz*const;    tloss[i]= loss+pen}
  min.idx=which.min(tloss);  best.la=la.grid[min.idx]
  bestmodel=fitsvm(x,y,best.la)
  timee=Sys.time(); time=timee-times
  return(list(model=bestmodel,lambda=best.la,time=time[[1]], loss=tloss))}


#  n=200;p=100
#  x=matrix(rnorm(n*p),n,p)
#  x[1:(n/2),(1:2)]=x[1:(n/2),(1:2)]+2
#  y=c(rep(-1,(n/2)),rep(1,(n/2)))
#  m=bic(x,y)
# m$time
# plot(x[,1:2],col=3-y)
# abline(a=-m$model$b0/m$model$coef[2], b=-m$model$coef[1]/m$model$coef[2])

# 
# 
#fit=bic(x,y)
# 