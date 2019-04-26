source("bic_v2.R")

k = 10
split_point=c()

for(j in 1:k){
  ##Simulated data
  b0=g0=1;b1=-1; b2=-2; b3=-1;  g1=1;g2=-1;g3=0
  n1=75;p=100
  x1=matrix(rnorm(n1*p),n1,p)
  y1=c(); y1[which(b0+b1*x1[,1]+b2*x1[,2]+b3*x1[,3]>=0)]=1;
  y1[which(b0+b1*x1[,1]+b2*x1[,2]+b3*x1[,3]<0)]=-1
  #plot(x1[,1:2],col=3-y1)
  n2=200;  x2=matrix(rnorm(n2*p),n2,p)
  y2=c(); y2[which(g0+g1*x2[,1]+g2*x2[,2]>=0)]=1;
  y2[which(g0+g1*x2[,1]+g2*x2[,2]<0)]=-1
  #plot(x2[,1:2],col=3-y2)
  x=rbind(x1,x2);  y=c(y1,y2)
  #plot(x[,1:2],col=3-y)
  
  
  ####
  n=nrow(x);
  sp=0.5; spi=floor(n*sp)
  pre=c(1:spi); post=c((spi+1):n)
  
  #prefit=bic(x[pre,],y[pre])$model
  #postfit=bic(x[post,],y[post])$model
  
  prefit=bic(x[pre,],y[pre])
  postfit=bic(x[post,],y[post])
  
  loss=c()
  for(i in 1:(n-1)){
    pre=c(1:i); post=c((i+1):n)
    pred_pre=c(prefit$model$b0)+x[pre,]%*%prefit$model$coef
    pred_post=c(postfit$model$b0)+x[post,]%*%postfit$model$coef
    pred=c(pred_pre,pred_post);  ypred=ifelse((pred>=0),1,-1)
    loss[i]=mean(ypred!=y)}
  
  split_point[j] = which.min(loss)
}
split_point

plot(loss,type="l")

