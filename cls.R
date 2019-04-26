###Classification loss
##input x with intercept column

cls=function(x,y,w,k,hb,hg){
  #if(is.finite(k)==F){} #which one do I choose? maybe add positive Inf just like in leastsq.
  #if(is.finite(k)==T){}
  pre=which(w<=k);post=which(w>k)
  pred=c();  pred[pre]= x[pre,]%*%hb; pred[post]=x[post,]%*%hg
  ypred=ifelse((pred>=0),1,-1);  cls=mean(ypred!=y)
  return(cls)
}
