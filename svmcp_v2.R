
svmcp=function(x,y,w,sp=NULL,la=NULL,museq=seq(0.01,1,by=0.01),bic.const=1){
  n=nrow(x); p=ncol(x)
  if(is.null(sp)==T){sp=quantile(w,0.5)[[1]]}
  if(is.null(la)==T){la=seq(7,20,by=1)}
  if(length(y)!=n |length(w)!=n){stop("x, y, and w not coherent: check input")}
  ##Step 0
  pre_initial=which(w<=sp); post_initial=which(w>sp)
  source("bic_v2.R")
  prefit=bic(x[pre_initial,-1],y[pre_initial],la.grid=la) ;hb0=c(prefit$model$b0,prefit$model$coef)
  postfit=bic(x[post_initial,-1],y[post_initial],la.grid=la); hg0=c(postfit$model$b0,postfit$model$coef)
  ###Step 1
  loss=c(); norm=c(); tsq=c(-Inf,sort(w)[11:(n-10)]); 
  source("cls.R")
  source("znorm.R")
  for(i in 1:(length(tsq))){loss[i]=cls(x,y,w,k=tsq[i],hb=hb0,hg=hg0); norm[i]= znorm(tsq[i])}
  obj=matrix(0,length(tsq),length(museq));
  for (j in 1:length(museq)){obj[,j]=(loss)+(museq[j])*norm}
  #minimums for each value of mu
  ind=c();for (j in 1:length(museq)){ind[j]=which.min(obj[,j])}
  k_new_seq=tsq[ind]
  nk_new=length(unique(k_new_seq));knew_unq=unique(k_new_seq);bicvals=c()
  for(j in 1:nk_new){kcp=knew_unq[j];
    if(is.finite(kcp)==T){pre=which(w<=kcp);post=which(w>kcp)
    pre.fit=bic(x[pre,-1],y[pre],la.grid=la); hb1=c(pre.fit$model$b0,pre.fit$model$coef)
    post.fit=bic(x[post,-1],y[post],la.grid=la); hb2=c(post.fit$model$b0,post.fit$model$coef)
    bicvals[j]=log(cls(x,y,w,k=kcp,hb1,hb2))+bic.const*((log(n))/n)}
    if(is.finite(kcp)==F){tfit=bic(x[,-1],y,la.grid = la); hb=c(tfit$model$b0,tfit$model$coef)
    pred=c();pred= x%*%hb;ypred=ifelse((pred>=0),1,-1)
    err=mean(ypred!=y);bicvals[j]=log(err)}}
  k_new=knew_unq[which.min(bicvals)]
  # Step 2
  if(is.finite(k_new)==F){hb_new=rep(0,p+1);
  fing=bic(x[,-1],y,la.grid=la);hg_new=c(fing$model$b0,fing$model$coef)}
  if(is.finite(k_new)==T){pre=which(w<=k_new);post=which(w>k_new)
  finb=bic(x[pre,-1],y[pre],la.grid=la);hb_new=c(finb$model$b0,finb$model$coef)
  fing=bic(x[post,-1],y[post],la.grid=la);hg_new=c(fing$model$b0,fing$model$coef)}
  out=list(beta=hb_new,gamma=hg_new,cp=k_new,sp=sp)
  return(out)}

  

