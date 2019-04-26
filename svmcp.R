
svmcp=function(x,y,w,sp=NULL){
  n=nrow(x); p=ncol(x)
  if(is.null(sp)==T){sp=quantile(w,0.5)[[1]]}
  if(length(y)!=n |length(w)!=n){stop("x, y, and w not coherent: check input")}
  ##Step 0
  pre_initial=which(w<=sp); post_initial=which(w>sp)
  source("bic_v2.R")
  prefit=bic(x[pre_initial,-1],y[pre_initial]) ;hb0=c(prefit$model$b0,prefit$model$coef)
  postfit=bic(x[post_initial,-1],y[post_initial]); hg0=c(postfit$model$b0, postfit$model$coef)
  ###Step 1
  loss=c();  tsq=sort(w)[11:(n-10)]; 
  source("cls.R")
  for(i in 1:(length(tsq))){loss[i]=cls(x,y,w,k=tsq[i],hb=hb0,hg=hg0)}
  ind=which.min(loss);   k_new=tsq[ind]
  return(list(cp=k_new,sp=sp,loss=loss))}

  

