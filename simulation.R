
n=200;p=100;s1=5;s2=5;tcp=0.25; nsim=10;NumCores=6

library(Rmosek); library(doSNOW); library(parallel)


sim_par_svmcp=function(n=400,p=100,s1=5,s2=5,tcp=0.25, nsim=100,NumCores=NULL){
  ##Coefficients  
  b=c(1,rep(1,s1),rep(0,p-s1))
  g=c(1,rep(0,s1),rep(1,s2),rep(0,p-s1-s2))
  if(is.null(NumCores)==T){NumCores <- max(1, detectCores())};  
  cl <- makeCluster(NumCores);  registerDoSNOW(cl)
  pb=txtProgressBar(max = nsim, style = 3)
  progress <- function(r) setTxtProgressBar(pb,r);opts <- list(progress = progress)
  f=foreach (i = 1:nsim, .packages = c("Matrix", "Rmosek")) %dopar% {
  source("svmcp_v2.R")
  w=runif(n,0,1);pre_true=which(w<=tcp); post_true=which(w>tcp)
  x=cbind(1,matrix(rnorm(n*p),n,p))
  y=c();y[pre_true]=ifelse((x[pre_true,]%*%b>=0),1,-1); 
  y[post_true]=ifelse((x[post_true,]%*%g>=0),1,-1); 
  m=svmcp(x,y,w)
    }
  closeAllConnections()
}

sim_par_svmcp()
#simulate data



m$cp


