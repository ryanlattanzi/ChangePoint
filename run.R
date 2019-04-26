


n=150;p=150;s1=5;s2=5;tcp=100; nsim=10
library(Rmosek);
##Coefficients 
b=c(1,rep(1,s1),rep(0,p-s1))
g=c(1,rep(0,s1),rep(1,s2),rep(0,p-s1-s2))
source("svmcp_v2.R")
w=c(1:n)
pre_true=which(w<=tcp); post_true=which(w>tcp)
x=cbind(1,matrix(rnorm(n*p),n,p))
y=c();y[pre_true]=ifelse((x[pre_true,]%*%b>=0),1,-1);
y[post_true]=ifelse((x[post_true,]%*%g>=0),1,-1);
m=svmcp(x,y,w)
m$cp
