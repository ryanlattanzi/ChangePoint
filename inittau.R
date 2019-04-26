

# Initial tau
source("svmcp_v2.R")

set.seed(123)
tau0=0.5; n=500; p=25; s1=5; s2=5; tcp=floor(tau0*n)
b=c(1,rep(1,s1),rep(0,p-s1))
g=c(1,rep(0,s1),rep(1,s2),rep(0,p-s1-s2))
w=c(1:n)
pre_true=which(w<=tcp); post_true=which(w>tcp)
x=cbind(1,matrix(rnorm(n*p),n,p))
y=c()
y[pre_true]=ifelse((x[pre_true,]%*%b>=0),1,-1)
y[post_true]=ifelse((x[post_true,]%*%g>=0),1,-1)

init = seq(40,460,length.out = 50)
htau = c()

for (i in 1:length(init)){
  initial = init[i]
  changepoint=svmcp(x,y,w,sp=initial,bic.const = 0.25)
  estimate = changepoint$cp
  if(is.finite(estimate) == F){estimate = 0}
  htau[i] = estimate
}
htau = htau/n; bias = abs(htau-tau0)
plot(init/n, bias, ann = FALSE, type="l")
mtext(side = 1, text = expression(tau^{(0)}), line = 2.5)
mtext(side = 2, text = expression(paste('| ', tau[0]-hat(tau), ' |')), line = 2.4)
lines(init/n, bias)

