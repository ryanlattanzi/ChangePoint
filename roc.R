
# ROC Curve for all three models
library(e1071)
source("svmcp_v2.R")
source('bic_v2.R')


tau0=0.7; n=250; p=250; s1=5; s2=5; tcp=floor(tau0*n)
b=c(1,rep(1,s1),rep(0,p-s1))
g=c(1,rep(0,s1),rep(1,s2),rep(0,p-s1-s2))
w=c(1:n)
pre_true=which(w<=tcp); post_true=which(w>tcp)
x=cbind(1,matrix(rnorm(n*p),n,p))
y=c()
y[pre_true]=ifelse((x[pre_true,]%*%b>=0),1,-1)
y[post_true]=ifelse((x[post_true,]%*%g>=0),1,-1)
negs = length(which(y==-1)); pos = length(which(y==1))


changepoint=svmcp(x,y,w); changepoint$cp
preds=c()
preds[1:changepoint$cp]=x[1:changepoint$cp,]%*%changepoint$beta
preds[(changepoint$cp+1):n]=x[(changepoint$cp+1):n,]%*%changepoint$gamma
lower = min(preds); upper = max(preds)
threshold1 = c(0,seq(lower,upper,length.out = 100))
fp1 = c(); tp1 = c()
for (i in 1:length(threshold1)){
  val = threshold1[i]
  ypredss=ifelse((preds>=val),1,-1)
  fp1[i] = length(which(ypredss==1 & y==-1))/negs
  tp1[i] = length(which(ypredss==1 & y==1))/pos
}





linear = bic(x,y,la.grid = seq(1,50,by=1))
pred = c(linear$model$b0+x%*%linear$model$coef)
lower = min(pred); upper = max(pred)
threshold2 = seq(lower,upper,length.out = 100)
fp2 = c(); tp2 = c()
for (i in 1:length(threshold2)){
  val = threshold2[i]
  ypredss=ifelse((pred>=val),1,-1)
  fp2[i] = length(which(ypredss==1 & y==-1))/negs
  tp2[i] = length(which(ypredss==1 & y==1))/pos
}






dat = as.data.frame(cbind(x,y))
tune.out = tune(svm,y~.,data = dat, 
                ranges = list(cost=c(0.1,1,10,100,1000),
                              gamma= c(0.5,1,2,3,4)))
radial = tune.out$best.model
rpreds=predict(radial,dat)
lower = min(rpreds); upper = max(rpreds)
threshold3 = seq(lower,upper,length.out = 100)
fp3 = c(); tp3 = c()
for (i in 1:length(threshold3)){
  val = threshold3[i]
  ypredss=ifelse((rpreds>=val),1,-1)
  fp3[i] = length(which(ypredss==1 & y==-1))/negs
  tp3[i] = length(which(ypredss==1 & y==1))/pos
}

plot(fp1,tp1, type = 'l', lty = 1, lwd=1, xlab = 'False Positive Rate', ylab = 'True Positive Rate',
     main = 'Comparative ROC Curves')
lines(fp2,tp2, col = 'blue', lty = 2,lwd=2)
lines(fp3,tp3, col = 'red', lty = 4, lwd=2)
legend('bottomright',legend=c("CPSVM", "LINEARSVM", 'RBFSVM'),
       col=c("black", "blue",'red'), lty=c(1,2,4), lwd = c(2,2,2), cex=0.8)




