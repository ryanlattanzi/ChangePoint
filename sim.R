library(MASS)


numsim = 100

linearsvm = matrix(0,9,8);colnames(linearsvm) = c("n","p","Bias","MSE","FP","FN","Prediction Loss","Time")
radialsvm = matrix(0,9,8);colnames(radialsvm) = c("n","p","Bias","MSE","FP","FN","Prediction Loss","Time")
cpsvm = matrix(0,9,8);colnames(cpsvm) = c("n","p","Bias","MSE","FP","FN","Prediction Loss","Time")
nvals = c(150,250,350)
pvals = c(25,150,250)
case = 0

for (j in 1:length(nvals)){
  n = nvals[j]
  for (k in 1:length(pvals)){
    p = pvals[k]
    t1=c();t2=c();t3=c()
    predloss1=c();predloss2=c();predloss3=c()
    bias=c();mse=c();fp=rep(0,numsim);fn=rep(0,numsim)
    case = case+1
    for (i in 1:numsim){
      # Simulate Data
      tau0=0.7
      s1=5;s2=5;tcp=floor(tau0*n)
      library(Rmosek);
      b=c(1,rep(1,s1),rep(0,p-s1))
      g=c(1,rep(0,s1),rep(1,s2),rep(0,p-s1-s2))
      w=c(1:n)
      pre_true=which(w<=tcp); post_true=which(w>tcp)
      
      # X1 Simulation
      # x=cbind(1,matrix(rnorm(n*p),n,p))
      # X2 Simulation
      rho=0.5; S = rho**toeplitz(0:(p-1)); x = cbind(1, mvrnorm(n, rep(0,p), S))
      
      y=c();y[pre_true]=ifelse((x[pre_true,]%*%b>=0),1,-1)
      y[post_true]=ifelse((x[post_true,]%*%g>=0),1,-1)
      
      # Linear SVM over whole data set
      source('bic_v2.R')
      start1 = Sys.time()
      linear = bic(x,y,la.grid = seq(1,50,by=1))
      end1 = Sys.time(); t1[i] = end1-start1
      pred = linear$model$b0+x%*%linear$model$coef
      ypred=ifelse((pred>=0),1,-1)
      predloss1[i] = mean(ypred!=y)
      
      # Radial Kernel SVM on the entire data
      dat = as.data.frame(cbind(x,y))
      library(e1071)
      start2 = Sys.time()
      tune.out = tune(svm,y~.,data = dat, 
                      ranges = list(cost=c(0.1,1,10,100,1000),
                                    gamma= c(0.5,1,2,3,4)))
      radial = tune.out$best.model
      end2 = Sys.time(); t2[i] = end2-start2
      rpreds=predict(radial,dat)
      ypreds=ifelse((rpreds>=0),1,-1)
      predloss2[i]=mean(ypreds!=y)
      
      # Changepoint SVM
      source("svmcp_v2.R")
      start3 = Sys.time()
      changepoint=svmcp(x,y,w)
      end3 = Sys.time(); t3[i]=end3-start3
      preds=c()
      if(is.finite(changepoint$cp)==F){preds=x%*%changepoint$gamma}
      if(is.finite(changepoint$cp)==T){
        preds[1:changepoint$cp]=x[1:changepoint$cp,]%*%changepoint$beta
        preds[(changepoint$cp+1):n]=x[(changepoint$cp+1):n,]%*%changepoint$gamma}
      ypredss=ifelse((preds>=0),1,-1);predloss3[i] = mean(ypredss!=y)
      htau = changepoint$cp/n
      #fp and fn
      if(is.finite(htau)==F & is.finite(tau0)==T){fn[i]=1}
      if(is.finite(htau)==T & is.finite(tau0)==F){fp[i]=1}
      
      # For tau0 finite
      if(is.finite(htau)==F & tau0<=0.5){htau=0};if(is.finite(htau)==F & tau0>0.5){htau=1}
      
      # For tau0 infinite
      #if(is.finite(htau)==F){htau=0;tau0=0}
      #if(is.finite(htau)==T & htau<=0.5){tau0=0};if(is.finite(htau)==T & htau>0.5){tau0=1}
      
      bias[i]=(htau)-tau0;mse[i]=((htau)-tau0)^2
    }
    linearsvm[case,] = c(n,p,NA,NA,NA,NA,mean(predloss1),mean(t1))
    radialsvm[case,] = c(n,p,NA,NA,NA,NA,mean(predloss2),mean(t2))
    cpsvm[case,] = c(n,p,mean(bias),mean(mse),sum(fp),sum(fn),mean(predloss3),mean(t3))
    print(paste0("Done with case ", case))
  }
  
}
