
# Application
source("svmcp_v2.R")

x = as.matrix(cbind(1,read.csv('feature.csv')))
yraw = as.matrix(read.csv('response.csv'));yraw = c(yraw)
w = as.matrix(read.csv('changepoint.csv'));w=c(w)
y = ifelse(yraw>median(yraw),1,-1)

changepoint=svmcp(x,y,w)
changepoint$cp


