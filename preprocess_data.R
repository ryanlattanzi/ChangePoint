
library(tidyverse)
dat <- read_csv("dat.csv")
####Analysis of real data#####
###Extract response variable and remove response from full data set########
y=dat$ViolentCrimesPerPop; dat_y_removed=dat[,-which((names(dat)) %in% "ViolentCrimesPerPop"==T)]
#### Extract independent variables (remove first five)
x_full=dat_y_removed[,-c(1,2,3,4,5)];r1=dim(x_full)[2] #remove first five
#############################################################################
##Reduce data to complete cases##############################################
df=data.frame(response=y,x_full)
df$OtherPerCap=as.numeric(df$OtherPerCap);df$LemasSwornFT=as.numeric(df$LemasSwornFT)
df$LemasSwFTPerPop=as.numeric(df$LemasSwFTPerPop)
#remove columns with more than 25%missing
comp_df <- df[lapply(df, function(x) sum(is.na(x)) / length(x)) < 0.85]
r3=dim(comp_df)[2]; comp_df = sapply(comp_df , as.numeric)
comp_df1=comp_df[complete.cases(comp_df),];
#############################################################################
####Change point variable target name
cptarget="medIncome"
#cptarget="population"
###identifying correlations with cp target variable
cor_full=cor(comp_df1[,-1]);
target_ind=which(colnames(cor_full)==cptarget)
#remove variables which are correlated (>0.7) with the cp variable
remove=names(which(abs(cor_full[target_ind,-target_ind])>0.75)) 
x_full2=comp_df1[,-which((colnames(comp_df1)) %in% remove==T)];
#############################################################################
####Extract change inducinf variable (scaled)#####
#w=scale(x_full2[,which(colnames(x_full2)==cptarget)],center = T,scale = T)
w=x_full2[,which(colnames(x_full2)==cptarget)]
###Design matrix without the change inducting variable####
x_full_without_w= x_full2[,-which((colnames(x_full2)) %in% cptarget==T)]
x_full3=scale(x_full_without_w,center=T,scale=T)##final design with response
x_full4=x_full3[,-1] #final design with correlated variables
#remove those iv's which are higly correlated (>0.95) to any other variables
corf=cor(x_full4)
additional_removals=colnames(x_full4)[which(table(which(abs(corf)>.95,arr.ind = T)[,2])>1)]
x_full5=x_full4[,-which(colnames(x_full4)%in% additional_removals ==T)]#final design
y=x_full3[,1] #response

write.csv(x_full5, file = 'feature.csv', row.names = FALSE)
write.csv(y, file = 'response.csv', row.names = FALSE)
write.csv(w, file = 'changepoint.csv', row.names = FALSE)




