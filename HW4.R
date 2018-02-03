library(mfp)
library(regtools)
library(freqparcoord)
#----------------page 61---------------------------------
##1. In Section 1.12.1.2, the reader was reminded that the results of a cross-validation are random, 
##due to the random partitioning into training and test sets. Try doing several runs of the linear and k-NN code in that section, 
##comparing results.
data(mlb)
# split train dataset
xvalpart<- function(data,p) {
  n<- nrow(data)
  ntrain<- round(p*n)
  trainidxs<- sample(1:n,ntrain,replace=F)
  list(train=data[trainidxs,],
       valid=data[-trainidxs ,])
}

# linear model cross-validation
xvallm<- function(data,ycol,predvars,p,meanabs=T){
  tmp<-xvalpart(data,p)
  train<-tmp$train
  valid<-tmp$valid
  trainy<-train[,ycol]
  trainpreds<-train[,predvars]
  trainpreds<-as.matrix(trainpreds)
  lmout<-lm(trainy ~ trainpreds)
  validpreds<-as.matrix(valid[,predvars])
  predy<-cbind(1,validpreds)%*%coef(lmout)
  realy<-valid[,ycol]
  if (meanabs) return (mean(abs(predy-realy)))
  list(predy=predy,realy=realy)
}

xvallm(mlb,5,c(4,6),2/3)
xvallm(mlb,5,c(4,6),1/3)

#knn cross-validation
xvalknn<- function(data,ycol,predvars,k,p,meanabs=T){
  data<-data[,c(predvars,ycol)]
  ycol<-length(predvars)+1
  tmp<-xvalpart(data,p)
  train<-tmp$train
  valid<-tmp$valid
  valid<-as.matrix(valid)
  xd<-preprocessx(train[,-ycol],k)
  kout<-knnest(train[,ycol],xd,k)
  predy<-predict(kout,valid[,-ycol],T)
  realy<-valid[,ycol]
  if (meanabs) return (mean(abs(predy-realy)))
  list(predy=predy,realy=realy)
}
xvalknn(mlb,5,c(4,6),25,2/3)
xvalknn(mlb,5,c(4,6),25,1/3)

##2. Extend (1.28) to include interaction terms for age and gender, and age2 and gender. 
##Run the new model, and find the estimated effect of being female, for a 32-year-old person with a Master's degree.
data(prgeng)
prgeng$age2<-prgeng$age^2
edu<-prgeng$educ
prgeng$ms<-as.integer(edu==14)
prgeng$phd<-as.integer(edu==16)
prgeng$fem<-prgeng$sex-1
tmp<-prgeng[edu>=13,]
pe<-tmp[,c(1,12,9,13,14,15,8)]
pe<-as.matrix(pe)
lm(wageinc ~ age+age2+wkswrkd+ms+phd+fem+age:fem+age2:fem, data=prgeng)
fm<-which(prgeng$fem==1)
female<-prgeng[fm,]
lm(wageinc~age,data = female)
lmout<-lm(wageinc~age+ms,data=female)
predict(lmout,data.frame(age=27,ms=1))
# Wageinc for a 32-year-old female with a Master is 54754.82

##3. Consider the bodyfat data mentioned in Section 1.2. Use lm() to form a prediction equation for density from the other variables (skipping the first three), 
##and comment on whether use of indirect methods in this way seems feasible.
data(bodyfat)
head(bodyfat)
model<-lm(density ~ age+weight+height+neck+chest+abdomen+hip+thigh+knee+ankle+biceps+forearm+wrist,data=bodyfat)
summary(model)
# the model seems to be feasible since R2 and adjust R2 is big enough.

##4. (a) Write English prose that relates the overall mean height of people and the gender-specific mean heights.
# The national mean height is a weighted average of female and male's mean heights.

## (b) Write English prose that relates the overall proportion of people taller than 70 inches to the gender-specific proportions.
# A weighted average proportion of female and male's mean heights which taller than 70 inches.



#----------------page 120---------------------------------
##1. (a) Form an approximate 95% confidence interval for b6 in the model(1.28).
## (b) Form an approximate 95% confidence interval for the gender effect for Master's degree holders, b6 + b7, in the model (1.28).

data(prgeng)
prgeng$age2<-prgeng$age^2
edu<-prgeng$educ
prgeng$ms<-as.integer(edu==14)
prgeng$phd<-as.integer(edu==16)
prgeng$fem<-prgeng$sex-1
tmp<-prgeng[edu>=13,]
pe<-tmp[,c(1,12,9,13,14,15,8)]
pe<-as.matrix(pe)
lmout<-lm(wageinc ~ age+age2+wkswrkd+ms+phd+fem, data=prgeng)
summary(lmout)
#a. 95% confidence interval for b6:
summary(lmout)$coefficients['fem',1]+1.96*summary(lmout)$coefficients['fem',2]
summary(lmout)$coefficients['fem',1]-1.96*summary(lmout)$coefficients['fem',2]
# CI: (-12866.88,-10102.1)
#b. 95% confidence interval for b6+b7:
lmout1<-lm(wageinc ~ age+age2+wkswrkd+ms+phd+fem+fem:ms, data=prgeng)
summary(lmout1)
summary(lmout1)$coefficients[7,1]+1.96*summary(lmout1)$coefficients[7,2]
summary(lmout1)$coefficients[7,1]-1.96*summary(lmout1)$coefficients[7,2]
# CI: (-12255.22,-9148.528)

##2. The full bikeshare dataset spans 3 years' time. Our analyses here have only used the first year. 
## Extend the analysis in Section 2.8.5 to the full data set, adding dummy variables indicating the second and third year.
## Form an approximate 95% confidence interval for the difference between the coefficients of these two dummies.
shar<-read.csv('~/Dropbox (Personal)/UC Davis/BAX452/MLDatasets/day.csv')
head(shar)
names(shar)[15]<-'reg'
tmp<-1:nrow(shar)
shar$year1<-ifelse(tmp<=365,1,2)
shar$year2<-ifelse(tmp>365,2,1)
View(shar)
shar$temp2 <- shar$temp ^ 2
shar$clearday <- as.integer(shar$weathersit == 1)
model_bike <- lm(reg ~ temp + temp2 + workingday + clearday + year1:year2, data = shar)
summary(model_bike)
summary(model_bike)$coefficients[6,1]+1.96*summary(model_bike)$coefficients[6,2]
summary(model_bike)$coefficients[6,1]-1.96*summary(model_bike)$coefficients[6,2]
# CI for year dummy vaiables: (535.0517,609.1182)

##3. Suppose we are studying growth patterns in children, at k particular ages. 
## Denote the height of the ith child in our sample data at age j by Hij , with Hi = (Hi1; :::;Hik)′ denoting the data for child i. 
## Suppose the population distribution of each Hi is k-variate normal with mean vector u and covariance matrix E.
## Say we are interested in successive differences in heights, Dij = Hi;j+1 􀀀Hij ; j = 1; 2; :::; k 􀀀 define Di = (Di1; :::;Dik)′.
## Explain why each Di is (k􀀀1)-variate normal, and derive matrix expressions for the mean vector and covariance matrices.




##4. In the simulation in Section 2.9.3, it is claimed that p2 = 0.50. Confirm this through derivation.
# p^2 = 1 - Var(E) / Var(Y) 
#     = 1 - Var(E) / (Var(u(X)) + Var(E)) 
#     = 1 - p/(p+p) = 0.5
