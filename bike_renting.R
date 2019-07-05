#load the data
bike_data=read.csv("C:/Users/Vikash Singh/Desktop/r and python/data/day.csv")
summary(bike_data)
str(bike_data)
#dteday and instant are not requried in the modelling of the data and some values of dteday are missing also so we try not to include that data
bike_data <- bike_data[-c(0:2)]
str(data)
str(bike_data)
#as all the variables are well normaliszed so we try to develop a model on the data
#split data into train and test
set.seed(123)
ind <-sample(2,nrow(bike_data),replace=TRUE,prob=c(0.7,0.3))
train <- bike_data[ind==1,]
validate<-bike_data[ind==2,]
#develop a regression model on the training data
library(rpart)
library(MASS)
#using dicision tree
dt=rpart(cnt ~ .,data=train,method = "anova")
dt
prediction_dt=predict(dt,validate[,-14])
#defining a function to detect the error in model
mape=function(x,xt)
{
  mean(abs((x-xt)/x))*100
}
mape(validate[,14],prediction_dt)
library(DMwR)
regr.eval(validate[,14],prediction_dt,stats = c("mae","rmse","mape"))
#     mae     rmse     mape 
#4498.279 4896.599      0.1370883 
#as decision tree gives mape error of 13%




#using linear regression
#load the data
bike_data=read.csv("C:/Users/Vikash Singh/Desktop/r and python/data/day.csv")
summary(bike_data)
str(bike_data)
#dteday and instant are not requried in the modelling of the data and some values of dteday are missing also so we try not to include that data
bike_data <- bike_data[-c(0:2)]
str(data)
str(bike_data)
#as all the variables are well normaliszed so we try to develop a model on the data
#split data into train and test
set.seed(123)
ind <-sample(2,nrow(bike_data),replace=TRUE,prob=c(0.7,0.3))
train <- bike_data[ind==1,]
validate<-bike_data[ind==2,]

library(usdm)
vif(bike_data[,-14])
vifcor(bike_data[,-14],th=0.9)
lm_model=lm(cnt ~ .,data=train)
summary(lm_model)
predictions_lm=predict(lm_model,validate[,1:13])
predictions_lm
regr.eval(validate[,14],predictions_lm,stats = c("mae","rmse","mape"))
#mae         rmse         mape 
#868.5882353 1120.6274099    0.2366936 



#using linear regression and deleting the variable which is having collinearity problem
#using linear regression
#load the data
bike_data=read.csv("C:/Users/Vikash Singh/Desktop/r and python/data/day.csv")
summary(bike_data)
str(bike_data)
#dteday and instant are not requried in the modelling of the data and some values of dteday are missing also so we try not to include that data
bike_data <- bike_data[-c(0:2)]
str(data)
str(bike_data)
#as all the variables are well normaliszed so we try to develop a model on the data
#split data into train and test
set.seed(123)
ind <-sample(2,nrow(bike_data),replace=TRUE,prob=c(0.7,0.3))
train <- bike_data[ind==1,]
validate<-bike_data[ind==2,]

library(usdm)
vif(bike_data[,-14])
vifcor(bike_data[,-14],th=0.9)
train<-train[-c(9)]
validate<-validate[-c(9)]
lm_model=lm(cnt ~ .,data=train)
summary(lm_model)
predictions_lm=predict(lm_model,validate[,1:12])
predictions_lm
regr.eval(validate[,13],predictions_lm,stats = c("mae","rmse","mape"))
#      mae         rmse         mape 
#1.358069e-12 1.770626e-12 4.611104e-16
plot(lm_model)



#support vector regression model
#load the data
bike_data=read.csv("C:/Users/Vikash Singh/Desktop/r and python/data/day.csv")
summary(bike_data)
str(bike_data)
#dteday and instant are not requried in the modelling of the data and some values of dteday are missing also so we try not to include that data
bike_data <- bike_data[-c(0:2)]
str(data)
str(bike_data)
#as all the variables are well normaliszed so we try to develop a model on the data
#split data into train and test
set.seed(123)
ind <-sample(2,nrow(bike_data),replace=TRUE,prob=c(0.7,0.3))
train <- bike_data[ind==1,]
validate<-bike_data[ind==2,]

library(e1071)
svmt<-svm(cnt~.,data=train,kernel='linear',cost=1.0,epsilon=0.1)
svmt
predictions_svr=predict(svmt,validate[,1:13])
predictions_svr
mape(validate[,14],predictions_svr)
regr.eval(validate[,14],predictions_svr,stats = c("mae","rmse","mape"))
#mae        rmse        mape 
#82.03522368 98.73907660  0.02579579 


