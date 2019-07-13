#Bike Renting problem solved and the number of bikes to be predicted given several condition using several regressor model.

#load the data file.
bike_data=read.csv("C:/Users/Vikash Singh/Desktop/r and python/data/day.csv")
summary(bike_data)
str(bike_data)
#dteday and instant are not requried in the modelling of the data and some values of dteday are missing also so we try not to include that data
bike_data <- bike_data[-c(0:2)]
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
train<-train[-c(12:13)]
validate<-validate[-c(12:13)]
#using dicision tree
dt=rpart(cnt ~ .,data=train,method = "anova")
dt
prediction_dt=predict(dt,validate[,-14])
#defining a function to detect the error in model
mape=function(x,xt)
{
  mean(abs((x-xt)/x))*100
}
mape(validate[,12],prediction_dt)
library(MLmetrics)#R2_score calculation
library(DMwR)
regr.eval(validate[,12],prediction_dt,stats = c("mae","rmse","mape"))
#        mae        rmse        mape 
#723.5856681 949.0313924   0.2546648
R2_Score(prediction_dt,validate[,12])
#R2_score=0.7888387



#using linear regression
#load the data
bike_data=read.csv("C:/Users/Vikash Singh/Desktop/r and python/data/day.csv")
summary(bike_data)
str(bike_data)
#dteday and instant are not requried in the modelling of the data and some values of dteday are missing also so we try not to include that data
bike_data <- bike_data[-c(0:2)]
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
train<-train[-c(12:13)]
validate<-validate[-c(12:13)]
lm_model=lm(cnt ~ .,data=train)
summary(lm_model)
predictions_lm=predict(lm_model,validate[,1:11])
predictions_lm
regr.eval(validate[,12],predictions_lm,stats = c("mae","rmse","mape"))
#      mae        rmse        mape 
#727.9427779 929.1479631   0.2318177 
R2_Score(predictions_lm,validate[,12])
#R2_score=0.7975942



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
train<-train[-c(11:12)]
validate<-validate[-c(11:12)]
lm_model2=lm(cnt ~ .,data=train)
summary(lm_model2)
predictions_lm2=predict(lm_model2,validate[,1:10])
predictions_lm2
regr.eval(validate[,11],predictions_lm2,stats = c("mae","rmse","mape"))
#       mae       rmse       mape 
#733.598349 934.099655   0.232666 
R2_Score(predictions_lm2,validate[,11])
#R2_score=0.7954311
plot(lm_model2)




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
ind <-sample(2,nrow(bike_data),replace=TRUE,prob=c(0.7,0.3))#divides the data into 70% and 30% for training and testing respectively.
train <- bike_data[ind==1,]
validate<-bike_data[ind==2,]
train<-train[-c(12:13)]
validate<-validate[-c(12:13)]

library(e1071)
svmt<-svm(cnt~.,data=train,kernel='linear',cost=1.0,epsilon=0.001)#can try epsilon values from 0.1 to 0.0001
svmt
predictions_svr=predict(svmt,validate[,1:11])
predictions_svr
mape(validate[,12],predictions_svr)
regr.eval(validate[,12],predictions_svr,stats = c("mae","rmse","mape"))
#        mae        rmse        mape 
#703.5182510 914.3685016   0.2287892 
R2_Score(predictions_svr,validate[,12])
#r2_score=0.8039821









#For Creator use only
#In case if we require the model for registered and casual users we can implement the model on both the variables and calcluate the required result out of it.
#we found out that support vector regression gives the best results and best value of r2 and rmse error is less.
## Now we apply Support vector regression model for casual and registered users
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
train<-train[-c(13:14)]
validate<-validate[-c(13:14)]

svmt_c<-svm(casual~.,data=train,kernel='linear',cost=1.0,epsilon=0.001)#can try epsilon values from 0.1 to 0.0001
svmt_c
predictions_svr_c=predict(svmt,validate[,1:11])
predictions_svr_c
mape(validate[,12],predictions_svr_c)
regr.eval(validate[,12],predictions_svr_c,stats = c("mae","rmse","mape"))
#       mae        rmse        mape 
#290.9666742 401.1489774   0.8366304 
R2_Score(predictions_svr_c,validate[,12])
#0.6790322


#For Self Use 
#Now we take registered users for prediction

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
train<-train[-c(12,14)]
validate<-validate[-c(12,14)]

svmt_r<-svm(registered~.,data=train,kernel='linear',cost=1.0,epsilon=0.001)#can try epsilon values from 0.1 to 0.0001 the values and model performance changes slightly.
svmt_r
predictions_svr_r=predict(svmt,validate[,1:11])
predictions_svr_r
mape(validate[,12],predictions_svr_r)
regr.eval(validate[,12],predictions_svr_r,stats = c("mae","rmse","mape"))
#       mae        rmse        mape 
#2831.1847734 3213.7567470    0.7505285  
R2_Score(predictions_svr_r,validate[,12])
#-2.801703
#if the r2_score is in -ve the model is performing worse, so we understand that for the registered users the model is not performing as expected.
#as data is limited the model performance can be affected.

