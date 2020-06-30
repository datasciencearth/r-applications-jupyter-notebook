library(tidyverse)
library(caret)
library(ggplot2)
library(splines)
library(mgcv)

data("Boston",package="MASS")
summary(Boston)

set.seed(123)
training.samples<-Boston$medv %>%
  createDataPartition(p=0.8,list=FALSE)
train.data<-Boston[training.samples, ]
test.data<-Boston[-training.samples, ]

ggplot(train.data, aes(lstat,medv))+
  geom_point()+
  stat_smooth()

plot(Boston$lstat,Boston$medv)

#linear regression
model<-lm(medv~lstat,data=train.data)
predictions<-model %>% predict(test.data)
data.frame(
  RMSE=RMSE(predictions,test.data$medv),
  R2=R2(predictions,test.data$medv)
)

ggplot(train.data, aes(lstat,medv))+
  geom_point()+
    stat_smooth(method=lm, formula=y~x)


##polynomial regression

model<-lm(medv~poly(lstat,5),data=train.data)
summary(model)
predictions<-model %>% predict(test.data)
data.frame(
  RMSE=RMSE(predictions,test.data$medv),
  R2=R2(predictions,test.data$medv)
)

ggplot(train.data, aes(lstat,medv))+
  geom_point()+
  stat_smooth(method=lm,formula=y~poly(x,5))

##log transformation

model<-lm(medv~log(lstat),data=train.data)
summary(model)
predictions<-model %>% predict(test.data)
data.frame(
  RMSE=RMSE(predictions,test.data$medv),
  R2=R2(predictions,test.data$medv)
)


ggplot(train.data, aes(lstat,medv))+
  geom_point()+
  stat_smooth(method=lm,formula=y~log(x))

##spline regression with R default quantile estimator

knots<-quantile(train.data$lstat,p=c(0.10,0.50,0.90))
model<-lm(medv~bs(lstat, knots=knots),data=train.data)
summary(model)
predictions<-model %>% predict(test.data)
data.frame(
  RMSE=RMSE(predictions,test.data$medv),
  R2=R2(predictions,test.data$medv)
)

ggplot(train.data, aes(lstat,medv))+
  geom_point()+
  stat_smooth(method=lm,formula=y~splines::bs(x,df=3))

##generalized additive models

model<-gam(medv~s(lstat),data=train.data)
summary(model)
predictions<-model %>% predict(test.data)
data.frame(
  RMSE=RMSE(predictions,test.data$medv),
  R2=R2(predictions,test.data$medv)
)


ggplot(train.data, aes(lstat,medv))+
  geom_point()+
  stat_smooth(method=gam,formula=y~s(x))
