library(tidyverse)
library(caret)
library(mlbench)
library(klaR)
library(ggplot2)
library(magrittr)
library(lattice)
library(e1071)
library(datasets)

data("PimaIndiansDiabetes2")
summary(PimaIndiansDiabetes2)
PimaIndiansDiabetes2<-na.omit(PimaIndiansDiabetes2)
summary(PimaIndiansDiabetes2)
plot(PimaIndiansDiabetes2)

set.seed(123)
training.samples<-PimaIndiansDiabetes2$diabetes %>%
  createDataPartition(p=0.8,list=FALSE)
train.data<-PimaIndiansDiabetes2[training.samples, ]
test.data<-PimaIndiansDiabetes2[-training.samples, ]

model<-NaiveBayes(diabetes~.,data=train.data)
predictions<-model %>% predict(test.data)
mean(predictions$class==test.data$diabetes)

predictions2<-model %>% predict(train.data)
mean(predictions2$class==train.data$diabetes)

##-------------------------------------------------------------------------------------

data2<-data.frame(diabetes=PimaIndiansDiabetes2$diabetes,pregnant=PimaIndiansDiabetes2$pregnant,glucose=PimaIndiansDiabetes2$glucose,mass=PimaIndiansDiabetes2$mass,pedigree=PimaIndiansDiabetes2$pedigree,age=PimaIndiansDiabetes2$age)

set.seed(123)
training.samples<-data2$diabetes %>%
  createDataPartition(p=0.8,list=FALSE)
train.data<-data2[training.samples, ]
test.data<-data2[-training.samples, ]

model<-NaiveBayes(diabetes~.,data=train.data)
predictions<-model %>% predict(test.data)
mean(predictions$class==test.data$diabetes)

model<-naiveBayes(diabetes~.,data=train.data,laplace = 1)
predictions<-model %>% predict(test.data)
mean(predictions==test.data$diabetes)


##----------------------------------------------------------------------------

data(iris)

summary(iris)

set.seed(123)
training.samples<-iris$Species %>%
  createDataPartition(p=0.8,list=FALSE)
train.data<-iris[training.samples, ]
test.data<-iris[-training.samples, ]

model<-NaiveBayes(Species~.,data=train.data)
predictions<-model %>% predict(test.data)
mean(predictions$class==test.data$Species)


