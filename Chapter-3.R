library(mlbench)
data("PimaIndiansDiabetes2")

summary(PimaIndiansDiabetes2)

PimaIndiansDiabetes2<-na.omit(PimaIndiansDiabetes2)
summary(PimaIndiansDiabetes2)

plot(PimaIndiansDiabetes2)

model<-glm(diabetes~glucose,data=PimaIndiansDiabetes2,family = binomial)
summary(model)


model<-glm(diabetes~.,data=PimaIndiansDiabetes2,family = binomial)
summary(model)

library(MASS)
stepAIC(model,trace=FALSE)

model<-glm(diabetes~pregnant+glucose+mass+pedigree+age,data=PimaIndiansDiabetes2,family = binomial)


plot(model,which=4,id.n=3)

std.resid<-rstandard(model)
z<-abs(std.resid)>3
table(z)["TRUE"]

car::vif(model)

probabilities<-model %>% predict(PimaIndiansDiabetes2,type="response")
predicted.classes<-ifelse(probabilities>0.6,"pos","neg")
observed.classes<-PimaIndiansDiabetes2$diabetes
mean(predicted.classes==observed.classes)
