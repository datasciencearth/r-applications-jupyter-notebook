install.packages("datarium")
library(datarium)
data(marketing)

#descriptive statistics
summary(marketing)
plot(marketing)

#linear model
model<-lm(sales~.,data=marketing)
summary(model)


##stepwise regression
install.packages("olsrr")
library(olsrr)
stepw_forward<-ols_step_forward_p(model)
stepw_backward<-ols_step_backward_p(model)


##multicollinearity
install.packages("car")
library(car)
vif(model)


#model2
model2<-lm(sales~youtube+facebook,data=marketing)
summary(model2)

par(mfrow=c(2,2))
plot(model2)

shapiro.test(model2$residuals)
bptest(sales~youtube+facebook,data=marketing)

#model3
youtube_new<-sqrt(marketing$youtube)

model3<-lm(marketing$sales~youtube_new+marketing$facebook)
summary(model3)

par(mfrow=c(2,2))
plot(model3)

shapiro.test(model3$residuals)
bptest(marketing$sales~youtube_new+marketing$facebook)


#model4
marketing_new<-marketing[-c(131),]
youtube_x<-sqrt(marketing_new$youtube)

model4<-lm(marketing_new$sales~youtube_x+marketing_new$facebook)
summary(model4)

par(mfrow=c(2,2))
plot(model4)

shapiro.test(model4$residuals)
bptest(marketing_new$sales~youtube_x+marketing_new$facebook)
