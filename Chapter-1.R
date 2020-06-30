ev_fiyati<-c(245,312,279,308,199,219,405,324,319,255)
ev_buyuklugu<-c(130,148,157,174,102,143,218,227,132,157)
plot(ev_buyuklugu,ev_fiyati)
model<-lm(ev_fiyati~ev_buyuklugu)
summary(model)

fitted(model)
model$residuals

abline(model)

par(mfrow=c(2,2))
plot(model)

shapiro.test(model$residuals)
install.packages("lmtest")
library(lmtest)
bptest(ev_fiyati~ev_buyuklugu)

#--------------------------------------

library(tidyverse)
library(broom)
library(datarium)

plot(marketing$youtube,marketing$sales)

model2<-lm(sales~youtube,data=marketing)
summary(model2)

abline(model2)

par(mfrow=c(2,2))
plot(model2)

shapiro.test(model2$residuals)
bptest(marketing$sales~marketing$youtube)

model3<-lm(log(sales)~youtube,data=marketing)
summary(model3)

shapiro.test(model3$residuals)
bptest(log(marketing$sales)~marketing$youtube)

par(mfrow=c(2,2))
plot(model3)

abline(model3)
