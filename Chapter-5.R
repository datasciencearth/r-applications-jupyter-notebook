library(quantreg)
rq()

library(datasets)
data("anscombe")

plot(anscombe$x1,anscombe$y1)

model_lm<-lm(y1~x1,data = anscombe)

model_qr<-rq(y1~x1,data = anscombe,tau=0.5)

orderx<-order(anscombe$x1)

lines(anscombe$x1[orderx],model_lm$fitted.values[orderx])

lines(anscombe$x1[orderx],model_qr$fitted.values[orderx],col="blue")

mean((anscombe$y1-model_lm$fitted.values)^2)
mean((anscombe$y1-model_qr$fitted.values)^2)


plot(anscombe$x1,anscombe$y3)

model_lm<-lm(y3~x1,data = anscombe)

model_qr<-rq(y3~x1,data = anscombe,tau=0.5)

orderx<-order(anscombe$x1)

lines(anscombe$x1[orderx],model_lm$fitted.values[orderx])

lines(anscombe$x1[orderx],model_qr$fitted.values[orderx],col="blue")


plot(anscombe$x1,anscombe$y1)

model_qr_01<-rq(y1~x1,data = anscombe,tau=0.1)

lines(anscombe$x1[orderx],model_qr_01$fitted.values[orderx],col="red")

model_qr_05<-rq(y1~x1,data = anscombe,tau=0.5)

lines(anscombe$x1[orderx],model_qr_05$fitted.values[orderx],col="green")

model_qr_09<-rq(y1~x1,data = anscombe,tau=0.9)

lines(anscombe$x1[orderx],model_qr_09$fitted.values[orderx],col="blue")

summary(model_qr_05)
