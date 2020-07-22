library(WRS2)
age<-c(5.2,8.8,10.5,10.6,10.4,1.8,12.7,15.6,5.8,1.9,2.2,4.8,7.9,5.2,0.9,
       11.8,7.9,1.5,10.6,8.5,11.1,12.8,11.3,1.0,14.5,11.9,8.1,13.8,15.5,9.8,11.0,
       12.4,11.1,5.1,4.8,4.2,6.9,13.2,9.9,12.5,13.2,8.9,10.8)

c_peptide<-c(4.8,4.1,5.2,5.5,5.0,3.4,3.4,4.9,5.6,3.7,3.9,4.5,4.8,4.9,3.0,
             4.6,4.8,5.5,4.5,5.3,4.7,6.6,5.1,3.9,5.7,5.1,5.2,3.7,4.9,4.8,4.4,5.2,5.1,4.6,
             3.9,5.1,5.1,6.0,4.9,4.1,4.6,4.9,5.1)

plot(age,c_peptide,xlab = "AGE",ylab = "C-PEPTIDE", pch="*")

kerreg(age,c_peptide,pch="*",expand = 0.5)

plot(age,c_peptide,xlab = "AGE",ylab = "C-PEPTIDE", pch="*")
lo_mod<-loess(c_peptide~age,span=0.75,degree = 2)

orderx<-order(age)
lines(age[orderx],lo_mod$fitted[orderx])

rungen(age,c_peptide,est=tmean,fr=1,LP=TRUE,pch="*")

r1<-rungen(age,c_peptide,est=quantile,fr=1,LP=TRUE,pch="*",prob=0.1,pyhat = T)
r2<-rungen(age,c_peptide,est=quantile,fr=1,LP=TRUE,pch="*",prob=0.5,pyhat = T)
r3<-rungen(age,c_peptide,est=quantile,fr=1,LP=TRUE,pch="*",prob=0.9,pyhat = T)

plot(age,c_peptide,xlab = "AGE",ylab = "C-PEPTIDE", pch="*")
orderx<-order(age)
lines(age[orderx],r1$output)
lines(age[orderx],r2$output)
lines(age[orderx],r3$output)

library(cobs)
sbs1<-cobs(age,c_peptide,tau=0.1,lambda = 0)
sbs2<-cobs(age,c_peptide,tau=0.5,lambda = 0)
sbs3<-cobs(age,c_peptide,tau=0.9,lambda = 0)

plot(age,c_peptide,xlab = "AGE",ylab = "C-PEPTIDE", pch="*")
orderx<-order(age)
lines(age[orderx],sbs1$fitted[orderx])
lines(age[orderx],sbs2$fitted[orderx])
lines(age[orderx],sbs3$fitted[orderx])
