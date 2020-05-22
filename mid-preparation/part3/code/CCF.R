setwd("C:/Users/cbb/Desktop/china")
library(forecast)
library(tseries)
library(fUnitRoots)
library(timeSeries)
mig<-read.csv("迁出指数分省.csv",encoding="UTF-8")
cov19<-read.csv("新增确诊.csv",encoding="UTF-8")


  q<-ts(mig[,4])
  p_d_q<-auto.arima(q)
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,4])
  y<-filter(y, filter = c(1,-1.5094,.5094), sides =1)
  ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,y,na.action=na.omit,plot=F,lag.max = 30) 
  
  q<-ts(mig[,5])
  p_d_q<-auto.arima(q)
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,5])
  y<-filter(y, filter = c(1,-1.3488,.3488), sides =1)
  ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,y,na.action=na.omit,plot=F,lag.max = 30) 
  
  q<-ts(mig[,6])
  p_d_q<-auto.arima(q)
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,6])
  y<-filter(y, filter = c(1,-1.5421,.8245,-0.4355,.1531), sides =1)
  ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,y,na.action=na.omit,lag.max = 30,plot=F)
  

  
  q<-ts(mig[,12])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,12]) 
  y<-filter(y, filter = c(1,-1.3877,.3877), sides =1)
  ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
  ccf(d,y,na.action=na.omit,plot=F,lag.max = 30)
  
  q<-ts(mig[,14])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,14]) 
  y<-filter(y, filter = c(1,-1.4944,.4944), sides =1)
  ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)
  
  q<-ts(mig[,17])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,17]) 
  y<-filter(y, filter = c(1,-1.4629,.4629), sides =1)
  ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)
  
  q<-ts(mig[,19])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,19]) 
  y<-filter(y, filter = c(1,-1.5871,.5871), sides =1)
  ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)
  
  q<-ts(mig[,21])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,21]) 
  y<-filter(y, filter = c(1,-1.5691,.5691), sides =1)
  ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)
  
  q<-ts(mig[,22])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,22]) 
  y<-filter(y, filter = c(1,-1.4896,.4896), sides =1)
  ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)
  
  q<-ts(mig[,23])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,23]) 
  y<-filter(y, filter = c(1,-1.8169,1.0718,-0.4631,.2082), sides =1)
  ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)
  
  q<-ts(mig[,25])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,25]) 
  y<-filter(y, filter = c(1,-1.4927,.4927), sides =1)
  ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)
  
  q<-ts(mig[,27])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,27]) 
  y<-filter(y, filter = c(1,-1.4385,.4385), sides =1)
  ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)
  
  q<-ts(mig[,28])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,28]) 
  y<-filter(y, filter = c(1,-1.9703,1.483,-1.0102,.4975), sides =1)
  ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)
  
  q<-ts(mig[,15])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,15]) 
  y<-filter(y, filter = c(1,-1), sides =1)
  x2=round(rnorm(51,mean = 0,sd=1))
  x3<-ts(x2)
  a<-0.3935*x3+y
  b<-ccf(x,a,na.action=na.omit,plot=F,lag.max = 30)
  f<-c(b$acf)
  f
  for (i in 1:100){ 
    x3<-round(rnorm(51,mean = 0,sd=1))
    x4<-ts(x3)
    a<-(.3935)*x4+y
    b<-ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
    b<-c(b$acf)
    f<-cbind(f,b)
    f1<-f[19,]
    f11<-mean(f1)
  }
  

  q<-ts(mig[,2])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,2]) 
  y<-filter(y, filter = c(1,-2,1), sides =1)
  x2=round(rnorm(51,mean = 0,sd=1))
  x3<-ts(x2)
  a<-(-0.5351)*x3+y-0.3037*x3
  ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,a,na.action=na.omit,plot=F,lag.max = 30)
  ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)
  
  q<-ts(mig[,3])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,3]) 
  y<-filter(y, filter = c(1,-2,1), sides =1)
  x2=round(rnorm(51,mean = 0,sd=1))
  x3<-ts(x2)
  a<-(0.8663)*x3+y
  ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,a,na.action=na.omit,plot=F,lag.max = 30)
  ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)
  
  q<-ts(mig[,10])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,10]) 
  y<-filter(y, filter = c(1,-2,1), sides =1)
  x2=round(rnorm(51,mean = 0,sd=1))
  x3<-ts(x2)
  a<-(-0.8862)*x3+y
  b<-ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,a,na.action=na.omit,plot=F,lag.max = 30)
  f<-c(b$acf)
  f
  for (i in 1:100){ 
    x2=round(rnorm(51,mean = 0,sd=1))
    x3<-ts(x2)
    a<-(-0.8862)*x3+y
    b<-ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
    b<-c(b$acf)
    f<-cbind(f,b)
    f1<-f[7,]
    f11<-mean(f1)
  }
  
  q<-ts(mig[,30])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,30]) 
  y<-filter(y, filter = c(1,-2,1), sides =1)
  x2=round(rnorm(51,mean = 0,sd=1))
  x3<-ts(x2)
  a<-(0.8842)*x3+y
  ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,a,na.action=na.omit,plot=F,lag.max = 30)
  ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)
  ccf(q,y)
  
  q<-ts(mig[,29])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,29]) 
  y<-filter(y, filter = c(1,-1.4383,-0.4928,1.093,.0456,-0.75), sides =1)
  x2=round(rnorm(51,mean = 0,sd=1))
  x3<-ts(x2)
  a<-(-0.8938)*x3+y
  ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,a,na.action=na.omit,plot=F,lag.max = 30)
  ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)
  ccf(q,y)
  
  q<-ts(mig[,31])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,31]) 
  y<-filter(y, filter = c(1,-1.787,0.256,.7813,-.1829,-0.0677), sides =1)
  x2=round(rnorm(51,mean = 0,sd=1))
  x3<-ts(x2)
  a<-(-0.9143)*x3+y
  ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,a,na.action=na.omit,plot=F,lag.max = 30)
  ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)
  ccf(q,y)
  
  q<-ts(mig[,7])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,7]) 
  y<-filter(y, filter = c(1,-1), sides =1)
  x2=round(rnorm(51,mean = 0,sd=1))
  x2<-ts(x2)
  x3<-lag(x2)
  a<-(0.3614)*x2+y+0.687*x3
  b<-ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,a,na.action=na.omit,plot=F,lag.max = 30)
  f<-c(b$acf)
  for (i in 1:200){ 
    x2=round(rnorm(51,mean = 0,sd=1))
    x2<-ts(x2)
    x3<-lag(x2)
    a<-(0.3614)*x2+y+0.687*x3
    b<-ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
    b<-c(b$acf)
    f<-cbind(f,b)
    f<-f[21,]
    f<-mean(f)}

q<-ts(mig[,2])
p_d_q<-auto.arima(q,method="ML")
p_d_q
order=c(arimaorder(p_d_q))
model1<-arima(q,order=order,method="ML") 
x<-c(model1$residuals)
y<-ts(cov19[,2]) 
y<-filter(y, filter = c(1,-2,1), sides =1)
x2=round(rnorm(51,mean = 0,sd=1))
x2<-ts(x2)
x3<-lag(x2)
a<-(-0.5351)*x2+y-0.3037*x3
ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,a,na.action=na.omit,plot=F,lag.max = 30)

q<-ts(mig[,11])
p_d_q<-auto.arima(q,method="ML")
p_d_q
order=c(arimaorder(p_d_q))
model1<-arima(q,order=order,method="ML") 
x<-c(model1$residuals)
y<-ts(cov19[,11]) 
y<-filter(y, filter = c(1,-1), sides =1)
x2=round(rnorm(51,mean = 0,sd=1))
x2<-ts(x2)
x3<-lag(x2)
a<-(0.6262)*x2+y+0.456*x3
b<-ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,a,na.action=na.omit,plot=F,lag.max = 30)
f<-c(b$acf)
for (i in 1:200){ 
  x2=round(rnorm(51,mean = 0,sd=1))
  x2<-ts(x2)
  x3<-lag(x2)
  a<-(0.6262)*x2+y+0.456*x3
  b<-ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
  c<-c(b$acf)
  f<-cbind(f,c)
  f1<-f[10,]
  f2<-mean(f1)}

q<-ts(mig[,16])
p_d_q<-auto.arima(q,method="ML")
p_d_q
order=c(arimaorder(p_d_q))
model1<-arima(q,order=order,method="ML") 
x<-c(model1$residuals)
y<-ts(cov19[,16]) 
y<-filter(y, filter = c(1,-1), sides =1)
x2=round(rnorm(51,mean = 0,sd=1))
x2<-ts(x2)
x3<-lag(x2)
a<-(0.8185)*x2+y+0.3275*x3
ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,a,na.action=na.omit,plot=F,lag.max = 30)
ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)  

q<-ts(mig[,24])
p_d_q<-auto.arima(q,method="ML")
p_d_q
order=c(arimaorder(p_d_q))
model1<-arima(q,order=order,method="ML") 
x<-c(model1$residuals)
y<-ts(cov19[,24]) 
y<-filter(y, filter = c(1,-2,1), sides =1)
x2=round(rnorm(51,mean = 0,sd=1))
x2<-ts(x2)
x3<-lag(x2)
a<-(-0.5427)*x2+y-0.3218*x3
ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,a,na.action=na.omit,plot=F,lag.max = 30)
ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)  

q<-ts(mig[,26])
p_d_q<-auto.arima(q,method="ML")
p_d_q
order=c(arimaorder(p_d_q))
model1<-arima(q,order=order,method="ML") 
x<-c(model1$residuals)
y<-ts(cov19[,26]) 
y<-filter(y, filter = c(1,-1), sides =1)
x2=round(rnorm(51,mean = 0,sd=1))
x2<-ts(x2)
x3<-lag(x2)
a<-(-0.7)*x2+y+0.2541*x3
ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,a,na.action=na.omit,plot=F,lag.max = 30)
ccf(x,y,na.action=na.omit,plot=F,lag.max = 30) 

q<-ts(mig[,8])
p_d_q<-auto.arima(q,method="ML")
p_d_q
order=c(arimaorder(p_d_q))
model1<-arima(q,order=order,method="ML") 
x<-c(model1$residuals)
y<-ts(cov19[,8]) 
y<-filter(y, filter = c(1,-0.3971,-0.6029), sides =1)
x2=round(rnorm(51,mean = 0,sd=1))
x2<-ts(x2)
x3<-lag(x2)
a<-(1.044)*x2+y+0.6496*x3
ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,a,na.action=na.omit,plot=F,lag.max = 30)
ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)

q<-ts(mig[,9])
p_d_q<-auto.arima(q,method="ML")
p_d_q
order=c(arimaorder(p_d_q))
model1<-arima(q,order=order,method="ML") 
x<-c(model1$residuals)
y<-ts(cov19[,9]) 
y<-filter(y, filter = c(1,-0.7063,-0.2937), sides =1)
x2=round(rnorm(51,mean = 0,sd=1))
x2<-ts(x2)
x3<-lag(x2)
a<-(1.1821)*x2+y+0.8691*x3
ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,a,na.action=na.omit,plot=F,lag.max = 30)
ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)

q<-ts(mig[,20])
p_d_q<-auto.arima(q,method="ML")
p_d_q
order=c(arimaorder(p_d_q))
model1<-arima(q,order=order,method="ML") 
x<-c(model1$residuals)
y<-ts(cov19[,20]) 
y<-filter(y, filter = c(1,-0.2164,-0.7836), sides =1)
x2=round(rnorm(51,mean = 0,sd=1))
x2<-ts(x2)
x3<-lag(x2)
a<-(1.5216)*x2+y+0.8106*x3
ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,a,na.action=na.omit,plot=F,lag.max = 30)
ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)

q<-ts(mig[,13])
p_d_q<-auto.arima(q,method="ML")
p_d_q
order=c(arimaorder(p_d_q))
model1<-arima(q,order=order,method="ML") 
x<-c(model1$residuals)
y<-ts(cov19[,13]) 
y<-filter(y, filter = c(1,-1,.5324,-0.5324), sides =1)
x2=round(rnorm(51,mean = 0,sd=1))
x2<-ts(x2)
x3<-lag(x2)
a<-(.648)*x2+y+0.9613*x3
ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,a,na.action=na.omit,plot=F,lag.max = 30)
ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)

q<-ts(mig[,18])
p_d_q<-auto.arima(q,method="ML")
p_d_q
order=c(arimaorder(p_d_q))
model1<-arima(q,order=order,method="ML") 
x<-c(model1$residuals)
y<-ts(cov19[,18]) 
y<-filter(y, filter = c(1,-1,.5331,-0.5331), sides =1)
y<-filter(y, filter = c(1,-0.8816,0.4147,-0.47,-0.0631), sides =1)
x2=round(rnorm(51,mean = 0,sd=1))
x2<-ts(x2)
x3<-lag(x2)
a<-(.6204)*x2+y+0.8698*x3
ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,a,na.action=na.omit,plot=F,lag.max = 30)
ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)

q<-ts(mig[,18])
p_d_q<-auto.arima(q,method="ML")
p_d_q
order=c(arimaorder(p_d_q))
model1<-arima(q,order=order,method="ML") 
x<-c(model1$residuals)
y<-ts(cov19[,18]) 
y<-filter(y, filter = c(1,-0.8816,0.4147,-0.47,-0.0631), sides =1)
x=round(rnorm(51,mean = 0,sd=1))
x<-round(rnorm(51,mean = 0,sd=1))
for (i in 1:5)
  x<-round(rnorm(51,mean = 0,sd=1))
  x<-ts(x)
  x3<-lag(x2)
  a<-(.6204)*x2+y+0.8698*x3
  ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)

  q<-ts(mig[,33])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,33]) 
  y<-filter(y, filter = c(1,-1), sides =1)
  x2=round(rnorm(51,mean = 0,sd=1))
  x2<-ts(x2)
  a<-(.422)*x2+y
  ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,a,na.action=na.omit,plot=F,lag.max = 30)
  ccf(x,y,na.action=na.omit,plot=F,lag.max = 30)
  
  q<-ts(mig[,33])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,33]) 
  y<-filter(y, filter = c(1,-1), sides =1)
  d<-ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
  f<-c(d$acf)
  for (i in 1:100){ 
    x3<-round(rnorm(51,mean = 0,sd=1))
    x4<-ts(x3)
    a<-(.422)*x4+y
    b<-ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
    b1<-c(b$acf)
    f<-cbind(f,b1)
    f1<-f[12,]
    f2<-mean(f1)
  }
  
  q<-ts(mig[,32])
  p_d_q<-auto.arima(q,method="ML")
  p_d_q
  order=c(arimaorder(p_d_q))
  model1<-arima(q,order=order,method="ML") 
  x<-c(model1$residuals)
  y<-ts(cov19[,32]) 
  y<-filter(y, filter = c(1,-1), sides =1)
  x2=round(rnorm(51,mean = 0,sd=1))
  x3<-ts(x2)
  a<-(0.3993)*x3+y
  b<-ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
  ccf(x,a,na.action=na.omit,plot=F,lag.max = 30)
  f<-c(b$acf)
  f
  for (i in 1:200){ 
    x2=round(rnorm(51,mean = 0,sd=1))
    x3<-ts(x2)
    a<-(0.3993)*x3+y
    b<-ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
    b<-c(b$acf)
    f<-cbind(f,b)
    f1<-f[12,]
    f11<-mean(f1)
  }

