setwd("C:/Users/cbb/Desktop/china")
require(forecast);require(lmtest);library(tseries)
library(fUnitRoots);library(timeSeries)library(graphics);library(stats)
sum_cov<-read.csv("新增疫情分类数据.csv",encoding="UTF-8")
sum_mig<-read.csv("迁出分类.csv",encoding="UTF-8")
mig_19<-read.csv("迁出分类去年.csv",encoding="UTF-8")
par(mfrow=c(1,3))
#------------------------------第一类------------------------------------------
sum1<-ts(sum_cov$s1) #读取疫情数据
sum1_34<-sum1[1:37]
adf.test(sum1_34)

mig1_34<-ts(sum_mig$s1[1:37] )    #读取迁移数据
mig19_34<-ts(mig_19$s1[1:37] )

p_d_q<-auto.arima(mig1_34)      #ccf部分  mig1_34
coeftest(p_d_q)
order=c(arimaorder(p_d_q))
model<-arima(mig1_34,order=order,method="ML") 
x<-c(model$residuals)
y<-filter(sum1_34, filter = c(1,-2.5421,2.0842,-0.5421), sides =1)
ccf(x,y,plot=T,na.action=na.omit,lag.max = 30,ylab="CCF")
ccf(x,y,plot=F,na.action=na.omit,lag.max = 30)

mm1<- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,mig1_34)   #制作滞后阶数
mmm1<- embed(mm1,14)
m1_7<-mmm1[,8]
m1_13<-mmm1[,14]
m1_11<-mmm1[,12]
mmmm1<-cbind(m1_16,m1_8,m1_11,m1_13)

f1<-auto.arima(sum1_34, xreg = m1_7)   #ARIMAX模型
coeftest(f1)                               #看p值
summary(f1)


p_m1_7<-c(sum_mig$s1[31:44])      #28:44
p_m1_11<-c(sum_mig$s1[24:40])
p_m1_13<-c(sum_mig$s1[22:38])
p_m1_16<-c(sum_mig$s1[19:35])

y_m1_7<-c(mig_19$s1[38:51])    #31:44

fnn1<- forecast(f1, xreg = p_m1_7, h = 14)
fnn1
plot(forecast(f1, xreg = p_m1_7))
plot(sum1)
fnn11<- forecast(f1, xreg = y_m1_7, h = 14)
fnn11
plot(forecast(f1, xreg = y_m1_7))

fn <- matrix(c(p_m1_7,p_m1_19),
             ncol = 2, nrow = 17)
#------------------------------第二类------------------------------------------
sum2<-ts(sum_cov$s2)   #读取疫情数据
sum2_37<-sum2[1:37]
sum2_34<-sum2[1:34]
adf.test(sum2_34)

mig2_37<-ts(sum_mig$s2[1:37] )  #读取迁移数据
mig2_34<-ts(sum_mig$s2[1:34] ) 

p_d_q<-auto.arima(mig2_37)      #ccf部分  mig2_37
coeftest(p_d_q)
order=c(arimaorder(p_d_q))
model<-arima(mig2_37,order=order,method="ML") 
x<-c(model$residuals)
y<-filter(sum2_37, filter = c(1,-2.5100,2.020,-0.5100), sides =1)
ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,y,plot=F,na.action=na.omit,lag.max = 30)

p_d_q<-auto.arima(mig2_34)      #ccf部分  mig2_34
coeftest(p_d_q)
order=c(arimaorder(p_d_q))
model<-arima(mig2_34,order=order,method="ML") 
x<-c(model$residuals)
y<-filter(sum2_34, filter = c(1,-2.5087,2.0174,-0.5087), sides =1)
ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,y,plot=F,na.action=na.omit,lag.max = 30)

mm2<- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,mig2_34)   #制作滞后阶数
mmm2<- embed(mm2,17)
m2_8<-mmm2[,9]
m2_11<-mmm2[,12]
m2_16<-mmm2[,17]
mmmm2<-cbind(m2_8,m2_11,m2_16)

f2<-auto.arima(sum2_34, xreg = mmmm2[,3])   #ARIMAX模型
coeftest(f2)                                  #看p值
summary(f2)

p_m2_16<-c(sum_mig$s2[19:35])    #正确19:35
p_m2_8<-c(sum_mig$s2[27:43])
p_m2_19<-c(sum_mig$s2[16:32])

fnn2<- forecast(f2, xreg = p_m2_16, h = 17)
fnn2
plot(forecast(f2, xreg = p_m2_16))
plot(sum2)
y_m2_16<-c(mig_19$s2[35:51])
fnn22<- forecast(f2, xreg = y_m2_16, h = 17)
fnn22
plot(forecast(f2, xreg = y_m2_16))
fn <- matrix(c(p_m2_7,p_m2_19),
             ncol = 2, nrow = 17)



#------------------------------第三类------------------------------------------
sum3<-ts(sum_cov$s3)#读取疫情数据
sum3_37<-sum3[1:37]
adf.test(sum3_37)


mig3_37<-ts(sum_mig$s3[1:37] )    #读取迁移数据

p_d_q<-auto.arima(mig3_37)      #ccf部分  mig3_34
coeftest(p_d_q)
order=c(arimaorder(p_d_q))
model<-arima(mig3_37,order=order,method="ML") 
x<-c(model$residuals)
y<-filter(sum3_37, filter = c(1,-2.8099,2.6198,-0.8099), sides =1)
y<-filter(sum3_37, filter = c(1,-2.8099,2.916,-1.6422,0.776,-0.2399), sides =1)
ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,y,plot=F,na.action=na.omit,lag.max = 30)

mm3<- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,mig3_37)   #制作滞后阶数
mmm3<- embed(mm3,27)
m3_13<-mmm3[,14]
m3_15<-mmm3[,16]
m3_18<-mmm3[,19]
m3_26<-mmm3[,27]

mmmm3<-cbind(m3_13,m3_15,m3_18,m3_26)

f3<-auto.arima(sum3_37, xreg = mmmm3[,4])   #ARIMAX模型
coeftest(f3)                                  #看p值
summary(f3)

p_m3_15<-c(sum_mig$s3[31:47])
p_m3_26<-c(sum_mig$s3[12:28])



fnn3<- forecast(f3, xreg = p_m3_26, h = 17)
fnn3
plot(forecast(f3, xreg = p_m3_26))
plot(sum3)
y_m3_26<-c(mig_19$s3[35:51])    #12:28
fnn33<- forecast(f3, xreg = y_m3_26, h = 17)
fnn33
plot(forecast(f3, xreg = y_m3_26))
#------------------------------湖北------------------------------------------
sum4<-ts(sum_cov$s4)   #读取疫情数据
sum4_37<-sum2[1:37]
sum4_34<-sum2[1:34]
adf.test(sum4_34)

mig4_37<-ts(sum_mig$s4[1:37] )  #读取迁移数据
mig4_34<-ts(sum_mig$s4[1:34] ) 

p_d_q<-auto.arima(mig2_37)      #ccf部分  mig2_37
coeftest(p_d_q)
order=c(arimaorder(p_d_q))
model<-arima(mig4_37,order=order,method="ML") 
x<-c(model$residuals)
y<-filter(sum4_37, filter = c(1,-2.5480,2.0960,-0.5480), sides =1)
ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,y,plot=F,na.action=na.omit,lag.max = 30)

p_d_q<-auto.arima(mig4_34)      #ccf部分  mig2_34
coeftest(p_d_q)
order=c(arimaorder(p_d_q))
model<-arima(mig4_34,order=order,method="ML") 
x<-c(model$residuals)
y<-filter(sum4_34, filter = c(1,-2,1), sides =1)
x2=round(rnorm(34,mean = 0,sd=1))
x3<-ts(x2)
a<-(0.4162)*x3+y
ccf(x,a,plot=T,na.action=na.omit,lag.max = 30)
ccf(x,y,plot=T,na.action=na.omit,lag.max = 30)

mm4<- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,mig4_37)   #制作滞后阶数
mmm4<- embed(mm4,20)
m4_8<-mmm4[,9]
m4_11<-mmm4[,12]
m4_14<-mmm4[,15]
m4_16<-mmm4[,17]
m4_19<-mmm4[,20]
mmmm4<-cbind(m4_8,m4_11,m4_14,m4_16,m4_19)

f4<-auto.arima(sum4_37, xreg = mmmm4[,4:5])   #ARIMAX模型
coeftest(f4)                                  #看p值
summary(f4)

p_m4_9<-c(sum_mig$s4[30:46])
p_m4_11<-c(sum_mig$s4[27:43])
p_m4_16<-c(sum_mig$s4[19:35])
p_m4_19<-c(sum_mig$s4[16:32])

fnn4<- forecast(f4, xreg = p_m4_19, h = 17)
fnn4
