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
x_res<-c(model1$residuals)
y<-ts(cov19[,4])
Hebei<-filter(y, filter = c(1,-1.5094,.5094), sides =1)
ccf(x_res,Hebei,plot=T,na.action=na.omit,lag.max = 30,ylab = "cross-correlation")
ccf(x,y,na.action=na.omit,plot=F,lag.max = 30) 

q<-ts(mig[,10])
p_d_q<-auto.arima(q,method="ML")
p_d_q
order=c(arimaorder(p_d_q))
model1<-arima(q,order=order,method="ML") 
x_res<-c(model1$residuals)
y<-ts(cov19[,10]) 
y<-filter(y, filter = c(1,-2,1), sides =1)
x2=round(rnorm(51,mean = 0,sd=1))
x3<-ts(x2)
Shangdong<-(-0.8862)*x3+y
b<-ccf(x_res,Shangdong,plot=T,na.action=na.omit,lag.max = 30,ylab = "cross-correlation")
b
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

library(jsonlite);library(rjson);library(RJSONIO)
library(ggplot2);library(RColorBrewer);library(lubridate)
library(plyr);library(dplyr);library(rgdal)
library(sf);library(geojsonio);library(sp);library(ggthemes)

x<-readOGR("省级行政区.shp",stringsAsFactors=FALSE,encoding="UTF-8")

prov_map<-fortify(x)

des_map=read.csv("des_map.csv")

totalp=data.frame(id=des_map$id,pnum=des_map$lag,
                  Timelag=cut(des_map$lag,breaks=c(0,8,15,22,35),
                                  labels=c("<7","8-14","15-21",">=22"),
                                  order = TRUE,include.lowest = T,right = F))

ttp<-merge.data.frame(prov_map,totalp,by.prov_map="id",by.totalp="id")

ggplot()+
  geom_polygon(data=ttp,aes(x=long,y=lat,group=group,
                            fill=Timelag),colour="black",size=0.25)+
  scale_fill_manual(values = brewer.pal(4,"Blues"))

totalp1=data.frame(id=des_map$id,pnum=des_map$ccf,
                  Timelag=cut(des_map$ccf,breaks=c(0,0.3,0.4,0.5,0.6,1),
                              labels=c("<0.3","0.3-0.4","0.4-0.5","0.5-0.6",">=0.6"),
                              order = TRUE,include.lowest = T,right = F))

ttp1<-merge.data.frame(prov_map,totalp1,by.prov_map="id",by.totalp1="id")

ggplot()+
  geom_polygon(data=ttp1,aes(x=long,y=lat,group=group,
                            fill=Timelag),colour="black",size=0.25)+
  scale_fill_manual(values = brewer.pal(5,"Reds"))

line(mig[,1],mig[,33],type="o",xlab="日期",ylab="新增病例"）
      
     